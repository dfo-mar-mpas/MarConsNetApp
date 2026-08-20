assess_protection_coverage <- function(
    data,
    areas,
    indicator_var_name,
    areaID,
    scoring,
    indicator = NULL,
    type = NULL,
    units = NULL,
    PPTID = NULL,
    project_short_title = NULL,
    climate = NULL,
    design_target = NULL,
    other_nest_variables = NULL
) {

  # ---------------------------------------------------------
  # 1. Validate inputs
  # ---------------------------------------------------------

  if (!inherits(data, "sf")) {
    stop("data must be an sf object for protection coverage scoring")
  }

  if (!inherits(areas, "sf")) {
    stop("areas must be an sf object")
  }

  indicator_col <- {{ indicator_var_name }}
  area_col <- {{ areaID }}

  if (!indicator_col %in% names(data)) {
    stop("indicator_var_name column not found in data")
  }

  if (!area_col %in% names(areas)) {
    stop("areaID column not found in areas")
  }

  if (!scoring %in% c("protection coverage","protection coverage: weighted")) {
    stop("scoring must be 'protection coverage' or ","'protection coverage: weighted'")
  }

  # Transform data to areas CRS
  data <- sf::st_transform(data, sf::st_crs(areas))

  # Make geometries valid
  data <- data |>
    sf::st_make_valid() |>
    dplyr::filter(!is.na(.data[[indicator_col]]))

  areas <- sf::st_make_valid(areas)

  geometry_col <- attr(data, "sf_column")

  # ---------------------------------------------------------
  # 2. Identify weighted variable
  # ---------------------------------------------------------

  if (scoring == "protection coverage: weighted") {

    if (is.null(other_nest_variables) ||
        length(other_nest_variables) < 1) {
      stop("protection coverage: weighted requires a weighting ","variable in other_nest_variables")
    }

    weight_col <- other_nest_variables[1]

    if (!weight_col %in% names(data)) {
      stop( "Weighting variable '", weight_col,"' not found in data")
    }
  }

  # ---------------------------------------------------------
  # 3. Calculate coverage
  # ---------------------------------------------------------

  if (scoring == "protection coverage") {
    # Total area of each feature
    data_with_area <- data |>
      dplyr::mutate(
        .feature_area = sf::st_area(.data[[geometry_col]])
      )

    # Intersect features with conservation areas
    represented <- sf::st_intersection(
      data_with_area,
      areas
    )

    if (nrow(represented) > 0) {

      represented <- represented |>
        dplyr::mutate(
          .covered_area = sf::st_area(.data[[geometry_col]])
        ) |>
        dplyr::group_by(
          .data[[area_col]],
          .data[[indicator_col]]
        ) |>
        dplyr::summarise(
          .covered_area = sum(.covered_area),
          .feature_area = dplyr::first(.feature_area),
          !!geometry_col := sf::st_union(.data[[geometry_col]]),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          coverage = as.numeric(
            .covered_area / .feature_area
          )
        )

    } else {
      represented <- tibble::tibble()
    }
  } else {
    # -------------------------------------------------------
    # Weighted coverage
    # -------------------------------------------------------

    data_with_weight <- data |>
      dplyr::mutate(
        .weight = .data[[weight_col]],
        .feature_area = sf::st_area(.data[[geometry_col]])
      )

    represented <- sf::st_intersection(
      data_with_weight,
      areas
    )

    if (nrow(represented) > 0) {
      represented <- represented |>
        dplyr::mutate(
          .intersection_area =
            sf::st_area(.data[[geometry_col]]),
          .weighted_coverage =
            .weight *
            as.numeric(.intersection_area / .feature_area)
        ) |>
        dplyr::group_by(
          .data[[area_col]],
          .data[[indicator_col]]
        ) |>
        dplyr::summarise(
          .weighted_coverage = sum(.weighted_coverage, na.rm = TRUE),
          .total_weight = dplyr::first(
            sum(.weight, na.rm = TRUE)
          ),
          !!geometry_col := sf::st_union(.data[[geometry_col]]),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          coverage =
            .weighted_coverage / .total_weight
        )
    } else {
      represented <- tibble::tibble()
    }
  }

  # ---------------------------------------------------------
  # 4. Calculate coverage by conservation area
  # ---------------------------------------------------------

  if (nrow(represented) > 0) {

    coverage_by_area <- represented |>
      sf::st_drop_geometry() |>
      dplyr::group_by(
        .data[[area_col]]
      ) |>
      dplyr::summarise(
        coverage = sum(coverage, na.rm = TRUE),
        .groups = "drop"
      )

    nesteddata <- represented |>
      dplyr::group_by(.data[[area_col]]) |>
      tidyr::nest(data = -dplyr::all_of(area_col)) |>
      dplyr::ungroup() |>
      dplyr::left_join(coverage_by_area, by = area_col)

  } else {
    nesteddata <- tibble::tibble(
      !!area_col := character(),
      data = list(),
      coverage = numeric()
    )
  }

  # ---------------------------------------------------------
  # 5. Add conservation areas with no coverage
  # ---------------------------------------------------------

  all_areas <- areas |>
    sf::st_drop_geometry() |>
    dplyr::select(
      dplyr::all_of(area_col)
    ) |>
    dplyr::distinct()

  nesteddata <- all_areas |>
    dplyr::left_join(
      nesteddata,
      by = area_col
    ) |>
    dplyr::mutate(
      coverage = dplyr::coalesce(coverage, 0)
    )

  # ---------------------------------------------------------
  # 6. Score against protection target
  # ---------------------------------------------------------

  if (!"min_target" %in% names(data) ||!"max_target" %in% names(data)) {
    stop(
      "data must contain 'min_target' and 'max_target' ",
      "for protection coverage scoring"
    )
  }

  min_target <- unique(data$min_target)[1]
  max_target <- unique(data$max_target)[1]

  nesteddata <- nesteddata |>
    dplyr::mutate(
      score = dplyr::case_when(
        coverage < min_target ~
          coverage / min_target * 100,

        coverage > max_target ~
          100 -
          (coverage - max_target) /
          (1 - max_target) * 100,

        TRUE ~ 100
      )
    )

  # ---------------------------------------------------------
  # 7. Status and trend statements
  # ---------------------------------------------------------

  nesteddata <- nesteddata |>
    dplyr::mutate(
      indicator = indicator,
      type = type,
      units = units,
      scoring = scoring,
      PPTID = paste0(PPTID, collapse = " ;;; "),
      project_short_title =
        paste0(project_short_title, collapse = " ;;; "),
      climate = climate,
      design_target = design_target,

      status_statement = paste0(
        .data[[area_col]],
        " has ",
        round(coverage * 100, 1),
        "% protection coverage of ",
        unique(data$plainname)[1],
        ". The established protection target is ",
        round(min_target * 100, 1),
        "% to ",
        round(max_target * 100, 1),
        "%."
      ),

      trend_statement =
        "There is no temporal dimension in this data."
    )

  # ---------------------------------------------------------
  # 8. Return
  # ---------------------------------------------------------

  nesteddata
}
