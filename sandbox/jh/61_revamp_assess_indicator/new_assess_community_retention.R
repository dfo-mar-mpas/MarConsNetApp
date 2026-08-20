assess_community_retention <- function(
    data,
    areas,
    areas_use,
    areaID,
    scoring,
    crs,
    indicator = NULL,
    type = NULL,
    units = NULL,
    PPTID = NULL,
    project_short_title = NULL,
    climate = NULL,
    design_target = NULL
) {

  # ---------------------------------------------------------
  # 1. Check scoring scheme
  # ---------------------------------------------------------

  if (scoring != "community retention") {
    stop(
      "This function requires scoring = 'community retention'"
    )
  }

  # ---------------------------------------------------------
  # 2. Check required columns
  # ---------------------------------------------------------

  required_columns <- c(
    "ID",
    "year_of_data_collection",
    "species",
    "detections"
  )

  missing_columns <- setdiff(
    required_columns,
    names(data)
  )

  if (length(missing_columns) > 0) {
    stop(
      "The following required columns are missing: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  # ---------------------------------------------------------
  # 3. Convert point data to sf
  # ---------------------------------------------------------

  if (!inherits(data, "sf")) {

    if (!all(c("latitude", "longitude") %in% names(data))) {
      stop(
        "Community retention requires latitude and longitude columns."
      )
    }

    data <- data |>
      dplyr::filter(
        !is.na(latitude),
        !is.na(longitude)
      ) |>
      sf::st_as_sf(
        coords = c("longitude", "latitude"),
        crs = crs
      )

  }

  # ---------------------------------------------------------
  # 4. Join samples to conservation areas
  # ---------------------------------------------------------

  data <- data |>
    sf::st_join(
      dplyr::select(
        areas_use,
        {{ areaID }}
      ),
      left = FALSE
    ) |>
    dplyr::rename(
      areaID = {{ areaID }}
    )

  # ---------------------------------------------------------
  # 5. Create community results
  # ---------------------------------------------------------

  nesteddata <- data |>
    dplyr::group_by(
      areaID
    ) |>
    tidyr::nest()

  # Add metadata
  nesteddata <- nesteddata |>
    dplyr::mutate(
      indicator = indicator,
      type = type,
      units = units,
      scoring = scoring,
      PPTID = paste0(
        PPTID,
        collapse = " ;;; "
      ),
      project_short_title = paste0(
        project_short_title,
        collapse = " ;;; "
      ),
      climate = climate,
      design_target = design_target
    )

  # ---------------------------------------------------------
  # 6. Preallocate results
  # ---------------------------------------------------------

  n <- nrow(nesteddata)

  score <- rep(NA_real_, n)

  status_statement <- rep(
    NA_character_,
    n
  )

  trend_statement <- rep(
    NA_character_,
    n
  )

  quality_statement <- rep(
    NA_character_,
    n
  )

  # ---------------------------------------------------------
  # 7. Assess each conservation area
  # ---------------------------------------------------------

  for (i in seq_len(n)) {

    DATA <- nesteddata$data[[i]]

    if (is.null(DATA) || nrow(DATA) == 0) {

      status_statement[i] <- "No data available."
      trend_statement[i] <- "No data available."
      quality_statement[i] <- "No data available."

      next
    }

    # -------------------------------------------------------
    # Identify recent year
    # -------------------------------------------------------

    years <- sort(
      unique(
        DATA$year_of_data_collection[
          !is.na(DATA$year_of_data_collection)
        ]
      )
    )

    if (length(years) < 2) {

      score[i] <- NA_real_

      status_statement[i] <-
        "Insufficient sampling years to assess community retention."

      trend_statement[i] <-
        "At least two sampling periods are required to assess community retention."

      quality_statement[i] <- paste0(
        "The assessment included ",
        nrow(DATA),
        " samples from ",
        length(years),
        " sampling year."
      )

      next
    }

    recent_year <- max(years)

    # -------------------------------------------------------
    # 1. Create community matrix
    # -------------------------------------------------------

    community <- DATA |>
      sf::st_drop_geometry() |>
      dplyr::select(
        ID,
        year_of_data_collection,
        species,
        detections
      ) |>
      tidyr::pivot_wider(
        names_from = species,
        values_from = detections,
        values_fill = 0,
        values_fn = sum
      )

    # -------------------------------------------------------
    # 2. Identify historical vs recent samples
    # -------------------------------------------------------

    metadata <- community |>
      dplyr::select(
        ID,
        year_of_data_collection
      )

    community_matrix <- community |>
      dplyr::select(
        -ID,
        -year_of_data_collection
      )

    historical <- metadata$year_of_data_collection < recent_year

    recent <- metadata$year_of_data_collection == recent_year

    historical_matrix <- community_matrix[
      historical,
      ,
      drop = FALSE
    ]

    recent_matrix <- community_matrix[
      recent,
      ,
      drop = FALSE
    ]

    # -------------------------------------------------------
    # 3. Collapse detections across samples
    # -------------------------------------------------------

    historical_total <- colSums(
      historical_matrix,
      na.rm = TRUE
    )

    recent_total <- colSums(
      recent_matrix,
      na.rm = TRUE
    )

    # -------------------------------------------------------
    # 4. Estimate historical expected richness
    # -------------------------------------------------------

    if (
      length(historical_total) == 0 ||
      sum(historical_total) == 0
    ) {

      score[i] <- NA_real_

      status_statement[i] <-
        "Insufficient historical detections to estimate expected community richness."

      trend_statement[i] <-
        "Insufficient historical data to assess community retention."

      quality_statement[i] <- paste0(
        "The assessment included ",
        nrow(recent_matrix),
        " recent samples and ",
        nrow(historical_matrix),
        " historical samples, but there were insufficient historical detections for a richness estimate."
      )

      next
    }

    historical_est <- vegan::estimateR(
      historical_total
    )

    expected_species <- as.numeric(
      historical_est["S.chao1"]
    )

    # -------------------------------------------------------
    # 5. Calculate observed recent richness
    # -------------------------------------------------------

    observed_recent <- sum(
      recent_total > 0
    )

    # -------------------------------------------------------
    # 6. Calculate score
    # -------------------------------------------------------

    score[i] <- min(
      (observed_recent / expected_species) * 100,
      100
    )

    # -------------------------------------------------------
    # 7. Identify species
    # -------------------------------------------------------

    historical_species <- names(
      historical_total[
        historical_total > 0
      ]
    )

    recent_species <- names(
      recent_total[
        recent_total > 0
      ]
    )

    missing_species <- setdiff(
      historical_species,
      recent_species
    )

    new_species <- setdiff(
      recent_species,
      historical_species
    )

    # -------------------------------------------------------
    # 8. Species names
    # -------------------------------------------------------

    if ("common_name" %in% names(DATA)) {

      species_lookup <- DATA |>
        sf::st_drop_geometry() |>
        dplyr::filter(
          !is.na(species)
        ) |>
        dplyr::distinct(
          species,
          common_name
        ) |>
        dplyr::group_by(
          species
        ) |>
        dplyr::summarise(
          common_name = dplyr::first(
            stats::na.omit(common_name)
          ),
          .groups = "drop"
        )

      species_labels <- stats::setNames(
        ifelse(
          is.na(species_lookup$common_name) |
            species_lookup$common_name == "",
          species_lookup$species,
          paste0(
            species_lookup$species,
            " (",
            species_lookup$common_name,
            ")"
          )
        ),
        species_lookup$species
      )

      format_species <- function(x) {

        out <- species_labels[x]

        out[is.na(out)] <- x[is.na(out)]

        out
      }

    } else {

      format_species <- function(x) x

    }

    # -------------------------------------------------------
    # 9. Status statement
    # -------------------------------------------------------

    status_statement[i] <- paste0(
      "In ",
      recent_year,
      ", ",
      observed_recent,
      " species were detected. ",
      "The historical community had an estimated expected richness of approximately ",
      round(expected_species, 0),
      " species based on the Chao1 estimator. ",
      "This represents ",
      round(score[i], 0),
      "% retention of the historically expected community."
    )

    # -------------------------------------------------------
    # 10. Trend statement
    # -------------------------------------------------------

    trend_statement[i] <- paste0(
      "The most recent community contained ",
      observed_recent,
      " detected species compared with ",
      round(expected_species, 0),
      " species expected based on the historical community. "
    )

    if (length(missing_species) > 0) {

      trend_statement[i] <- paste0(
        trend_statement[i],
        length(missing_species),
        " historically detected species were not detected in the most recent sampling period: ",
        paste(
          format_species(missing_species),
          collapse = ", "
        ),
        "."
      )

    } else {

      trend_statement[i] <- paste0(
        trend_statement[i],
        "All historically detected species were also detected in the most recent sampling period."
      )

    }

    if (length(new_species) > 0) {

      trend_statement[i] <- paste0(
        trend_statement[i],
        " ",
        length(new_species),
        " additional species were detected in the most recent sampling period: ",
        paste(
          format_species(new_species),
          collapse = ", "
        ),
        "."
      )
    }

    # -------------------------------------------------------
    # 11. Quality statement
    # -------------------------------------------------------

    quality_statement[i] <- paste0(
      "The assessment was based on ",
      nrow(recent_matrix),
      " samples collected in ",
      recent_year,
      " and ",
      nrow(historical_matrix),
      " historical samples. ",
      "Historical expected richness was estimated using the Chao1 estimator to account for potentially undetected species."
    )
  }

  # ---------------------------------------------------------
  # 12. Return results
  # ---------------------------------------------------------

  nesteddata$score <- score

  nesteddata$status_statement <-
    status_statement

  nesteddata$trend_statement <-
    trend_statement

  nesteddata$quality_statement <-
    quality_statement

  nesteddata
}
