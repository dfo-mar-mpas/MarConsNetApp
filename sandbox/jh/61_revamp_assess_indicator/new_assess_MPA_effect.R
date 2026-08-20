assess_mpa_effect <- function(
    data,
    scoring,
    areas_use,
    control_polygons,
    areaID,
    indicator_var_name,
    latitude = NULL,
    longitude = NULL,
    crs = NULL,
    other_nest_variables = NULL,
    indicator = NULL,
    type = NULL,
    units = NULL,
    PPTID = NULL,
    project_short_title = NULL,
    climate = NULL,
    design_target = NULL,
    year
) {

  # ------------------------------------------------------------
  # Check scoring
  # ------------------------------------------------------------

  valid_scoring <- c(
    "mpa effect: median difference",
    "mpa effect: trend difference",
    "mpa effect: regional proportion"
  )
  if (!scoring %in% valid_scoring) {
    stop(
      "scoring must be one of: ",
      paste(valid_scoring, collapse = ", ")
    )
  }

  # ------------------------------------------------------------
  # Common metadata
  # ------------------------------------------------------------

  metadata <- list(
    indicator = indicator,
    type = type,
    units = units,
    scoring = scoring,
    PPTID = paste0(PPTID, collapse = " ;;; "),
    project_short_title = paste0(
      project_short_title,
      collapse = " ;;; "
    ),
    climate = climate,
    design_target = design_target
  )

  # ============================================================
  # MPA EFFECT – MEDIAN DIFFERENCE
  # ============================================================

  if (scoring == "mpa effect: median difference") {

    if (!inherits(data, "stars")) {
      stop("data must be a stars object for ","'mpa effect: median difference'")
    }

    # ----------------------------------------------------------
    # Keep data as a raster.
    #
    # DO NOT use st_as_sf(data, as_points = TRUE).
    # That would create ~30 million points for the vessel raster.
    # ----------------------------------------------------------
    raster_data <- terra::rast(data)

    value_col <- names(raster_data)[1]

    # Transform polygons to raster CRS
    mpa_polygons <- sf::st_transform(
      areas_use,
      terra::crs(raster_data)
    )
    ## Trying to get the data
    mpa_rasters <- lapply(seq_len(nrow(mpa_polygons)), function(i) {
      terra::mask(
        terra::crop(
          raster_data,
          terra::vect(mpa_polygons[i, ])
        ),
        terra::vect(mpa_polygons[i, ])
      )
    })

    control_polygons_raster <- sf::st_transform(
      control_polygons,
      terra::crs(raster_data)
    )

    # ----------------------------------------------------------
    # Extract raster values within MPAs
    # ----------------------------------------------------------

    mpa_values <- terra::extract(
      raster_data,
      terra::vect(mpa_polygons),
      na.rm = TRUE
    )

    mpa_medians <- mpa_values |>
      dplyr::group_by(ID) |>
      dplyr::summarise(
        median_inside = median(
          .data[[value_col]],
          na.rm = TRUE
        ),
        n_inside = sum(
          !is.na(.data[[value_col]])
        ),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        areaID = mpa_polygons[[areaID]][ID]
      )

    # ----------------------------------------------------------
    # Extract raster values within control polygons
    # ----------------------------------------------------------

    control_values <- terra::extract(
      raster_data,
      terra::vect(control_polygons_raster),
      na.rm = TRUE
    )

    control_medians <- control_values |>
      dplyr::group_by(ID) |>
      dplyr::summarise(
        median_outside = median(
          .data[[value_col]],
          na.rm = TRUE
        ),
        n_outside = sum(
          !is.na(.data[[value_col]])
        ),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        areaID = control_polygons_raster[[areaID]][ID],
        buffer_distance =
          control_polygons_raster$buffer_distance[ID]
      )

    # ----------------------------------------------------------
    # Select the control polygon to use
    #
    # Prefer the smallest available control buffer.
    # ----------------------------------------------------------

    control_medians <- control_medians |>
      dplyr::mutate(
        buffer_order = match(
          buffer_distance,
          c(
            "twenty_km",
            "forty_km",
            "sixty_km",
            "eighty_km"
          )
        )
      ) |>
      dplyr::arrange(
        areaID,
        buffer_order
      ) |>
      dplyr::group_by(areaID) |>
      dplyr::slice_head(n = 1) |>
      dplyr::ungroup()

    # ----------------------------------------------------------
    # Calculate MPA effect
    # ----------------------------------------------------------

    nesteddata <- mpa_medians |>
      dplyr::left_join(
        control_medians |>
          dplyr::select(
            areaID,
            median_outside,
            n_outside,
            buffer_distance
          ),
        by = "areaID"
      ) |>
      dplyr::mutate(

        # Difference between median inside and median outside
        difference = median_inside - median_outside,

        # Score direction
        #
        # Negative difference = lower inside
        # Positive difference = higher inside
        #
        # For this scoring scheme:
        # lower inside = better
        # higher inside = worse
        #
        score = dplyr::case_when(
          difference < 0 ~ 100,
          difference > 0 ~ 0,
          difference == 0 ~ 50,
          TRUE ~ NA_real_
        ),

        indicator = metadata$indicator,
        type = metadata$type,
        units = metadata$units,
        scoring = metadata$scoring,
        PPTID = metadata$PPTID,
        project_short_title = metadata$project_short_title,
        climate = metadata$climate,
        design_target = metadata$design_target,

        status_statement = paste0(
          areaID,
          " had a median ",
          indicator_var_name,
          " value of ",
          round(median_inside, 2),
          " inside the MPA compared with ",
          round(median_outside, 2),
          " in the associated ",
          buffer_distance,
          " control area. The difference was ",
          round(difference, 2),
          "."
        ),

        trend_statement = "There is no temporal dimension in this data.",

        # Store the raster for each MPA
        data = mpa_rasters

      ) |>
      dplyr::select(
        areaID,
        score,
        indicator,
        type,
        units,
        scoring,
        PPTID,
        project_short_title,
        climate,
        design_target,
        status_statement,
        trend_statement,
        data
      )

    ## Adding quality statement:
    nesteddata$quality_statement <- NA
    for (i in seq_along(nesteddata$trend_statement)) {
      nesteddata$quality_statement[i] <- paste0("This assessment is based off of the median value of ",
                        length(which(mpa_values$ID == i)), " raster cells within the area compared to the median value of ", length(which(!mpa_values$ID == i)),
                        " raster cells outside of the area, for ", stringr::str_extract(names(mpa_values)[2], "\\d{4}"))
    }
    # ============================================================
    # MPA EFFECT – TREND DIFFERENCE
    # ============================================================

  } else if (scoring == "mpa effect: trend difference") {

    # ----------------------------------------------------------
    # Prepare data
    # ----------------------------------------------------------
    if (!inherits(data, "sf")) {
      if (is.null(latitude) || !latitude %in% names(data)) {
        stop("latitude column not found")
      }

      if (is.null(longitude) ||!longitude %in% names(data)) {
        stop("longitude column not found")
      }

      data <- sf::st_as_sf(
        data,
        coords = c(
          longitude,
          latitude
        ),
        crs = crs
      )

    } else {

      data <- sf::st_transform(
        data,
        sf::st_crs(areas_use)
      )
    }

    # ----------------------------------------------------------
    # Match observations to MPAs and control polygons
    # ----------------------------------------------------------

    buffers_sorted <- c(
      "twenty_km",
      "forty_km",
      "sixty_km",
      "eighty_km"
    )

    data <- data |>
      sf::st_join(
        dplyr::select(
          areas_use,
          {{ areaID }}
        )
      ) |>
      dplyr::rename(
        site_areaID = {{ areaID }}
      ) |>
      sf::st_join(
        dplyr::select(
          control_polygons,
          buffer_distance,
          {{ areaID }}
        )
      ) |>
      dplyr::rename(
        control_areaID = {{ areaID }}
      )

    # ----------------------------------------------------------
    # Determine which control buffer is needed
    # ----------------------------------------------------------

    year_value <- year
    data <- data |>
      dplyr::group_by(control_areaID) |>
      dplyr::mutate(
        buffer_order = match(
          buffer_distance,
          buffers_sorted
        )
      ) |>
      dplyr::mutate(
        needs_this_buffer = purrr::map_lgl(
          buffer_order,
          function(current_order) {

            if (is.na(current_order)) {
              return(FALSE)
            }

            if (current_order == 1) {
              return(TRUE)
            }

            max_years_in_smaller <- max(
              purrr::map_dbl(
                1:(current_order - 1),
                function(smaller_order) {

                  dplyr::n_distinct(
                    year_value[
                      buffer_order <= smaller_order
                    ]
                  )
                }
              )
            )

            max_years_in_smaller < 5
          }
        ),

        max_buffer_used = ifelse(
          any(needs_this_buffer),
          buffers_sorted[
            max(
              buffer_order[
                needs_this_buffer
              ],
              na.rm = TRUE
            )
          ],
          NA_character_
        )
      ) |>
      dplyr::ungroup()

    # ----------------------------------------------------------
    # Identify MPA vs control observations
    # ----------------------------------------------------------

    data <- data |>
      dplyr::mutate(

        inmpa =
          !is.na(site_areaID) &
          site_areaID != "Non_Conservation_Area",

        control = dplyr::case_when(

          is.na(site_areaID) &
            is.na(control_areaID) ~ NA,

          !is.na(site_areaID) &
            is.na(control_areaID) ~ FALSE,

          (
            is.na(site_areaID) |
              site_areaID ==
              "Non_Conservation_Area"
          ) &
            !is.na(control_areaID) &
            needs_this_buffer ~ TRUE,

          TRUE ~ FALSE
        ),

        areaID = dplyr::case_when(

          inmpa ~ site_areaID,

          control ~ control_areaID,

          TRUE ~ "Non_Conservation_Area"
        )
      ) |>
      dplyr::select(
        -site_areaID,
        -control_areaID,
        -buffer_order,
        -needs_this_buffer,
        -buffer_distance,
        -inmpa
      )

    # ----------------------------------------------------------
    # Nest data by MPA/control area
    # ----------------------------------------------------------

    nest_cols <- c(
      year_value,
      indicator_var_name,
      attr(data, "sf_column"),
      other_nest_variables,
      "control",
      "max_buffer_used"
    )

    nest_cols <- nest_cols[
      !is.na(nest_cols)
    ]

    nesteddata <- data |>
      dplyr::filter(
        !is.na(control)
      ) |>
      dplyr::filter(
        !is.na(
          .data[[indicator_var_name]]
        )
      ) |>
      dplyr::group_by(areaID) |>
      tidyr::nest(
        data = dplyr::all_of(nest_cols)
      ) |>
      dplyr::ungroup()

    # ----------------------------------------------------------
    # Fit inside vs outside trend model
    # ----------------------------------------------------------

    nesteddata <- nesteddata |>
      dplyr::mutate(

        model = purrr::map(
          data,
          function(x) {

            stats::lm(
              stats::as.formula(
                paste0(
                  indicator_var_name,
                  " ~ ",
                  year,
                  " + control")
              ),
              data = x
            )
          }
        ),

        summaries = purrr::map(
          model,
          summary
        ),

        coeffs = purrr::map(
          summaries,
          coefficients
        ),

        control_effect = purrr::map_dbl(
          coeffs,
          function(coef_table) {

            if (nrow(coef_table) >= 3) {
              coef_table[3, 1]
            } else {
              NA_real_
            }
          }
        ),

        p = purrr::map_dbl(
          coeffs,
          function(coef_table) {

            if (nrow(coef_table) >= 3) {
              coef_table[3, 4]
            } else {
              NA_real_
            }
          }
        ),

        # Lower inside = better
        # Higher inside = worse
        score = dplyr::case_when(

          control_effect < 0 &
            p < 0.05 ~ 100,

          control_effect > 0 &
            p < 0.05 ~ 0,

          p >= 0.05 ~ 50,

          TRUE ~ NA_real_
        ),

        indicator = metadata$indicator,
        type = metadata$type,
        units = metadata$units,
        scoring = metadata$scoring,
        PPTID = metadata$PPTID,
        project_short_title =
          metadata$project_short_title,
        climate = metadata$climate,
        design_target = metadata$design_target
      )

    # ----------------------------------------------------------
    # Status and trend statements
    # ----------------------------------------------------------
    nesteddata <- nesteddata |>
      dplyr::mutate(

        status_statement = purrr::map_chr(
          data,
          function(x) {

            years <- sort(unique(x[[year]]))
            years <- years[!is.na(years)]

            n_years <- length(years)

            indicator_values <- x[[indicator_var_name]]
            indicator_values <- indicator_values[!is.na(indicator_values)]

            if (n_years == 0) {

              paste0(
                "There were no available ",
                indicator_var_name,
                " observations for this MPA and associated control area."
              )

            } else if (n_years == 1) {

              paste0(
                "The ",
                indicator_var_name,
                " indicator was assessed using data collected in ",
                years,
                "."
              )

            } else {

              paste0(
                "The ",
                indicator_var_name,
                " indicator was assessed using data collected from ",
                min(years),
                " to ",
                max(years),
                " (",
                n_years,
                " years)."
              )
            }
          }
        ),

        trend_statement = purrr::map2_chr(
          p,
          control_effect,
          function(p_value, effect) {

            if (is.na(p_value) ||
                is.na(effect)) {

              return(
                paste0(
                  "There was insufficient data to estimate the effect of the MPA on the ",
                  indicator_var_name,
                  " trend."
                )
              )
            }

            if (p_value < 0.05) {

              paste0(
                "There was a significant difference in the ",
                indicator_var_name,
                " trend between the MPA and control area ",
                "(p = ",
                round(p_value, 3),
                "). The estimated MPA effect was ",
                round(effect, 3),
                " (inside minus outside)."
              )

            } else {

              paste0(
                "There was no significant difference in the ",
                indicator_var_name,
                " trend between the MPA and control area ",
                "(p = ",
                round(p_value, 3),
                ")."
              )
            }
          }
        ),

        quality_statement = purrr::map_chr(
          data,
          function(x) {

            years <- sort(unique(x[[year]]))
            years <- years[!is.na(years)]

            n_samples <- sum(
              !is.na(x[[indicator_var_name]])
            )

            if (length(years) == 0) {

              paste0(
                "This assessment was based on ",
                n_samples,
                " samples with no available year information."
              )

            } else if (length(years) == 1) {

              paste0(
                "This assessment was based on ",
                n_samples,
                " samples collected in ",
                years,
                "."
              )

            } else {

              paste0(
                "This assessment was based on ",
                n_samples,
                " samples collected between ",
                min(years),
                " and ",
                max(years),
                ", covering ",
                length(years),
                " years."
              )
            }
          }
        )
      ) |>
      dplyr::select(
        -model,
        -summaries,
        -coeffs,
        -control_effect,
        -p
      )
  } else if (scoring == "mpa effect: regional proportion") {

    if (!inherits(data, "sf")) {

      if (is.null(latitude) ||
          !latitude %in% names(data)) {
        stop("latitude column not found")
      }

      if (is.null(longitude) ||
          !longitude %in% names(data)) {
        stop("longitude column not found")
      }

      data <- sf::st_as_sf(
        data,
        coords = c(
          longitude,
          latitude
        ),
        crs = crs
      )

    } else {

      data <- sf::st_transform(
        data,
        sf::st_crs(areas_use)
      )
    }

    # ----------------------------------------------------------
    # Prepare species column
    # ----------------------------------------------------------

    if ("species" %in% names(data)) {
      names(data)[
        names(data) == "species"
      ] <- "scientificName"
    }

    if (!"scientificName" %in% names(data)) {
      stop(
        "The data must contain a 'scientificName' or 'species' column."
      )
    }

    # ----------------------------------------------------------
    # Make geometries valid
    # ----------------------------------------------------------

    areas_use <- sf::st_make_valid(
      areas_use
    )

    data <- sf::st_make_valid(
      data
    )

    # ----------------------------------------------------------
    # Identify observations within MPAs
    # ----------------------------------------------------------

    nesteddata <- data |>
      sf::st_join(
        dplyr::select(
          areas_use,
          {{ areaID }}
        ),
        left = FALSE
      ) |>
      dplyr::rename(
        areaID = {{ areaID }}
      ) |>
      dplyr::filter(
        !is.na(.data[[indicator_var_name]])
      ) |>
      dplyr::group_by(
        areaID
      ) |>
      tidyr::nest() |>
      dplyr::ungroup()

    # ----------------------------------------------------------
    # Identify all species detected in the region
    # ----------------------------------------------------------

    regional_species <- unique(
      data$scientificName[
        !is.na(data$scientificName)
      ]
    )

    # ----------------------------------------------------------
    # Calculate regional proportion
    # ----------------------------------------------------------

    nesteddata <- data |>
      sf::st_join(
        dplyr::select(
          areas_use,
          {{ areaID }}
        ),
        left = FALSE
      ) |>
      dplyr::rename(
        areaID = {{ areaID }}
      ) |>
      dplyr::filter(
        !is.na(.data[[indicator_var_name]])
      ) |>
      dplyr::group_by(areaID) |>
      tidyr::nest() |>
      dplyr::ungroup()

    # ----------------------------------------------------------
    # Identify all species detected in the region
    # ----------------------------------------------------------

    regional_species <- unique(
      data$scientificName[
        !is.na(data$scientificName)
      ]
    )

    # ----------------------------------------------------------
    # Calculate score and statements
    # ----------------------------------------------------------

    nesteddata <- nesteddata |>
      dplyr::mutate(

        score = purrr::map_dbl(
          data,
          function(mpa_data) {

            identified_species_for_area <- unique(
              mpa_data$scientificName[
                !is.na(mpa_data$scientificName)
              ]
            )

            identified_species_for_area <-
              identified_species_for_area[
                identified_species_for_area %in%
                  regional_species
              ]

            length(
              identified_species_for_area
            ) /
              length(regional_species) *
              100
          }
        ),

        indicator = metadata$indicator,
        type = metadata$type,
        units = metadata$units,
        scoring = metadata$scoring,
        PPTID = metadata$PPTID,
        project_short_title =
          metadata$project_short_title,
        climate = metadata$climate,
        design_target = metadata$design_target,

        # ------------------------------------------------------
        # Status statement
        # ------------------------------------------------------

        status_statement = purrr::map_chr(
          data,
          function(mpa_data) {

            identified_species_for_area <- unique(
              mpa_data$scientificName[
                !is.na(mpa_data$scientificName)
              ]
            )

            identified_species_for_area <-
              identified_species_for_area[
                identified_species_for_area %in%
                  regional_species
              ]

            paste0(
              "Target species detected within this area: ",
              paste(
                identified_species_for_area,
                collapse = ", "
              ),
              ". The following target species were detected in the region: ",
              paste(
                regional_species,
                collapse = ", "
              ),
              ". ",
              length(
                identified_species_for_area
              ),
              " of ",
              length(
                regional_species
              ),
              " target species detected in the region ",
              "were detected within this area (",
              round(
                length(identified_species_for_area) /
                  length(regional_species) * 100,
                1
              ),
              "%)."
            )
          }
        ),

        # ------------------------------------------------------
        # Trend statement
        # ------------------------------------------------------

        trend_statement =
          "There is no temporal dimension in this scoring scheme.",

        # ------------------------------------------------------
        # Quality statement
        # ------------------------------------------------------

        quality_statement = purrr::map_chr(
          data,
          function(mpa_data) {

            n_samples <- nrow(mpa_data)

            years <- sort(
              unique(
                mpa_data$year
              )
            )

            years <- years[
              !is.na(years)
            ]

            if (length(years) == 1) {

              paste0(
                "This is based off of ",
                n_samples,
                " samples in ",
                years,
                "."
              )

            } else if (length(years) > 1) {

              paste0(
                "This is based off of ",
                n_samples,
                " samples collected across the years ",
                paste(years, collapse = ", "),
                "."
              )

            } else {

              paste0(
                "This is based off of ",
                n_samples,
                " samples. The year of sampling was not available."
              )
            }
          }
        )
      ) |>
      dplyr::select(
        areaID,
        score,
        indicator,
        type,
        units,
        scoring,
        PPTID,
        project_short_title,
        climate,
        design_target,
        status_statement,
        trend_statement,
        quality_statement,
        data
      )
  }

  return(nesteddata)
}
