assess_desired_trend <- function(
    data, areas,areas_use, year,
    indicator_var_name,latitude,longitude,areaID,scoring,crs,other_nest_variables = NULL,indicator = NULL,  type = NULL,
    units = NULL,PPTID = NULL,project_short_title = NULL,climate = NULL,design_target = NULL
) {

  # ---------------------------------------------------------
  # 1. Convert arguments to column names
  # ---------------------------------------------------------

  year_col <- {{year}}
  indicator_col <- {{indicator_var_name}}
  latitude_col <- {{latitude}}
  longitude_col <- {{longitude}}
  area_col <- {{areaID}}
  # ---------------------------------------------------------
  # 2. Helper function for species group name
  # ---------------------------------------------------------


  get_group_name <- function(x) {
    unique_species <- unique(stats::na.omit(x$species))

    if (length(unique_species) == 1) {
      return(unique_species)
    }

    common_words <- Reduce(intersect, strsplit(tolower(unique_species), "\\s+"))

    if (length(common_words) > 0) {
      group_name <- paste(common_words, collapse = " ")
    } else {
      group_name <- unique(stats::na.omit(x$subclass))[1]

      if (is.na(group_name) || length(group_name) == 0) {
        group_name <- unique(stats::na.omit(x$class))[1]
      }

      if (is.na(group_name) || length(group_name) == 0) {
        group_name <- unique(stats::na.omit(x$superclass))[1]
      }
    }

    if (grepl("trophic", indicator, ignore.case = TRUE)) {
      trophic_name <- unique(stats::na.omit(x$ai_trophic_level))[1]
      if (!is.na(trophic_name)) group_name <- trophic_name
    }

    group_name
  }

  # ---------------------------------------------------------
  # 3. Check required columns
  # ---------------------------------------------------------

  if (!year_col %in% names(data)) {
    stop("year column not found")
  }

  if (!indicator_col %in% names(data) && !grepl('no decrease', scoring)) {
    stop("indicator_var_name column not found")
  }


  # ---------------------------------------------------------
  # 4. Convert to sf and join with areas
  # ---------------------------------------------------------

  if (!inherits(data, "sf")) {

    if (!latitude_col %in% names(data)) {
      stop("latitude column not found")
    }

    if (!longitude_col %in% names(data)) {
      stop("longitude column not found")
    }

    data <- data |>
      dplyr::filter(
        !is.na(.data[[longitude_col]]),
        !is.na(.data[[latitude_col]])
      ) |>
      sf::st_as_sf(
        coords = c(
          longitude_col,
          latitude_col
        ),
        crs = crs
      ) |>
      sf::st_join(
        dplyr::select(
          areas_use,
          {{ areaID }}
        )
      ) |>
      dplyr::rename(
        areaID = {{ areaID }}
      )

  } else {

    data <- data |>
      sf::st_join(
        dplyr::select(
          areas_use,
          {{ areaID }}
        )
      ) |>
      dplyr::rename(
        areaID = {{ areaID }}
      )
  }


  # ---------------------------------------------------------
  # 5. Assign points outside conservation areas
  # ---------------------------------------------------------

  data$areaID[
    is.na(data$areaID)
  ] <- "Non_Conservation_Area"


  # Remove latitude/longitude if they remain
  data <- data |>
    dplyr::select(
      -dplyr::any_of(
        c(
          latitude_col,
          longitude_col
        )
      )
    )

  # ---------------------------------------------------------
  # 6. Create nested data
  # ---------------------------------------------------------

  geometry_col <- attr(
    data,
    "sf_column"
  )

  nest_cols <- c(
    year_col,
    indicator_col,
    geometry_col,
    other_nest_variables
  )


  # Species information is required for "no decrease"

  if (grepl('no decrease', scoring)) {

    nest_cols <- c(indicator_var_name, "geometry", other_nest_variables)
  }


  # Only retain columns that actually exist

  nest_cols <- nest_cols[
    !is.na(nest_cols) &
      nest_cols %in% names(data)
  ]


  nesteddata <- data.frame(
    data,
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
  ) |>
    tidyr::nest(
      data = dplyr::all_of(nest_cols)
    )


  # ---------------------------------------------------------
  # 7. Preallocate results
  # ---------------------------------------------------------

  n <- nrow(nesteddata)

  slope_year <- numeric(n)
  p <- numeric(n)
  score <- numeric(n)

  status_statement <- character(n)
  trend_statement <- character(n)
  score_note <- character(n)


  # Text appended to values when units are supplied

  unit_text <- if (!is.na(units) && units != "") {
    paste0(" (",units,")")
  } else {
    ""
  }


  # ---------------------------------------------------------
  # 8. Assess each area
  # ---------------------------------------------------------

  for (i in seq_len(n)) {
    message("nested i = ",i)

    DATA <- nesteddata$data[[i]]


    if (is.null(DATA) || nrow(DATA) == 0) {
      status_statement[i] <- "No data available."
      trend_statement[i] <- "No data available."
      score_note[i] <- "No data available."
      next
    }

    # -------------------------------------------------------
    # Establishment date
    # -------------------------------------------------------
    current_area <- nesteddata$areaID[i]
    estab_date <- areas$date_of_establishment[
      areas[[area_col]] == current_area
    ]


    # -------------------------------------------------------
    # Select data for trend + score
    # -------------------------------------------------------

    if (current_area != "Non_Conservation_Area" && "year_of_data_collection" %in% names(DATA) && !grepl("since establishment", nesteddata$scoring[i])) {
      DATA_post <- DATA[
        !is.na(
          DATA$year_of_data_collection
        ) &
          DATA$year_of_data_collection >= estab_date,
      ]

      if (nrow(DATA_post) >= 1) {
        DATA_use <- DATA_post
        score_note[i] <- "Score only based on data post establishment."
      } else {
        DATA_use <- DATA
        score_note[i] <- "Not enough post-establishment data; score based on full dataset."
      }
    } else if (current_area == "Non_Conservation_Area") {
      DATA_use <- DATA
      score_note[i] <-
        "Score based on all data (no establishment date for Non Conservation Areas)."
    } else if (!{{year}} %in% names(DATA)) {
      DATA_use <- DATA
      score_note[i] <- paste0("No ",  {{year}},  " column available. Score based on full dataset.")
    } else {
      DATA_use <- DATA
      score_note[i] <- "Score based on all data."
    }


    # -------------------------------------------------------
    # Full-data years
    # -------------------------------------------------------

    yrs <- sort(unique(as.numeric(DATA[[year_col]][!is.na(DATA[[year_col]])])))
    if (length(yrs) == 0) {
      status_statement[i] <- "No data available."
      trend_statement[i] <-
        "Insufficient data for trend analysis."
      next
    }

    last_year <- tail(yrs,1)
    # -------------------------------------------------------
    # 9. Trend model
    # -------------------------------------------------------

    # Species richness "no decrease" does not use linear
    # regression.
    sc <- nesteddata$scoring[i]


    if (!grepl('no decrease', sc)) {
      valid_years <- DATA_use[[year_col]][
        !is.na(DATA_use[[indicator_col]]) & !is.na(DATA_use[[year_col]])]
      if (length(unique(valid_years)) > 1) {
        model_i <- stats::lm(
          stats::reformulate(
            response = indicator_col,
            termlabels = year_col
          ),
          data = DATA_use
        )

        coeffs <- summary(
          model_i
        )$coefficients

        slope_year[i] <- coeffs[2, 1]
        p[i] <- coeffs[2, 4]

      } else {

        slope_year[i] <- NA_real_
        p[i] <- NA_real_
      }

    } else {
      slope_year[i] <- NA_real_
      p[i] <- NA_real_
    }

    # -------------------------------------------------------
    # Increase / decrease
    # -------------------------------------------------------

    if (grepl('increase', sc) || grepl('decrease', sc)) {
      # 1 = increase
      # -1 = decrease

      desired_direction <- ifelse(grepl('increase', sc),1,-1)

      significant_trend <- (!is.na(p[i]) && p[i] < 0.05)

      if (!significant_trend) {
        # No significant trend
        score[i] <- 50
      } else if (
        sign(slope_year[i]) ==
        desired_direction
      ) {
        # Significant trend in desired direction
        score[i] <- 100
      } else {
        # Significant trend in undesired direction
        score[i] <- 0
      }
      # -------------------------------------------------------
      # Stable
      # -------------------------------------------------------

    } else if (grepl('stable', sc)) {
      score[i] <- ifelse(!is.na(p[i]) && p[i] < 0.05, 0,100)
      # -------------------------------------------------------
      # No decrease
      # -------------------------------------------------------
    } else if (grepl('no decrease', sc)) {
      # -------------------------------------------------------
      # Probability of detection – no decrease
      # -------------------------------------------------------

      if (grepl("probability of detection", names(data), ignore.case = TRUE)) {
        browser()
        # Calculate whether the target species was detected
        # in each sample
        yearly_detection <- aggregate(
          detected ~ year_of_data_collection + ID,
          data = transform(
            DATA_use,
            detected = !is.na(species)
          ),
          FUN = any
        )

        # Calculate the proportion of samples detecting
        # the target species in each year
        yearly_detection <- aggregate(
          detected ~ year_of_data_collection,
          data = yearly_detection,
          FUN = mean
        )

        names(yearly_detection)[2] <- "proportion_detected"

        yearly_detection <- yearly_detection[
          order(yearly_detection$year_of_data_collection),
        ]

        # Cannot calculate first vs. last year with only one year
        if (nrow(yearly_detection) <= 1) {

          score[i] <- NA_real_

        } else {

          first_detection <-
            yearly_detection$proportion_detected[1]

          last_detection <-
            yearly_detection$proportion_detected[
              nrow(yearly_detection)
            ]

          # No decrease = 100
          # Decrease = 0
          score[i] <- ifelse(
            last_detection >= first_detection,
            100,
            0
          )
        }

      } else {

        # -------------------------------------------------------
        # Existing no-decrease species richness calculation
        # -------------------------------------------------------

        yearly_species <- DATA_use |>
          dplyr::filter(
            !is.na(.data[[year_col]]),
            !is.na(.data$species)
          ) |>
          dplyr::group_by(
            .data[[year_col]]
          ) |>
          dplyr::summarise(
            richness = dplyr::n_distinct(
              .data$species
            ),
            .groups = "drop"
          ) |>
          dplyr::arrange(
            .data[[year_col]]
          )

        if (nrow(yearly_species) <= 1) {

          score[i] <- NA_real_

        } else {

          score[i] <- ifelse(
            dplyr::last(yearly_species$richness) >=
              dplyr::first(yearly_species$richness),
            100,
            0
          )
        }
      }
    }

    # =======================================================
    # 11. Status statement
    # =======================================================

    if (grepl('no decrease', sc)) {
      latest_year <- max(DATA[[year_col]], na.rm = TRUE)

      latest_species <- DATA |>
        dplyr::filter(
          .data[[year_col]] == latest_year,
          !is.na(
            .data$species
          )
        ) |>
        dplyr::distinct(
          .data$species
        ) |>
        dplyr::pull(
          .data$species
        )


      group_name <- get_group_name(
        DATA
      )


      # Species/common-name lookup

      if ("common_name" %in% names(DATA)) {

        species_lookup <- DATA |>
          dplyr::filter(
            !is.na(
              .data$species
            )
          ) |>
          dplyr::distinct(
            .data$species,
            .data$common_name
          ) |>
          dplyr::group_by(
            .data$species
          ) |>
          dplyr::summarise(
            common_name = dplyr::first(
              stats::na.omit(
                .data$common_name
              )
            ),
            .groups = "drop"
          )


        species_labels <- stats::setNames(
          ifelse(
            is.na(
              species_lookup$common_name
            ) |
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


        latest_species_labels <-
          species_labels[
            as.character(
              latest_species
            )
          ]


        latest_species_labels[
          is.na(
            latest_species_labels
          )
        ] <- latest_species[
          is.na(
            latest_species_labels
          )
        ]

      } else {
        latest_species_labels <-
          latest_species
      }


      status_statement[i] <- paste0(
        "The most recent sampling year was ",latest_year, ", during which ",length(latest_species)," unique ",group_name," species were detected: ",
        paste(unname(latest_species_labels),collapse = ", "),".")
    } else {
      # -----------------------------------------------------
      # Standard desired-trend status statement
      # -----------------------------------------------------

      DATA_latest <- DATA[
        DATA[[year_col]] == last_year,
      ]


      DATA_5_YEARS <- DATA[
        DATA[[year_col]] %in%
          tail(
            yrs,
            5
          ),
      ]


      status_statement[i] <- paste0(
        "The most recent year, ",
        last_year,
        ", shows a mean of ",
        round(
          mean(
            DATA_latest[[indicator_col]],
            na.rm = TRUE
          ),
          2
        ),
        unit_text,
        " (sd = ",
        round(
          sd(
            DATA_latest[[indicator_col]],
            na.rm = TRUE
          ),
          2
        ),
        "). ",
        "The most recent 5 years (",
        paste(
          tail(
            yrs,
            5
          ),
          collapse = ","
        ),
        ") showed a mean of ",
        round(
          mean(
            DATA_5_YEARS[[indicator_col]],
            na.rm = TRUE
          ),
          2
        ),
        unit_text,
        " (sd = ",
        round(
          sd(
            DATA_5_YEARS[[indicator_col]],
            na.rm = TRUE
          ),
          2
        ),
        ")."
      )
    }


    # =======================================================
    # 12. Trend statement
    # =======================================================

    if (grepl('no decrease', sc)) {
      yearly_species <- DATA_use |>
        dplyr::filter(
          !is.na(
            .data[[year_col]]
          ),
          !is.na(
            .data$species
          )
        ) |>
        dplyr::group_by(
          .data[[year_col]]
        ) |>
        dplyr::summarise(
          richness = dplyr::n_distinct(
            .data$species
          ),
          .groups = "drop"
        ) |>
        dplyr::arrange(
          .data[[year_col]]
        )


      group_name <- get_group_name(
        DATA_use
      )


      if (nrow(yearly_species) <= 1) {
        trend_statement[i] <- paste0("There is only one year of sampling data ","available for ",group_name,".")
      } else {
        first_richness <- dplyr::first(
          yearly_species$richness
        )

        last_richness <- dplyr::last(
          yearly_species$richness
        )

        first_year <- dplyr::first(
          yearly_species[[year_col]]
        )

        last_year_use <- dplyr::last(
          yearly_species[[year_col]]
        )


        if (last_richness > first_richness) {
          direction <- "increased"
        } else if (
          last_richness < first_richness
        ) {
          direction <- "declined"
        } else {
          direction <- "remained stable"
        }

        trend_statement[i] <- paste0("The number of unique ",group_name," species detected ",direction, " from ",first_richness," to ",last_richness,
          " between ",first_year," and ",last_year_use,".")
      }

    } else {
      # -----------------------------------------------------
      # Standard linear-regression trend statement
      # -----------------------------------------------------

      yrs_use <- sort(unique(
        as.numeric(
          DATA_use[[year_col]][
            !is.na(
              DATA_use[[year_col]]
            )
          ]
        )
      ))


      if (length(yrs_use) > 1 && !is.na(slope_year[i])) {
        trend_dir <- ifelse(
          slope_year[i] > 0,
          "increase",
          "decrease"
        )

        DATA_use_5 <- DATA_use[DATA_use[[year_col]] %in% tail(yrs_use, 5),]


        if (length(unique(DATA_use_5[[year_col]])) > 1) {
          model5 <- stats::lm(stats::reformulate(response = indicator_col,termlabels = year_col),data = DATA_use_5)
          coeffs5 <- summary(
            model5
          )$coefficients

          slope5 <- coeffs5[2, 1]
          p5 <- coeffs5[2, 4]

          trend5 <- ifelse(
            slope5 > 0,
            "increase",
            "decrease"
          )


          trend_statement[i] <- paste0( "Linear regression shows a ",trend_dir," of ", round(slope_year[i],2),unit_text,
            " over ",length(yrs_use)," years (p = ",round(p[i],3),"). ","Over the last 5 years (",
            paste(tail(yrs_use,5),collapse = ","),") the trend shows a ",trend5," of ",round(slope5, 2),
            unit_text," (p = ",round(p5,3),").")
        } else {
          trend_statement[i] <- paste0("Linear regression shows a ",trend_dir," of ",round(slope_year[i],2),unit_text,
            " over ",length(yrs_use), " years (p = ",round(p[i], 3),"). ","Insufficient data for 5-year trend.")
        }
      } else {
        trend_statement[i] <-
          "Insufficient data for trend analysis."
      }
    }
  }

  nesteddata$slope_year <- slope_year
  nesteddata$p <- p
  nesteddata$score <- score
  nesteddata$status_statement <- status_statement
  nesteddata$trend_statement <- trend_statement
  nesteddata$score_note <- score_note
  nesteddata
}
