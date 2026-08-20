new_assess_indicator <- function(
    data,
    scoring,
    direction,
    areas,
    year,
    indicator_var_name,
    areaID,
    other_nest_variables,
    type,
    units,
    PPTID,
    project_short_title,
    climate,
    design_target,
    latitude,
    longitude,
    crs,
    indicator,
    control_polygon,
    regionID
) {
  assumptions_storage <- attr(data, 'assumptions')
  caveats_storage <- attr(data, 'caveats')

  score_note <- "No year_of_data_collection column available. Score based on full dataset."


  ## Determining which geom to use for join (for externalData, e.g. seals)

  old_geom_col <- attr(areas, "sf_column")  # original geometry

  geom_to_use <- if ("geom_external_buffer" %in% names(areas)) {
    "geom_external_buffer"   # 🔴 use buffered
  } else {
    attr(areas, "sf_column") # 🔴 use original geometry
  }
  areas_use <- areas
  if (geom_to_use == 'geom_external_buffer') {
    areas_use <- areas_use |>
      st_set_geometry("geom_external_buffer")
  }
  ## end externalData

  if (startsWith(scoring, "desired trend")) {
    nesteddata <- assess_desired_trend(
      data = data,
      areas = areas,
      areas_use = areas_use,
      year = {{ year }},
      indicator_var_name = {{ indicator_var_name }},
      latitude = {{ latitude }},
      longitude = {{ longitude }},
      areaID = {{ areaID }},
      scoring = scoring,
      crs = crs,
      other_nest_variables = other_nest_variables,
      indicator = indicator,
      type = type,
      units = units,
      PPTID = PPTID,
      project_short_title = project_short_title,
      climate = climate,
      design_target = design_target
    )
  } else if (startsWith(scoring, "representation")) {
    nesteddata <- assess_representation_regional(
      data = data,
      areas = areas,
      indicator_var_name = indicator_var_name,
      areaID = areaID,
      regionID = regionID,
      scoring = scoring,
      indicator = indicator,
      type = type,
      units = units,
      PPTID = PPTID,
      project_short_title = project_short_title,
      climate = climate,
      design_target = design_target,
      other_nest_variables = other_nest_variables
    )
  } else if (startsWith(scoring, "protection coverage")) {
    nesteddata <- assess_protection_coveraage(
      data = data,
      areas = areas,
      indicator_var_name = indicator_var_name,
      areaID = areaID,
      scoring = scoring,
      indicator = indicator,
      type = type,
      units = units,
      PPTID = PPTID,
      project_short_title = project_short_title,
      climate = climate,
      design_target = design_target,
      other_nest_variables = other_nest_variables
    )

  } else if (startsWith(scoring, "mpa effect")) {
    nesteddata <- assess_mpa_effect(
      data = data,
      scoring = scoring,
      areas_use = areas_use,
      control_polygons = control_polygons,
      areaID = areaID,
      indicator_var_name = indicator_var_name,
      latitude = latitude,
      longitude = longitude,
      crs = crs,
      other_nest_variables = other_nest_variables,
      indicator = indicator,
      type = type,
      units = units,
      PPTID = PPTID,
      project_short_title = project_short_title,
      climate = climate,
      design_target = design_target,
      year=year
    )

    } else if (scoring %in% c('community retention')) {
      nesteddata <- assess_community_retention(
        data = data,
        areas = areas,
        areas_use = areas_use,
        areaID = {{ areaID }},
        scoring = scoring,
        crs = crs,
        indicator = indicator,
        type = type,
        units = units,
        PPTID = PPTID,
        project_short_title = project_short_title,
        climate = climate,
        design_target = design_target
      )




  } else {
    warning("scoring method not supported")
  }

  if (direction == "inverse") {
    nesteddata <- nesteddata |>
      mutate(score = 100 - score)
  } else if (direction != "normal") {
    stop("direction must be 'normal' or 'inverse'")
  }

  if (!('quality_statement' %in% names(nesteddata))) {
    nesteddata$quality_statement <- NA

    if (any(is.na(nesteddata$areaID))) {nesteddata$areaID[which(is.na(nesteddata$areaID))] <- "Non_Conservation_Area"}
    #good until here
    for (i in seq_along(nesteddata$data)) {
      # Note a sample means unique date and geometry. If there are multiple depths in a single sample it counts as one sample
      message(i)
      quality_data <- nesteddata$data[[i]]

      if (!(is.null(quality_data))) {
        if (!(grepl("Network design", indicator))) {
          GEOM <- attr(quality_data, "sf_column")

          if (is.null(GEOM)) {
            GEOM <- names(quality_data)[which(grepl("geom", names(quality_data)))]
          }
          if (any(grepl("GEOMETRYCOLLECTION", class(quality_data[[GEOM]][1])))) {
            nesteddata$quality_statement[i] <- paste0(
              nesteddata$areaID[i],
              ": ",
              "There are no quality statements available for GEOMETRYCOLLECTION type"
            )
          } else if (any(grepl("POLYGON", class(quality_data[[GEOM]][1])))) {
            nesteddata$quality_statement[i] <- paste0(
              nesteddata$areaID[i],
              ": ",
              "There are no quality statements available for POLYGON type"
            )
          } else if (year %in% names(quality_data)) {
            number_of_samples <- quality_data %>%
              distinct({{ year }}, .data[[GEOM]]) %>%
              summarise(n_samples = n())

            if (any(grepl("data.frame", class(number_of_samples)))) {
              number_of_samples <- number_of_samples$n_samples
            }
            min_year <- min(sort(as.numeric(unique(quality_data[[year]]))))
            max_year <- max(sort(as.numeric(unique(quality_data[[year]]))))
            if (min_year == max_year) {
              nesteddata$quality_statement[i] <- paste0(
                nesteddata$areaID[i],
                ": ",
                number_of_samples,
                " samples taken (",
                min_year,
                ")"
              )
            } else {
              nesteddata$quality_statement[i] <- paste0(
                nesteddata$areaID[i],
                ": ",
                number_of_samples,
                " samples taken (",
                min_year,
                "-",
                max_year,
                ")"
              )
            }
          } else {
            nesteddata$quality_statement[i] <- paste0(
              nesteddata[[areaID]][i],
              ": based on ",
              nrow(quality_data), " data point"
            )
          }
        } else {
          nesteddata$quality_statement[i] <- NA
        }
      } else {
        nesteddata$quality_statement[i] <- NA
      }
    }

  }
  if ("geom_external_buffer" %in% names(areas)) {
    assumptions_storage <- paste0(assumptions_storage, " Note: This analysis includes data that is outside of the conservation area boundary. It assumes that the data outside of the boundary is comparible.")

  }

  attr(nesteddata, "assumptions") <- assumptions_storage
  attr(nesteddata, "caveats") <- caveats_storage

  # SWITCH BACK TO ORIGINAL GEOMETRY
  areas_use <- areas_use |>
    st_set_geometry(old_geom_col)
  return(nesteddata)
}
