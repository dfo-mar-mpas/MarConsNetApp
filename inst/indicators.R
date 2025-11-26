source("inst/set_up.R")

indicator_targets <- list(

  # INDICATORS WITH DATA ----

  tar_target(ind_otn_number_of_recievers,
             command={


               # Looking at number of receivers

               DF <- data_otn_recievers[-which(is.na(data_otn_recievers$stn_lat) | is.na(data_otn_recievers$stn_long)),]
               df <- DF %>%
                 st_as_sf(coords = c("stn_long", "stn_lat"), crs = 4326) %>%  # create geometry
                 dplyr::select(FID, deploy_date, geometry)

               df_with_areaID <- st_join(df, MPAs %>% dplyr::select(NAME_E), left = TRUE)

               # Rename for clarity
               df_with_areaID <- df_with_areaID %>%
                 rename(areaID = NAME_E)

               number_receivers_in_mpas <- list()

               for (i in seq_along(unique(df_with_areaID$areaID))) {
                 if (!(is.na(unique(df_with_areaID$areaID)[i]))) {
                   number_receivers_in_mpas[[i]] <- length(which(df_with_areaID$areaID == unique(df_with_areaID$areaID)[i]))
                 } else {
                   number_receivers_in_mpas[[i]] <- length(which(is.na(df_with_areaID$areaID)))

                 }
               }
               names(number_receivers_in_mpas) <- unique(df_with_areaID$areaID)

             }),

  tar_target(ind_otn_proportion_tags_detected_in_multiple_mpas,
             command={

               tester <- data_otn_tags
               names(tester)[which(names(tester) == "NAME_E.x")] <- "areaID"

               #names(data_otn_tags)[which(names(data_otn_tags) == "NAME_E.x")] <- "areaID"

               x <- data.frame("areaID"=MPAs$NAME_E)
               x$indicator <- "Proportion of Tags Detected in More than One MPA"
               x$type <- "Ocean Tracking Network"
               x$units <- "%"
               x$scoring <- "connectivity-proportion"
               x$PPTID <- NA
               x$project_short_title <- "Ocean Tracking Network Project"
               x$climate <- FALSE
               x$design_target <- FALSE
               x$data <- NULL
               x$score <- NA
               x$status_statement <- NA
               x$trend_statement <- "There is no relevant trend statement available."
               x$source <- "Ocean Tracking Network"
               x$climate_expectation <- "FIXME"
               x$objectives = paste0(c(
                 "Minimize harmful impacts from human activities on cetacean populations and their habitats",
                 "Help maintain healthy populations of species of Aboriginal, commercial, and/or recreational importance",
                 "Contribute to the recovery and conservation of depleted species"
               ), collapse=" ;;; ")
               x$indicator_rationale <- "The exchange of individuals between conservation sites can support ecosystem resilience, population recovery, genetic exchange, and the maintenance of biodiversity"
               x$bin_rationale <- NA
               x$plot <-NULL
               x$readiness <- "Ready"


               # Doing the score and statement status

               tracking <- split(tester , tester $tag_id)

               otn_areas <- vector("list", length = length(tracking))
               names(otn_areas) <- names(tracking)
               for (i in seq_along(tracking)) {
                 message(i)
                 oa <- unique(tracking[[i]]$areaID)
                 if (length(oa) == 1 && oa == "Non_Conservation_Area") {
                   oa <- NULL
                 }

                 if (!(is.null(oa)) && any(oa == "Non_Conservation_Area")) {
                   oa <- oa[-which(oa == "Non_Conservation_Area")]
                 }
                 otn_areas[[i]] <- oa
               }
               otn_areas_old <- otn_areas
               otn_areas <- Filter(Negate(is.null), otn_areas)

               proportion_in_different_mpa <- data.frame(areaID=unique(unlist(otn_areas)), number_in_mpa=NA, proportion_in_mpa_and_another=NA, connected_MPA=NA)

               for (i in seq_along(proportion_in_different_mpa$areaID)) {
                 message(i)
                 keep <- unname(which(sapply(otn_areas, function(x) proportion_in_different_mpa$areaID[i] %in% x)))
                 proportion_in_different_mpa$number_in_mpa[i] <- length(keep)

                 if(!any(unname(unlist(lapply(otn_areas, length)))[keep] > 1)) { # Checking if in more than 1 MPA
                   proportion_in_different_mpa$proportion_in_mpa_and_another[i] <- 0
                 } else {
                   multiple_mpas <- length(which(unname(unlist(lapply(otn_areas, length)))[keep] > 1))
                   proportion_in_different_mpa$proportion_in_mpa_and_another[i] <- round(multiple_mpas/length(keep)*100,2)
                   proportion_in_different_mpa$connected_MPA[i] <- paste0(unique(unlist(unname(otn_areas[keep])))[-which(unique(unlist(unname(otn_areas[keep]))) == proportion_in_different_mpa$areaID[i])], collapse=",")
                 }
               }

               for (i in seq_along(unique(x$areaID))) {
                 if (any(proportion_in_different_mpa$areaID == unique(x$areaID)[i])) {
                   x$score[which(x$areaID == unique(x$areaID)[i])] <- proportion_in_different_mpa$proportion_in_mpa_and_another[which(proportion_in_different_mpa$areaID == unique(x$areaID)[i])]
                   keep <- which(proportion_in_different_mpa$areaID == unique(x$areaID)[i])
                   connected_mpa <- ifelse(proportion_in_different_mpa$proportion_in_mpa_and_another[keep] == 0, ".",paste0("(", proportion_in_different_mpa$connected_MPA[keep], ")."))

                   ss <- paste0("This protected area has had ",proportion_in_different_mpa$number_in_mpa[keep], " OTN tags detected in the area. Of those, ", proportion_in_different_mpa$proportion_in_mpa_and_another[keep], " % were detected in another MPA ",connected_mpa )
                   x$status_statement[which(x$areaID == unique(x$areaID)[i])] <- ss
                 } else {
                   x$score[which(x$areaID == unique(x$areaID)[i])] <- NA
                   x$status_statement[which(x$areaID == unique(x$areaID)[i])] <- NA

                 }

               }
               # Plotting
               x$plot <- vector("list", nrow(x))
               x$data <- vector("list", nrow(x))

               for (i in seq_along(unique(x$areaID))) {
                 message(i)
                 name_of_interest <- unique(x$areaID)[i]

                 k1 <- which(!unname(sapply(otn_areas_old, is.null)))
                 k2 <- which(unname(sapply(otn_areas_old, function(xx) name_of_interest %in% xx)))
                 keep <- intersect(k1, k2)

                 if (length(keep) > 0) {
                   # Combine tracking data
                   df <- do.call(rbind, tracking[keep])
                   df <- df[df$areaID != "Non_Conservation_Area", ]
                   df$tag <- as.character(df$tag)
                   df$eventDate <- as.POSIXct(df$eventDate)

                   if (!inherits(df, "sf")) {
                     df <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
                   }

                   # Land background
                   land <- ne_countries(scale = "medium", returnclass = "sf")

                   # Relevant MPAs
                   mpas_sub <- MPAs$geoms[MPAs$NAME_E %in% unique(df$areaID), ]

                   # Split tracks by tag
                   subset_track <- split(df, df$tag_id)
                   all_tags <- unique(unlist(lapply(subset_track, function(tt) tt$tag)))

                   # Color palette
                   pal <- viridis(length(all_tags))
                   names(pal) <- all_tags

                   # Extract coordinates
                   df_coords <- df %>%
                     mutate(
                       X = st_coordinates(geometry)[,1],
                       Y = st_coordinates(geometry)[,2]
                     )

                   df_path <- df_coords %>%
                     arrange(tag, eventDate)

                   # Determine plot limits based on MPAs or tracking data
                   combined_geom <- c(st_geometry(mpas_sub), st_geometry(df))

                   # Compute bounding box
                   bbox <- st_bbox(st_sfc(combined_geom, crs = st_crs(df)))

                   # Create ggplot
                   map <- ggplot() +
                     geom_sf(data = land, fill = "grey90", color = "white", size = 0.2) +
                     geom_sf(data = mpas_sub, fill = "grey60", alpha = 0.3, color = "grey40") +
                     geom_path(
                       data = df_path,
                       aes(x = X, y = Y, group = tag, color = tag),
                       linewidth = 0.8
                     ) +
                     geom_point(
                       data = df_coords,
                       aes(x = X, y = Y, color = tag),
                       size = 1.5
                     ) +
                     scale_color_manual(values = pal) +
                     coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
                              ylim = c(bbox["ymin"], bbox["ymax"])) +  # zoom to region
                     theme_minimal() +
                     theme(legend.position = "bottom") +
                     labs(color = "Tag",
                          x = "Longitude",
                          y = "Latitude")

                   # Store results
                   x$plot[[i]] <- map
                   x$data[[i]] <- tester[tester$areaID == name_of_interest, c("tag_id", "geometry")]

                   desired_order <- c(
                     "areaID", "indicator", "type", "units", "scoring",
                     "PPTID", "project_short_title", "climate", "design_target", "data",
                     "score", "status_statement", "trend_statement", "source", "climate_expectation",
                     "indicator_rationale", "objectives", "bin_rationale", "plot"
                   )

                   x <- x[ , desired_order]




                 }
               }

               as_tibble(x)


             }
  ),

  tar_target(ind_fish_length,
             command = {

               data <- rv_data_det |>
                 mutate(longitude = LONGITUDE,
                        latitude = LATITUDE,
                        fish_length = FLEN,
                        year = YEAR)  |>
                 dplyr::select(longitude, latitude, year, fish_length)

               x <- process_indicator(data = data,
                                 indicator_var_name = "fish_length",
                                 indicator = "Fish Length",
                                 type = "Ecosystem Trawl Survey",
                                 units = "cm",
                                 scoring = "desired state: increase",
                                 PPTID = 726,
                                 source="RV Survey",
                                 project_short_title = "RV Survey",
                                 areas = MPAs,
                                 climate_expectation="FIXME",
                                 indicator_rationale="FIXME",
                                 bin_rationale="FIXME",
                                 plot_type = c("violin", "map"),
                                 objectives= c(
                                   "Maintain productivity of harvested species",
                                   "Help maintain healthy populations of species of Aboriginal, commercial, and/or recreational importance",
                                   "Allow sufficient escapement from exploitation for spawning",
                                   "Contribute to the recovery and conservation of depleted species"
                                 ),
                                 plot_lm=FALSE)

               x
             }
  ),

  tar_target(ind_fish_weight,
             command = {

               data <- rv_data_det |>
                 mutate(longitude = LONGITUDE,
                        latitude = LATITUDE,
                        fish_weight = FWT,
                        year = YEAR)  |>
                 dplyr::select(longitude, latitude, year, fish_weight)

               process_indicator(data = data,
                                 indicator_var_name = "fish_weight",
                                 indicator = "Fish Weight",
                                 type = "Ecosystem Trawl Survey",
                                 units = "g",
                                 scoring = "desired state: increase",
                                 PPTID = 726,
                                 source="RV Survey",
                                 climate_expectation="FIXME",
                                 indicator_rationale="FIXME",
                                 bin_rationale="FIXME",
                                 project_short_title = "RV Survey",
                                 areas = MPAs,
                                 plot_type = c("violin", "map"),
                                 plot_lm=FALSE,
                                 objectives = c(
                                   "Maintain productivity of harvested species",
                                   "Help maintain healthy populations of species of Aboriginal, commercial, and/or recreational importance",
                                   "Allow sufficient escapement from exploitation for spawning",
                                   "Contribute to the recovery and conservation of depleted species"
                                 ))
             }
  ),

  tar_target(ind_haddock_counts,
             command={
               data = rv_data |>
                 filter(COMM %in% c("HADDOCK")) |>
                 mutate(longitude = LONGITUDE,
                        latitude = LATITUDE,
                        haddock_counts = TOTNO,
                        year = YEAR)  |>
                 dplyr::select(longitude, latitude, year, haddock_counts)

               if (any(is.na(data$latitude) | is.na(data$longitude))) {
                 data <- data[-which(is.na(data$latitude) | is.na(data$longitude)),]
               }

               process_indicator(data = data,
                                 indicator_var_name = "haddock_counts",
                                 indicator = "Haddock Number per Tow",
                                 type = "Ecosystem Trawl Survey",
                                 units = "Number per Tow",
                                 scoring = "desired state: increase",
                                 PPTID = 726,
                                 source="RV Survey",
                                 climate_expectation="FIXME",
                                 indicator_rationale="FIXME",
                                 bin_rationale="FIXME",
                                 project_short_title = "RV Survey",
                                 areas = MPAs,
                                 plot_type = c("violin", "map"),
                                 plot_lm=FALSE,
                                 objectives = c(
                                   "Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock",
                                   "Manage the disturbance of benthic habitat that supports juvenile and adult haddock and other groundfish species",
                                   "Maintain productivity of harvested species",
                                   "Help maintain healthy populations of species of Aboriginal, commercial, and/or recreational importance",
                                   "Allow sufficient escapement from exploitation for spawning",
                                   "Contribute to the recovery and conservation of depleted species"
                                 ))
             }),

  tar_target(ind_haddock_biomass,
             command={
               data <- rv_data |>
                 filter(COMM %in% c("HADDOCK")) |>
                 mutate(longitude = LONGITUDE,
                        latitude = LATITUDE,
                        haddock_biomass = TOTWGT,
                        year = YEAR)  |>
                 dplyr::select(longitude, latitude, year, haddock_biomass)

               if (any(is.na(data$latitude) | is.na(data$longitude))) {
                 data <- data[-which(is.na(data$latitude) | is.na(data$longitude)),]
               }

               process_indicator(data = data,
                                 indicator_var_name = "haddock_biomass",
                                 indicator = "Biomass of Haddock per Tow",
                                 type = "Ecosystem Trawl Survey",
                                 units = "kg per tow",
                                 scoring = "desired state: increase",
                                 PPTID = 726,
                                 source="RV Survey",
                                 climate_expectation="FIXME",
                                 indicator_rationale="FIXME",
                                 bin_rationale="FIXME",
                                 project_short_title = "RV Survey",
                                 areas = MPAs,
                                 plot_type = c("violin", "map"),
                                 plot_lm=FALSE,
                                 objectives = c(
                                   "Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock",
                                   "Manage the disturbance of benthic habitat that supports juvenile and adult haddock and other groundfish species",
                                   "Maintain productivity of harvested species",
                                   "Help maintain healthy populations of species of Aboriginal, commercial, and/or recreational importance",
                                   "Allow sufficient escapement from exploitation for spawning",
                                   "Contribute to the recovery and conservation of depleted species"
                                 )
               )
             }),


  tar_target(ind_zooplankton,
             command={

               data <- data_azmp_zooplankton_annual_stations |>
                 mutate(Calanus_finmarchicus_biomass = Calanus_finmarchicus_log10)  |>
                 dplyr::select(longitude, latitude, year, Calanus_finmarchicus_biomass)

               process_indicator(data = data,
                                 indicator = "Biomass of Zooplankton (Calanus finmarchicus)",
                                 indicator_var_name = "Calanus_finmarchicus_biomass",
                                 type = "Zooplankton Net Tows",
                                 units = "log10 of abundance",
                                 scoring = "desired state: increase",
                                 PPTID = 579,
                                 source="AZMP",
                                 climate_expectation="FIXME",
                                 indicator_rationale="FIXME",
                                 bin_rationale="FIXME",
                                 project_short_title = "AZMP",
                                 areas = MPAs,
                                 plot_type=c('time-series','map'),
                                 plot_lm=FALSE,
                                 objectives = c(
                                   "Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                   "Maintain/promote ecosystem structure and functioning",
                                   "Maintain Functional Biodiversity",
                                   "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"
                                 ))
             }),


  tar_target(ind_zooplankton_community_composition,
             command={
               data <- data_azmp_zooplankton_annual_stations %>%
                 pivot_longer(cols = matches("_log10$"), names_to = "taxa", values_to = "log_biomass") %>%
                 mutate(biomass = 10^log_biomass) %>%
                 group_by(station, year) %>%
                 mutate(relative_biomass = biomass / sum(biomass, na.rm = TRUE)) %>%
                 ungroup()

               data <- data[-which(is.na(data$relative_biomass)),]


               process_indicator(data = data,
                                 indicator = "Zooplankton Community Composition",
                                 indicator_var_name = "relative_biomass",
                                 type = "Zooplankton Net Tows",
                                 units = NA,
                                 scoring = "desired state: increase",
                                 PPTID = 579,
                                 source="AZMP",
                                 climate_expectation="FIXME",
                                 indicator_rationale="Zooplankton shifts driven by climate change can cause declines in food quality for fish (e.g., Heneghan et al. 2023).",
                                 bin_rationale="FIXME",
                                 project_short_title = "AZMP",
                                 other_nest_variables = c("zooplankton_meso_dry_weight","log_biomass", "biomass", "relative_biomass", "station", "taxa"),
                                 areas = MPAs,
                                 plot_type=c('community-composition', 'map'),
                                 plot_lm=FALSE,
                                 objectives = c(
                                   "Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                   "Maintain/promote ecosystem structure and functioning",
                                   "Maintain Functional Biodiversity",
                                   "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"
                                 ))
             }),




  tar_target(ind_surface_height,
             command={
               data <- azmpdata::Derived_Monthly_Stations |>
                 left_join(data_azmp_fixed_stations, by = "station")  |>
                 dplyr::select(longitude, latitude, year, sea_surface_height)

               process_indicator(data = data,
                                 indicator_var_name = "sea_surface_height",
                                 indicator = "sea surface height",
                                 type = "derived (AZMP)",
                                 units = "m",
                                 scoring = "desired state: decrease",
                                 PPTID = 579,
                                 source="AZMP",
                                 project_short_title = "AZMP",
                                 climate = TRUE,
                                 climate_expectation="FIXME",
                                 indicator_rationale="FIXME",
                                 bin_rationale="FIXME",
                                 areas = MPAs,
                                 plot_type=c('time-series','map'),
                                 plot_lm=FALSE,
                                 objectives=NA)
             }),

  tar_target(ind_nitrate,
             command={
               data <- data_azmp_Discrete_Occupations_Sections  |>
                 dplyr::select(longitude, latitude, year, depth, nitrate)
               process_indicator(data = data,
                                 indicator_var_name = "nitrate",
                                 indicator = "Nutrient Conditions (Nitrate)",
                                 type = "Discrete Occupations Sections",
                                 units = "mmol/m3",
                                 scoring = "desired state: decrease",
                                 PPTID = 579,
                                 source="AZMP",
                                 project_short_title = "AZMP",
                                 climate = TRUE,
                                 climate_expectation="FIXME",
                                 indicator_rationale="Changes in nutrient levels can affect biological productivity of the ocean and lead to trophic cascades (e.g., Petersen et al. 2017; Thingstad 2020).",
                                 bin_rationale="FIXME",
                                 other_nest_variables="depth",
                                 areas = MPAs,
                                 plot_type = c('time-series','map'),
                                 plot_lm=FALSE,
                                 objectives = c(
                                   "Control alteration of nutrient concentrations affecting primary production",
                                   "Maintain/promote ecosystem structure and functioning",
                                   "Maintain Ecosystem Resistance",
                                   "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"
                                 ))
             }),

  tar_target(ind_oxygen,
             command={
               MPAs
               data2 <- data_gliders
               year <- as.numeric(format(data2$time, "%Y"))
               data2$year <- year
               data2 <- data2[which(!is.na(data2$DOXY)),]
               data2 <- data2[,c("longitude", "latitude", "year", "DOXY", "depth")]
               process_indicator(data = data2,
                                 indicator_var_name = "DOXY",
                                 indicator = "Oxygen",
                                 type = "Gliders",
                                 units = "mu * mol/kg",
                                 scoring = "desired state: increase",
                                 PPTID = 385,
                                 source="Glider Program",
                                 project_short_title = "Glider Program",
                                 climate = TRUE,
                                 climate_expectation="FIXME",
                                 indicator_rationale="Deoxygenation can impact marine life and its ecosystem directly and indirectly, and lead to changes in the abundance and distribution of fish, which, in turn, affects fisheries and productivity (e.g., Kim et al. 2023). This variable may be particularly important to monitor in deep habitats, where oxygen levels are depleted.",
                                 bin_rationale="FIXME",
                                 other_nest_variables="depth",
                                 areas = MPAs,
                                 plot_type = c('time-series','map'),
                                 plot_lm=FALSE,
                                 objectives = c(
                                   "Maintain/promote ecosystem structure and functioning",
                                   "Maintain Ecosystem Resistance",
                                   "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"
                                 ))

             }),

  tar_target(ind_stratification,
             command={
               MPAs
               data <- data_gliders
               year <- as.numeric(format(data$time, "%Y"))
               data$year <- year
               data <- data[which(!is.na(data$mld)),]
               data <- data[,c("longitude", "latitude", "year", "mld", "depth")]
               x <- process_indicator(data = data,
                                      indicator_var_name = "mld",
                                      indicator = "Mixed Layer Depth",
                                      type = "Gliders",
                                      units = "m",
                                      scoring = "desired state: increase",
                                      PPTID = 385,
                                      source="Glider Program",
                                      project_short_title = "Glider Program",
                                      climate = TRUE,
                                      climate_expectation="FIXME",
                                      indicator_rationale="Stratification of the mixed layer plays a complementary role in phytoplankton blooms (e.g., Greenan et al. 2004).",
                                      bin_rationale="FIXME",
                                      other_nest_variables="depth",
                                      areas = MPAs,
                                      plot_type = c('time-series','map'),
                                      plot_lm=FALSE,
                                      objectives=NA)
               x

             }),




  tar_target(ind_silicate,
             command={
               data <- data_azmp_Discrete_Occupations_Sections  |>
                 dplyr::select(longitude, latitude, year, depth, silicate)
               process_indicator(data = data,
                                 indicator_var_name = "silicate",
                                 indicator = "Nutrient Conditions (Silicate)",
                                 type = "Discrete Occupations Sections",
                                 units = "mmol/m3",
                                 scoring = "desired state: decrease",
                                 PPTID = 579,
                                 source="AZMP",
                                 project_short_title = "AZMP",
                                 climate = TRUE,
                                 climate_expectation="FIXME",
                                 indicator_rationale="Changes in nutrient levels can affect biological productivity of the ocean and lead to trophic cascades (e.g., Petersen et al. 2017; Thingstad 2020). ",
                                 bin_rationale="FIXME",
                                 other_nest_variables="depth",
                                 areas = MPAs,
                                 plot_type = c('time-series','map'),
                                 plot_lm=FALSE,
                                 objectives = c(
                                   "Control alteration of nutrient concentrations affecting primary production",
                                   "Maintain/promote ecosystem structure and functioning",
                                   "Maintain Ecosystem Resistance",
                                   "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"
                                 ))
             }),

  tar_target(ind_phosphate,
             command={
               data <- data_azmp_Discrete_Occupations_Sections  |>
                 dplyr::select(longitude, latitude, year, depth, phosphate)
               process_indicator(data = data,
                                 indicator_var_name = "phosphate",
                                 indicator = "Nutrient Conditions (Phosphate)",
                                 type = "Discrete Occupations Sections",
                                 units = "mmol/m3",
                                 scoring = "desired state: decrease",
                                 PPTID = 579,
                                 source="AZMP",
                                 project_short_title = "AZMP",
                                 climate = TRUE,
                                 climate_expectation="FIXME",
                                 indicator_rationale="Changes in nutrient levels can affect biological productivity of the ocean and lead to trophic cascades (e.g., Petersen et al. 2017; Thingstad 2020).",
                                 bin_rationale="FIXME",
                                 other_nest_variables="depth",
                                 areas = MPAs[-(which(MPAs$NAME_E =="Musquash Estuary Marine Protected Area")),],
                                 plot_type = c('time-series','map'),
                                 plot_lm=FALSE,
                                 objectives = c(
                                   "Control alteration of nutrient concentrations affecting primary production",
                                   "Maintain/promote ecosystem structure and functioning",
                                   "Maintain Ecosystem Resistance",
                                   "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"
                                 ))
             }),


  tar_target(ind_salinity,
             command={

               data <- data_azmp_Discrete_Occupations_Sections  |>
                 dplyr::select(longitude, latitude, year, depth, salinity)
               process_indicator(data = data,
                                 indicator_var_name = "salinity",
                                 indicator = "Salinity",
                                 type = "Discrete Occupations Sections",
                                 units = "psu",
                                 scoring = "desired state: stable",
                                 PPTID = 579,
                                 source="AZMP",
                                 project_short_title = "AZMP",
                                 other_nest_variables="depth",
                                 areas = MPAs,
                                 climate_expectation="FIXME",
                                 indicator_rationale="Salinity changes can impact ocean biological functions and may produce community shifts including trophic cascades (e.g., Röthig et al. 2023). Changes in salinity can also adversely affect the temperature tolerance of aquatic organisms (e.g., Farias et al. 2024)",
                                 bin_rationale="FIXME",
                                 plot_type=c('time-series','map'),
                                 plot_lm=FALSE,
                                 objectives = c(
                                   "Maintain/promote ecosystem structure and functioning",
                                   "Maintain Ecosystem Resistance",
                                   "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"
                                 ))
             }),

  tar_target(ind_temperature,
             command={
               data <- data_azmp_Discrete_Occupations_Sections  |>
                 dplyr::select(longitude, latitude, year, depth, temperature)

               x <- process_indicator(data = data,
                                      indicator_var_name = "temperature",
                                      indicator = "Temperature",
                                      type = "Discrete Occupations Sections",
                                      units = "C",
                                      scoring = "desired state: decrease",
                                      PPTID = 579,
                                      source="AZMP",
                                      project_short_title = "AZMP",
                                      climate = TRUE,
                                      other_nest_variables="depth",
                                      areas = MPAs,
                                      climate_expectation="FIXME",
                                      indicator_rationale="Changes in temperature influence not only the distribution of species associated with particular water masses (e.g., Alvarez Perez and Santana 2022), but also affect growth and development rates, generation times and productivity of all species (e.g., Shoji et al. 2011; Szuwalski et al. 2021; Millington et al. 2022).",
                                      bin_rationale="FIXME",
                                      plot_type=c('time-series','map'),
                                      plot_lm=FALSE,
                                      objectives = c(
                                        "Maintain/promote ecosystem structure and functioning",
                                        "Maintain Ecosystem Resistance",
                                        "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"
                                      ))
               x
             }),

  tar_target(ind_chlorophyll,
             command={
               data <- data_azmp_Discrete_Occupations_Sections  |>
                 dplyr::select(longitude, latitude, year, depth, chlorophyll)


               process_indicator(data = data,
                                 indicator_var_name = "chlorophyll",
                                 indicator = "Chlorophyll",
                                 type = "Discrete Occupations Sections",
                                 units = "ug/L",
                                 scoring = "desired state: stable",
                                 PPTID = 579,
                                 source="AZMP",
                                 project_short_title = "AZMP",
                                 other_nest_variables="depth",
                                 areas = MPAs,
                                 climate_expectation="FIXME",
                                 indicator_rationale="Chlorophyll a measurements are typically used as a proxy for primary production at the ocean surface, which, in turn, can influence ocean bottom conditions through benthic/pelagic coupling.",
                                 bin_rationale="FIXME",
                                 plot_type=c('time-series','map'),
                                 plot_lm=FALSE,
                                 objectives = c(
                                   "Maintain/promote ecosystem structure and functioning",
                                   "Maintain Functional Biodiversity",
                                   "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"
                                 ))
             }),

  tar_target(ind_bloom_amplitude,
             command={
               script_lines <- readLines("https://raw.githubusercontent.com/BIO-RSG/PhytoFit/refs/heads/master/tools/tools_00c_define_polygons.R")

               k1 <- which(grepl("poly\\$atlantic = list", script_lines))
               k2 <- which(grepl("-61.1957, -61.1957, -59.54983, -59.54983, -61.1957", script_lines))
               script <- script_lines[k1:k2]
               poly <- list()
               eval(parse(text=script))
               DF <- poly$atlantic$AZMP$CSS_V02

               coords <- matrix(c(DF$lon, DF$lat), ncol = 2, byrow = FALSE)
               coords <- rbind(coords, coords[1,])
               polygon_sf <- st_sfc(st_polygon(list(coords)))
               st_crs(polygon_sf) <- 4326


               data <- azmpdata::RemoteSensing_Annual_Broadscale |>
                 filter(area == "CSS_remote_sensing") |>
                 mutate(geometry = polygon_sf) |>
                 st_as_sf() |>
                 dplyr::select(year, bloom_amplitude, geometry) |>
                 st_make_valid()



               process_indicator(data = data,
                                 indicator_var_name = "bloom_amplitude",
                                 indicator = "Bloom Amplitude",
                                 type = "Remote Sensing",
                                 units = "(unit unknown)",
                                 scoring = "desired state: stable",
                                 PPTID = 579,
                                 source="AZMP",
                                 climate_expectation="FIXME",
                                 indicator_rationale="FIXME",
                                 bin_rationale="FIXME",
                                 project_short_title = "AZMP",
                                 areas = MPAs,
                                 plot_type = c("time-series","map"),
                                 plot_lm=FALSE,
                                 objectives = c(
                                   "Maintain/promote ecosystem structure and functioning",
                                   "Maintain Functional Biodiversity",
                                   "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"
                                 ))
             }
  ),

  tar_target(ind_bloom_timing,
             command={
               script_lines <- readLines("https://raw.githubusercontent.com/BIO-RSG/PhytoFit/refs/heads/master/tools/tools_00c_define_polygons.R")

               k1 <- which(grepl("poly\\$atlantic = list", script_lines))
               k2 <- which(grepl("-61.1957, -61.1957, -59.54983, -59.54983, -61.1957", script_lines))
               script <- script_lines[k1:k2]
               poly <- list()
               eval(parse(text=script))
               DF <- poly$atlantic$AZMP$CSS_V02

               coords <- matrix(c(DF$lon, DF$lat), ncol = 2, byrow = FALSE)
               coords <- rbind(coords, coords[1,])
               polygon_sf <- st_sfc(st_polygon(list(coords)))
               st_crs(polygon_sf) <- 4326


               data <- azmpdata::RemoteSensing_Annual_Broadscale |>
                 filter(area == "CSS_remote_sensing") |>
                 mutate(geometry = polygon_sf) |>
                 st_as_sf() |>
                 dplyr::select(year, bloom_start, geometry) |>
                 st_make_valid()


               process_indicator(data = data,
                                 indicator_var_name = "bloom_start",
                                 indicator = "Bloom Start (Timing)",
                                 type = "Remote Sensing",
                                 units = "(days since January 1st)", # FIXME: I think
                                 scoring = "desired state: stable",
                                 PPTID = 579,
                                 source="AZMP",
                                 climate_expectation="FIXME",
                                 indicator_rationale="FIXME",
                                 bin_rationale="The timing of the spring bloom can directly influence the survival success of fish larvae. For example, the spring peak in phytoplankton production, along with high rates of C. finmarchicus reproduction, have been shown to occur within the historical haddock spawning period (Head et al. 2005).",
                                 project_short_title = "AZMP",
                                 areas = MPAs,
                                 plot_type = c("time-series","map"),
                                 plot_lm=FALSE,
                                 objectives = c(
                                   "Maintain/promote ecosystem structure and functioning",
                                   "Maintain Functional Biodiversity",
                                   "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"
                                 ))
             }
  ),

  tar_target(name=ind_phytoplankton,
             command={
               data_azmp_fixed_stations
               # Add rows one by one
               data <- azmpdata::Phytoplankton_Occupations_Stations
               data$latitude <- NA
               data$longitude <- NA
               for (i in seq_along(unique(data$station))) {
                 data$latitude[which(data$station == unique(data$station)[i])] <- data_azmp_fixed_stations$latitude[which(data_azmp_fixed_stations$station == unique(data$station)[i])]
                 data$longitude[which(data$station == unique(data$station)[i])] <- data_azmp_fixed_stations$longitude[which(data_azmp_fixed_stations$station == unique(data$station)[i])]
               }

               data <- data |>
                 st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
                 st_make_valid() |>
                 mutate(sum_phytoplankton = rowSums(across(c(diatoms, dinoflagellates, flagellates)), na.rm = TRUE)) |> # FIXME: I think
                 mutate(year=as.numeric(format(date, "%Y")))|>
                 dplyr::select(sum_phytoplankton, year, geometry)




               process_indicator(data = data,
                                 indicator = "Abundance of Phytoplankton (Diatoms, Dinoflagellates,Flagellates)",
                                 indicator_var_name = "sum_phytoplankton",
                                 type = "Continuous Plankton Recorder",
                                 units = "unit unknown",
                                 scoring = "desired state: increase",
                                 PPTID = 579,
                                 source="AZMP",
                                 climate_expectation="FIXME",
                                 indicator_rationale="Phytoplankton constitutes the base of the marine food web and, consequently, their production sets an upper limit on the production of all higher trophic levels.",
                                 bin_rationale="FIXME",
                                 project_short_title = "AZMP",
                                 areas = MPAs,
                                 plot_type=c('time-series','map'),
                                 plot_lm=FALSE,
                                 objectives = c(
                                   "Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                   "Maintain/promote ecosystem structure and functioning",
                                   "Maintain Functional Biodiversity",
                                   "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"
                                 ))


             }),


  tar_target(name = ind_QC_gulf_biogenic_habitat_representation,
             command = {
               process_indicator(data = data_QC_gulf_biogenic_habitat,
                                 indicator_var_name = "layer",
                                 indicator = "Biogenic Habitat Representation",
                                 type = "Model",
                                 units = NA,
                                 scoring = "representation: cumulative distribution with regional thresholds",
                                 PPTID = NA,
                                 source="Open Data (DFO)",
                                 climate_expectation="FIXME",
                                 indicator_rationale="FIXME",
                                 bin_rationale="FIXME",
                                 project_short_title = "Biogenic Habitat",
                                 areas = MPAs[MPAs$region %in% c("Quebec","Gulf"),],
                                 plot_type='map',
                                 plot_lm=FALSE,
                                 objectives = c(
                                   "Protect cold-water corals",
                                   "Protect cold-water corals and deep water frontier area",
                                   "Protect Vazella pourtalesi glass sponges",
                                   "Protect corals, particularly significant concentrations of sea pens, from harm due to human activities (e.g., fishing, oil and gas exploratory drilling, submarine cable installation and anchoring) in the Laurentian Channel",
                                   "Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
                                   "Conserve and protect marine areas of high biodiversity at the community, species, population and genetic levels within the MPA",
                                   "Protect unique, rare, or sensitive ecological features",
                                   "Protect representative examples of identified ecosystem and habitat types",
                                   "Habitat required for all species, particularly priority species, is maintained and protected"
                                 ))
             }),

  tar_target(name = ind_ebsa_representation,
             command = {
               process_indicator(data = ebsa,
                                 indicator_var_name = "Name",
                                 indicator = "EBSA Representation",
                                 type = "TBD",
                                 units = NA,
                                 scoring = "representation: cumulative distribution with regional thresholds",
                                 PPTID = NA,
                                 source="Open Data (DFO)",
                                 climate_expectation="FIXME",
                                 indicator_rationale="FIXME",
                                 bin_rationale="FIXME",
                                 project_short_title = "Biogenic Habitat",
                                 areas = MPAs,
                                 plot_type='map',
                                 plot_lm=FALSE,
                                 objectives = c(
                                   "Protect cold-water corals",
                                   "Protect cold-water corals and deep water frontier area",
                                   "Protect Vazella pourtalesi glass sponges",
                                   "Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
                                   "Conserve and protect marine areas of high biodiversity at the community, species, population and genetic levels within the MPA",
                                   "Protect unique, rare, or sensitive ecological features",
                                   "Protect representative examples of identified ecosystem and habitat types",
                                   "Habitat required for all species, particularly priority species, is maintained and protected"
                                 )
               )
             }),

  tar_target(name = ind_SAR_CH_representation,
             command = {
               data <- sar_ch |>
                 rowwise() |>
                 mutate(name = if_else(is.na(POP_E),
                                       paste0("Critical Habitat for ", COMMON_E),
                                       paste0("Critical Habitat for ", COMMON_E,": ",POP_E))) |>
                 group_by(name) |>
                 reframe(geoms = st_make_valid(st_union(Shape))) |>
                 st_as_sf()

               process_indicator(data = data,
                                 indicator_var_name = "name",
                                 indicator = "Species At Risk Critical Habitat Representation",
                                 type = "Model",
                                 units = NA,
                                 scoring = "representation: cumulative distribution with regional thresholds",
                                 PPTID = NA,
                                 source="Open Data (DFO)",climate_expectation="FIXME",
                                 indicator_rationale="FIXME",
                                 bin_rationale="FIXME",
                                 project_short_title = "SAR CH Habitat",
                                 areas = MPAs,
                                 plot_type='map',
                                 plot_lm=FALSE,
                                 objectives = c(
                                   "Ensure the conservation and protection of threatened or endangered species",
                                   "Protect cold-water corals",
                                   "Protect cold-water corals and deep water frontier area",
                                   "Protect Vazella pourtalesi glass sponges",
                                   "Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
                                   "Protect unique, rare, or sensitive ecological features",
                                   "Habitat required for all species, particularly priority species, is maintained and protected"
                                 ))
             }),

  tar_target(name = ind_species_representation,
             command = {

               data <- data_obis |>
                 group_by(scientificName) |>
                 reframe(subclass = unique(subclass[!is.na(subclass)])[1],
                         geoms = st_make_valid(st_union(geometry))) |>
                 ungroup() |>
                 st_as_sf()

               process_indicator(data = data,
                                 indicator_var_name = "scientificName",
                                 indicator = "Species Richness (OBIS)",
                                 type = "Observations",
                                 units = NA,
                                 scoring = "representation: cumulative distribution with regional thresholds",
                                 PPTID = NA,
                                 source="OBIS",
                                 project_short_title = "OBIS Occurrences",
                                 bin_rationale="FIXME",
                                 other_nest_variables = "subclass",
                                 climate_expectation="FIXME",
                                 indicator_rationale="FIXME",
                                 areas = MPAs,
                                 plot_type='map-species',
                                 plot_lm=FALSE,
                                 objectives = c(
                                   "Maintain Species Biodiversity",
                                   "Maintain Functional Biodiversity",
                                   "Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                   "Conserve and protect marine areas of high biodiversity at the community, species, population and genetic levels within the MPA",
                                   "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"
                                 ))
             }),

  tar_target(name = ind_MAR_cum_impact,
             command = {
               process_indicator(data = data_MAR_cumulative_impacts$Cumul_Impact_Maritimes_ALL.tif,
                                 indicator_var_name = "Cumul_Impact_Maritimes_ALL.tif",
                                 indicator = "Cumulative Impacts",
                                 type = "Model",
                                 units = NA,
                                 scoring = "median",
                                 direction = "inverse",
                                 PPTID = NA,
                                 source="Open Data (DFO)",
                                 climate_expectation="FIXME",
                                 indicator_rationale="FIXME",
                                 bin_rationale="FIXME",
                                 project_short_title = "Cumulative Impacts",
                                 areas = MPAs[MPAs$region=="Maritimes",],
                                 plot_type='map',
                                 plot_lm=FALSE,
                                 objectives = c(
                                   "Minimize harmful impacts from human activities on cetacean populations and their habitats",
                                   "Minimize the disturbance of seafloor habitat and associated benthic communities caused by human activities",
                                   "Manage human activities to minimize impacts on other commercial and non-commercial living resources",
                                   "Control unintended incidental mortality for all species",
                                   "Distribute population component mortality in relation to component biomass",
                                   "Minimize unintended introduction and transmission of invasive species",
                                   "Control introduction and proliferation of disease/pathogens",
                                   "Minimize aquaculture escapes",
                                   "Pollution is prevented and reduced",
                                   "Protect corals, particularly significant concentrations of sea pens, from harm due to human activities (e.g., fishing, oil and gas exploratory drilling, submarine cable installation and anchoring) in the Laurentian Channel",
                                   "Protect Black Dogfish from human induced mortality (e.g., bycatch in the commercial fishery) in the Laurentian Channel",
                                   "Protect Smooth Skate from human induced mortality (e.g., bycatch in the commercial fishery) in the Laurentian Channel",
                                   "Protect Porbeagle sharks from human induced mortality (e.g., bycatch in the commercial fishery, seismic activities) in the Laurentian Channel",
                                   "Promote the survival and recovery of Northern Wolffish by minimizing risk of harm from human activities (e.g., bycatch in the commercial fishery) in the Laurentian Channel",
                                   "Promote the survival and recovery of Leatherback Sea Turtles by minimizing risk of harm from human activities (e.g., entanglement in commercial fishing gear, seismic activities) in the Laurentian Channel"
                                 ))
             }),

  tar_target(name = ind_MAR_biofouling_AIS,
             command = {
               data <- data_MAR_biofouling_AIS$AIS_AllSpecies_2021_PA |>
                 filter(cover_index == 1) |>
                 group_by(species_name) |>
                 reframe(geoms = st_make_valid(st_union(Shape))) |>
                 st_as_sf()

               process_indicator(data = data,
                                 indicator_var_name = "species_name",
                                 indicator = "Biofouling AIS representation",
                                 type = "Observations",
                                 units = NA,
                                 scoring = "representation",
                                 direction = "inverse",
                                 PPTID = NA,
                                 climate_expectation="FIXME",
                                 indicator_rationale="FIXME",
                                 bin_rationale="FIXME",
                                 source="Open Data (DFO)",
                                 project_short_title = "Biofouling AIS",
                                 areas = MPAs[MPAs$region=="Maritimes",],
                                 plot_type='map',
                                 plot_lm=FALSE,
                                 objectives = c(
                                   "Minimize unintended introduction and transmission of invasive species",
                                   "Prevent and Mitigate Invasive Alien Species"
                                 ))
             }),

  tar_target(name = ind_musquash_infaunal_diversity,
             command = {
               data <- data_musquash_benthic_infauna |>
                 group_by(scientificName_Nom_scientifique) |>
                 reframe(geoms = st_make_valid(st_union(geoms))) |>
                 st_as_sf()

               subclass <- NULL
               for (i in seq_along(data$scientificName_Nom_scientifique)) {
                 message(i)
                 result <- try(worrms::wm_records_name(data$scientificName_Nom_scientifique[i]), silent=TRUE)
                 if (inherits(result, "try-error")) {
                   subclass[i] <- NA
                 } else {
                   aphia_id <- result$AphiaID[1]  # Use the first match, or refine if needed
                   classification <- worrms::wm_classification(id = aphia_id)
                   subclass[i] <- ifelse(length(classification$scientificname[which(classification$rank == "Subclass")]) == 0, NA, classification$scientificname[which(classification$rank == "Subclass")])
                 }
               }

               data$subclass <- subclass

               x <- process_indicator(data = data,
                                      indicator_var_name = "scientificName_Nom_scientifique",
                                      indicator = "Infaunal Diversity",
                                      type = "Observations",
                                      units = NA,
                                      scoring = "representation: cumulative distribution with regional thresholds",
                                      PPTID = 827,
                                      source="Open Data (DFO)",
                                      climate_expectation="FIXME",
                                      indicator_rationale="FIXME",
                                      bin_rationale="FIXME",
                                      other_nest_variables="subclass",
                                      project_short_title = "Musquash benthic monitoring",
                                      areas = MPAs,
                                      plot_type='map-species',
                                      plot_lm=FALSE,
                                      objectives = c(
                                        "Maintain Species Biodiversity",
                                        "Maintain Functional Biodiversity",
                                        "Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                        "Conserve and protect marine areas of high biodiversity at the community, species, population and genetic levels within the MPA",
                                        "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"
                                      ))
               x
             }),
  tar_target(name = ind_musquash_nekton_diversity,
             command = {
               data <- data_musquash_nekton_occurence |>
                 group_by(scientificName) |>
                 reframe(geoms = st_make_valid(st_union(geometry))) |>
                 st_as_sf()

               process_indicator(data = data,
                                 indicator_var_name = "scientificName",
                                 indicator = "Nekton Diversity (ECW)",
                                 type = "Observations",
                                 units = NA,
                                 scoring = "representation: cumulative distribution with regional thresholds",
                                 PPTID = NA,
                                 source="Eastern Charlotte Waterways",
                                 climate_expectation="FIXME",
                                 indicator_rationale="FIXME",
                                 bin_rationale="FIXME",
                                 project_short_title = "ECW Nekton Project",
                                 areas = MPAs[MPAs$NAME_E=="Musquash Estuary Marine Protected Area",],
                                 plot_type='map',
                                 plot_lm=FALSE,
                                 objectives = c(
                                   "Maintain Species Biodiversity",
                                   "Maintain Functional Biodiversity",
                                   "Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                   "Conserve and protect marine areas of high biodiversity at the community, species, population and genetic levels within the MPA",
                                   "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"
                                 ))
             }),

  tar_target(name=ind_musquash_ph,
             command= {
               data <- data_musquash_eutrophication  |>
                 dplyr::select(Lon, Lat, pH, year)

               process_indicator(data = data,
                                 indicator_var_name = "pH",
                                 indicator = "pH",
                                 type = "Discrete Occupations Sections",
                                 units = NA,
                                 scoring = "desired state: increase",
                                 PPTID = NA,
                                 source="Eastern Charlotte Waterways",
                                 project_short_title = "ECW Project",
                                 climate = TRUE,
                                 climate_expectation="FIXME",
                                 indicator_rationale="FIXME",
                                 bin_rationale="FIXME",
                                 areas = MPAs[MPAs$NAME_E=="Musquash Estuary Marine Protected Area",],
                                 plot_type=c('time-series-no-line','map'),
                                 plot_lm=FALSE,
                                 latitude='Lat',
                                 longitude='Lon',
                                 objectives = c(
                                   "Maintain/promote ecosystem structure and functioning",
                                   "Maintain Ecosystem Resistance",
                                   "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"
                                 ))

             }
  ),
  tar_target(name=ind_musquash_dissolved_oxygen,
             command= {
               data <- data_musquash_eutrophication |>
                 rename(DO_mg_L= `DO (mg/L)`) |>
                 dplyr::select(Lon, Lat, DO_mg_L, year)

               process_indicator(data = data,
                                 indicator_var_name = "DO_mg_L",
                                 indicator = "Dissolved Oxygen",
                                 type = "Discrete Occupations Sections",
                                 units = "mg/L",
                                 scoring = "desired state: increase",
                                 PPTID = NA,
                                 source="Eastern Charlotte Waterways",
                                 project_short_title = NA,
                                 climate = TRUE,
                                 climate_expectation="FIXME",
                                 indicator_rationale="FIXME",
                                 bin_rationale="FIXME",
                                 areas = MPAs[MPAs$NAME_E=="Musquash Estuary Marine Protected Area",],
                                 plot_type=c('time-series-no-line','map'),
                                 plot_lm=FALSE,
                                 latitude='Lat',
                                 longitude='Lon',
                                 objectives = c(
                                   "Maintain/promote ecosystem structure and functioning",
                                   "Maintain Ecosystem Resistance",
                                   "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"
                                 ))
             }
  ),

  tar_target(name=ind_musquash_phosphate,
             command= {
               data <- data_musquash_eutrophication |>
                 rename(phosphate= `tot P (mg/L)`) |>
                 dplyr::select(Lon, Lat, phosphate, year)

               process_indicator(data = data,
                                 indicator_var_name = "phosphate",
                                 indicator = "Nutrient Conditions (Phosphate)",
                                 type = "Discrete Occupations Sections",
                                 units = "mg/L",
                                 scoring = "desired state: increase",
                                 PPTID = NA,
                                 source="Eastern Charlotte Waterways",
                                 project_short_title = NA,
                                 climate = TRUE,
                                 climate_expectation="FIXME",
                                 indicator_rationale="Changes in nutrient levels can affect biological productivity of the ocean and lead to trophic cascades (e.g., Petersen et al. 2017; Thingstad 2020).",
                                 bin_rationale="FIXME",
                                 areas = MPAs[MPAs$NAME_E=="Musquash Estuary Marine Protected Area",],
                                 plot_type=c('time-series-no-line','map'),
                                 plot_lm=FALSE,
                                 latitude='Lat',
                                 longitude='Lon',
                                 objectives = c(
                                   "Control alteration of nutrient concentrations affecting primary production",
                                   "Maintain/promote ecosystem structure and functioning",
                                   "Maintain Ecosystem Resistance",
                                   "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"
                                 ))
             }
  ),

  tar_target(name=ind_musquash_secchi,
             command= {
               data <- data_musquash_eutrophication |>
                 rename(secchi= `Secchi  (m)`) |>
                 dplyr::select(Lon, Lat, secchi, year)

               process_indicator(data = data,
                                 indicator_var_name = "secchi",
                                 indicator = "Secchi Depth",
                                 type = "Discrete Occupations Sections",
                                 units = "m",
                                 scoring = "desired state: increase",
                                 PPTID = NA,
                                 source="Eastern Charlotte Waterways",
                                 project_short_title = NA,
                                 climate = TRUE,
                                 climate_expectation="FIXME",
                                 indicator_rationale="The secchi depth can be a powerful indicator of water clarity and can reveal important information about ecosystem health. A higher secchi depth (clearer water) often implies low turbidity, fewer suspended sediments, and potentially lower levels of pollution or eutrophication. Lower secchi depth can indicator higher nutrient inputs, runoff, algal blooms, or sediment disturbance - possibly from nearby land use, fishing activity or climate events.",
                                 bin_rationale="The secchi depth can be a powerful indicator of water clarity and can reveal important information about ecosystem health.",
                                 areas = MPAs[MPAs$NAME_E=="Musquash Estuary Marine Protected Area",],
                                 plot_type=c('time-series-no-line','map'),
                                 plot_lm=FALSE,
                                 latitude='Lat',
                                 longitude='Lon',
                                 objectives = c(
                                   "Maintain/promote ecosystem structure and functioning",
                                   "Maintain Functional Biodiversity",
                                   "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"
                                 ))
             }
  ),

  tar_target(name=ind_musquash_coliform,
             command= {
               data <- data_musquash_coliform |>
                 dplyr::select(latitude, longitude, MPN, year)

               process_indicator(data = data,
                                 indicator_var_name = "MPN",
                                 indicator = "Coliform",
                                 type = "Discrete Occupations Sections",
                                 units = "MPN",
                                 scoring = "desired state: decrease",
                                 PPTID = NA,
                                 source="Eastern Charlotte Waterways",
                                 project_short_title = NA,
                                 climate_expectation="FIXME",
                                 indicator_rationale="FIXME",
                                 bin_rationale="FIXME",
                                 climate = TRUE,
                                 areas = MPAs[MPAs$NAME_E=="Musquash Estuary Marine Protected Area",],
                                 plot_type=c('time-series-no-line','map'),
                                 plot_lm=FALSE,
                                 objectives = c(
                                   "Maintain/promote ecosystem structure and functioning",
                                   "Pollution is prevented and reduced",
                                   "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"
                                 ))
             }
  ),




  tar_target(ind_nitrate_inside_outside,
             command={
               control_polygons
               MPAs
               data <- data_azmp_Discrete_Occupations_Sections  |>
                 dplyr::select(longitude, latitude, year, depth, nitrate)

               x <- process_indicator(data = data,
                                      indicator_var_name = "nitrate",
                                      indicator = "Nutrient Conditions (Nitrate) Inside Outside Comparison",
                                      type = "Discrete Occupations Sections",
                                      units = "mmol/m3",
                                      scoring = "control site linear trend: less inside",
                                      PPTID = 579,
                                      source="AZMP",
                                      project_short_title = "AZMP",
                                      climate_expectation="FIXME",
                                      indicator_rationale="FIXME",
                                      bin_rationale="FIXME",
                                      climate = TRUE,
                                      other_nest_variables="depth",
                                      areas = MPAs,
                                      plot_type = c('outside-comparison','map'),
                                      plot_lm=FALSE,
                                      control_polygon=control_polygons,
                                      objectives = c(
                                        "Control alteration of nutrient concentrations affecting primary production",
                                        "Maintain/promote ecosystem structure and functioning",
                                        "Maintain Ecosystem Resistance",
                                        "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"
                                      ))
               x
             }),

  tar_target(ind_temperature_inside_outside,
             command={
               control_polygons
               MPAs
               data <- data_azmp_Discrete_Occupations_Sections  |>
                 dplyr::select(longitude, latitude, year, depth, temperature)

               x <- process_indicator(data = data,
                                      indicator_var_name = "temperature",
                                      indicator = "Temperature Inside Outside Comparison",
                                      type = "Discrete Occupations Sections",
                                      units = "C",
                                      scoring = "control site linear trend: less inside",
                                      PPTID = 579,
                                      source="AZMP",
                                      project_short_title = "AZMP",
                                      climate = TRUE,
                                      other_nest_variables="depth",
                                      areas = MPAs,
                                      climate_expectation="FIXME",
                                      indicator_rationale="Changes in temperature influence not only the distribution of species associated with particular water masses (e.g., Alvarez Perez and Santana 2022), but also affect growth and development rates, generation times and productivity of all species (e.g., Shoji et al. 2011; Szuwalski et al. 2021; Millington et al. 2022).",
                                      bin_rationale="FIXME",
                                      plot_type=c('outside-comparison','map'),
                                      control_polygon = control_polygons,
                                      plot_lm=FALSE,
                                      objectives = c(
                                        "Maintain/promote ecosystem structure and functioning",
                                        "Maintain Ecosystem Resistance",
                                        "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"
                                      ))
               x
             }),


  tar_target(ind_musquash_phosphate_inside_outside,
             command={

               control_polygons
               MPAs
               data_musquash_eutrophication
               data <- data_musquash_eutrophication |>
                 rename(phosphate= `tot P (mg/L)`) |>
                 dplyr::select(Lon, Lat, phosphate, year)

               x <- process_indicator(data = data,
                                      indicator_var_name = "phosphate",
                                      indicator = "Nutrient Conditions (Phosphate) Inside Outside Comparison",
                                      type = "Discrete Occupations Sections",
                                      units = "mg/L",
                                      scoring = "control site linear trend: less inside",
                                      PPTID = NA,
                                      climate_expectation="FIXME",
                                      indicator_rationale="FIXME",
                                      bin_rationale="FIXME",
                                      source="Eastern Charlotte Waterways",
                                      project_short_title = NA,
                                      climate = TRUE,
                                      areas = MPAs[MPAs$NAME_E=="Musquash Estuary Marine Protected Area",],
                                      plot_type=c('outside-comparison','map'),
                                      plot_lm=FALSE,
                                      latitude='Lat',
                                      longitude="Lon",
                                      control_polygon=control_polygons,
                                      objectives = c(
                                        "Control alteration of nutrient concentrations affecting primary production",
                                        "Maintain/promote ecosystem structure and functioning",
                                        "Maintain Ecosystem Resistance",
                                        "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"
                                      ))
               x
             }),

  tar_target(ind_musquash_coliform_inside_outside,
             command={

               control_polygons
               MPAs
               data_musquash_coliform

               data <- data_musquash_coliform |>
                 dplyr::select(latitude, longitude, MPN, year)

               data <- st_as_sf(data,
                                coords = c("longitude", "latitude"),
                                crs = 4326)

               x <- process_indicator(data = data,
                                      indicator_var_name = "MPN",
                                      indicator = "Coliform Inside Outside Comparison",
                                      type = "Discrete Occupations Sections",
                                      units = "MPN",
                                      scoring = "control site linear trend: less inside",
                                      PPTID = NA,
                                      climate_expectation="FIXME",
                                      indicator_rationale="FIXME",
                                      bin_rationale="FIXME",
                                      source="Eastern Charlotte Waterways",
                                      project_short_title = NA,
                                      climate = TRUE,
                                      areas = MPAs[MPAs$NAME_E=="Musquash Estuary Marine Protected Area",],
                                      plot_type=c('outside-comparison','map'),
                                      plot_lm=FALSE,
                                      control_polygon=control_polygons,
                                      objectives = c(
                                        "Maintain/promote ecosystem structure and functioning",
                                        "Pollution is prevented and reduced",
                                        "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"
                                      ))
             }),

  tar_target(ind_musquash_birds_sample_coverage, command = {
    areaID <- "Musquash Estuary Marine Protected Area"
    sc <- data_musquash_MMMP_birds |>
      as.data.frame() |>
      select(-geometry) |>
      calc_sample_coverage()

    ind <- data.frame(
      areaID = areaID,
      indicator = "Birds Sample Coverage",
      type = "Observations",
      units = NA,
      scoring = "custom",
      PPTID = NA,
      source = "https://naturecounts.ca/nc/default/datasets.jsp?code=MMMP&sec=bmdr",
      project_short_title = "Maritime Marsh Monitoring Program",
      climate = FALSE,
      design_target = FALSE,
      data = I(list(data_musquash_MMMP_birds)),
      score = sc$means[length(sc$means)],
      status_statement = paste0(
        "The sample coverage for the ",
        areaID,
        " is ",
        round(max(sc$means) * 100, 1),
        "%."
      ),
      trend_statement = paste0(
        "The sample coverage is expected to increase by approximately ",
        round(
          (sc$means[length(sc$means)] - sc$means[length(sc$means) - 1]) * 100,
          3
        ),
        "% per sample."
      ),
      climate_expectation = "FIXME",
      indicator_rationale = "Seabirds are outside the scope of DFO mandate and so are not assessed here; however, indicators have been developed and are monitored by the Canadian Wildlife Service of Environment and Climate Change Canada.",
      objectives = paste0(c(
        "Maintain Species Biodiversity",
        "Maintain Functional Biodiversity",
        "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"
      ), collapse=" ;;; "),
      bin_rationale = "FIXME"
    )

    ind$plot <- list(
      ggplot(as.data.frame(sc[-1]), aes(x = N, y = means * 100)) +
        geom_line() +
        geom_ribbon(
          aes(ymin = (means - sd) * 100, ymax = (means + sd) * 100),
          alpha = 0.2
        ) +
        labs(
          title = "Sample Coverage",
          x = "Sample Size",
          y = "Coverage"
        ) +
        theme_classic()
    )
    ind$readiness <- 'Ready'

    as_tibble(ind)
  }),

  tar_target(ind_protconn,
             command = {
               bind_rows(data_protconn_EL_by_region) |>
                 mutate(,
                        indicator = "ProtConn (50km median negative binomial dispersal)",
                        type = "Model",
                        units = NA,
                        scoring = "custom",
                        PPTID = NA,
                        source = NA,
                        project_short_title = "ProtConn",
                        climate = FALSE,
                        design_target = FALSE,
                        status_statement = paste0(
                          ""
                        ),
                        trend_statement = paste0(
                          ""
                        ),
                        climate_expectation = "FIXME",
                        indicator_rationale = "The exchange of individuals between conservation sites can support ecosystem resilience, population recovery, genetic exchange, and the maintenance of biodiversity",
                        bin_rationale = "FIXME",
                        readiness="Ready")
             }),


  # PLACEHOLDER INDICATORS ----

  tar_target(ind_placeholder_df,ind_placeholder(areas = MPAs)),


  tar_target(name = ind_sst,
             command = {
               ind_placeholder(ind_name="Sea Surface Temperature", areas = MPAs, readiness = "Readily Available", source = "AZMP", objectives = c("Protect continental shelf habitats and associated benthic and demersal communities"))
             }), # Environmental Representativity

  tar_target(name = ind_temp_at_depth,
             command = {
               ind_placeholder(ind_name="Temperature at Depth", areas = MPAs, readiness = "Readily Available",
                               source = "AZMP", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities"))
             }), # Environmental Representativity, Ocean Conditions


  tar_target(name = ind_sea_surface_salinity,
             command = {
               ind_placeholder(ind_name="Sea Surface Salinity", areas = MPAs, readiness = "Readily Available",
                               source = "AZMP", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities"))
             }), # Environmental Representativity, Ocean Conditions

  tar_target(name = ind_subsurface_salinity,
             command = {
               ind_placeholder(ind_name="Subsurface Salinity", areas = MPAs, readiness = "Readily Available",
                               source = "AZMP", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities"))
             }), # Environmental Representativity, Ocean Conditions

  tar_target(name = ind_oxygen_saturation,
             command = {
               ind_placeholder(ind_name="Oxygen Saturation", areas = MPAs, readiness = "Readily Available",
                               source = "AZMP", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities",
                                                               "Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock"))
             }), # Environmental Representativity, Ocean Conditions

  tar_target(name = ind_ave_ph_level,
             command = {
               ind_placeholder(ind_name="Average pH Level", areas = MPAs, readiness = "Readily Available",
                               source = "AZMP", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities",
                                                               "Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock"))
             }), # Environmental Representativity, Ocean Conditions

  tar_target(name = ind_carbonate,
             command = {
               ind_placeholder(ind_name="Carbonate", areas = MPAs, readiness = "Readily Available",
                               source = "AZMP", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities",
                                                               "Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock"))
             }), # Environmental Representativity, Ocean Conditions

  tar_target(name = ind_ave_mixed_layer_depth,
             command = {
               ind_placeholder(ind_name="Average Mixed Layer Depth", areas = MPAs, readiness = "Unknown",
                               source = "BNAM", objectives = c("Safeguard habitat, including the physical and chemical properties of the ecosystem, by maintaining water and sediment quality"))
             }), # Environmental Representativity, Ocean Conditions


  tar_target(name = ind_ave_position_of_shelf_slope_front,
             command = {
               ind_placeholder(ind_name="Average Position of Shelf Slope Front", areas = MPAs, readiness = "Unknown",
                               source = "BNAM", objectives = c("Protect continental shelf habitats and associated benthic and demersal communities",
                                                               "Safeguard habitat, including the physical and chemical properties of the ecosystem, by maintaining water and sediment quality"))
             }), # Environmental Representativity, Ocean Conditions?

  tar_target(name = ind_wind_speed_and_storminess,
             command = {
               ind_placeholder(ind_name="Wind Speed and Storminess", areas = MPAs, readiness = "Unknown",
                               source = "Gliders", objectives = c("Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"))
             }), # Environmental Representativity, Ocean Conditions

  tar_target(name = ind_chlorophyll_a,
             command = {
               ind_placeholder(ind_name="Chlorophyll a", areas = MPAs, readiness = "Unknown",
                               source = "AZMP", objectives = c("Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                                               "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)",
                                                               "Control alteration of nutrient concentrations affecting primary production"))
             }), # Structure and Function, Primary Production

  tar_target(name = ind_phytoplankton_biomass_and_diversity,
             command = {
               ind_placeholder(ind_name="Phytoplankton Biomass and Diversity", areas = MPAs, readiness = "Unknown",
                               source = "AZMP", objectives = c("Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                                               "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)",
                                                               "Control alteration of nutrient concentrations affecting primary production"))
             }), # Biomass Metrics, Primary Production

  tar_target(name = ind_spring_bloom,
             command = {
               ind_placeholder(ind_name="Start Date of Spring Bloom", areas = MPAs, readiness = "Unknown",
                               source = "AZMP", objectives = c("Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                                               "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"))
             }), # Structure and Function, Primary Production

  tar_target(name = ind_calanus_finmarchicus,
             command = {
               ind_placeholder(ind_name="Biomass of Calanus finmarchicus", areas = MPAs, readiness = "Unknown",
                               source = "AZMP", objectives = c("Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA"))
             }), # Biomass Metrics, Secondary Production

  tar_target(name = ind_fish_eggs_and_larve,
             command = {
               ind_placeholder(ind_name="Fish Eggs and Larve", areas = MPAs, readiness = "Unknown",
                               source = "Dedicated Surveys", objectives = c("Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock",
                                                                            "Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA"))
             }), # Biomass Metrics, Fish and Fishery Resources

  tar_target(name = ind_seasonal_presence_cetaceans,
             command = {
               ind_placeholder(ind_name="Seasonal Presence/Absence of Cetaceans", areas = MPAs, readiness = "Unknown",
                               source = "Passive Acoustics", objectives = c("Protect unique, rare, or sensitive ecological features"))
             }), # Connectivity, Marine Mammals other top Predators

  tar_target(name = ind_seasonal_presence_seals,
             command = {
               ind_placeholder(ind_name="Seasonal Presence/Absence of Seals", areas = MPAs, readiness = "Unknown",
                               source = "Tagging", objectives = c("Protect unique, rare, or sensitive ecological features"))
             }), # Connectivity, Marine Mammals and other Top Predators

  tar_target(name = ind_seasonal_presence_pelagics,
             command = {
               ind_placeholder(ind_name="Seasonal Presence/Absence of Large Pelagics", areas = MPAs, readiness = "Unknown",
                               source = "Tagging", objectives = c("Protect unique, rare, or sensitive ecological features"))
             }), # Connectivity, Marine Mammals and other Top Predators

  tar_target(name = ind_seasonal_presence_seabirds,
             command = {
               ind_placeholder(ind_name="Seasonal Presence/Absence of Seabirds", areas = MPAs, readiness = "Unknown",
                               source = "CWS Data", objectives = c("Protect unique, rare, or sensitive ecological features"))
             }), # Connectivity, Marine Mammals and other Top Predators?

  tar_target(name = ind_proportion_demersal_fish,
             command = {
               ind_placeholder(ind_name="Proportion of Large and Small Demersal Fish Species", areas = MPAs, readiness = "Unknown",
                               source = "RV Survey", objectives = c("Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock"))
             }), # Structure and Function, Fish and Fishery Resources


  tar_target(name = ind_abundance_invasive_species_webmr,
             command = {
               ind_placeholder(ind_name="Abundance of Aquatic Invasive Species in WEBMER", areas = MPAs, readiness = "Unknown",
                               source = "RV Survey", objectives = c("Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock"))
             }), # Biomass Metrics, Trophic Structure and Function

  tar_target(name = ind_biomass_groundfish_prey_webmr,
             command = {
               ind_placeholder(ind_name="Biomass of Groundfish Prey Species within WEBMR", areas = MPAs, readiness = "Unknown",
                               source = "RV Survey", objectives = c("Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                                                    "Maintain/promote ecosystem structure and functioning"))
             }), # Biomass Metrics, Fish and Fishery Resources

  tar_target(name = ind_abundance_sea_pens_webmr,
             command = {
               ind_placeholder(ind_name="Abundance of Sea Pens within WEBMR", areas = MPAs, readiness = "Unknown",
                               source = "RV Survey", objectives = c("Protect continental shelf habitats and associated benthic and demersal communities",
                                                                    "conserve and protect benthic (seabed) habitats"))
             }), # Biomass Metrics, Benthic Environment

  tar_target(name = ind_epibenthic_infaunal,
             command = {
               ind_placeholder(ind_name="Diversity of Epibenthic and Infaunal Communities", areas = MPAs, readiness = "Unknown",
                               source = "RV Survey", objectives = c("Maintain biodiversity of individual species, communities and populations within the different ecotypes",
                                                                    "Maintain Species Biodiversity"))
             }), # Species Diversity, Benthic Environment

  tar_target(name = ind_community_comp_epibenthic_infaunal,
             command = {
               ind_placeholder(ind_name="Community Composition of Epibenthic and Infaunal Benthic Communities", areas = MPAs, readiness = "Unknown",
                               source = "RV Survey", objectives = c("Maintain biodiversity of individual species, communities and populations within the different ecotypes",
                                                                    "Maintain Species Biodiversity"))
             }), # Biomass Metrics, Benthic Environment

  tar_target(name = ind_biomass_bioturbators,
             command = {
               ind_placeholder(ind_name="Biomass of Bioturbators", areas = MPAs, readiness = "Unknown",
                               source = "RV Survey", objectives = c("Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"))
             }), #  Biomass Metrics, Benthic Environment

  tar_target(name = ind_spatial_extent_ebsa_webmr,
             command = {
               ind_placeholder(ind_name="Spatial Extent of EBSA with WEBMR", areas = MPAs, readiness = "Unknown",
                               source = "Dedicated Surveys", objectives = c("Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
                                                                            "Protect unique, rare, or sensitive ecological features"))
             }), # Environmental Representativity, Ocean Structure and Movement?

  tar_target(name = ind_rel_abundance_groundfish,
             command = {
               ind_placeholder(ind_name="Relative abundance and biomass of select groundfish species", areas = MPAs, readiness = "Readily Available",
                               source = "RV Survey", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities",
                                                                    "Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock"))
             }), # Biomass Metrics, Fish and Fishery Resources

  tar_target(name = ind_size_distribution_groundfish,
             command = {
               ind_placeholder(ind_name="Size Distribution of Select Groundfish Species", areas = MPAs, readiness = "Unknown",
                               source = "RV Survey", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities",
                                                                    "Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock"))
             }), # Biomass Metrics, Fish and Fishery Resources

  tar_target(name = ind_condition_groundfish,
             command = {
               ind_placeholder(ind_name="Condition of Select Groundfish Species", areas = MPAs, readiness = "Unknown",
                               source = "RV Survey", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities",
                                                                    "Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock"))
             }), # Biomass Metrics, Fish and Fishery Resources

  tar_target(name = ind_fecundity_groundfish,
             command = {
               ind_placeholder(ind_name="Fecundity of Select Groundfish Species", areas = MPAs, readiness = "Unknown",
                               source = "RV Survey", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities",
                                                                    "Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock"))
             }), # Biomass Metrics, Fish and Fishery Resources

  tar_target(name = ind_community_comp_demersal,
             command = {
               ind_placeholder(ind_name="Community Composition of Demersal Fish", areas = MPAs, readiness = "Readily Available",
                               source = "RV Survey", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities",
                                                                    "Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock"))
             }), # Biomass Metrics, Fish and Fishery Resources

  tar_target(name = ind_distribution_juv_haddock_habitat,
             command = {
               ind_placeholder(ind_name="Distriburion of Juvenile Haddock Habitat", areas = MPAs, readiness = "Unknown",
                               source = "RV Survey", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities",
                                                                    "Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock"))
             }), # Key Fish Habitat, Fish and Fishery Resources?

  tar_target(name = ind_distribution_key_fish_habitat,
             command = {
               ind_placeholder(ind_name="Distribution of Key Fish Species Habitat", areas = MPAs, readiness = "Readily Available",
                               source = "RV Survey", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities",
                                                                    "Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock"))
             }), # Key Fish Habitat, Fish and Fishery Resources?

  tar_target(name = ind_unauthorized_fishing,
             command = {
               ind_placeholder(ind_name="Amount of Unauthorized Fishing", areas = MPAs, readiness = "Unknown",
                               source = "C&P", objectives = c("Manage human activities to minimize impacts on other commercial and non-commercial living resources",
                                                              "Help maintain healthy populations of species of Aboriginal, commercial, and/or recreational importance"))
             }), # Threats to Productivity, Anthropogenic Pressure and Impacts

  tar_target(name = ind_seabed_disruption_in_around_webmr_fishing,
             command = {
               ind_placeholder(ind_name="Seabed area in and surrounding WEBMR swept by bottom-tending commercial fishing gear", areas = MPAs, readiness = "Unknown",
                               source = "AIS Data", objectives = c("Minimize the disturbance of seafloor habitat and associated benthic communities caused by human activities",
                                                                   "Manage the disturbance of benthic habitat that supports juvenile and adult haddock and other groundfish species",
                                                                   "Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
                                                                   "conserve and protect benthic (seabed) habitats"))
             }), # Threats to Habitat, Anthropogenic Pressure and Impacts

  tar_target(name = ind_total_annual_landings,
             command = {
               ind_placeholder(ind_name="Total annual landings form 4W for directed groundfish fisheries and common bycatch only stocks", areas = MPAs, readiness = "Unknown",
                               source = "Commercial Catch Information", objectives = c("Manage human activities to minimize impacts on other commercial and non-commercial living resources",
                                                                                       "Help maintain healthy populations of species of Aboriginal, commercial, and/or recreational importance"))
             }), # Threats to Productivity, Anthropogenic Pressure and Impacts

  tar_target(name = ind_seabed_disruption_in_around_webmr_research,
             command = {
               ind_placeholder(ind_name="Seabed area in and surrounding WEBMR swept by bottom-tending commercial fishing gear for research and monitoring", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Minimize the disturbance of seafloor habitat and associated benthic communities caused by human activities",
                                                           "Manage the disturbance of benthic habitat that supports juvenile and adult haddock and other groundfish species",
                                                           "Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
                                                           "conserve and protect benthic (seabed) habitats"))
             }), # Threats to Habitat, Anthropogenic Pressure and Impacts

  tar_target(name = ind_vessel_traffic,
             command = {
               ind_placeholder(ind_name="Vessel Traffic Intensity", areas = MPAs, readiness = "Unknown",
                               source = "VMS", objectives = c("Minimize harmful impacts from human activities on cetacean populations and their habitats",
                                                              "Control unintended incidental mortality for all species",
                                                              "Limit disturbing activity in important reproductive areas/seasons"))
             }), # Threats to Habitat, Anthropogenic Pressure and Impacts

  tar_target(name = ind_ocean_sound,
             command = {
               ind_placeholder(ind_name="Ocean Sound", areas = MPAs, readiness = "Unknown",
                               source = "VMS", objectives = c("Minimize harmful impacts from human activities on cetacean populations and their habitats",
                                                              "Limit disturbing activity in important reproductive areas/seasons"))
             }), # Threats to Habitat, Anthropogenic Pressure and Impacts

  tar_target(name = ind_cables,
             command = {
               ind_placeholder(ind_name="Number of cables by type", areas = MPAs, readiness = "Unknown",
                               source = "NRCan", objectives = c("Minimize the disturbance of seafloor habitat and associated benthic communities caused by human activities",
                                                                "Manage the disturbance of benthic habitat that supports juvenile and adult haddock and other groundfish species",
                                                                "Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
                                                                "conserve and protect benthic (seabed) habitats"))
             }), # Threats to Habitat, Anthropogenic Pressure and Impacts

  tar_target(name = ind_well_proximity,
             command = {
               ind_placeholder(ind_name="Number of wells in proximity to WEBMR", areas = MPAs, readiness = "Unknown",
                               source = "Offshore Energy Regulator", objectives = c("Minimize the disturbance of seafloor habitat and associated benthic communities caused by human activities",
                                                                                    "Manage the disturbance of benthic habitat that supports juvenile and adult haddock and other groundfish species",
                                                                                    "Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
                                                                                    "conserve and protect benthic (seabed) habitats"))
             }), # Threats to Habitat, Anthropogenic Pressure and Impacts

  tar_target(name = ind_offshore_wind_developments,
             command = {
               ind_placeholder(ind_name="Number of offshore wind developments within in the vacinity of WEBMR", areas = MPAs, readiness = "Unknown",
                               source = "Offshore Energy Regulator", objectives = c("Manage human activities to minimize impacts on other commercial and non-commercial living resources",
                                                                                    "Habitat required for all species, particularly priority species, is maintained and protected"))
             }), # Threats to Habitat, Anthropogenic Pressure and Impacts

  tar_target(name = ind_contaminant_concentration,
             command = {
               ind_placeholder(ind_name="Concentrations of Contaminants by Type", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Manage human activities to minimize impacts on other commercial and non-commercial living resources",
                                                           "Safeguard habitat, including the physical and chemical properties of the ecosystem, by maintaining water and sediment quality",
                                                           "Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes"))
             }), # Threats to Habitat, Anthropogenic Pressure and Impacts

  tar_target(name = ind_anthropogenic_debris,
             command = {
               ind_placeholder(ind_name="Quantity of Anthropogenic Debris", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Manage human activities to minimize impacts on other commercial and non-commercial living resources",
                                                           "Safeguard habitat, including the physical and chemical properties of the ecosystem, by maintaining water and sediment quality",
                                                           "Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes"))
             }), # Threats to Habitat, Anthropogenic Pressure and Impacts

  tar_target(name = ind_biomass_abund_distribution_musquash,
             command = {
               ind_placeholder(ind_name="Total biomass, abundance, and dustribution of key species in each trophic level", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Maintain productivity of harvested species",
                                                           "Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA"))
             }), # Biomass Metrics, Trophic Structure and Function

  tar_target(name = ind_species_per_trophic,
             command = {
               ind_placeholder(ind_name="Species per trophic level within each habitat type", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Maintain productivity of harvested species",
                                                           "Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA"))
             }), # Functional Diversity, Trophic Structure and Function

  tar_target(name = ind_species_at_risk,
             command = {
               ind_placeholder(ind_name="Number of at risk species within MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Maintain biodiversity of individual species, communities and populations within the different ecotypes"))
             }), # Biomass Metrics, Trophic Structure and Function?

  tar_target(name = ind_disturbed_area,
             command = {
               ind_placeholder(ind_name="Total area and location of habitat type and proportion and frequency disturbed or lost", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Safeguard habitat, including the physical and chemical properties of the ecosystem, by maintaining water and sediment quality",
                                                           "Protect representative examples of identified ecosystem and habitat types",
                                                           "Habitat required for all species, particularly priority species, is maintained and protected"))
             }), # Threats to Habitat, Anthropogenic Pressure and Impacts

  tar_target(name = ind_sediment_regime,
             command = {
               ind_placeholder(ind_name="Hydrodynamic and sediment regime in estuary", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Safeguard habitat, including the physical and chemical properties of the ecosystem, by maintaining water and sediment quality
"))
             }), # Environmental Representativity, Ocean Structure and Movement

  tar_target(name = ind_nutrients,
             command = {
               ind_placeholder(ind_name="Nutrient Concentrations", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Safeguard habitat, including the physical and chemical properties of the ecosystem, by maintaining water and sediment quality",
                                                           "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)",
                                                           "Control alteration of nutrient concentrations affecting primary production"))
             }), # Environmental Representataivity, Ocean Conditions

  tar_target(name = ind_cpue,
             command = {
               ind_placeholder(ind_name="Commercial and recreational fishing catch per unit effort", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Maintain productivity of harvested species",
                                                           "Help maintain healthy populations of species of Aboriginal, commercial, and/or recreational importance",
                                                           "Distribute population component mortality in relation to component biomass",
                                                           "Keep fishing and other forms of mortality moderate"))
             }), # Biomass Metrics, Fish and Fishery Resources

  tar_target(name = ind_bycatch,
             command = {
               ind_placeholder(ind_name="By-catch number per impacted species", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Maintain biodiversity of individual species, communities and populations within the different ecotypes",
                                                           "Control unintended incidental mortality for all species"))
             }), # Biomass Metrics, Anthropogenic Pressure and Impacts

  tar_target(name = ind_nonindigenous_rel_indigenous,
             command = {
               ind_placeholder(ind_name="Number of non-indigenous species relative to indigenous species in MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Maintain biodiversity of individual species, communities and populations within the different ecotypes",
                                                           "Minimize unintended introduction and transmission of invasive species"))
             }), # Biomass Metrics, Trophic Structure and Function?

  tar_target(name = ind_human_perturbation,
             command = {
               ind_placeholder(ind_name="Degree of human induced perturbation or loss", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Safeguard habitat, including the physical and chemical properties of the ecosystem, by maintaining water and sediment quality",
                                                           "Protect representative examples of identified ecosystem and habitat types",
                                                           "Habitat required for all species, particularly priority species, is maintained and protected"))
             }), # Threats to Habitat, Anthropogenic Pressure and Impacts

  tar_target(name = ind_abundance_nbw,
             command = {
               ind_placeholder(ind_name="Abundance of Northern Bottlenose Whales", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Minimize harmful impacts from human activities on cetacean populations and their habitats",
                                                           "Protect unique, rare, or sensitive ecological features",
                                                           "Contribute to the recovery and conservation of depleted species"))
             }), # Biomass Metrics, Marine Mammals and Other Top Predators

  tar_target(name = ind_mpa_use_nbw,
             command = {
               ind_placeholder(ind_name="Use of the Gully by Northern Bottlenose Whales ", areas = MPAs,readiness = "Unknown",
                               source = NA, objectives = c("Minimize harmful impacts from human activities on cetacean populations and their habitats",
                                                           "Protect unique, rare, or sensitive ecological features",
                                                           "Contribute to the recovery and conservation of depleted species",
                                                           "Habitat required for all species, particularly priority species, is maintained and protected"))
             }), # Connectivity?, Marine Mammals and other Top Predators

  tar_target(name = ind_population_characteristics,
             command = {
               ind_placeholder(ind_name="Size, age, and sex structure of the Scotian Shelf population of Northern Bottlenose Whales", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Minimize harmful impacts from human activities on cetacean populations and their habitats",
                                                           "Contribute to the recovery and conservation of depleted species",
                                                           "Ensure the conservation and protection of threatened or endangered species",
                                                           "promote the recovery of at-risk whales and wolffish"))
             }), # Structure and Function, Marine Mammals and other Top Predators

  tar_target(name = ind_fresh_scars,
             command = {
               ind_placeholder(ind_name="% of individuals in the Scotian Shelf Northern Bottlenose population showing fresh scars", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Minimize harmful impacts from human activities on cetacean populations and their habitats",
                                                           "Control unintended incidental mortality for all species",
                                                           "Ensure the conservation and protection of threatened or endangered species",
                                                           "promote the recovery of at-risk whales and wolffish"))
             }), # Threats to Habitat, Marine Mammals and other Top Predators

  tar_target(name = ind_genetic_diversity_nbw,
             command = {
               ind_placeholder(ind_name="Genetic diversity within the Northern Bottlenose Whales", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Conserve and protect marine areas of high biodiversity at the community, species, population and genetic levels",
                                                           "Maintain biodiversity of individual species, communities and populations within the different ecotypes",
                                                           "Ensure the conservation and protection of threatened or endangered species"))
             }), # Genetic Diversity, Marine Mammals and other Top Predators

  tar_target(name = ind_blubber_contaminants,
             command = {
               ind_placeholder(ind_name="Level of contaminants in the blubber of individuals in the Scotian Shelf population of Northern Bottlenose Whales", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Maintain and monitor the quality of water and sediments of the Gully; and",
                                                           "Safeguard habitat, including the physical and chemical properties of the ecosystem, by maintaining water and sediment quality",
                                                           "Minimize harmful impacts from human activities on cetacean populations and their habitats",
                                                           "Ensure the conservation and protection of threatened or endangered species"))
             }), # Threats to Productivity, Marine Mammals and other Top Predators

  tar_target(name = ind_rel_abundance_cetaceans,
             command = {
               ind_placeholder(ind_name="Relative abundances of cetaceans beside Northern Bottlenose Whales in The Gully", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Maintain biodiversity of individual species, communities and populations within the different ecotypes"))
             }), # Biomass Metrics, Marine Mammals and other Top Predators

  tar_target(name = ind_cetacean_presence,
             command = {
               ind_placeholder(ind_name="Cetacean presence and activity in the MPA, year-round", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Minimize harmful impacts from human activities on cetacean populations and their habitats"))
             }), # Structure and Function, Marine Mammals and other Top Predators

  tar_target(name = ind_strandings,
             command = {
               ind_placeholder(ind_name="Number of reported strandings of Scotian Shelf Northern Bottlenose Whales", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Minimize harmful impacts from human activities on cetacean populations and their habitats",
                                                           "Ensure the conservation and protection of threatened or endangered species",
                                                           "Control unintended incidental mortality for all species"))
             }), # Threats to Productivity, Marine Mammals and other Top Predators

  tar_target(name = ind_ship_strikes,
             command = {
               ind_placeholder(ind_name="Number of reported ship strikes on cetaceans in or near the Gully and of strikes on Scotian Shelf Northern Bottlenose Whales elsewhere", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Minimize harmful impacts from human activities on cetacean populations and their habitats",
                                                           "Control unintended incidental mortality for all species",
                                                           "Ensure the conservation and protection of threatened or endangered species"))
             }), # Threats to Habitat, Marine Mammals and other Top Predators, Anthropogenic Pressure and Impacts

  tar_target(name = ind_entanglement_gully,
             command = {
               ind_placeholder(ind_name="Number of reported gear entanglements of cetaceans in or near the Gully and of entanglements of Scotian Shelf Northern Bottlenose Whales elsewhere", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Minimize harmful impacts from human activities on cetacean populations and their habitats",
                                                           "Control unintended incidental mortality for all species",
                                                           "Ensure the conservation and protection of threatened or endangered species"))
             }), # Threats to Habitat, Marine Mammals and other Top Predators, Anthropogenic Pressure and Impacts

  tar_target(name = ind_human_interaction,
             command = {
               ind_placeholder(ind_name="Number of reports of other interactions between human activities and cetaceans in or near the Gully and of interactions with Scotian Shelf Northern Bottlenose Whales", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Minimize harmful impacts from human activities on cetacean populations and their habitats",
                                                           "Control unintended incidental mortality for all species"))
             }), # Threats to Habitat, Marine Mammals and other Top Predators, Anthropogenic Pressure and Impacts

  tar_target(name = ind_coral_distribution,
             command = {
               ind_placeholder(ind_name="Coral distribution, density, and size structure by species at select monitoring sites", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Protect cold-water corals",
                                                           "Protect cold-water corals and deep water frontier area",
                                                           "Protect corals, particularly significant concentrations of sea pens, from harm due to human activities (e.g., fishing, oil and gas exploratory drilling, submarine cable installation and anchoring) in the Laurentian Channel"))
             }), # Biomass Metrics, Benthic Environment

  tar_target(name = ind_coral_diversity,
             command = {
               ind_placeholder(ind_name="Coral Diversity at selcted monitoring sites", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Protect cold-water corals",
                                                           "Protect cold-water corals and deep water frontier area",
                                                           "Conserve and protect marine areas of high biodiversity at the community, species, population and genetic levels within the MPA"))
             }), # Species Diversity, Benthic Environment

  tar_target(name = ind_coral_proportions,
             command = {
               ind_placeholder(ind_name="Proportions of live and dead corals, by species, at selected monitoring sites within the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Protect cold-water corals",
                                                           "Protect cold-water corals and deep water frontier area",
                                                           "Protect corals, particularly significant concentrations of sea pens, from harm due to human activities (e.g., fishing, oil and gas exploratory drilling, submarine cable installation and anchoring) in the Laurentian Channel"))
             }), # Biomass Metrics, Benthic Environment

  tar_target(name = ind_zoanthid_overgrowth,
             command = {
               ind_placeholder(ind_name="Proportion on live corals at selected monitoring sites with the MPA that show zoanthid over-growths, and extent of over-growth in affected colonies", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Protect cold-water corals",
                                                           "Protect cold-water corals and deep water frontier area",
                                                           "Protect corals, particularly significant concentrations of sea pens, from harm due to human activities (e.g., fishing, oil and gas exploratory drilling, submarine cable installation and anchoring) in the Laurentian Channel"))
             }), # Threats to Productivity, Benthic Environment, Trophic Structure and Function

  tar_target(name = ind_trawl_vunerable,
             command = {
               ind_placeholder(ind_name="Relative abundances, size distributions and diversity of selected groundfish and trawl-vunerable invertebrates in Zone 3 of the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Maintain productivity of harvested species",
                                                           "Manage the disturbance of benthic habitat that supports juvenile and adult haddock and other groundfish species",
                                                           "Help maintain healthy populations of species of Aboriginal, commercial, and/or recreational importance",
                                                           "Conserve and protect marine areas of high biodiversity at the community, species, population and genetic levels within the MPA"))
             }), # Biomass Metrics, Fish and Fishery Resources, Anthropogenic Pressure and Impacts

  tar_target(name = ind_longline_vunerable,
             command = {
               ind_placeholder(ind_name="Relative abundances, size distributions and diversity of selected longline-vunerable species in Zones 2 and 3 of the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Maintain productivity of harvested species",
                                                           "Manage the disturbance of benthic habitat that supports juvenile and adult haddock and other groundfish species",
                                                           "Help maintain healthy populations of species of Aboriginal, commercial, and/or recreational importance",
                                                           "Conserve and protect marine areas of high biodiversity at the community, species, population and genetic levels within the MPA"))
             }), # Biomass Metrics, Fish and Fishery Resources, Anthropogenic Pressure and Impacts

  tar_target(name = ind_trap_vunerable,
             command = {
               ind_placeholder(ind_name="Relative abundances, size distributions and diversity of selected trap-vunerable species in Zonea 1 and 2 of the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Maintain productivity of harvested species",
                                                           "Manage the disturbance of benthic habitat that supports juvenile and adult haddock and other groundfish species",
                                                           "Help maintain healthy populations of species of Aboriginal, commercial, and/or recreational importance",
                                                           "Conserve and protect marine areas of high biodiversity at the community, species, population and genetic levels within the MPA"))
             }), # Biomass Metrics, Fish and Fishery Resources, Anthropogenic Pressure and Impacts

  tar_target(name = ind_mesopelagic_nektonic,
             command = {
               ind_placeholder(ind_name="Relative abundances, size distributions and diversity of selected mesopelagic nektonic species in Zones 1 and 2 of the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
                                                           "Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                                           "Maintain biodiversity of individual species, communities and populations within the different ecotypes"))
             }), # Biomass Metrics,Trophic Structure and Function

  tar_target(name = ind_environmental_conditions_near_seabed,
             command = {
               ind_placeholder(ind_name="Temperature, salinity, oxygen concentration, alkalinity, pH, light levels, chlorophyll, pigments and nutrients in water column within the MPA, including in close proximity to the seabed", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Safeguard habitat, including the physical and chemical properties of the ecosystem, by maintaining water and sediment quality",
                                                           "Maintain and monitor the quality of water and sediments of the Gully; and",
                                                           "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"))
             }), # Environmental Representativity, Ocean Conditions

  tar_target(name = ind_environmental_conditions_azmp_lines,
             command = {
               ind_placeholder(ind_name="Temperature, salinity, oxygen concentration, light levels, chlorophyll, pigments and nutrients in waters flowing into and past the MPA, as measured on the Louisbourg Line, the Halifax Line and the Extended Halifax Line", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Safeguard habitat, including the physical and chemical properties of the ecosystem, by maintaining water and sediment quality",
                                                           "Maintain and monitor the quality of water and sediments of the Gully; and",
                                                           "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"))
             }), # Environmental Representativity, Ocean Conditions

  tar_target(name = ind_physical_biological_surface_properties,
             command = {
               ind_placeholder(ind_name="Physical (temperature, salinity, wind, sea-surface height) and biological (ocean colour) sea surface properties in the MPA and the surrounding region", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Safeguard habitat, including the physical and chemical properties of the ecosystem, by maintaining water and sediment quality",
                                                           "Maintain and monitor the quality of water and sediments of the Gully; and",
                                                           "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"))
             }), # Environmental Representativity, Ocean Conditions

  tar_target(name = ind_weather_station_buoy_sites,
             command = {
               ind_placeholder(ind_name="Weather conditions at the Sable Island weather station, Banquereau and Laurentian Fan weather-buoy sites, including wind direction and speed, air pressure and sea-level air temperatures, and at buoy sites sea surface temperatures, wave height and dominant wave period", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Safeguard habitat, including the physical and chemical properties of the ecosystem, by maintaining water and sediment quality",
                                                           "Maintain and monitor the quality of water and sediments of the Gully; and",
                                                           "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"))
             }), # Environmental Representativity, Ocean Conditions

  tar_target(name = ind_water_masses,
             command = {
               ind_placeholder(ind_name="Three-dimensional distribution and movements of water masses within and around the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Safeguard habitat, including the physical and chemical properties of the ecosystem, by maintaining water and sediment quality",
                                                           "Maintain and monitor the quality of water and sediments of the Gully; and",
                                                           "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"))
             }), # Environmental Representativity, Ocean Structure and Movement

  tar_target(name = ind_acoustic_scattering,
             command = {
               ind_placeholder(ind_name="Acoustic scattering in the water column within the MPA (as a measure of mesopelagic and zooplankton densities and distribution)", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                                           "Conserve and protect marine areas of high biodiversity at the community, species, population and genetic levels within the MPA"))
             }), # Biomass Metrics, Secondary Production

  tar_target(name = ind_seabird_abundance,
             command = {
               ind_placeholder(ind_name="Distribution and abundance of seabird species within the MPA, including an index of planktivorous seabird species", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA"))
             }), # Biomass Metrics, Trophic Structure and Function

  tar_target(name = ind_vessel_transits,
             command = {
               ind_placeholder(ind_name="Number of transits of the MPA by vessels other than pleasure craft, broken down into mercantile vessels, surface naval vessels and fishing vessels not fishing in the area", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Minimize harmful impacts from human activities on cetacean populations and their habitats"))
             }), # Threats to Habitat, Anthorpogenic Pressure and Impacts

  tar_target(name = ind_vessel_operation,
             command = {
               ind_placeholder(ind_name="Hours of operation within the MPA by vessels other than commercial fishing vessels or pleasure craft, broken down into research and monitoring vessels, other government vessels, and ecotourism vessels", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Minimize harmful impacts from human activities on cetacean populations and their habitats"))
             }), # Threats to Habitat, Anthropogenic Pressure and Impacts

  tar_target(name = ind_fishing_effort,
             command = {
               ind_placeholder(ind_name="Commercial fishing effort within the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Maintain productivity of harvested species",
                                                           "Manage human activities to minimize impacts on other commercial and non-commercial living resources",
                                                           "Keep fishing and other forms of mortality moderate"))
             }), # Biomass Metrics, Anthropogenic Pressure and Impacts

  tar_target(name = ind_fishing_effort_nearby,
             command = {
               ind_placeholder(ind_name="Commercial fishing effort in close proximity to the MPA boundary", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Maintain productivity of harvested species",
                                                           "Manage human activities to minimize impacts on other commercial and non-commercial living resources",
                                                           "Keep fishing and other forms of mortality moderate"))
             }), #Biomass Metrics, Anthropogenic Pressure and Impacts

  tar_target(name = ind_corals_removed,
             command = {
               ind_placeholder(ind_name="Quantities of corals removed from within the MPA by commercial fishing or research activity", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Protect cold-water corals",
                                                           "Protect cold-water corals and deep water frontier area",
                                                           "Protect corals, particularly significant concentrations of sea pens, from harm due to human activities (e.g., fishing, oil and gas exploratory drilling, submarine cable installation and anchoring) in the Laurentian Channel",
                                                           "Minimize the disturbance of seafloor habitat and associated benthic communities caused by human activities"))
             }), # Biomass Metrics, Anthropogenic Pressure and Impacts

  tar_target(name = ind_target_org_removed_gully,
             command = {
               ind_placeholder(ind_name="Quantities of taget organisims removed from within the MPA and of bycatch organisms other than corals removed from the MPA by commercial fishing", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Maintain productivity of harvested species",
                                                           "Manage human activities to minimize impacts on other commercial and non-commercial living resources",
                                                           "Control unintended incidental mortality for all species",
                                                           "Keep fishing and other forms of mortality moderate"))
             }), # Threats to Productivity, Anthropogenic Pressure and Impacts

  tar_target(name = ind_organisms_removed,
             command = {
               ind_placeholder(ind_name="Quantities of organisms besides corals removed from the MPA by research activities", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Maintain productivity of harvested species",
                                                           "Manage human activities to minimize impacts on other commercial and non-commercial living resources",
                                                           "Control unintended incidental mortality for all species"))
             }), # Threats to Productivity, Anthropogenic Pressure and Impacts

  tar_target(name = ind_seabed_swept,
             command = {
               ind_placeholder(ind_name="Area of seabed swept by bottom-tending mobile research and monitoring gear in the MPA as a total and subdivided by seabed habitat type", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Minimize the disturbance of seafloor habitat and associated benthic communities caused by human activities",
                                                           "Manage the disturbance of benthic habitat that supports juvenile and adult haddock and other groundfish species",
                                                           "Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
                                                           "Habitat required for all species, particularly priority species, is maintained and protected"))
             }), # Threats to Habitat, Anthropogenic Pressure and Impacts

  tar_target(name = ind_seabed_occupied,
             command = {
               ind_placeholder(ind_name="Length of lines of, and seabed area occupied by, bottom-set fixed commercial fishing and research and monitoring gears within the MPA, as totals and subdivided by seabed habitat type", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Minimize the disturbance of seafloor habitat and associated benthic communities caused by human activities",
                                                           "Manage the disturbance of benthic habitat that supports juvenile and adult haddock and other groundfish species",
                                                           "Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
                                                           "Habitat required for all species, particularly priority species, is maintained and protected"))
             }), # Threats to Habitat, Anthropogenic Pressure and Impacts

  tar_target(name = ind_petroleum_activity,
             command = {
               ind_placeholder(ind_name="Number and types of offshore-petroleum exploration and development activities on the eastern Scotian Shelf", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Safeguard habitat, including the physical and chemical properties of the ecosystem, by maintaining water and sediment quality",
                                                           "Minimize the disturbance of seafloor habitat and associated benthic communities caused by human activities"))
             }), # Threats to Habitat, Anthropogenic Pressure and Impacts

  tar_target(name = ind_ballast,
             command = {
               ind_placeholder(ind_name="Number of ships’ ballast-water exchanges in the proximity of the MPA and the quantities of ballast exchanged", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Maintain and monitor the quality of water and sediments of the Gully; and",
                                                           "Control introduction and proliferation of disease/pathogens",
                                                           "Safeguard habitat, including the physical and chemical properties of the ecosystem, by maintaining water and sediment quality"))
             }), # Threats to Habitat, Anthropogenic Pressure and Impacts

  tar_target(name = ind_other_discharge,
             command = {
               ind_placeholder(ind_name="Number, quantities and type of other discharges from shipping within or in proximity to the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Maintain and monitor the quality of water and sediments of the Gully; and",
                                                           "Control introduction and proliferation of disease/pathogens",
                                                           "Safeguard habitat, including the physical and chemical properties of the ecosystem, by maintaining water and sediment quality",
                                                           "Minimize unintended introduction and transmission of invasive species"))
             }), # Threats to Habitat, Anthropogenic Pressure and Impacts

  tar_target(name = ind_floating_debris_gully,
             command = {
               ind_placeholder(ind_name="Quantity of floating debris (i.e., large objects) in the Gully MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Minimize harmful impacts from human activities on cetacean populations and their habitats",
                                                           "Safeguard habitat, including the physical and chemical properties of the ecosystem, by maintaining water and sediment quality,"))
             }), # Threats to Habitat, Anthropogenic Pressure and Impacts

  tar_target(name = ind_seabed_debris_gully,
             command = {
               ind_placeholder(ind_name="Quantity of anthropogenic debris on the seabed at selected monitoring sites in the Gully MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Minimize the disturbance of seafloor habitat and associated benthic communities caused by human activities",
                                                           "Safeguard habitat, including the physical and chemical properties of the ecosystem, by maintaining water and sediment quality,",
                                                           "Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes"))
             }), # Threats to Habitat, Anthropogenic Pressure and Impacts

  tar_target(name = ind_invasive_gully,
             command = {
               ind_placeholder(ind_name="Reports of known invasive species in the Gully MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Minimize unintended introduction and transmission of invasive species",
                                                           "Control introduction and proliferation of disease/pathogens"))
             }), # Threats to Productivity, Trophic Structure and Function

  tar_target(name = ind_anthropogenic_sound,
             command = {
               ind_placeholder(ind_name="Quantitative characteristics of anthropogenic sound within the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Minimize harmful impacts from human activities on cetacean populations and their habitats",
                                                           "Limit disturbing activity in important reproductive areas/seasons"))
             }), # Threats to Habitat, Anthropogenic Pressure and Impacts

  tar_target(name = ind_physandbio_properties,
             command = {
               ind_placeholder(ind_name="Temperature, salinity, oxygen concentration, light levels, chlorophyll, pigments, nutrients and zooplankton within the AOI and both upstream and downstream", areas = MPAs, readiness = "Unknown",
                               source = AZMP, objectives = c("Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                                             "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"))
             }), #Environmental Representativity, Ocean Conditions

  tar_target(name = ind_physandbio_seasurface_properties,
             command = {
               ind_placeholder(ind_name="Physical (e.g. temperature, salinity, wind, sea-surface height) and biological (e.g. ocean colour) sea surface properties in the MPA and the surrounding region", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Safeguard habitat, including the physical and chemical properties of the ecosystem, by maintaining water and sediment quality,",
                                                           "Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                                           "Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA"))
             }), # Environmental Representativity, Ocean Conditions

  tar_target(name = ind_weather_condition,
             command = {
               ind_placeholder(ind_name="Weather conditions at the Sydney Airport and Fourchu Head weather stations, including wind direction and speed, air pressure and sea-level air temperature", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"))
             }), # Environmental Representativity, Ocean Conditions, Ocean Structure and Movement

  tar_target(name = ind_ice_cover,
             command = {
               ind_placeholder(ind_name="Extent of ice cover within and around the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)",
                                                           "Conserve and protect marine areas of high biodiversity at the community, species, population and genetic levels within the MPA"))
             }), # Environmenntal Representativity, Ocean Conditions

  tar_target(name = ind_fluxes,
             command = {
               ind_placeholder(ind_name="Fluxes, other than those of nekton, across the boundaries of the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
                                                           "Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                                           "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"))
             }), # Connectivity, Trophic structure and Function?

  tar_target(name = ind_exchanges,
             command = {
               ind_placeholder(ind_name="Bentho-pelagic exchanges", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
                                                           "Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                                           "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"))
             }), # Structure and Function, Trophic Structure and Function?

  tar_target(name = ind_plankton_production,
             command = {
               ind_placeholder(ind_name="Phytoplankton production and the timing and intensity of the spring bloom in the MPA and the surrounding region", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
                                                           "Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                                           "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"))
             }), # Biomass Metrics, Primary Production

  tar_target(name = ind_mesozooplankton_community,
             command = {
               ind_placeholder(ind_name="Mesozooplankton community composition within the AOI and both upstream and downstream", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
                                                           "Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                                           "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)",
                                                           "Maintain biodiversity of individual species, communities and populations within the different ecotypes"))
             }), # Biomass Metrics, Secondary Production

  tar_target(name = ind_harmful_algae,
             command = {
               ind_placeholder(ind_name="Blooms of harmful algal in or near the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Safeguard habitat, including the physical and chemical properties of the ecosystem, by maintaining water and sediment quality,",
                                                           "Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                                           "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"))
             }), # Threats to Productivity, Trophic structure and Function?

  tar_target(name = ind_benthic_characteristics,
             command = {
               ind_placeholder(ind_name="Diversity and community composition of the benthos, abundance or biomass and size composition of selected benthic taxa, and characteristics of surficial geology at selected sampling stations, distributed across the seabed environment types represented in the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
                                                           "Minimize the disturbance of seafloor habitat and associated benthic communities caused by human activities",
                                                           "Habitat required for all species, particularly priority species, is maintained and protected",
                                                           "Maintain biodiversity of individual species, communities and populations within the different ecotypes"))
             }), # Biomass Metrics, Benthic Environment

  tar_target(name = ind_compared_benthic_characteristics,
             command = {
               ind_placeholder(ind_name="Diversity and community composition of the benthos, abundance or biomass and size composition of selected benthic taxa and characteristics of surficial geology at comparable sampling stations outside the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
                                                           "Minimize the disturbance of seafloor habitat and associated benthic communities caused by human activities",
                                                           "Habitat required for all species, particularly priority species, is maintained and protected",
                                                           "Maintain biodiversity of individual species, communities and populations within the different ecotypes"))
             }), # Biomass Metrics, Benthic Environment

  tar_target(name = ind_distinctive_benthic_characteristics,
             command = {
               ind_placeholder(ind_name="Diversity and community composition of the benthos and characteristics of surficial geology at selected sampling stations located in the identified distinctive seabed features of the AOI, plus abundance or biomass and size composition of the defining benthic taxa of those features", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
                                                           "Minimize the disturbance of seafloor habitat and associated benthic communities caused by human activities",
                                                           "Habitat required for all species, particularly priority species, is maintained and protected",
                                                           "Maintain biodiversity of individual species, communities and populations within the different ecotypes"))
             }), # Biomass Metrics, Benthic Environment

  tar_target(name = ind_seabed_feature_extent,
             command = {
               ind_placeholder(ind_name="Spatial extent of identified distinctive seabed features of the AOI", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Protect unique, rare, or sensitive ecological features",
                                                           "Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
                                                           "Habitat required for all species, particularly priority species, is maintained and protected"))
             }), # Uniqueness, Ocean Structure and Movement?

  tar_target(name = ind_resource_species_abundance,
             command = {
               ind_placeholder(ind_name="Population-wide abundances and size distributions of those populations of resource species which utilize the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Maintain productivity of harvested species",
                                                           "Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                                           "Maintain biodiversity of individual species, communities and populations within the different ecotypes",
                                                           "Allow sufficient escapement from exploitation for spawning"))
             }), #Biomass Metrics, Fish and Fishery Resources

  tar_target(name = ind_groundfish_abundance_sab,
             command = {
               ind_placeholder(ind_name="Relative abundances, biomasses, size distributions and population fecundities of selected groundfish and invertebrates, plus diversity and community composition of trawl-vulnerable species, in appropriate portions of the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Maintain productivity of harvested species",
                                                           "Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                                           "Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes"))
             }), #Biomass Metrics, Fish and Fishery Resources

  tar_target(name = ind_nekton_abundance,
             command = {
               ind_placeholder(ind_name="Relative abundances, biomasses and size distributions of selected mesopelagic nekton and micronekton species in the Laurentian Channel portion of the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                                           "Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes"))
             }), #Biomass Metrics, Trophic Structure and Function?

  tar_target(name = ind_compared_groundfish_abundance,
             command = {
               ind_placeholder(ind_name="Relative abundances, biomasses, size distributions and population fecundities of selected groundfish and invertebrates, plus diversity and community composition of trawl-vulnerable species, in comparable areas outside the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Maintain productivity of harvested species",
                                                           "Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                                           "Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes"))
             }), #Biomass Metrics, Fish and Fishery Resources

  tar_target(name = ind_compared_longline_vunerable,
             command = {
               ind_placeholder(ind_name="Relative abundances, biomasses, size distributions and population fecundities of selected longline-vulnerable species in comparable areas outside the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Maintain productivity of harvested species",
                                                           "Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                                           "Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes"))
             }), #Biomass Metrics, Fish and Fishery Resources, Anthropogenic Pressure and Impacts

  tar_target(name = ind_large_wolffish,
             command = {
               ind_placeholder(ind_name="Abundance of large wolffish in sub-tidal rocky areas along the coastline adjacent to the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("promote the recovery of at-risk whales and wolffish",
                                                           "Maintain biodiversity of individual species, communities and populations within the different ecotypes"))
             }), #Biomass Metrics, Trophic Structure and Function

  tar_target(name = ind_fish_nekton_fluxes,
             command = {
               ind_placeholder(ind_name="Fluxes of fish and other nekton across the boundaries of the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
                                                           "Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                                           "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"))
             }), #Connectivity, Trophic Structure and Function

  tar_target(name = ind_mammal_bird_reptile,
             command = {
               ind_placeholder(ind_name="Distributions, relative abundances, diversity, community composition and activities of mammals, birds and reptiles in the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                                           "Conserve and protect marine areas of high biodiversity at the community, species, population and genetic levels within the MPA",
                                                           "Maintain biodiversity of individual species, communities and populations within the different ecotypes"))
             }), #Biomass Metrics, Marine Mammals and other Top Predators

  tar_target(name = ind_seal_breeding,
             command = {
               ind_placeholder(ind_name="Grey and harp seal breeding in the vicinity of the AOI", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                                           "Conserve and protect marine areas of high biodiversity at the community, species, population and genetic levels within the MPA",
                                                           "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"))
             }), #Biomass Metrics, Marine Mammals and other Top Predator

  tar_target(name = ind_seabird_nesting,
             command = {
               ind_placeholder(ind_name="Seabird nesting in the vicinity of the AOI", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                                           "Conserve and protect marine areas of high biodiversity at the community, species, population and genetic levels within the MPA",
                                                           "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"))
             }), #Biomass Metrics, Trophic Structure and Function?

  tar_target(name = ind_trophic_relationships,
             command = {
               ind_placeholder(ind_name="Trophic relationships in the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
                                                           "Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                                           "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"))
             }), #Structure and Function, Trophic Structure and Function

  tar_target(name = ind_ecosystem_function,
             command = {
               ind_placeholder(ind_name="Ecosystem function in the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
                                                           "Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
                                                           "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"))
             }), #Structure and Function, Trophic Structure and Function

  tar_target(name = ind_marxan_input,
             command = {
               ind_placeholder(ind_name="Data inputs to the MARXAN analysis", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = NA)
             }), #Environmental Representation, ??

  tar_target(name = ind_bottomset_length,
             command = {
               ind_placeholder(ind_name="Length of bottom-set fixed commercial fishing, research and monitoring lines set within the MPA, both as totals and subdivided by zone or seabed habitat type", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Minimize the disturbance of seafloor habitat and associated benthic communities caused by human activities",
                                                           "Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
                                                           "Habitat required for all species, particularly priority species, is maintained and protected"))
             }), #Threats to Habitat, Anthropogenic Pressure and Impacts

  tar_target(name = ind_vertical_lines,
             command = {
               ind_placeholder(ind_name="Number of vertical lines and length of midwater lines set within the MPA as part of commercial fishing, research or monitoring gears, both as a total and subdivided by zone", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Minimize the disturbance of seafloor habitat and associated benthic communities caused by human activities",
                                                           "Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
                                                           "Habitat required for all species, particularly priority species, is maintained and protected"))
             }), #Threats to Habitat, Anthropogenic Pressure and Impacts

  tar_target(name = ind_bait,
             command = {
               ind_placeholder(ind_name="Quantities and types of baits introduced to the MPA as part of commercial fishing, research or monitoring gears, both as a total and subdivided by zone", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Maintain productivity of harvested species",
                                                           "Minimize unintended introduction and transmission of invasive species"))
             }), #Threats to Productivity, Anthropogenic Pressure and Impacts

  tar_target(name = ind_development_sab,
             command = {
               ind_placeholder(ind_name="Number and types of seabed cables, offshore-petroleum exploration and development activities, other mineral exploration and development activities, channel dredging projects or other large-scale engineering works in the general vicinity of the MPA, including any within the MPA itself", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Minimize the disturbance of seafloor habitat and associated benthic communities caused by human activities",
                                                           "Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
                                                           "Habitat required for all species, particularly priority species, is maintained and protected"))
             }), #Threats to Habitat, Anthropogenic Pressure and Impacts

  tar_target(name = ind_anchoring,
             command = {
               ind_placeholder(ind_name="Incidents of vessels anchoring within the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Minimize the disturbance of seafloor habitat and associated benthic communities caused by human activities",
                                                           "Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
                                                           "Habitat required for all species, particularly priority species, is maintained and protected"))
             }), #Threats to Habitat, Anthropogenic Pressure and Impacts

  tar_target(name = ind_other_discharge_offshore,
             command = {
               ind_placeholder(ind_name="Number, quantities and type of discharges from vessels of all kinds or from offshore installations within, or in proximity to the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Maintain and monitor the quality of water and sediments of the Gully; and",
                                                          "Control introduction and proliferation of disease/pathogens",
                                                          "Safeguard habitat, including the physical and chemical properties of the ecosystem, by maintaining water and sediment quality",
                                                          "Minimize unintended introduction and transmission of invasive species"))
             }), #Threats to Habitat, Anthropogenic Pressure and Impacts

  tar_target(name = ind_other_discharge_coastal,
             command = {
               ind_placeholder(ind_name="Number, quantities and type of discharges from coastal sources within or in proximity to the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Maintain and monitor the quality of water and sediments of the Gully; and",
                                                           "Control introduction and proliferation of disease/pathogens",
                                                           "Safeguard habitat, including the physical and chemical properties of the ecosystem, by maintaining water and sediment quality",
                                                           "Minimize unintended introduction and transmission of invasive species"))
             }), #Threats to Habitat, Anthropogenic Pressure and Impacts

  tar_target(name = ind_ocean_noise_sab,
             command = {
               ind_placeholder(ind_name="Characterization of deep-water natural and anthropogenic noise within the MPA", areas = MPAs, readiness = "Unknown",
                               source = NA, objectives = c("Minimize harmful impacts from human activities on cetacean populations and their habitats",
                                                           "Limit disturbing activity in important reproductive areas/seasons"))
             }) #Threats to Habitat,Anthropogenic Pressure and Impacts
)
