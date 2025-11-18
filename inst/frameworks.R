source("inst/set_up.R")

framework_targets <- list(

  # OBJECTIVES ----

  tar_target(objectives_csv,
             command = "data_raw/objectives.csv",
             format = "file",
             deployment = "worker"
  ),

  tar_target(objectives_df,
             command = {
               read.csv(objectives_csv, stringsAsFactors = FALSE)
             }
  ),

  tar_target(objective_indicators,
             command={
               objectives_df
               #cat(paste0("The length of pillar_ecol is ", length(pillar_ecol_df$bin)))
               ped <- pillar_ecol_df[-which(is.na(pillar_ecol_df$objectives)),]

               indicator_objectives <- trimws(unique(unlist(strsplit(ped$objectives, ";;;"))), 'both')
               indicator_objectives <- indicator_objectives[-which(indicator_objectives == "NA")]

               ped_objectives <- vector("list", length=length(indicator_objectives))

               for (i in seq_along(ped_objectives)) {
                 message(i)
                 keep <- which(grepl(indicator_objectives[i], ped$objectives, fixed = TRUE))
                 if (objectives_df$Framework[which(objectives_df$Objective == indicator_objectives[i])] %in% MPAs$NAME_E) {
                   keep2 <- which(ped$areaID == objectives_df$Framework[which(objectives_df$Objective == indicator_objectives[i])])
                   keep <- intersect(keep, keep2)
                 }

                 ped_objectives[[i]] <- ped[keep, ]

               }
               names(ped_objectives) <- indicator_objectives

               # Adding objectives that aren't yet accounted for

               new_objectives <- objectives_df$Objective[which(!(objectives_df$Objective %in% names(ped_objectives)))]

               for (obj_name in new_objectives) {
                 # create a blank NA df with the same columns
                 na_df <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(ped)))
                 names(na_df) <- names(ped)

                 # add it to the list with the name
                 ped_objectives[[obj_name]] <- na_df
               }


               ped_objectives

             }),

  # EBM BINS ----

  tar_target(bin_biodiversity_FunctionalDiversity_df,
             aggregate_groups("bin",
                              "Functional Diversity",
                              weights_ratio=1,
                              weights_sum = 1,
                              ind_placeholder_df,
                              ind_species_per_trophic
             )),
  tar_target(bin_biodiversity_GeneticDiversity_df,
             aggregate_groups("bin",
                              "Genetic Diversity",
                              weights_ratio=1,
                              weights_sum = 1,
                              ind_placeholder_df,
                              ind_genetic_diversity_nbw
             )),
  tar_target(bin_biodiversity_SpeciesDiversity_df,
             aggregate_groups("bin",
                              "Species Diversity",
                              weights_ratio = 1,
                              weights_sum = 1,
                              ind_species_representation,
                              ind_musquash_infaunal_diversity,
                              ind_musquash_nekton_diversity,
                              ind_musquash_birds_sample_coverage,
                              ind_epibenthic_infaunal,
                              ind_coral_diversity

             )
  ),
  tar_target(bin_habitat_Connectivity_df,
             aggregate_groups("bin",
                              "Connectivity",
                              weights_ratio=1,
                              weights_sum = 1,
                              ind_placeholder_df,
                              ind_otn_proportion_tags_detected_in_multiple_mpas,
                              ind_seasonal_presence_cetaceans,
                              ind_seasonal_presence_pelagics,
                              ind_seasonal_presence_seals,
                              ind_seasonal_presence_seabirds,
                              ind_mpa_use_nbw,
                              ind_fluxes,
                              ind_fish_nekton_fluxes
             )),
  tar_target(bin_habitat_EnvironmentalRepresentativity_df,
             aggregate_groups("bin",
                              "Environmental Representativity",
                              weights_sum = 1,
                              weights_ratio = 1,
                              ind_nitrate,
                              ind_silicate,
                              ind_phosphate,
                              ind_salinity,
                              ind_chlorophyll,
                              ind_temperature,
                              ind_surface_height,
                              ind_bloom_amplitude,
                              ind_bloom_timing,
                              ind_musquash_ph,
                              ind_musquash_dissolved_oxygen,
                              ind_musquash_phosphate,
                              ind_musquash_secchi,
                              ind_oxygen,
                              ind_musquash_coliform,
                              ind_sst,
                              ind_temp_at_depth,
                              ind_sea_surface_salinity,
                              ind_subsurface_salinity,
                              ind_oxygen_saturation,
                              ind_ave_ph_level,
                              ind_nitrate,
                              ind_carbonate,
                              ind_ave_mixed_layer_depth,
                              ind_ave_position_of_shelf_slope_front,
                              ind_wind_speed_and_storminess,
                              ind_chlorophyll_a,
                              ind_spatial_extent_ebsa_webmr,
                              ind_sediment_regime,
                              ind_nutrients,
                              ind_environmental_conditions_near_seabed,
                              ind_environmental_conditions_azmp_lines,
                              ind_physical_biological_surface_properties,
                              ind_weather_station_buoy_sites,
                              ind_water_masses,
                              ind_physandbio_properties,
                              ind_physandbio_seasurface_properties,
                              ind_weather_condition,
                              ind_ice_cover,
                              ind_marxan_input
             )),
  tar_target(bin_habitat_KeyFishHabitat_df,
             aggregate_groups("bin",
                              "Key Fish Habitat",
                              weights_ratio = 1,
                              weights_sum = 1,
                              ind_SAR_CH_representation,
                              ind_distribution_juv_haddock_habitat,
                              ind_distribution_key_fish_habitat
             )),
  tar_target(bin_habitat_ThreatstoHabitat_df,
             aggregate_groups("bin",
                              "Threats to Habitat",
                              weights_ratio=1,
                              weights_sum = 1,
                              ind_MAR_biofouling_AIS,
                              ind_MAR_cum_impact,
                              ind_placeholder_df,
                              ind_nitrate_inside_outside,
                              ind_temperature_inside_outside,
                              ind_musquash_phosphate_inside_outside,
                              ind_musquash_coliform_inside_outside,
                              ind_seabed_disruption_in_around_webmr_fishing,
                              ind_seabed_disruption_in_around_webmr_research,
                              ind_vessel_traffic,
                              ind_ocean_sound,
                              ind_cables,
                              ind_well_proximity,
                              ind_offshore_wind_developments,
                              ind_contaminant_concentration,
                              ind_anthropogenic_debris,
                              ind_cumulative_impact,
                              ind_disturbed_area,
                              ind_human_perturbation,
                              ind_fresh_scars,
                              ind_ship_strikes,
                              ind_entanglement_gully,
                              ind_human_interaction,
                              ind_vessel_transits,
                              ind_vessel_operation,
                              ind_seabed_swept,
                              ind_seabed_occupied,
                              ind_petroleum_activity,
                              ind_ballast,
                              ind_other_discharge,
                              ind_floating_debris_gully,
                              ind_seabed_debris_gully,
                              ind_anthropogenic_sound,
                              ind_bottomset_length,
                              ind_vertical_lines,
                              ind_development_sab,
                              ind_anchoring,
                              ind_other_discharge_offshore,
                              ind_other_discharge_coastal,
                              ind_ocean_noise_sab
             )),
  tar_target(bin_habitat_Uniqueness_df,
             aggregate_groups("bin",
                              "Uniqueness",
                              weights_ratio=1,
                              weights_sum = 1,
                              ind_QC_gulf_biogenic_habitat_representation,
                              ind_ebsa_representation,
                              ind_seabed_feature_extent
             )),
  tar_target(bin_productivity_BiomassMetrics_df,
             aggregate_groups("bin",
                              "Biomass Metrics",
                              weights_ratio=1,
                              weights_sum = 1,
                              ind_fish_length,
                              ind_fish_weight,
                              ind_haddock_biomass,
                              ind_haddock_counts,
                              ind_zooplankton,
                              ind_zooplankton_community_composition,
                              ind_phytoplankton,
                              ind_phytoplankton_biomass_and_diversity,
                              ind_calanus_finmarchicus,
                              ind_fish_eggs_and_larve,
                              ind_abundance_invasive_species_webmr,
                              ind_biomass_groundfish_prey_webmr,
                              ind_abundance_sea_pens_webmr,
                              ind_community_comp_epibenthic_infaunal,
                              ind_biomass_bioturbators,
                              ind_rel_abundance_groundfish,
                              ind_size_distribution_groundfish,
                              ind_condition_groundfish,
                              ind_fecundity_groundfish,
                              ind_community_comp_demersal,
                              ind_biomass_abund_distribution_musquash,
                              ind_species_at_risk,
                              ind_cpue,
                              ind_bycatch,
                              ind_nonindigenous_rel_indigenous,
                              ind_abundance_nbw,
                              ind_rel_abundance_cetaceans,
                              ind_coral_distribution,
                              ind_coral_proportions,
                              ind_trawl_vunerable,
                              ind_longline_vunerable,
                              ind_trap_vunerable,
                              ind_mesopelagic_nektonic,
                              ind_acoustic_scattering,
                              ind_seabird_abundance,
                              ind_fishing_effort,
                              ind_fishing_effort_nearby,
                              ind_corals_removed,
                              ind_plankton_production,
                              ind_mesozooplankton_community,
                              ind_benthic_characteristics,
                              ind_compared_benthic_characteristics,
                              ind_distinctive_benthic_characteristics,
                              ind_resource_species_abundance,
                              ind_groundfish_abundance_sab,
                              ind_nekton_abundance,
                              ind_compared_groundfish_abundance,
                              ind_compared_longline_vunerable,
                              ind_large_wolffish,
                              ind_mammal_bird_reptile,
                              ind_seal_breeding,
                              ind_seabird_nesting
             )),
  tar_target(bin_productivity_StructureandFunction_df,
             aggregate_groups("bin",
                              "Structure and Function",
                              weights_ratio=1,
                              weights_sum = 1,
                              ind_stratification,
                              ind_spring_bloom,
                              ind_proportion_demersal_fish,
                              ind_population_characteristics,
                              ind_cetacean_presence,
                              ind_exchanges,
                              ind_trophic_relationships,
                              ind_ecosystem_function

             )),
  tar_target(bin_productivity_ThreatstoProductivity_df,
             aggregate_groups("bin",
                              "Threats to Productivity",
                              weights_ratio=1,
                              weights_sum = 1,
                              ind_placeholder_df,
                              ind_unauthorized_fishing,
                              ind_total_annual_landings,
                              ind_blubber_contaminants,
                              ind_strandings,
                              ind_zoanthid_overgrowth,
                              ind_target_org_removed_gully,
                              ind_organisms_removed,
                              ind_invasive_gully,
                              ind_harmful_algae,
                              ind_bait
             )),

  tar_target(ecol_obj_biodiversity_df,
             aggregate_groups("objective",
                              "Biodiversity",
                              weights_ratio=NA,
                              weights_sum = NA,
                              bin_biodiversity_FunctionalDiversity_df,
                              bin_biodiversity_GeneticDiversity_df,
                              bin_biodiversity_SpeciesDiversity_df)),
  tar_target(ecol_obj_habitat_df,
             aggregate_groups("objective",
                              "Habitat",
                              weights_ratio=NA,
                              weights_sum = NA,
                              bin_habitat_Connectivity_df,
                              bin_habitat_EnvironmentalRepresentativity_df,
                              bin_habitat_KeyFishHabitat_df,
                              bin_habitat_ThreatstoHabitat_df,
                              bin_habitat_Uniqueness_df)),
  tar_target(ecol_obj_productivity_df,
             aggregate_groups("objective",
                              "Productivity",
                              weights_ratio=NA,
                              weights_sum = NA,
                              bin_productivity_BiomassMetrics_df,
                              bin_productivity_StructureandFunction_df,
                              bin_productivity_ThreatstoProductivity_df)),


  tar_target(pillar_ecol_df,
             {
               APPTABS
               regions
               target_bin_weight <- 1

               pedf <- aggregate_groups("pillar",
                                        "Ecological",
                                        weights_ratio=NA,
                                        weights_sum = NA,
                                        dplyr::select(ecol_obj_biodiversity_df,-data,-plot),
                                        dplyr::select(ecol_obj_habitat_df,-data,-plot),
                                        dplyr::select(ecol_obj_productivity_df,-data,-plot)) |>
                 mutate(PPTID = as.character(PPTID)) |>
                 left_join(dplyr::select(as.data.frame(MPAs), NAME_E, region), by = c("areaID"="NAME_E"))

               rm(ecol_obj_biodiversity_df)
               rm(ecol_obj_habitat_df)
               rm(ecol_obj_productivity_df)

               pedf_filtered <- pedf |> filter(areaID != "Non_Conservation_Area")

               x <- pedf_filtered |>
                 group_by(objective, bin, areaID, region) |>
                 reframe(
                   indicator = unique(areaID),
                   areaID = unique(region),
                   score = weighted.mean(score, weight, na.rm = TRUE),
                   score = if_else(is.nan(score), NA, score),
                   PPTID = paste(PPTID, collapse = "; ")
                 ) |>
                 group_by(bin) |>
                 mutate(weight = target_bin_weight / n()) |>
                 ungroup() |>

                 ##### Bind in full data, including Non_Conservation_Area
                 bind_rows(pedf) |>

                 mutate(tab = paste0("tab_", seq(length(APPTABS$flower) + 1, length(APPTABS$flower) + length(objective))))

               areas <- unique(x$areaID)

               pillar_list <- split(x, x$areaID)

               ##### Keep Non_Conservation_Area in pillar_list
               mpa_list <- pillar_list[!names(pillar_list) %in% regions$NAME_E]

               mpa_list <- lapply(mpa_list, function(ddff) {
                 ddff[order(ddff$score, na.last = TRUE), ]
               })

               region_list <- pillar_list[names(pillar_list) %in% regions$NAME_E]
               region_list <- lapply(region_list, function(ddff) {
                 ddff[order(ddff$score, na.last = TRUE), ]
               })

               for (i in seq_along(region_list)) {
                 reg <- region_list[[i]]
                 for (j in 1:nrow(reg)) {
                   reg2 <- reg[j,]
                   region_bin <- reg2$indicator
                   keep <- which(names(mpa_list) == region_bin)
                   if (length(keep) > 0) {
                     mpa_list[[keep]] <- rbind(reg2, mpa_list[[keep]])
                   }
                 }
               }

               do.call(rbind, mpa_list)


             }),


  # THEMES ----

  tar_target(theme_ocean_conditions,
             command={
               x <- rbind(
                 ind_oxygen[ , setdiff(names(ind_oxygen), c("data", "plot"))],
                 ind_salinity[ , setdiff(names(ind_salinity), c("data", "plot"))],
                 ind_temperature[ , setdiff(names(ind_temperature), c("data", "plot"))]
               )
               x$theme <- "Ocean Conditions"
               x$weight <- NA
               for (i in seq_along(x$areaID)) {
                 message(i)
                 X <- x[i,]
                 keep <- which(pillar_ecol_df$areaID == X$areaID & pillar_ecol_df$indicator == X$indicator)
                 x$weight[i] <- pillar_ecol_df$weight[keep]
               }
               x
             }),

  tar_target(theme_ocean_structure_and_movement,
             command={
               x <- rbind(
                 ind_otn_proportion_tags_detected_in_multiple_mpas[ , setdiff(names(ind_otn_proportion_tags_detected_in_multiple_mpas), c("data", "plot"))],
                 ind_surface_height[ , setdiff(names(ind_surface_height), c("data", "plot"))],
                 ind_stratification[ , setdiff(names(ind_stratification), c("data", "plot"))]
               )
               x$theme <- "Ocean Structure and Movement"
               x$weight <- NA
               for (i in seq_along(x$areaID)) {
                 message(i)
                 X <- x[i,]
                 keep <- which(pillar_ecol_df$areaID == X$areaID & pillar_ecol_df$indicator == X$indicator)
                 x$weight[i] <- pillar_ecol_df$weight[keep]
               }
               x
             }),

  tar_target(theme_primary_production,
             command={
               x <- rbind(
                 ind_nitrate[ , setdiff(names(ind_nitrate), c("data", "plot"))],
                 ind_silicate[ , setdiff(names(ind_silicate), c("data", "plot"))],
                 ind_phosphate[ , setdiff(names(ind_phosphate), c("data", "plot"))],
                 ind_chlorophyll[ , setdiff(names(ind_chlorophyll), c("data", "plot"))],
                 ind_bloom_amplitude[ , setdiff(names(ind_bloom_amplitude), c("data", "plot"))],
                 ind_bloom_timing[ , setdiff(names(ind_bloom_timing), c("data", "plot"))]
               )
               x$theme <- "Primary Production"
               x$weight <- NA
               for (i in seq_along(x$areaID)) {
                 X <- x[i,]
                 keep <- which(pillar_ecol_df$areaID == X$areaID & pillar_ecol_df$indicator == X$indicator)
                 x$weight[i] <- pillar_ecol_df$weight[keep]
               }
               x
             }),

  tar_target(theme_secondary_production,
             command={
               x <- rbind(
                 ind_zooplankton[ , setdiff(names(ind_zooplankton), c("data", "plot"))],
                 ind_zooplankton_community_composition[ , setdiff(names(ind_zooplankton_community_composition), c("data", "plot"))]
               )
               x$theme <- "Secondary Production"

               x$weight <- NA
               for (i in seq_along(x$areaID)) {
                 message(i)
                 X <- x[i,]
                 keep <- which(pillar_ecol_df$areaID == X$areaID & pillar_ecol_df$indicator == X$indicator)
                 x$weight[i] <- pillar_ecol_df$weight[keep]
               }
               x
             }),

  tar_target(theme_marine_mammals_and_other_top_predators,
             command={
               x <- rbind(
                 ind_musquash_birds_sample_coverage[ , setdiff(names(ind_musquash_birds_sample_coverage), c("data", "plot"))]
               )
               x$theme <- "Marine Mammals and Other Top Predators"

               x$weight <- NA
               for (i in seq_along(x$areaID)) {
                 message(i)
                 X <- x[i,]
                 keep <- which(pillar_ecol_df$areaID == X$areaID & pillar_ecol_df$indicator == X$indicator)
                 x$weight[i] <- pillar_ecol_df$weight[keep]
               }

               x
             }),

  tar_target(theme_trophic_structure_and_function,
             command={
               data.frame(
                 areaID = NA,
                 indicator = NA,
                 type = NA,
                 units = NA,
                 scoring = NA,
                 PPTID = NA,
                 project_short_title = NA,
                 climate = NA,
                 design_target = NA,
                 score = NA,
                 status_statement = NA,
                 trend_statement = NA,
                 source = NA,
                 climate_expectation = NA,
                 indicator_rationale = NA,
                 objectives = NA,
                 bin_rationale = NA,
                 theme="Trophic Structure and Function",
                 weight=NA
               )

             }),

  tar_target(theme_benthic_environment,
             command={
               data.frame(
                 areaID = NA,
                 indicator = NA,
                 type = NA,
                 units = NA,
                 scoring = NA,
                 PPTID = NA,
                 project_short_title = NA,
                 climate = NA,
                 design_target = NA,
                 score = NA,
                 status_statement = NA,
                 trend_statement = NA,
                 source = NA,
                 climate_expectation = NA,
                 indicator_rationale = NA,
                 objectives = NA,
                 bin_rationale = NA,
                 theme="Benthic Environment",
                 weight=NA
               )
             }),

  tar_target(theme_fish_and_fishery_resources,
             command={
               x <- rbind(
                 ind_fish_length[ , setdiff(names(ind_fish_length), c("data", "plot"))],
                 ind_fish_weight[ , setdiff(names(ind_fish_weight), c("data", "plot"))],
                 ind_haddock_counts[ , setdiff(names(ind_haddock_counts), c("data", "plot"))],
                 ind_haddock_biomass[ , setdiff(names(ind_haddock_biomass), c("data", "plot"))]
               )
               x$theme <- "Fish and Fishery Resources"

               x$weight <- NA
               for (i in seq_along(x$areaID)) {
                 message(i)
                 X <- x[i,]
                 keep <- which(pillar_ecol_df$areaID == X$areaID & pillar_ecol_df$indicator == X$indicator)
                 x$weight[i] <- pillar_ecol_df$weight[keep]
               }

               x
             }),

  tar_target(theme_anthropogenic_pressure_and_impacts,
             command={
               x <- data.frame(
                 areaID = NA,
                 indicator = NA,
                 type = NA,
                 units = NA,
                 scoring = NA,
                 PPTID = NA,
                 project_short_title = NA,
                 climate = NA,
                 design_target = NA,
                 score = NA,
                 status_statement = NA,
                 trend_statement = NA,
                 source = NA,
                 climate_expectation = NA,
                 indicator_rationale = NA,
                 objectives = NA,
                 bin_rationale = NA,
                 theme="Anthropogenic Pressure And Impacts",
                 weight=NA
               )
               x
             }),



  tar_target(theme_table,
             command={
               rbind(theme_ocean_conditions,
                     theme_ocean_structure_and_movement,
                     theme_primary_production,
                     theme_secondary_production,
                     theme_marine_mammals_and_other_top_predators,
                     theme_trophic_structure_and_function,
                     theme_benthic_environment,
                     theme_fish_and_fishery_resources,
                     theme_anthropogenic_pressure_and_impacts)

             }),



  # CLIMATE CHANGE ----
  tar_target(climate_change,
             command= {
               doc <- read_docx(file.path(system.file(package = "MarConsNetAnalysis"), "data", "climate.docx"))
               doc_text <- docx_summary(doc)
               keep <- which(grepl("CC:", doc_text$text))
               keep <- c(keep, which(grepl("References", doc_text$text, ignore.case=TRUE)))
               indicators <- gsub("CC: ","", doc_text$text[keep])
               indicators <- indicators[-length(indicators)]

               split_text <- list()
               for (i in seq_along(keep)) {
                 start_index <- keep[i]
                 end_index <- ifelse(i == length(keep), length(lines), keep[i + 1] - 1)
                 split_text[[i]] <- doc_text$text[start_index:end_index]
               }
               split_text <- split_text[-length(split_text)]
               summary <- unlist(lapply(split_text, function(x) x[2]))
               cc <- data.frame(indicators=indicators, summary=summary)
             })



)
