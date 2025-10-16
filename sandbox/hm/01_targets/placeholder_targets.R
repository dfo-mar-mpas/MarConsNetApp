

#WEBMR From Jaimies table


tar_target(name = ind_sst,
           command = {
             ind_placeholder(ind_name="Sea Surface Temperature", areas = MPAs, readiness = "Readily Available",
                             source = "AZMP", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities"))
           }), # Environmental Representativity


tar_target(name = ind_temp_at_depth,
           command = {
             ind_placeholder(ind_name="Temperature at Depth", areas = MPAs, readiness = "Readily Available",
                             source = "AZMP", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities"))
           }), # Environmental Representativity


tar_target(name = ind_sea_surface_salinity,
           command = {
             ind_placeholder(ind_name="Sea Surface Salinity", areas = MPAs, readiness = "Readily Available",
                             source = "AZMP", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities"))
           }), # Environmental Representativity


tar_target(name = ind_subsurface_salinity,
           command = {
             ind_placeholder(ind_name="Subsurface Salinity", areas = MPAs, readiness = "Readily Available",
                             source = "AZMP", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities"))
           }), # Environmental Representativity

tar_target(name = ind_oxygen_saturation,
           command = {
             ind_placeholder(ind_name="Oxygen Saturation", areas = MPAs, readiness = "Readily Available",
                             source = "AZMP", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities",
                                                             "Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock"))
           }), # Environmental Representativity

tar_target(name = ind_ave_ph_level,
           command = {
             ind_placeholder(ind_name="Average pH Level", areas = MPAs, readiness = "Readily Available",
                             source = "AZMP", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities",
                                                             "Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock"))
           }), # Environmental Representativity


tar_target(name = ind_nitrate,
           command = {
             ind_placeholder(ind_name="Nitrate", areas = MPAs, readiness = "Readily Available",
                             source = "AZMP", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities",
                                                             "Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock"))
           }), # Environmental Representativity


tar_target(name = ind_carbonate,
           command = {
             ind_placeholder(ind_name="Carbonate", areas = MPAs, readiness = "Readily Available",
                             source = "AZMP", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities",
                                                             "Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock"))
           }), # Environmental Representativity


tar_target(name = ind_ave_mixed_layer_depth,
           command = {
             ind_placeholder(ind_name="Average Mixed Layer Depth", areas = MPAs, readiness = NA,
                             source = "BNAM", objectives = NA)
           }), # Environmental Representativity


tar_target(name = ind_ave_position_of_shelf_slope_front,
           command = {
             ind_placeholder(ind_name="Average Position of Shelf Slope Front", areas = MPAs, readiness = NA,
                             source = "BNAM", objectives = NA))
           }), # Environmental Representativity


tar_target(name = ind_wind_speed_and_storminess,
           command = {
             ind_placeholder(ind_name="Wind Speed and Storminess", areas = MPAs, readiness = NA,
                             source = "Gliders", objectives = NA))
           }), # Environmental Representativity


tar_target(name = ind_chlorophyll_a,
           command = {
             ind_placeholder(ind_name="Chlorophyll a", areas = MPAs, readiness = NA,
                             source = "AZMP", objectives = NA))
           }), # Structure and Function, maybe Biomass Metrics


tar_target(name = ind_phytoplankton_biomass_and_diversity,
           command = {
             ind_placeholder(ind_name="Phytoplankton Biomass and Diversity", areas = MPAs, readiness = NA,
                             source = "AZMP", objectives = NA))
           }), # Biomass Metrics, maybe species/functional Diversity


tar_target(name = ind_start_date_spring_bloom,
           command = {
             ind_placeholder(ind_name="Start Date of Spring Bloom", areas = MPAs, readiness = NA,
                             source = "AZMP", objectives = NA))
           }), # Structure and Function??


tar_target(name = ind_biomass_calanus_finmarchicus,
           command = {
             ind_placeholder(ind_name="Biomass of Calanus finmarchicus  ", areas = MPAs, readiness = NA,
                             source = "AZMP", objectives = NA))
           }), # Biomass Metrics


tar_target(name = ind_zooplankton_community_composition,
           command = {
             ind_placeholder(ind_name="Zooplankton Community Composition", areas = MPAs, readiness = NA,
                             source = "AZMP", objectives = NA))
           }), # Functional Diversity??


tar_target(name = ind_fish_eggs_and_larve,
           command = {
             ind_placeholder(ind_name="Fish Eggs and Larve", areas = MPAs, readiness = NA,
                             source = "Dedicated Surveys", objectives = NA))
           }), # Biomass Metrics?


tar_target(name = ind_seasonal_presence_cetaceans,
           command = {
             ind_placeholder(ind_name="Seasonal Presence/Absence of Cetaceans", areas = MPAs, readiness = NA,
                             source = "Passive Acoustics", objectives = NA))
           }), # Connectivity

tar_target(name = ind_seasonal_presence_seals,
           command = {
             ind_placeholder(ind_name="Seasonal Presence/Absence of Seals", areas = MPAs, readiness = NA,
                             source = "Tagging", objectives = NA))
           }), # Connectivity


tar_target(name = ind_seasonal_presence_pelagics,
           command = {
             ind_placeholder(ind_name="Seasonal Presence/Absence of Large Pelagics", areas = MPAs, readiness = NA,
                             source = "Tagging", objectives = NA))
           }), # Connectivity


tar_target(name = ind_seasonal_presence_seabirds,
           command = {
             ind_placeholder(ind_name="Seasonal Presence/Absence of Seabirds", areas = MPAs, readiness = NA,
                             source = "CWS Data", objectives = NA))
           }), # Connectivity


tar_target(name = ind_proportion_demersal_fish,
           command = {
             ind_placeholder(ind_name="Proportion of Large and Small Demersal Fish Species", areas = MPAs, readiness = NA,
                             source = "RV Survey", objectives = c("Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock"))
           }), # Structure and Function


tar_target(name = ind_abundance_invasive_species_webmr,
           command = {
             ind_placeholder(ind_name="Abundance of Aquatic Invasive Species in WEBMER", areas = MPAs, readiness = NA,
                             source = "RV Survey", objectives = c("Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock"))
           }), # Structure and Function


tar_target(name = ind_biomass_groundfish_prey_webmr,
           command = {
             ind_placeholder(ind_name="Biomass of Groundfish Prey Ppecies within WEBMR", areas = MPAs, readiness = NA,
                             source = "RV Survey", objectives = NA))
           }), # Biomass Metrics, Structure and Function


tar_target(name = ind_abundance_sea_pens_webmr,
           command = {
             ind_placeholder(ind_name="Abundance of Sea Pens within WEBMR", areas = MPAs, readiness = NA,
                             source = "RV Survey", objectives = NA))
           }), # Functional Diversity


tar_target(name = ind_richness,
           command = {
             ind_placeholder(ind_name="Diversity of Epibenthic and Infaunal Communities", areas = MPAs, readiness = NA,
                             source = "RV Survey", objectives = NA))
           }), # Species Diversity


tar_target(name = ind_community_comp_epibenthic_infaunal,
           command = {
             ind_placeholder(ind_name="Community Composition of Epibenthic and Infaunal Benthic Communities", areas = MPAs, readiness = NA,
                             source = "RV Survey", objectives = NA))
           }), # Structure and Function?


tar_target(name = ind_biomass_bioturbators,
           command = {
             ind_placeholder(ind_name="Biomass of Bioturbators", areas = MPAs, readiness = NA,
                             source = "RV Survey", objectives = NA))
           }), # Structure and Function?, Biomass Metrics?

tar_target(name = ind_spatial_extent_ebsa_webmr,
           command = {
             ind_placeholder(ind_name="Spatial Extent of EBSA with WEBMR", areas = MPAs, readiness = NA,
                             source = "Dedicated Surveys", objectives = NA))
           }), # Environmental Representativity


tar_target(name = ind_rel_abundance_groundfish,
           command = {
             ind_placeholder(ind_name="Relative abundance and biomass of select groundfish species", areas = MPAs, readiness = "Readily Available",
                             source = "RV Survey", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities",
                                                             "Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock"))
           }), # Biomass Metrics


tar_target(name = ind_size_distribution_groundfish,
           command = {
             ind_placeholder(ind_name="Size Distribution of Select Groundfish Species", areas = MPAs, readiness = NA,
                             source = "RV Survey", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities",
                                                                  "Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock"))
           }), # Biomass Metrics??


tar_target(name = ind_condition_groundfish,
           command = {
             ind_placeholder(ind_name="Condition of Select Groundfish Species", areas = MPAs, readiness = NA,
                             source = "RV Survey", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities",
                                                                  "Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock"))
           }), # Biomass Metrics??


tar_target(name = ind_fecundity_groundfish,
           command = {
             ind_placeholder(ind_name="Fecundity of Select Groundfish Species", areas = MPAs, readiness = NA,
                             source = "RV Survey", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities",
                                                                  "Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock"))
           }), # Biomass Metrics


tar_target(name = ind_community_comp_demersal,
           command = {
             ind_placeholder(ind_name="Community Composition of Demersal Fish", areas = MPAs, readiness = "Readily Available",
                             source = "RV Survey", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities",
                                                                  "Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock"))
           }), # Structure and Function


tar_target(name = ind_distribution_juv_haddock_habitat,
           command = {
             ind_placeholder(ind_name="Distriburion of Juvenile Haddock Habitat", areas = MPAs, readiness = NA,
                             source = "RV Survey", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities",
                                                                  "Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock"))
           }), # Key Fish Habitat


tar_target(name = ind_distribution_key_fish_habitat,
           command = {
             ind_placeholder(ind_name="Distribution of Key Fish Species Habitat", areas = MPAs, readiness = "Readily Available",
                             source = "RV Survey", objectives = c("Protect continential shelf habitats and associated benthic and demersal communities",
                                                                  "Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock"))
           }), # Key Fish Habitat


tar_target(name = ind_unauthorized_fishing,
           command = {
             ind_placeholder(ind_name="Amount of Unauthorized Fishing", areas = MPAs, readiness = NA,
                             source = "C&P", objectives = NA)
           }), # Threats to Productivity


tar_target(name = ind_seabed_disruption_in_around_webmr_fishing,
           command = {
             ind_placeholder(ind_name="Seabed area in and surrounding WEBMR swept by bottom-tending commercial fishing gear", areas = MPAs, readiness = NA,
                             source = "AIS Data", objectives = NA)
           }), # Threats to Habitat


tar_target(name = ind_total_annual_landings,
           command = {
             ind_placeholder(ind_name="Total annual landings form 4W for directed groundfish fisheries and common bycatch only stocks", areas = MPAs, readiness = NA,
                             source = "Commercial Catch Information", objectives = NA)
           }), # Threats to Productivity


tar_target(name = ind_seabed_disruption_in_around_webmr_research,
           command = {
             ind_placeholder(ind_name="Seabed area in and surrounding WEBMR swept by bottom-tending commercial fishing gear for research and monitoring", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Threats to Habitat


tar_target(name = ind_vessel_traffic,
           command = {
             ind_placeholder(ind_name="Vessel Traffic Intensity", areas = MPAs, readiness = NA,
                             source = "VMS", objectives = NA)
           }), # Threats to Habitat


tar_target(name = ind_ocean_sound,
           command = {
             ind_placeholder(ind_name="Ocean Sound", areas = MPAs, readiness = NA,
                             source = "VMS", objectives = NA)
           }), # Threats to Habitat


tar_target(name = ind_cables,
           command = {
             ind_placeholder(ind_name="Number of cables by type", areas = MPAs, readiness = NA,
                             source = "NRCan", objectives = NA)
           }), # Threats to Habitat


tar_target(name = ind_well_proximity,
           command = {
             ind_placeholder(ind_name="Number of wells in proximity to WEBMR", areas = MPAs, readiness = NA,
                             source = "Offshore Energy Regulator", objectives = NA)
           }), # Threats to Habitat


tar_target(name = ind_offshore_wind_developments,
           command = {
             ind_placeholder(ind_name="Number of offshore wind developments within in the vacinity of WEBMR", areas = MPAs, readiness = NA,
                             source = "Offshore Energy Regulator", objectives = NA)
           }), # Threats to Habitat, Threats to Productivity


tar_target(name = ind_contaminant_concentration_webmr,
           command = {
             ind_placeholder(ind_name="Concentrations of Contaminants by Type", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Threats to Productivity, Threats to Habitat


tar_target(name = ind_anthropogenic_debris,
           command = {
             ind_placeholder(ind_name="Quantity of Anthropogenic Debris", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Threats to Habitat


tar_target(name = ind_cumulative_impact,
           command = {
             ind_placeholder(ind_name="Change in Relative Cumulative Impact", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Threats to Productivity, Threats to Habitat



#Musquash from Halles Spreadsheet

tar_target(name = ind_biomass_abund_distribution_musquash,
           command = {
             ind_placeholder(ind_name="Total biomass, abundance, and dustribution of key species in each trophic level", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Biomass Metrics


tar_target(name = ind_species_per_trophic,
           command = {
             ind_placeholder(ind_name="Species per trophic level within each habitat type", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Structure and Function


tar_target(name = ind_species_at_risk,
           command = {
             ind_placeholder(ind_name="Number of at risk species within MPA", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Threats to Productivity, Biomass Metrics, Functional Diversity


tar_target(name = ind_disturbed_area,
           command = {
             ind_placeholder(ind_name="Total area and location of habitat type and proportion and frequency disturbed or lost", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Threats to Habitat


tar_target(name = ind_sediment_regime,
           command = {
             ind_placeholder(ind_name="Hydrodynamic and sediment regime in estuary", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Environmental Representativity


tar_target(name = ind_temp_salinity,
           command = {
             ind_placeholder(ind_name="Temperature and salinity within estuary", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Environmental Representativity


tar_target(name = ind_nutrients,
           command = {
             ind_placeholder(ind_name="Nutrient Concentrations", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Environmental Representataivity


tar_target(name = ind_cpue,
           command = {
             ind_placeholder(ind_name="Commercial and recreational fishing catch per unit effort", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Structure and Function, Biomass Metrics


tar_target(name = ind_bycatch,
           command = {
             ind_placeholder(ind_name="By-catch number per impacted species", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Threats to Productivity, Biomass Metrics


tar_target(name = ind_nonindigenous_rel_indigenous,
           command = {
             ind_placeholder(ind_name="Number of non-indigenous species relative to indigenous species in MPA", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Threats to Productivity, Functional Diversity


tar_target(name = ind_human_perturbation,
           command = {
             ind_placeholder(ind_name="Degree of human induced perturbation or loss", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Threats to Habitat


tar_target(name = ind_contaminant_concentration_musquash,
           command = {
             ind_placeholder(ind_name="Contaminant concentrations within the estuary", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Threats to Productivity, Biomass Metrics


#The Gully from Halles spreadsheet

tar_target(name = ind_abundance_nbw,
           command = {
             ind_placeholder(ind_name="Abundance of Northern Bottlenose Whales", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Biomass Metrics


tar_target(name = ind_mpa_use_nbw,
           command = {
             ind_placeholder(ind_name="Use of the Gully by Northern Bottlenose Whales ", areas = MPAs,readiness = NA,
                             source = NA, objectives = NA)
           }), # Connectivity?, Key Fish Habitat?


tar_target(name = ind_population_characteristics,
           command = {
             ind_placeholder(ind_name="Size, age, and sex structure of the Scotian Shelf population of Northern Bottlenose Whales", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Structure and Function?


tar_target(name = ind_fresh_scars,
           command = {
             ind_placeholder(ind_name="% of individuals in the Scotian Shelf Nortyhern Bottlenose population showing fresh scars ", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Threats to Habitat?


tar_target(name = ind_genetic_diversity_nbw,
           command = {
             ind_placeholder(ind_name="Genetic diversity within the Northern Bottlenose Whales", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Genetic Diversity


tar_target(name = ind_blubber_contaminants,
           command = {
             ind_placeholder(ind_name="Level of contaminants in the blubber of individuals in the Scotian Shelf population of Northern Bottlenose Whales ", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # ??


tar_target(name = ind_rel_abundance_cetaceans,
           command = {
             ind_placeholder(ind_name="Relative abundances of cetaceans beside Northern Bottlenose Whales in The Gully", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Biomass Metrics, Species Diversity


tar_target(name = ind_cetacean_presence,
           command = {
             ind_placeholder(ind_name="Cetacean presence and activity in the MPA, year-round", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Structure and Function


tar_target(name = ind_strandings,
           command = {
             ind_placeholder(ind_name="Number of reported strandings of Scotian Shelf Northern Bottlenose Whales", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # ??


tar_target(name = ind_ship_strikes,
           command = {
             ind_placeholder(ind_name="Number of reported ship strikes on cetaceans in or near the Gully and of strikes on Scotian Shelf Northern Bottlenose Whales elsewhere", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Threats to Habitat?


tar_target(name = ind_entanglements,
           command = {
             ind_placeholder(ind_name="Number of reported gear entanglements of cetaceans in or near the Gully and of entanglements of Scotian Shelf Northern Bottlenose Whales elsewhere", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Threats to Habitat?


tar_target(name = ind_human_interaction,
           command = {
             ind_placeholder(ind_name="Number of reports of other interactions between human activities and cetaceans in or near the Gully and of interactions with Scotian Shelf Northern Bottlenose Whales", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Threats to Habitat


tar_target(name = ind_coral_distribution,
           command = {
             ind_placeholder(ind_name="Coral distribution, density, and size structure by species at select monitoring sites", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Structure and Function


tar_target(name = ind_coral_diversity,
           command = {
             ind_placeholder(ind_name="Coral Diversity at selcted monitoring sites", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Species Diversity


tar_target(name = ind_coral_proportions,
           command = {
             ind_placeholder(ind_name="Proportions of live and dead corals, by species, at selected monitoring sites within the MPA", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Structure and Function?


tar_target(name = ind_zoanthid_overgrowth,
           command = {
             ind_placeholder(ind_name="Proportion on live corals at selected monitoring sites with the MPA that show zoanthid over-growths, and extent of over-growth in affected colonies", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Threats to Habitat, Threats to Productivity


tar_target(name = ind_trawl_vunerable,
           command = {
             ind_placeholder(ind_name="Relative abundances, size distributions and diversity of selected groundfish and trawl-vunerable invertebrates in Zone 3 of the MPA", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Biomass Metrics?


tar_target(name = ind_longline_vunerable,
           command = {
             ind_placeholder(ind_name="Relative abundances, size distributions and diversity of selected longline-vunerable species in Zones 2 and 3 of the MPA", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Biomass Metrics?


tar_target(name = ind_trap_vunerable,
           command = {
             ind_placeholder(ind_name="Relative abundances, size distributions and diversity of selected trap-vunerable species in Zonea 1 and 2 of the MPA", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Biomass Metrics?


tar_target(name = ind_mesopelagic_nektonic,
           command = {
             ind_placeholder(ind_name="Relative abundances, size distributions and diversity of selected mesopelagic nektonic species in Zones 1 and 2 of the MPA", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Species Diversity


tar_target(name = ind_environmental_conditions_near_seabed,
           command = {
             ind_placeholder(ind_name="Temperature, salinity, oxygen concentration, alkalinity, pH, light levels, chlorophyll, pigments and nutrients in water column within the MPA, including in close proximity to the seabed", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Environmental Representativity


tar_target(name = ind_environmental_conditions_azmp_lines,
           command = {
             ind_placeholder(ind_name="Temperature, salinity, oxygen concentration, light levels, chlorophyll, pigments and nutrients in waters flowing into and past the MPA, as measured on the Louisbourg Line, the Halifax Line and the Extended Halifax Line", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Environmental Representativity


tar_target(name = ind_physical_biological_surface_properties,
           command = {
             ind_placeholder(ind_name="Physical (temperature, salinity, wind, sea-surface height) and biological (ocean colour) sea surface properties in the MPA and the surrounding region", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Environmental Representativity


tar_target(name = ind_weather_station_buoy_sites,
           command = {
             ind_placeholder(ind_name="Weather conditions at the Sable Island weather station, Banquereau and Laurentian Fan weather-buoy sites, including wind direction and speed, air pressure and sea-level air temperatures, and at buoy sites sea surface temperatures, wave height and dominant wave period", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Environmental Representativity


tar_target(name = ind_water_masses,
           command = {
             ind_placeholder(ind_name="Three-dimensional distribution and movements of water masses within and around the MPA", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Environmental Representativity


tar_target(name = ind_phytoplankton_production,
           command = {
             ind_placeholder(ind_name="Phytoplankton production, community composition and timing of the spring bloom in the MPA and the surrounding region", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Structure and Function?


tar_target(name = ind_zooplankton_biomass,
           command = {
             ind_placeholder(ind_name="Zooplankton biomass, community composition, and the biomass of selected species within the MPA", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Biomass Metrics


tar_target(name = ind_acoustic_scattering,
           command = {
             ind_placeholder(ind_name="Acoustic scattering in the water column within the MPA (as a measure of mesopelagic and zooplankton densities and distribution)", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Biomass Metrics


tar_target(name = ind_seabird_abundance,
           command = {
             ind_placeholder(ind_name="Distribution and abundance of seabird species within the MPA, including an index of planktivorous seabird species", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Biomass Metrics


tar_target(name = ind_vessel_transits,
           command = {
             ind_placeholder(ind_name="Number of transits of the MPA by vessels other than pleasure craft, broken down into mercantile vessels, surface naval vessels and fishing vessels not fishing in the area", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Threats to Habitat?


tar_target(name = ind_operation_hours,
           command = {
             ind_placeholder(ind_name="Hours of operation within the MPA by vessels other than commercial fishing vessels or pleasure craft, broken down into research and monitoring vessels, other government vessels, and ecotourism vessels", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Threats to Habitat


tar_target(name = ind_fishing_effort_inside,
           command = {
             ind_placeholder(ind_name="Commercial fishing effort within the MPA", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Biomass Metrics


tar_target(name = ind_fishing_effort_nearby,
           command = {
             ind_placeholder(ind_name="Commercial fishing effort in close proximity to the MPA boundary", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # ??


tar_target(name = ind_unauthorized_fishing,
           command = {
             ind_placeholder(ind_name="Unauthoirized fishing activity within or close proximity to the MPA", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # ??


tar_target(name = ind_corals_removed,
           command = {
             ind_placeholder(ind_name="Quantities of corals removed from within the MPA by commercial fishing or research activity", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # ??


tar_target(name = ind_target_org_removed,
           command = {
             ind_placeholder(ind_name="Quantities of taget organisims removed from within the MPA and of bycatch organisms other than corals removed from the MPA by commercial fishing", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # ??


tar_target(name = ind_organisms_removed,
           command = {
             ind_placeholder(ind_name="Quantities of organisms besides corals removed from the MPA by research activities", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # ??


tar_target(name = ind_seabed_swept,
           command = {
             ind_placeholder(ind_name="Area of seabed swept by bottom-tending mobile research and monitoring gear in the MPA as a total and subdivided by seabed habitat type", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Threats to Habitat


tar_target(name = ind_seabed_occupied,
           command = {
             ind_placeholder(ind_name="Length of lines of, and seabed area occupied by, bottom-set fixed commercial fishing and research and monitoring gears within the MPA, as totals and subdivided by seabed habitat type", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Threats to Habitat


tar_target(name = ind_petroleum_activity,
           command = {
             ind_placeholder(ind_name="Number and types of offshore-petroleum exploration and development activities on the eastern Scotian Shelf", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Threats to Habitat


tar_target(name = ind_ballast,
           command = {
             ind_placeholder(ind_name="Number of ships’ ballast-water exchanges in the proximity of the MPA and the quantities of ballast exchanged", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Threats to Habitat


tar_target(name = ind_other_discharge,
           command = {
             ind_placeholder(ind_name="Number, quantities and type of other discharges from shipping within or in proximity to the MPA", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Threats to Habitat


tar_target(name = ind_floating_debris,
           command = {
             ind_placeholder(ind_name="Quantity of floating debris (i.e., large objects) in the Gully MPA", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Threats to Habitat


tar_target(name = ind_seabed_debris,
           command = {
             ind_placeholder(ind_name="Quantity of anthropogenic debris on the seabed at selected monitoring sites in the Gully MPA", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Threats to Habitat


tar_target(name = ind_invasive_gully,
           command = {
             ind_placeholder(ind_name="Reports of known invasive species in the Gully MPA", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Threats to Habitat


tar_target(name = ind_anthropogenic_sound,
           command = {
             ind_placeholder(ind_name="Quantitative characteristics of anthropogenic sound within the MPA", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), # Threats to Habitat



#St. Anns Bank from Halles spreadsheet

tar_target(name = ind_,
           command = {
             ind_placeholder(ind_name="", areas = MPAs, readiness = NA,
                             source = NA, objectives = NA)
           }), #



















