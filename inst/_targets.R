# Load packages required to define the pipeline:
library(targets)

if(dir.exists(file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","app_targets"))){
  tar_config_set(store = file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","app_targets"))
}



# Set target options here if they will be used in many targets, otherwise, you can set target specific packages in tar_targets below
tar_option_set(
  packages = c("MarConsNetApp", "sf", "targets", "viridis", "dataSPA", "arcpullr", "argoFloats", "raster",
               "TBSpayRates", "readxl", "ggplot2", "shinyBS", "Mar.datawrangling", "DT", "magrittr", "RColorBrewer", "dplyr", "tidyr", "stringr", "officer",
               "RColorBrewer", "car", "purrr", "MarConsNetAnalysis","MarConsNetData",
               "rnaturalearth"),
  #controller = crew::crew_controller_local(workers = 2),
  # imports = c("civi"),
  format = "qs"
)

# sapply(c(list.files("../MarConsNetAnalysis/R/","ind_",full.names = TRUE),
#          "../MarConsNetAnalysis/R/aggregate_groups.R",
#          "../MarConsNetAnalysis/R/plot_flowerplot.R",
#          "../MarConsNetData/R/data_bioregion.R",
#          "../MarConsNetData/R/data_CPCAD_areas.R"),source,.GlobalEnv)


#### targets list ####

list(
  ##### Areas #####
  tar_target(name = regions,
             command = {
               PA <- get_spatial_layer("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/eastern_canada_marine_spatial_planning_areas/MapServer/0")

               canada <- ne_states(country = "Canada", returnclass = "sf")

               A <- PA[grepl(PA$NAME_E, pattern = "Gulf"), ]
               B <- canada[canada$name_en %in% c("Quebec","Newfoundland and Labrador"), ]
               C <- canada[canada$name_en %in% c("New Brunswick","Nova Scotia","Prince Edward Island"), ]



               # Create a fine resolution grid within A
               grid_points <- st_make_grid(A, cellsize = 0.025, what = "polygons") %>%
                 st_as_sf() %>%
                 st_filter(A) # Keep only points inside A


               # Determine which part is closer to which reference polygon
               regions <- grid_points |>
                 st_as_sf() |>
                 mutate(centroids = st_centroid(x),
                        dist_to_B = as.numeric(st_distance(centroids, st_union(B))),
                        dist_to_C = as.numeric(st_distance(centroids, st_union(C))),
                        NAME_E = ifelse(dist_to_B < dist_to_C, "Quebec", "Gulf")) |>
                 group_by(NAME_E) |>
                 summarise(geoms = st_union(x)) |>
                 bind_rows(PA[!grepl(PA$NAME_E, pattern = "Gulf"), ]) |>
                 mutate(NAME_E = if_else(NAME_E == "Scotian Shelf and Bay of Fundy","Maritimes", NAME_E),
                        NAME_E = if_else(NAME_E == "Newfoundland-Labrador Shelves","Newfoundland & Labrador", NAME_E))

             }),

  # get the Protected and Conserved areas in the bioregion
  tar_target(name = MPAs,
             command = {
               areas <- get_spatial_layer("https://maps-cartes.ec.gc.ca/arcgis/rest/services/CWS_SCF/CPCAD/MapServer/0",
                                 where="BIOME='M' AND MGMT_E='Fisheries And Oceans Canada'") |>
                 group_by(NAME_E, NAME_F) |>
                 summarise(geoms = st_make_valid(st_union(geoms)))

               centroids <- st_centroid(areas$geoms) |>
                 st_as_sf() |>
                 st_join(regions)

               areas$region <- centroids$NAME_E

               areas |>
                 filter(!is.na(region))
             }),

  tar_target(Outside, #FIXME: this is only for WEBCA at the moment
             st_transform(read_sf(system.file("data","WEBCA_10k_85k.shp", package = "MarConsNetAnalysis"))$geometry, crs=4326)
  ),

  tar_target(name = Ecological,
             command = {
               data.frame(grouping=rep(c("Biodiversity",
                                         "Habitat",
                                         "Productivity"),
                                       times=c(3,5,3)),
                          labels=c("Genetic Diversity",
                                   "Species Diversity",
                                   "Functional Diversity",

                                   "Environmental Representativity",
                                   "Key Fish Habitat",
                                   "Connectivity",
                                   "Uniqueness",
                                   "Threats to Habitat",

                                   "Biomass Metrics",
                                   "Structure and Function",
                                   "Threats to Productivity")) |>
                 mutate(weight=runif(11,1,10),
                        angle=(cumsum(weight)-weight/2)/sum(weight)*360)
             }),

  tar_target(name = APPTABS,
             command = {
               ftabs <- expand.grid(flower=unique(c(Ecological$grouping, Ecological$labels)), place=unique(MPAs$region))
               mytabs <- NULL
               for (i in seq_along(MPAs$NAME_E)) {
                 df <- ftabs
                 df$place <- MPAs$NAME_E[i]
                 mytabs[[i]] <- df
               }

               MYTABS <- do.call(rbind, mytabs)
               apptabs <- rbind(ftabs, MYTABS)
               apptabs$tab <- paste0("tab_", seq_along(1:length(apptabs$flower)))
               apptabs$link <- paste0("link_", seq_along(1:length(apptabs$flower)))
               home <- data.frame(flower="home", place="home", tab="tab_0", link="link_0")
               rbind(home, apptabs)
             }),

  tar_target(name = om, # FIXME: need use a real cookie or otherwise update the data
             command = {
               OM <- dataSPA::getData(type="om", age=3000, cookie="hi",
                                      path = file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","data"))

               OM[-(which(OM$activity_type == "Other")),]

             }),

  tar_target(name = areas,
             command = {
               MPAs$NAME_E
             }),

  tar_target(name = objectives,
             command = {
               lapply(areas, function(x) data_objectives(type="site", area=x))
               }),

  tar_target(name = Objectives_processed,
             command = {
               OBJECTIVES <- vector(mode="list", length(objectives))
               for (i in seq_along(OBJECTIVES)) {
                 O <- objectives[[i]]
                 for (j in seq_along(O)) {
                   OBJECTIVES[[i]][[j]] <- newLine(O[j])
                 }
               }
               OBJECTIVES <- lapply(OBJECTIVES, unlist)
               names(OBJECTIVES) <- areas
               OBJECTIVES <- OBJECTIVES[which(unname(unlist(lapply(OBJECTIVES, function(x) !(is.null(x))))))]
               OBJECTIVES
             }),



  tar_target(name = N_Objectives,
             command = {
               NO <- data_objectives(type="network")

               no <- vector(mode="list", length(NO))
               for (i in seq_along(NO)) {
                 O <- NO[[i]]

                 NO[[i]] <- newLine(O)
               }
               unlist(NO)
             }),

  tar_target(name=rv_rounded_location,
             command= {

               df <- data.frame(latitude=haddock_biomass$latitude, longitude=haddock_biomass$longitude)
               df2 <- data.frame(latitude=all_haddock$latitude, longitude=all_haddock$longitude)
               df <- rbind(df,df2)
               latitude <- round(df$latitude,1)
               longitude <- round(df$longitude,1)
               coord <- data.frame(latitude, longitude)

               # Get unique pairs
               unique_coords <- unique(coord)
               latitude <- unique_coords$latitude
               longitude <- unique_coords$longitude
               df <- data.frame(latitude=latitude, longitude=longitude, type="RV Survey")
               df
             }
  ),

  tar_target(name = Context,
             command = {
               c <- lapply(areas, function(x) data_context(type="site", area=x))
               names(c) <- areas
               c <- c[which(unname(unlist(lapply(c, function(x) !(is.null(x))))))]
               c
               }),

  tar_target(name = fp,
             command = {
               read_excel(file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","data","metaframework.xlsx"))
             }),



  tar_target(name = flowerPalette,
             command = {
               grades <- c("A", "B", "C", "D", "F")
               palette <- rev(colorRampPalette(brewer.pal(5,"RdYlBu"))(length(grades)))
               names(palette) <- grades
               palette
               }),

  tar_target(name=indicatorFlower,
             command = {
               grades <- c(100, 50, 0)
               palette <- c("#2C7BB6", "#FFFFBF","#D7191C")
               names(palette) <- grades
               palette

             }
             ),

  tar_target(name = odf,
             command = {
               O <- data.frame(
                 objectives = c(0, unlist(Objectives_processed, use.names = FALSE), N_Objectives)
               )
               O$flower_plot <- 0
               O$area <- 0
               get_first_four_words <- function(texts) { # 7 words
                 lapply(texts, function(text) {
                   words <- strsplit(text, " ")[[1]] # Split each string into words
                   first_four_words <- paste(words[1:min(7, length(words))], collapse = " ") # Concatenate the first four words (or fewer if there are not enough words)
                   return(first_four_words)
                 })
               }

               for (i in seq_along(O$objectives)) {
                 message(i)
                 ob <- gsub("[-\n]", "", O$objectives[i])
                 if (!(O$objectives[i] == "0")) {
                   keep <- which(tolower(get_first_four_words(fp$label_Objective)) == tolower(get_first_four_words(ob)[[1]]))
                   if (length(keep) > 1) {
                     browser()
                   }
                   if (!(length(keep) == 0)) {
                     O$flower_plot[i] <- fp$Flowerplot_connection[keep]
                     O$area[i] <- fp$label_Framework[keep]
                   } else {
                     message("i is also wrong ", i)
                   }
                 } else {
                   O$flower_plot[i] <- "flower_0"
                   O$area[i] <= "area_0"
                 }
               }

               O$tab <- 0
               O$link <- 0
               for (i in seq_along(O$objectives)) {
                 message("i = ", i)
                 if (!(i == 1)) {
                   if (!(grepl("Indicator", O$flower_plot[i]))) {
                     k1 <- which(APPTABS$place %in% O$area[i]) # SAME AREA AND FLOWER
                     k2 <- which(APPTABS$flower == O$flower_plot[i])
                     if (length(k2) == 0) {
                       if (grepl("Environmental", O$flower_plot[i], ignore.case=TRUE)) {
                         k2 <- which(APPTABS$flower == "Environmental Representativity")
                       }
                     }
                     keep <- intersect(k1,k2)
                     O$tab[i] <- APPTABS$tab[keep]
                     O$link[i] <- APPTABS$link[keep]
                   } else {
                     k <- which(pillar_ecol_df$indicators == trimws(gsub("-", "", gsub("\n", "", O$objectives[i]))), "right")
                     O$tab[i] <- pillar_ecol_df$tab[k]
                     O$link[i] <- pillar_ecol_df$link[k]

                   }
                 } else {
                   O$tab[i] <- "tab_0"
                   O$link[i] <- "link_0"
                 }

               }
               O

             }),

 tar_target(ds_all,
            # because this is loaded with the Mar.datawrangling package and not mentioned in the arguments to many of it's functions
            # mention ds_all whenever using e.g. self_filter() or summarize_catches()
            ds_all),


 tar_target(rv_rawdata_env,{
   temp <- new.env()

   get_data(db = 'rv',
            data.dir = file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","data","rv"),
            env = temp
   )

   temp
 }
 ),

 tar_target(rv_data,{
   # for whatever reason, we need to run:
   # tar_invalidate(c("ds_all","rv_rawdata_env"))
   # before re-running this target
   temp <- rv_rawdata_env
   ds_all # mentioned here because otherwise it won't be available for self_filter

   temp$GSINF <- rv_rawdata_env$GSINF |>
     filter(!is.na(LONGITUDE),!is.na(LATITUDE)) |>
     st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
              crs = 4326,
              remove = FALSE) |>
     st_filter(regions[regions$NAME_E=="Maritimes",]) |>
     st_join(MPAs |> select(NAME_E), left=TRUE) |>
     as.data.frame() |>
     select(-geometry)


   self_filter(env = temp)

   summarize_catches('rv',env = temp)
 }
 ),

 tar_target(rv_data_det,{
   # for whatever reason, we need to run:
   # tar_invalidate(c("ds_all","rv_rawdata_env"))
   # before re-running this target
   temp <- rv_rawdata_env
   ds_all # mentioned here because otherwise it won't be available for self_filter

   temp$GSINF <- rv_rawdata_env$GSINF |>
     filter(!is.na(LONGITUDE),!is.na(LATITUDE)) |>
     st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
              crs = 4326,
              remove = FALSE) |>
     st_filter(regions[regions$NAME_E=="Maritimes",]) |>
     st_join(MPAs |> select(NAME_E), left=TRUE) |>
     as.data.frame() |>
     select(-geometry)


   self_filter(env = temp)

   summarize_catches('rv',morph_dets=TRUE,env = temp)
 }
 ),

 tar_target(name = fish_weight_per_1.75kn_tow,
            command = {
              rv_data |>
                filter(COMM %in% c("HADDOCK"#,
                                   # "COD(ATLANTIC)",
                                   # "AMERICAN PLAICE",
                                   # "WINTER SKATE"
                                   ),
                       NAME_E == "Western/Emerald Banks Conservation Area (Restricted Fisheries Zone)") |>
                mutate(latitute = LATITUDE,
                       longitude = LONGITUDE,
                       `Total Weight per 1.75kn Tow (kg)` = TOTWGT/DIST*1.75,
                       units = "kg",
                       type = "RV Survey",
                       year = YEAR) |>
                select(latitute,
                       longitude,
                       year,
                       `Total Weight per 1.75kn Tow (kg)`,
                       type,
                       units)


            }),

 tar_target(name = data_QC_gulf_biogenic_habitat,
            command = {
              # Sea pens significant areas
              # from https://open.canada.ca/data/en/dataset/87ae08e8-5fc2-476a-b3f5-c8f0ea4be9ef
              seapens <- get_spatial_layer("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/sea_pens_plumes_mer_significant_benthic_areas/MapServer/0")

              # Sponge Fields from https://osdp-psdo.canada.ca/dp/en/search/metadata/NRCAN-FGP-1-7b71b73b-0d05-4c61-958d-4beccd1bd3b1
              spongeREST <- "https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/csas_corals_sponges_2010_en/MapServer/"
              sponge <- bind_rows(get_spatial_layer(paste0(spongeREST,"10")),
                                  get_spatial_layer(paste0(spongeREST,"11")) |>
                                    mutate(Threshold = as.character(Threshold)))

              data_QC_gulf_biogenic_habitat <- bind_rows(seapens |>
                                        select(geoms) |>
                                        mutate(layer = "Seapen Significant Areas"),
                                      sponge |>
                                        select(geoms) |>
                                        mutate(layer = "Sponge Fields")) |>
                group_by(layer) |>
                reframe(geoms = st_make_valid(st_combine(geoms))) |>
                st_as_sf()
            }),



 tar_target(name = data_azmp_fixed_stations,
            command = {

              DOS  <- azmpdata::Derived_Occupations_Stations

              # Add rows one by one
              azmpdata::Zooplankton_Annual_Stations |>
                select(station) |>
                unique() |>
                rowwise() |>
                mutate(longitude = if_else(station=="HL2",
                                           DOS$longitude[DOS$station==station][274],
                                           DOS$longitude[DOS$station==station][1]),
                       latitude = if_else(station=="HL2",
                                          DOS$latitude[DOS$station==station][274],
                                          DOS$latitude[DOS$station==station][1])) |>
                add_row(station = "Halifax", latitude = 43.5475, longitude = -63.5714) |>
                add_row(station = "Yarmouth", latitude = 43.8377, longitude = -66.1150) |>
                add_row(station = "North Sydney", latitude = 46.2051, longitude = -60.2563)
            }),


 tar_target(name = data_azmp_zooplankton_annual_stations,
            command = {
              azmpdata::Zooplankton_Annual_Stations |>
                left_join(data_azmp_fixed_stations, by = "station")
            }),

 tar_target(name = data_azmp_Discrete_Occupations_Sections,
            command = {
              df <- azmpdata::Discrete_Occupations_Sections |>
                mutate(year = as.numeric(format(date, "%Y")))
            }),

 tar_target(name=whale_biodiversity,
            command= {
              ws <- project_whale_biodiversity()
              ws
            }),

 ##### Indicators #####

 # tar_target(ind_placeholder_df,ind_placeholder(areas = all_areas)),
 tar_target(ind_placeholder_df,
            command = {
             process_indicator(data = NA,
                               indicator = "placeholder",
                               areas = MPAs,
                               plot_type = 'time-series')
            }),

 tar_target(ind_fish_length,
            command = {

              data <- rv_data_det |>
                mutate(longitude = LONGITUDE,
                       latitude = LATITUDE,
                       fish_length = FLEN,
                       year = YEAR)  |>
                dplyr::select(longitude, latitude, year, fish_length)

              process_indicator(data = data,
                               indicator_var_name = "fish_length",
                               indicator = "Fish Length",
                               type = "Ecosystem Trawl Survey",
                               units = "cm",
                               scoring = "desired state: increase",
                               PPTID = 726,
                               project_short_title = "RV Survey",
                               areas = MPAs,
                               plot_type = "violin",
                               plot_lm=FALSE)
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
                               project_short_title = "RV Survey",
                               areas = MPAs[MPAs$region=="Maritimes",],
                               plot_type = "violin",
                               plot_lm=FALSE)
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

              process_indicator(data = data,
                               indicator_var_name = "haddock_counts",
                               indicator = "Haddock Number per Tow",
                               type = "Ecosystem Trawl Survey",
                               units = "Number per Tow",
                               scoring = "desired state: increase",
                               PPTID = 726,
                               project_short_title = "RV Survey",
                               areas = MPAs,
                               plot_type = "violin",
                               plot_lm=FALSE)
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

              process_indicator(data = data,
                               indicator_var_name = "haddock_biomass",
                               indicator = "Biomass of Haddock per Tow",
                               type = "Ecosystem Trawl Survey",
                               units = "kg per tow",
                               scoring = "desired state: increase",
                               PPTID = 726,
                               project_short_title = "RV Survey",
                               areas = MPAs,
                               plot_type = "violin",
                               plot_lm=FALSE)
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
                               project_short_title = "AZMP",
                               areas = MPAs,
                               plot_type='time-series',
                               plot_lm=FALSE)
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
                               project_short_title = "AZMP",
                               climate = TRUE,
                               areas = MPAs,
                               plot_type='time-series',
                               plot_lm=FALSE)
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
                               project_short_title = "AZMP",
                               climate = TRUE,
                               other_nest_variables="depth",
                               areas = MPAs,
                               plot_type = 'time-series',
                               plot_lm=FALSE)
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
                               project_short_title = "AZMP",
                               other_nest_variables="depth",
                               areas = MPAs,
                               plot_type='time-series',
                               plot_lm=FALSE)
            }),

 tar_target(ind_temperature,
            command={
              data <- data_azmp_Discrete_Occupations_Sections  |>
                dplyr::select(longitude, latitude, year, depth, temperature)

              process_indicator(data = data,
                               indicator_var_name = "temperature",
                               indicator = "Temperature",
                               type = "Discrete Occupations Sections",
                               units = "C",
                               scoring = "desired state: decrease",
                               PPTID = 579,
                               project_short_title = "AZMP",
                               climate = TRUE,
                               other_nest_variables="depth",
                               areas = MPAs,
                               plot_type='time-series',
                               plot_lm=FALSE)
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
                               project_short_title = "AZMP",
                               other_nest_variables="depth",
                               areas = MPAs,
                               plot_type='time-series',
                               plot_lm=FALSE)
            }),

 tar_target(ind_bloom_amplitude,
            command={
              script_lines <- readLines("https://raw.githubusercontent.com/BIO-RSG/PhytoFit/refs/heads/master/tools/tools_01a_define_polygons.R")

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
                               project_short_title = "AZMP",
                               areas = MPAs,
                               plot_type = "time-series",
                               plot_lm=FALSE)
            }
 ),

 tar_target(name = ind_QC_gulf_biogenic_habitat_representation,
            command = {
                            process_indicator(data = data_QC_gulf_biogenic_habitat,
                               indicator_var_name = "layer",
                               indicator = "Biogenic Habitat Representation",
                               type = "Model",
                               units = NA,
                               scoring = "representation",
                               PPTID = NA,
                               project_short_title = "Biogenic Habitat",
                               areas = MPAs[MPAs$region %in% c("Quebec","Gulf"),],
                               plot_type='map',
                               plot_lm=FALSE)
            }),

 ##### Indicator Bins #####
 tar_target(bin_biodiversity_FunctionalDiversity_df,
            aggregate_groups("bin",
                             "Functional Diversity",
                             weights_ratio=1,
                             weights_sum = 1,
                             ind_placeholder_df
                             )),
 tar_target(bin_biodiversity_GeneticDiversity_df,
            aggregate_groups("bin",
                             "Genetic Diversity",
                             weights_ratio=1,
                             weights_sum = 1,
                             ind_placeholder_df
            )),
 tar_target(bin_biodiversity_SpeciesDiversity_df,
            aggregate_groups("bin",
                             "Species Diversity",
                             weights_ratio=1,
                             weights_sum = 1,
                             ind_placeholder_df
            )),
 tar_target(bin_habitat_Connectivity_df,
            aggregate_groups("bin",
                             "Connectivity",
                             weights_ratio=1,
                             weights_sum = 1,
                             ind_placeholder_df
            )),
 tar_target(bin_habitat_EnvironmentalRepresentativity_df,
            aggregate_groups("bin",
                             "Environmental Representativity",
                             weights_ratio = c(1,1,1,0.5,1,1),
                             weights_sum = 1,
                             ind_nitrate,
                             ind_salinity,
                             ind_chlorophyll,
                             ind_temperature,
                             ind_surface_height,
                             ind_bloom_amplitude
                             )),
 tar_target(bin_habitat_KeyFishHabitat_df,
            aggregate_groups("bin",
                             "Key Fish Habitat",
                             weights_ratio = 1,
                             weights_sum = 1,
                             ind_temperature
            )),
 tar_target(bin_habitat_ThreatstoHabitat_df,
            aggregate_groups("bin",
                             "Threats to Habitat",
                             weights_ratio=1,
                             weights_sum = 1,
                             ind_placeholder_df
            )),
 tar_target(bin_habitat_Uniqueness_df,
            aggregate_groups("bin",
                             "Uniqueness",
                             weights_ratio=1,
                             weights_sum = 1,
                             ind_QC_gulf_biogenic_habitat_representation,
                             ind_placeholder_df
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
                             ind_zooplankton
            )),
 tar_target(bin_productivity_StructureandFunction_df,
            aggregate_groups("bin",
                             "Structure and Function",
                             weights_ratio=1,
                             weights_sum = 1,
                             ind_placeholder_df
            )),
 tar_target(bin_productivity_ThreatstoProductivity_df,
            aggregate_groups("bin",
                             "Threats to Productivity",
                             weights_ratio=1,
                             weights_sum = 1,
                             ind_placeholder_df
            )),


 ##### Ecological Objectives #####
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

 ##### Ecological Pillar #####

 tar_target(pillar_ecol_df,

            {
            APPTABS
            target_bin_weight <- 1

            pedf <- aggregate_groups("pillar",
                             "Ecological",
                             weights_ratio=NA,
                             weights_sum = NA,
                             ecol_obj_biodiversity_df,
                             ecol_obj_habitat_df,
                             ecol_obj_productivity_df)|>
              mutate(PPTID = as.character(PPTID)) |>
              left_join(select(as.data.frame(MPAs),NAME_E, region),by = c("areaID"="NAME_E"))

          x <-  pedf |>
   group_by(objective, bin, areaID, region) |>
   reframe(indicator = unique(areaID),
           areaID = unique(region),
           score = weighted.mean(score,weight,na.rm = TRUE),
           score = if_else(is.nan(score),NA,score),
           PPTID = paste(PPTID, collapse = "; ")) |>
   group_by(bin) |>
   mutate(weight=target_bin_weight/n()) |>
   ungroup() |>
   bind_rows(pedf) |>
            # mutate(tab=make.names(paste0(areaID,
            #                              "_",
            #                              indicator)))
            mutate(tab=paste0("tab_", seq(length(APPTABS$flower) + 1, length(APPTABS$flower) + length(objective))))
 }),

 tar_target(all_project_geoms,
             command = {

               pillar_ecol_df |>
                 filter(!map_lgl(data, is.null)) |>
                 mutate(data = map(data,~.x |> dplyr::select(year,geometry) |> distinct())) |>
                 dplyr::select(data,type, project_short_title, PPTID,areaID) |>
                 unnest(cols = data) |>
                 st_as_sf()

             }),


tar_target(plot_files,
            command = {
              allplotnames <- NULL
              for(i in 1:nrow(pillar_ecol_df)){
                if(!is.null(pillar_ecol_df$plot[[i]])){
                filename <-  file.path(Sys.getenv("OneDriveCommercial"),
                                       "MarConsNetTargets",
                                       "data", "plots",
                                       make.names(paste0("plot_",
                                                         pillar_ecol_df$areaID[i],
                                                         "_",
                                                         pillar_ecol_df$indicator[i],
                                                         ".png")))

                allplotnames <- c(allplotnames,filename)
                ggsave(filename,pillar_ecol_df$plot[[i]])
                }
              }
              allplotnames
            }),


 tar_target(fish_length,
            command = {
              get_data('rv', data.dir = "C:/Users/HarbinJ/Documents/data/rv")

              GSDET$latitude <- 0
              GSDET$longitude <- 0
              GSDET$year <- 0
              missions <- unique(GSDET$MISSION)

              GSINF <-GSINF[-which(is.na(GSINF$SDATE)),]
              for (i in seq_along(missions)) {
                GSDET$latitude[which(GSDET$MISSION == missions[i])] <- GSINF$LATITUDE[which(GSINF$MISSION == missions[i])][1]
                GSDET$longitude[which(GSDET$MISSION == missions[i])]  <- GSINF$LONGITUDE[which(GSINF$MISSION == missions[i])][1]
                GSDET$year[which(GSDET$MISSION == missions[i])]  <- unique(as.numeric(substr(GSINF$SDATE[which(GSINF$MISSION == missions[i])],1,4)))
              }
              GSDET$type <- "RV Survey"
              names(GSDET)[which(names(GSDET) == "FLEN")] <- "fish_length"
              GS <- GSDET[,c("longitude", "latitude", "year", "fish_length", "type")]
              GS$units <- "cm"
              GS
            }

 ),

 tar_target(fish_weight,
            command = {
              get_data('rv', data.dir = "C:/Users/HarbinJ/Documents/data/rv")

              GSDET$latitude <- 0
              GSDET$longitude <- 0
              GSDET$year <- 0
              missions <- unique(GSDET$MISSION)

              GSINF <-GSINF[-which(is.na(GSINF$SDATE)),]
              for (i in seq_along(missions)) {
                GSDET$latitude[which(GSDET$MISSION == missions[i])] <- GSINF$LATITUDE[which(GSINF$MISSION == missions[i])][1]
                GSDET$longitude[which(GSDET$MISSION == missions[i])]  <- GSINF$LONGITUDE[which(GSINF$MISSION == missions[i])][1]
                GSDET$year[which(GSDET$MISSION == missions[i])]  <- unique(as.numeric(substr(GSINF$SDATE[which(GSINF$MISSION == missions[i])],1,4)))
              }
              GSDET$type <- "RV Survey"
              names(GSDET)[which(names(GSDET) == "FWT")] <- "fish_weight"

              GS <- GSDET[,c("longitude", "latitude", "year", "fish_weight", "type")]
              GS$units <- "g"
              GS
            }

 ),

 tar_target(all_haddock,
            command={
              get_data('rv', data.dir = "C:/Users/HarbinJ/Documents/data/rv")

              # All haddock
              GSSPECIES = GSSPECIES[GSSPECIES$CODE %in% c(11),]
              Mar.datawrangling::self_filter(keep_nullsets = F)
              ah = Mar.datawrangling::summarize_catches()
              names(ah)[which(names(ah) == "LATITUDE")] <- "latitude"
              names(ah)[which(names(ah) == "LONGITUDE")] <- "longitude"
              names(ah)[which(names(ah) == "SDATE")] <- "date"
              names(ah)[which(names(ah) == "TOTNO")] <- "haddock_abundance"
              ah$type <- "RV"
              ah$units <- "(counts)"
              names(ah)[which(names(ah) == "TOTWGT")] <- "haddock_biomass"
              ah$year <- as.numeric(format(ah$date, "%Y"))
              ah <- ah[-(which(ah$longitude > -10)),] # remove outlier points

              AH <- ah[,c("longitude", "latitude","year", "haddock_abundance", "type", "units"),]

              AH
            }),

 tar_target(haddock_biomass,
            command={
              get_data('rv', data.dir = "C:/Users/HarbinJ/Documents/data/rv")

              # All haddock
              GSSPECIES = GSSPECIES[GSSPECIES$CODE %in% c(11),]
              Mar.datawrangling::self_filter(keep_nullsets = F)
              ah = Mar.datawrangling::summarize_catches()
              names(ah)[which(names(ah) == "LATITUDE")] <- "latitude"
              names(ah)[which(names(ah) == "LONGITUDE")] <- "longitude"
              names(ah)[which(names(ah) == "SDATE")] <- "date"
              names(ah)[which(names(ah) == "TOTNO")] <- "haddock_abundance"
              ah$type <- "RV"
              ah$units <- "(counts)"
              names(ah)[which(names(ah) == "TOTWGT")] <- "haddock_biomass"
              ah$year <- as.numeric(format(ah$date, "%Y"))
              ah <- ah[-(which(ah$longitude > -10)),] # remove outlier points

              AH <- ah[,c("longitude", "latitude","year", "haddock_biomass", "type"),]
              AH$units <- "kg"
              AH
            }),

 tar_target(zooplankton,
            command={

              df <- azmpdata::Zooplankton_Annual_Stations
              sdf <- azmpdata::Derived_Occupations_Stations
              df$latitude <- 0
              df$longitude <- 0
              for (i in seq_along(unique(df$station))) {
                if (unique(df$station)[i] == "HL2") { # ISSUE 53
                  k <- 274
                } else {
                  k <- 1
                }
                df$latitude[which(df$station == unique(df$station)[i])] <- sdf$latitude[which(sdf$station == unique(df$station)[i])][k]
                df$longitude[which(df$station == unique(df$station)[i])] <- sdf$longitude[which(sdf$station == unique(df$station)[i])][k]
              }

              df$type <- "Zooplankton AZMP"
              df$Calanus_finmarchicus_biomass <- df$Calanus_finmarchicus_log10

              DF <- df[c("latitude", "longitude", "type", "Calanus_finmarchicus_biomass", "year")]
              DF$units <- "log_10"
              DF

            }),




 tar_target(surface_height,
            command={
              df <- azmpdata::Derived_Monthly_Stations
              # Add latitude and longitude
              df$latitude <- 0
              df$longitude <- 0
              type <- NULL
              df$latitude[which(df$station == "Halifax")] <- 43.5475
              df$longitude[which(df$station == "Halifax")] <- -63.5714

              df$latitude[which(df$station == "Yarmouth")] <- 43.8377
              df$longitude[which(df$station == "Yarmouth")] <- -66.1150

              df$latitude[which(df$station == "North Sydney")] <- 46.2051
              df$longitude[which(df$station == "North Sydney")] <- -60.2563
              df$units <- "m"
              df$type <- "derived (AZMP)"
              df <- df[,c("latitude", "longitude", "year", "units", "type", "sea_surface_height")]
              df
            }),

 tar_target(nitrate,
            command={
              df <- azmpdata::Discrete_Occupations_Sections
              df$year <- as.numeric(format(df$date, "%Y"))
              df$type <- "AZMP"

              DF <- df[,c("latitude", "longitude", "year", "nitrate", "type", "depth")]
              DF$units <- "mmol/m3"
              DF
            }),

 tar_target(salinity,
            command={
              df <- azmpdata::Discrete_Occupations_Sections
              df$year <- as.numeric(format(df$date, "%Y"))
              df$type <- "AZMP"

              DF <- df[,c("latitude", "longitude", "year", "salinity", "type", "depth")]
              DF$units <- "psu"
              DF
            }),

 tar_target(temperature,
            command={
              df <- azmpdata::Discrete_Occupations_Sections
              df$year <- as.numeric(format(df$date, "%Y"))
              df$type <- "AZMP"

              DF <- df[,c("latitude", "longitude", "year", "temperature", "type", "depth")]
              DF$units <- "C"
              DF
            }),

 tar_target(chlorophyll,
            command={
              df <- azmpdata::Discrete_Occupations_Sections
              df$year <- as.numeric(format(df$date, "%Y"))
              df$type <- "In situ AZMP"

              DF <- df[,c("latitude", "longitude", "year", "chlorophyll", "type", "depth")]
              DF$units <- "ug/L"
              DF
            }),








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
            }),


 tar_target(bloom_df,
            command={
              script_lines <- readLines("https://raw.githubusercontent.com/BIO-RSG/PhytoFit/refs/heads/master/tools/tools_01a_define_polygons.R")

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


              df <- azmpdata::RemoteSensing_Annual_Broadscale
              df <- df[which(df$area == "CSS_remote_sensing"),]
              df$geom <- rep(polygon_sf)

              DF <- df[,c("area", "year", "bloom_amplitude", "geom"),]
              DF$type <- "Remote Sensing"
              DF$units <- "(unit unknown)"
              DF
            }
            ),


 ##### Pillar #####

tar_target(name = MPA_report_card,
           command = {
             left_join(MPAs,pillar_ecol_df |>
               select(-data,-plot) |>
               filter(indicator %in% MPAs$NAME_E) |>
               calc_group_score(grouping_var = "indicator") |>
               mutate(grade = if_else(is.nan(score),
                                      NA,
                                      calc_letter_grade(score))),
               by=c("NAME_E"="indicator"))
           }),
tar_target(name = upload_all_data_to_shiny,
            command = {

              if(Sys.getenv("USERPROFILE")=="C:\\Users\\DaigleR"){
                serveruser="rdaigle"
              } else if(Sys.getenv("USERPROFILE")=="C:\\Users\\HarbinJ"){
                serveruser="jharbin"
              } else {
                return(TRUE)
              }

              system(paste0('scp -r -i ',
                            file.path(Sys.getenv("USERPROFILE"),'.ssh','id_rsa'),
                            ' -P 22018 "',
                            file.path(Sys.getenv("OneDriveCommercial"),'MarConsNetTargets','app_targets'),
                            '" "',
                            serveruser,
                            '@glf-proxy:/home/rdaigle/MarConsNetTargets/'))

              system(paste0('scp -r -i ',
                            file.path(Sys.getenv("USERPROFILE"),'.ssh','id_rsa'),
                            ' -P 22018 "',
                            file.path(Sys.getenv("OneDriveCommercial"),'MarConsNetTargets','data'),
                            '" "',
                            serveruser,
                            '@glf-proxy:/home/rdaigle/MarConsNetTargets/'))

              TRUE
            },
            deployment = "main",
            priority = 0,
            cue = tar_cue(mode = "always"))


) |>
  unlist(recursive = FALSE)
