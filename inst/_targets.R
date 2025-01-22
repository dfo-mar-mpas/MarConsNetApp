# Load packages required to define the pipeline:
library(targets)

tar_config_set(store = file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","app_targets"))


# Set target options here if they will be used in many targets, otherwise, you can set target specific packages in tar_targets below
tar_option_set(
  packages = c("MarConsNetApp", "sf", "targets", "viridis", "dataSPA", "arcpullr", "argoFloats", "raster",
               "shiny", "leaflet", "dplyr", "shinyjs", "devtools", "MarConsNetAnalysis", "MarConsNetData",
               "TBSpayRates", "readxl", "ggplot2", "shinyBS", "Mar.datawrangling", "DT", "magrittr", "RColorBrewer", "dplyr", "tidyr", "stringr", "officer",
               "RColorBrewer"),
  #controller = crew::crew_controller_local(workers = 2),
  imports = c("civi"),
  format = "qs"
)

sapply(c(list.files("../MarConsNetAnalysis/R/","ind_",full.names = TRUE),
         "../MarConsNetAnalysis/R/aggregate_groups.R",
         "../MarConsNetAnalysis/R/plot_flowerplot.R",
         "../MarConsNetData/R/data_bioregion.R",
         "../MarConsNetData/R/data_CPCAD_areas.R"),source,.GlobalEnv)


#### targets list ####

list(
  tar_target(name = MPAs,
             command = {
               data_CPCAD_areas(data_bioregion("Scotian Shelf"),  zones = FALSE)
             }),


  tar_target(name = subarea_coords,
             command = {
               getLatLon(MPAs)
             }),


  # Coastal Material
  tar_target(name = EBM,
             command =  { data.frame(grouping=rep(c("Ecological",
                                                 "Economic",
                                                 "Governance",
                                                 "Social & Cultural"),
                                               times=c(3,3,3,4)),
                                  labels=c("Habitat",
                                           "Biodiversity",
                                           "Productivity",
                                           "Economic Effiency",
                                           "Economic Equity",
                                           "Economic Sustainability",
                                           "Governance Outcomes",
                                           "Governance Structure & Processes",
                                           "Legal Obligations & Other Commitments",
                                           "Culture",
                                           "Ethical & Just Activities",
                                           "Health & Well-being",
                                           "Sustainable Communities"),
                                  score=runif(13,55,100)) |>
               group_by(grouping) |>
               mutate(weight=1/n()) |>
               ungroup()
             }
             ),

  tar_target(name = Ecological,
             command = {
               ee <- data.frame(grouping=rep(c("Biodiversity",
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
                                                 "Threats to Productivity"),
                                        score=runif(11,55,100)) |>
                 # group_by(grouping) |>
                 # mutate(weight=1/n()) |>
                 mutate(weight=runif(11,1,10)) |>
                 ungroup()|>
                 mutate(angle=(cumsum(weight)-weight/2)/sum(weight)*360)
               ee

             }),

  tar_target(name = grade,
             command = {
               function(percent){
                 cutoffs=c(0, seq(60, 100, by = 10/3))
                 letters=c("F", paste0(toupper(rep(letters[4:1], each = 3)), rep(c("-","","+"),4)))
                 cut(percent,cutoffs,letters)
               }
               }),

  tar_target(name = ftabs,
             command = {
               data.frame(flower=unique(c(Ecological$grouping, Ecological$labels)), place=tolower("Scotian_Shelf"))
             }),

  tar_target(name = MYTABS,
             command = {
               mytabs <- NULL
               for (i in seq_along(MPAs$NAME_E)) {
                 df <- ftabs
                 df$place <- MPAs$NAME_E[i]
                 mytabs[[i]] <- df
               }

               do.call(rbind, mytabs)
             }),

  tar_target(name = APPTABS,
             command = {
               apptabs <- rbind(ftabs, MYTABS)
               apptabs$tab <- paste0("tab_", seq_along(1:length(apptabs$flower)))
               apptabs$link <- paste0("link_", seq_along(1:length(apptabs$flower)))
               home <- data.frame(flower="home", place="home", tab="tab_0", link="link_0")
               apptabs <- rbind(home, apptabs)
               NAME_to_tag(apptabs)
             }),

  tar_target(name = om, # FIXME: need to get the saved om (or add a real cookie)
             command = {
               OM <- dataSPA::getData(type="om", age=3000, cookie="cookie")
               OM[-(which(OM$activity_type == "Other")),]

             }),

  tar_target(name = areas,
             command = {
               c("st_Anns_Bank_MPA", "musquash_MPA", "gully_MPA", "WEBCA") # Only including Maritimes
             }),

  tar_target(name = objectives,
             command = {
               lapply(areas, function(x) data_objectives(type="site", area=x))
               }),

  tar_target(name = Objectives,
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

  tar_target(dataTable_file,
             file.path(system.file(package = "MarConsNetData"), "data", "dataTable.rda"),
             format = "file"),

  tar_target(dataTable,
             {
               load(dataTable_file)  # This will load the object in the .rda file into the environment
               dataTable <- dataTable[-which(dataTable$title == "argoProgram"),]

               dataTable  # Return the loaded object explicitly
             }),


  tar_target(name=rv_rounded_location,
             command= {

               df <- data.frame(latitude=gsdet$latitude, longitude=gsdet$longitude)
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



  tar_target(name = projectData,
             command = {
               message("Class of rv_rounded_location: ", class(rv_rounded_location))
               dataProject <- NULL
               for (i in seq_along(dataTable$id)) {
                 message("i = ", i)
                 func_name <- dataTable$get_function[i]
                 if (!(grepl("TARGET", func_name))) {
                  func <- get(func_name)  # Get the function object
                 arguments <- names(formals(func))
                 if (dataTable$package[i] == "MarConsNetData" && "taxize" %in% arguments) {
                   pd <- get_project_data(ids=dataTable$id[i], taxize=FALSE)
                 } else {
                   default_args <- formals(func)
                   default_args[is.null(default_args)] <- NA

                   # If you want to handle this gracefully:
                   filled_args <- lapply(default_args, function(arg) {
                     if (is.null(arg)) return(NA)
                     else return(arg)
                   })

                   # Use do.call to call the function with filled arguments
                   if (!(is.null(arguments))) {
                   pd <- do.call(func, filled_args)
                   } else {
                     pd <- eval(parse(text=paste0(dataTable$get_function[i], "()")))
                   }
                 }
                 } else {
                   n <- sub(".*::", "", func_name)
                   pd <- eval(parse(text=n))
                 }

                 dataProject[[i]] <- pd
               }

               names(dataProject) <- dataTable$id
               dataProject

             }),

  tar_target(name = indicators,
             command = {
               data_indicators()
               }),

  tar_target(name = Context,
             command = {
               lapply(areas, function(x) data_context(type="site", area=x))
               }),

  tar_target(name = fp,
             command = {
               read.csv("../MarConsNetAnalysis/sandbox/JH/02_meta_names/metaframework.csv") #FIXME
             }),

  tar_target(name = grades,
             command = {
               c("F", paste0(toupper(rep(letters[4:1], each = 3)), rep(c("-","","+"),4)))
               }),

  tar_target(name = flowerPalette,
             command = {
               grades <- c("A", "B", "C", "D", "F")
               palette <- colorRampPalette(brewer.pal(11,"RdBu"))(length(grades))
               names(palette) <- grades
               palette
               }),

  tar_target(name = binned_indicators,
             command = {
               bi <- read_excel(system.file("data", "indicator_binning.xlsx", package = "MarConsNetAnalysis"))
               ## Giving indicator links
               bi$tab<- paste0("tab_", length(APPTABS$flower)+(1:length(bi$indicators)))
               bi$link <- paste0("link_", length(APPTABS$flower)+(1:length(bi$indicators)))
               bi
             }),

  tar_target(name = odf,
             command = {
               O <- data.frame(
                 objectives = c(0, unlist(Objectives, use.names = FALSE), N_Objectives)
               )
               O$flower_plot <- 0
               O$area <- 0
               get_first_four_words <- function(texts) {
                 lapply(texts, function(text) {
                   words <- strsplit(text, " ")[[1]] # Split each string into words
                   first_four_words <- paste(words[1:min(4, length(words))], collapse = " ") # Concatenate the first four words (or fewer if there are not enough words)
                   return(first_four_words)
                 })
               }

               for (i in seq_along(O$objectives)) {
                 ob <- gsub("[-\n]", "", O$objectives[i])
                 if (!(O$objectives[i] == "0")) {
                   keep <- which(tolower(get_first_four_words(fp$label_Objective)) == tolower(get_first_four_words(ob)[[1]]))
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
                     k1 <- which(gsub("\\.","", APPTABS$place) == tolower(sub("_CO$", "", O$area[i]))) # SAME AREA AND FLOWER
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
                     k <- which(binned_indicators$indicators == trimws(gsub("-", "", gsub("\n", "", O$objectives[i]))), "right")
                     O$tab[i] <- binned_indicators$tab[k]
                     O$link[i] <- binned_indicators$link[k]

                   }
                 } else {
                   O$tab[i] <- "tab_0"
                   O$link[i] <- "link_0"
                 }

               }
               O

             }),

  tar_target(name = calc_letter_grade,
             command = {
               cg <- function(scores) {
                 # Vectorized assignment of grades based on fixed criteria
                 sapply(scores, function(score) {
                   if (is.na(score)) {
                     return(NA)  # Return NA if the input is NA
                   } else if (score >= 100) {
                     return("A")
                   } else if (score >= 80) {
                     return("B")
                   } else if (score >= 60) {
                     return("C")
                   } else if (score >= 40) {
                     return("D")
                   } else {
                     return("F")
                   }
                 })
               }
               cg
             }),

 tar_target(name = species,
            command = {
              species <- c("COD(ATLANTIC)", "HADDOCK")

            }),


  tar_target(name = ABUNDANCE_RV,
             command = {
               rv <- vector(mode="list", length(areas))
               get_data('rv', data.dir="C:/Users/HarbinJ/Documents/data/rv") #FIXME
               for (i in seq_along(areas)) {
                 for (j in seq_along(species)) { # This has up to
                   message("i = ", i, " and j = ", j)
                   rv[[i]][[j]] <- ind_rv_abundance(species = species[j], area=areas[i], MPAs=MPAs, data=GSSPECIES)
                 }
               }
               names(rv) <- areas
               for (i in seq_along(rv)) {
                 names(rv[[i]]) <- species
               }

               rv

             }),

  tar_target(name = plotindy,
             command = {
               c(binned_indicators$indicators)
             }),

 tar_target(name=whale_sighting,
            command= {
              ws <- project_whale_sighting()
              ws
            }),

 tar_target(name = indicator_to_plot,
            command = {

              DF <- list(bloom_df=bloom_df, all_haddock=all_haddock, gsdet=gsdet, zooplankton=zooplankton, surface_height=surface_height,Discrete_Occupations_Sections=azmpdata::Discrete_Occupations_Sections,whale_sighting=whale_sighting)

              ITP <- analysis(DF=DF, bi=binned_indicators)

              ITP
            }

              ),

 tar_target(name = mapData, # JAIM HERE
            command = {

              # BANDAID FIX (ISSUE 54 APP)

              mpa <- eval(MPAs)
              GS <- eval(gsdet)
              zoo <- eval(zooplankton)
              ah <- eval(all_haddock)
              bd <- eval(bloom_df)
              sh <- eval(surface_height)
              ws <- eval(whale_sighting)

              maps <- indicator_to_plot$plot[which(!(indicator_to_plot$plot == 0))]

              map <- NULL
              for (i in seq_along(maps)) {
                message(i)
                m <- maps[i]
                if (grepl("MPAs", m)) {
                  m <- gsub("MPAs", "mpa", m)
                }
                if (grepl("gsdet", m)) {
                  m <- gsub("gsdet", "GS", m)
                }
                if (grepl("df=zooplankton", m)) {
                  m <- gsub("df=zooplankton","df=zoo", m)
                }
                if (grepl("df=all_haddock", m)) {
                  m <- gsub("df=all_haddock","df=ah", m)
                }
                if (grepl("df=bloom_df", m)) {
                  m <- gsub("df=bloom_df","df=bd", m)
                }
                if (grepl("df=surface_height", m)) {
                  m <- gsub("df=sh","df=surface_height", m)
                }

                if (grepl("df=whale_sighting", m)) {
                  m <- gsub("df=ws","df=whale_sighting", m)
                }

                m <- paste0(substr(m, 1, nchar(m) - 1), ", map=TRUE)")
                map[[i]] <- eval(parse(text=m))
              }
              names(map) <- maps

              for (i in seq_along(map)) {
                if (length(map[[i]]$latitude) == 0 & length(map[[i]]$geom) == 0) {
                  message(i)
                  # There is nothing inside or outside (e.g. sea surface)
                  string <- str_match(maps[i], "df=([a-zA-Z_]+)")[, 2]
                  map[[i]]$latitude <- eval(parse(text=paste0(string, "$latitude")))
                  map[[i]]$longitude <- eval(parse(text=paste0(string, "$longitude")))
                  map[[i]]$notIncluded <- TRUE
                }
              }

              map
            }

 ),





 ####### FLOWER PLOT ######

 ##### Areas #####
 tar_target(name=bioregion,
            data_bioregion(),
            format = "qs",
            packages = c("sf","arcpullr")),

 # get the Protected and Conserved areas in the bioregion
 tar_target(name=consAreas,
            data_CPCAD_areas(bioregion,zones=TRUE),
            format = "qs",
            packages = c("sf","arcpullr")),

 tar_target(name = all_areas,
            bind_rows(bioregion,
                      consAreas),
            format = "qs",
            packages = c("dplyr","sf","arcpullr")),

 ##### Indicators #####

  tar_target(ind_placeholder_1_df,ind_placeholder(areas = all_areas)),
  tar_target(ind_placeholder_2_df,ind_placeholder(areas = all_areas)),
  tar_target(ind_placeholder_3_df,ind_placeholder(areas = all_areas)),
  tar_target(ind_placeholder_4_df,ind_placeholder(areas = all_areas)),
  tar_target(ind_placeholder_5_df,ind_placeholder(areas = all_areas)),
  tar_target(ind_placeholder_6_df,ind_placeholder(areas = all_areas)),
  tar_target(ind_placeholder_7_df,ind_placeholder(areas = all_areas)),
  tar_target(ind_placeholder_8_df,ind_placeholder(areas = all_areas)),
  tar_target(ind_placeholder_9_df,ind_placeholder(areas = all_areas)),
  tar_target(ind_placeholder_10_df,ind_placeholder(areas = all_areas)),
  tar_target(ind_placeholder_11_df,ind_placeholder(areas = all_areas)),

 ##### Indicator Bins #####
 tar_target(bin_biodiversity_FunctionalDiversity_df,
            aggregate_groups("bin",
                             "Functional Diversity",
                             weights=1,
                             ind_placeholder_1_df
            )),
 tar_target(bin_biodiversity_GeneticDiversity_df,
            aggregate_groups("bin",
                             "Genetic Diversity",
                             weights=1,
                             ind_placeholder_1_df,
                             ind_placeholder_2_df
            )),
 tar_target(bin_biodiversity_SpeciesDiversity_df,
            aggregate_groups("bin",
                             "Species Diversity",
                             weights=1,
                             ind_placeholder_3_df
            )),
 tar_target(bin_habitat_Connectivity_df,
            aggregate_groups("bin",
                             "Connectivity",
                             weights=1,
                             ind_placeholder_4_df
            )),
 tar_target(bin_habitat_EnvironmentalRepresentativity_df,
            aggregate_groups("bin",
                             "Environmental Representativity",
                             weights=1,
                             ind_placeholder_5_df
            )),
 tar_target(bin_habitat_KeyFishHabitat_df,
            aggregate_groups("bin",
                             "Key Fish Habitat",
                             weights=1,
                             ind_placeholder_6_df
            )),
 tar_target(bin_habitat_ThreatstoHabitat_df,
            aggregate_groups("bin",
                             "Threats to Habitat",
                             weights=1,
                             ind_placeholder_7_df
            )),
 tar_target(bin_habitat_Uniqueness_df,
            aggregate_groups("bin",
                             "Uniqueness",
                             weights=1,
                             ind_placeholder_8_df
            )),
 tar_target(bin_productivity_BiomassMetrics_df,
            aggregate_groups("bin",
                             "Biomass Metrics",
                             weights=1,
                             ind_placeholder_9_df
            )),
 tar_target(bin_productivity_StructureandFunction_df,
            aggregate_groups("bin",
                             "Structure and Function",
                             weights=1,
                             ind_placeholder_10_df
            )),
 tar_target(bin_productivity_ThreatstoProductivity_df,
            aggregate_groups("bin",
                             "Threats to Productivity",
                             weights=1,
                             ind_placeholder_11_df
            )),


 ##### Ecological Objectives #####
 tar_target(ecol_obj_biodiversity_df,
            aggregate_groups("objective",
                             "Biodiversity",
                             weights = NA,
                             bin_biodiversity_FunctionalDiversity_df,
                             bin_biodiversity_GeneticDiversity_df,
                             bin_biodiversity_SpeciesDiversity_df)),
 tar_target(ecol_obj_habitat_df,
            aggregate_groups("objective",
                             "Habitat",
                             weights = NA,
                             bin_habitat_Connectivity_df,
                             bin_habitat_EnvironmentalRepresentativity_df,
                             bin_habitat_KeyFishHabitat_df,
                             bin_habitat_ThreatstoHabitat_df,
                             bin_habitat_Uniqueness_df)),
 tar_target(ecol_obj_productivity_df,
            aggregate_groups("objective",
                             "Productivity",
                             weights = NA,
                             bin_productivity_BiomassMetrics_df,
                             bin_productivity_StructureandFunction_df,
                             bin_productivity_ThreatstoProductivity_df)),

 tar_target(gsdet,
            command = {
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

              GDD <- GSDET
              GDD

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
              ah
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
              df

            }),




 tar_target(surface_height,
            command={
              df <- azmpdata::Derived_Monthly_Stations
              # Add latitude and longitude
              df$latitude <- 0
              df$longitude <- 0
              type <- NULL
              df$latitude[which(df$station == "Halifax")] <- 43.5475
              df$longitude[which(df$station == "Halifax")] <- 63.5714

              df$latitude[which(df$station == "Yarmouth")] <- 43.8377
              df$longitude[which(df$station == "Yarmouth")] <- 66.1150

              df$latitude[which(df$station == "North Sydney")] <- 46.2051
              df$longitude[which(df$station == "North Sydney")] <- 60.2563
              df

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


              df <- azmpdata::RemoteSensing_Annual_Broadscale
              df <- df[which(df$area == "CSS_remote_sensing"),]
              df$geom <- rep(polygon_sf)
              df
            }
            ),

 ##### Pillar #####


 tar_target(name = pillar_ecol_df,
            command = {

              Ecological <- data.frame(grouping=rep(c("Biodiversity",
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
                                        "Threats to Productivity"),
                               score=runif(11,55,100))

              # REMOVING HYPOTHETICAL DATA
              area_name <- binned_indicators$mpa_name

              ind_name <- indicator_to_plot$indicators

              ind_status <- NULL
              for (i in seq_along(indicator_to_plot$status_grade)) {
                if (indicator_to_plot$status_grade[i] == "A") {
                  ind_status[[i]] <- 5
                } else if (indicator_to_plot$status_grade[i] == "B") {
                  ind_status[[i]] <- 4
                } else if (indicator_to_plot$status_grade[i] == "C") {
                  ind_status[[i]] <- 3
                } else if (indicator_to_plot$status_grade[i] == "D") {
                  ind_status[[i]] <- 2
                } else if (indicator_to_plot$status_grade[i] == "F") {
                  ind_status[[i]] <- 1
                } else {
                  ind_status[[i]] <- 0
                }
              }
              ind_status <- unlist(ind_status)


              extract_first_number <- function(sentence) {
                match <- regexpr("-?\\d+\\.?\\d*", sentence)
                if (match != -1) {
                  as.numeric(regmatches(sentence, match))
                } else {
                  NA
                }
              }

              ind_trend <- NULL
              for (i in seq_along(indicator_to_plot$trend)) {
                if (grepl("BLANK", indicator_to_plot$trend[i])) {
                  ind_trend[i] <- NA
                } else {
                  ind_trend[i] <- extract_first_number(indicator_to_plot$trend[i])

                }
              }
              ind_trend <- unlist(lapply(indicator_to_plot$trend, function(x) extract_first_number(x)))

              ind_projects <- indicator_to_plot$project

              ind_rawdata_type <- "Expert opinion"

              ind_certainty <- "certain"

              bin <- binned_indicators$indicator_bin #FIXME

              weight <- NULL

              Ecological$labels[which(Ecological$labels == "Environmental Representativity")] <- "Environmental (Representativity)"

              objectives <- list()  # Initialize as a list
              for (i in seq_along(indicator_to_plot$indicator_bin)) {
                II <- indicator_to_plot$indicator_bin[i]
                sp <- trimws(strsplit(II, ";")[[1]], "both")
                weight[i] <- 1/length(sp)
                objectives[[i]] <- vector("list", length(sp))  # Initialize objectives[[i]] as a list with the correct length
                for (j in seq_along(sp)) {
                  message("i =", i, " j = ", j)
                  objectives[[i]][[j]] <- Ecological$grouping[which(tolower(Ecological$labels) == tolower(sp[j]))]
                }
              }

              objectives <- lapply(objectives, function(x) unlist(x))
              objectives <- lapply(objectives, unique)
              objective <- unlist(lapply(objectives, function(x) paste0(x, collapse=" ; ")))


              pillar <- "Ecological"



              df <- data.frame(area_name=area_name, ind_name=ind_name, ind_status=ind_status, ind_trend=ind_trend, ind_projects=ind_projects,
                               ind_rawdata_type=ind_rawdata_type, ind_certainty=ind_certainty, bin=bin, weight=weight, objective=objective,
                               pillar=pillar)
              df$bin <- trimws(toupper(df$bin), "both")
              df$objective <- trimws(toupper(df$objective), "both")


              df <- df %>%
                mutate(bin = strsplit(as.character(bin), "; ")) %>%
                unnest(bin)

              for (i in seq_along(df$objective)) {
                message(i)
                df$objective[i] <- Ecological$grouping[which(tolower(Ecological$labels) == trimws(tolower(df$bin[i]), "both"))]
              }

              target_bin_weight <- 1

              for (j in unique(df$area_name)) {

              for (i in seq_along(Ecological$labels)) {
                keep <-which(tolower(df$bin) == tolower(Ecological$labels[i]) & df$area_name == j)
                bl <- target_bin_weight/sum(df$weight[keep])
                df$weight[keep] <- df$weight[keep]*bl
              }
              }

              # target_weight <- sum(df$weight)/length(Ecological$labels)
              #
              #
              # for (i in seq_along(Ecological$labels)) {
              #   keep <-which(tolower(df$bin) == tolower(Ecological$labels[i]))
              #   bl <- target_weight/sum(df$weight[keep])
              #   df$weight[keep] <- df$weight[keep]*bl
              # }

              ### calculate network status
              df <- df |>
                mutate(ind_status = if_else(ind_status < 0.00000001,
                                            NA,
                                            ind_status)) |>
                group_by(objective, bin, area_name) |>
                reframe(ind_name = unique(area_name),
                        area_name = "Scotian Shelf",
                        ind_status = weighted.mean(ind_status,weight,na.rm = TRUE),
                        ind_trend = weighted.mean(ind_trend,weight,na.rm = TRUE),
                        ind_projects = paste(ind_projects, collapse = "; "),
                        ind_rawdata_type = paste(ind_rawdata_type, collapse = "; "),
                        ind_certainty = paste(ind_certainty, collapse = "; ")
                        )|>
                group_by(bin) |>
                mutate(weight=target_bin_weight/n()) |>
                ungroup() |>
                bind_rows(df)


              df <-  df %>%
                arrange(objective, bin)

              df$ind_status <- df$ind_status*20

              df

              # pdf <- aggregate_groups("pillar",
              #                          "Ecological",
              #                          weights = NA,
              #                          ecol_obj_biodiversity_df,
              #                          ecol_obj_habitat_df,
              #                          ecol_obj_productivity_df)
              #
              # pdf <- pdf %>%
              #   mutate(angle=(cumsum(weight)-weight/2)/sum(weight)*360)
              # pdf

            })



) |>
  unlist(recursive = FALSE)
