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
               "rnaturalearth","DBI","duckdb", "rmarkdown", "shiny", "measurements","mregions2","patchwork", "units", "oceglider", "RCurl"),
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

  tar_target(name=climate,
             command = {
               x <- list.files(file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","data"), full.names = TRUE)[which(grepl("climate_change",list.files(file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","data"), full.names = TRUE) ))]
               read_excel(x)
             }),

  tar_target(Outside, #FIXME: this is only for WEBCA at the moment
             st_transform(read_sf(system.file("data","WEBCA_10k_85k.shp", package = "MarConsNetAnalysis"))$geometry, crs=4326)
  ),

  tar_target(name = ebsa,
             command = {
               ebsa <- get_spatial_layer("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/ecologically_and_biologically_significant_areas/MapServer/1") |>
                 st_make_valid()|>
                 st_filter(regions)
             }),

  tar_target(name = sar_ch,
             command = {
               # ESRI REST cannot handle the query, downloading the FGDB manually instead from:
               # https://open.canada.ca/data/en/dataset/db177a8c-5d7d-49eb-8290-31e6a45d786c

               # Create a temporary directory for the download
               temp_dir <- tempdir()
               zip_file <- file.path(temp_dir, "CriticalHabitat_HabitatEssentiel.zip")
               unzip_dir <- file.path(temp_dir, "extracted_fgdb")

               # Download the file
               download.file(
                 "https://api-proxy.edh-cde.dfo-mpo.gc.ca/catalogue/records/db177a8c-5d7d-49eb-8290-31e6a45d786c/attachments/CriticalHabitat_HabitatEssentiel.zip",
                 destfile = zip_file,
                 mode = "wb"
               )

               # Create directory for extraction
               dir.create(unzip_dir, showWarnings = FALSE)

               # Unzip the file
               unzip(zip_file, exdir = unzip_dir)

               sar_ch <- st_read(file.path(unzip_dir, "CriticalHabitat_EDH_2025.gdb"),
                                 layer = "DFO_SARA_CH_EDH")|>
                 st_cast("MULTIPOLYGON")|>
                 st_make_valid() |>
                 st_transform(st_crs(regions)) |>
                 st_filter(regions)
             }),

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
               apptabs <- expand.grid(flower=unique(c(Ecological$grouping, Ecological$labels)),
                                    place=unique(c(MPAs$region,MPAs$NAME_E))) |>
                 mutate(tab = paste0("tab_", 1:length(flower)),
                        link = paste0("link_", 1:length(flower)))
               # mytabs <- NULL
               # for (i in seq_along(MPAs$NAME_E)) {
               #   df <- ftabs
               #   df$place <- MPAs$NAME_E[i]
               #   mytabs[[i]] <- df
               # }

               # MYTABS <- do.call(rbind, mytabs)
               # apptabs <- rbind(ftabs, MYTABS)
               # apptabs$tab <- paste0("tab_", seq_along(1:length(apptabs$flower)))
               # apptabs$link <- paste0("link_", seq_along(1:length(apptabs$flower)))
               home <- data.frame(flower="home", place="home", tab="tab_0", link="link_0")
               rbind(home, apptabs)
             }),

  tar_target(name = om, # FIXME: need use a real cookie or otherwise update the data
             command = {
               OM <- dataSPA::getData(type="om", age=3000, cookie="hi",
                                      path = file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","data"))

               OM[-(which(OM$activity_type == "Other")),]

             }),


  tar_target(name = collaborations, # FIXME: need use a real cookie or otherwise update the data
             command = {
              cookie <- "cookie"
               col <- dataSPA::getData(type='collaboration', cookie=cookie)
             }),

  tar_target(name = deliverables, # FIXME: need use a real cookie or otherwise update the data
             command = {
               cookie <- "cookie"
               debug <- 0
               links <- c("https://dmapps/api/ppt/project-years/", "https://dmapps/api/ppt/activities-full/")

               API_DATA <- NULL

               for (i in seq_along(links)) {
                 req <- httr2::request(links[i])

                 # Add custom headers
                 req <- req |> httr2::req_headers("Cookie" = cookie)
                 req <- req |> httr2::req_headers("Accept" = "application/json")

                 # Automatically retry if the request fails
                 req <- req |> httr2::req_retry(max_tries = 5)

                 # Get the requested data by querying the API
                 resp <- try(httr2::req_perform(req), silent=TRUE)

                 page_data <- httr2::resp_body_json(resp)


                 api_data <- page_data$results

                 # Get the information about the next page in the API results
                 next_page <- page_data$`next`
                 cat(paste0(next_page, '\n'))

                 cat(paste0('Number of API records = ', length(api_data), '\n'))

                 # Check if the next page is not null (end of pages) before extract the data from
                 # next page.
                 while (!is.null(next_page)) {
                   # Modifying API Call
                   req <- httr2::request(next_page)
                   # Add custom headers
                   req <- req |> httr2::req_headers("Cookie" = cookie)
                   req <- req |> httr2::req_headers("Accept" = "application/json")
                   # Automatically retry if the request fails
                   req <- req |> httr2::req_retry(max_tries = 5)
                   # Get the requested data by querying the API
                   resp <- httr2::req_perform(req)
                   # Read the returned data as a JSON file
                   page_data <- httr2::resp_body_json(resp)
                   # Add current page data to full list
                   api_data <- c(api_data, page_data$results)
                   cat(paste0('Number of API records = ', length(api_data), '\n'))

                   # Get the information about the next page in the API results
                   next_page <- page_data$`next`
                   if(debug>0){
                     if(exists("debug_page")){
                       debug_page <- debug_page-1
                     } else {
                       debug_page <- debug-1
                     }

                     if(debug_page==1) {
                       debug_page <- debug
                       next_page <- NULL
                     }
                   }
                   cat(paste0(next_page, '\n'))
                 }

                 API_DATA[[i]] <- api_data

               }

               names(API_DATA) <- links

               project_year_id <- unlist(lapply(api_data, function(x) x$project_year_id))
               type_display <- unlist(lapply(api_data, function(x) x$type_display))
               classification_display <- lapply(api_data, function(x) {
                 if (is.null(x$classification_display)) NA else x$classification_display
               })
               year <- unlist(lapply(api_data, function(x) x$project_year_obj$display_name))

               description <- lapply(api_data, function(x) {
                 if (is.null(x$description)) NA else x$description
               })

               df <- data.frame("project_year_id"=project_year_id,
                                "type_display"=type_display,
                                "classification_display"=unlist(classification_display),
                                "year"=year,
                                "description"=unlist(description))


               p_ids <- lapply(API_DATA[[which(names(API_DATA)== "https://dmapps/api/ppt/project-years/")]], function(x) x$project$id)

               my_list <- vector("list", length = length(unique(unlist(p_ids))))
               names(my_list) <- unique(unlist(p_ids))

               for (i in seq_along(names(my_list))) {
                 keep <- which(p_ids == names(my_list)[i][[1]])
                 my_list[[i]] <- unlist(lapply(API_DATA[[which(names(API_DATA) == "https://dmapps/api/ppt/project-years/")]][[which(p_ids == names(my_list)[i])[1]]]$project$year, function(x) x$id))
               }

               # Assigning year and project ID

               assign_id <- function(df) {
                 # 1. Assign project_id by matching to my_list
                 df$project_id <- sapply(df$project_year_id, function(id) {
                   match_idx <- which(sapply(my_list, function(x) id %in% x))
                   if (length(match_idx) > 0) as.numeric(names(my_list)[match_idx[1]]) else NA
                 })
                 return(df)
               }


               DD <- assign_id(df)

               DD <- DD[which(DD$type_display == "Deliverable"),]

               DD <- DD[which(!grepl("Agreement", DD$classification_display, ignore.case=TRUE)),]
               DD <- DD[, c("project_id", "classification_display", "description", "year")]
               DD <- DD[-which(is.na(DD$classification_display)),]

               deliverables <- DD
             }),

  tar_target(name = csas,
             command = {
               cookie <- "cookie" # FIXME: need real cookie to update

               links <- c("https://dmapps/api/csas/meetings/")

               req <- httr2::request(links)

               # Add custom headers
               req <- req |> httr2::req_headers("Cookie" = cookie)
               req <- req |> httr2::req_headers("Accept" = "application/json")

               # Automatically retry if the request fails
               req <- req |> httr2::req_retry(max_tries = 5)

               # Get the requested data by querying the API
               resp <- try(httr2::req_perform(req), silent = TRUE)

               page_data <- httr2::resp_body_json(resp)


               api_data <- page_data$results

               # Get the information about the next page in the API results
               next_page <- page_data$`next`
               cat(paste0(next_page, '\n'))

               cat(paste0('Number of API records = ', length(api_data), '\n'))

               # Check if the next page is not null (end of pages) before extract the data from
               # next page.
               while (!is.null(next_page)) {
                 # Modifying API Call
                 req <- httr2::request(next_page)
                 # Add custom headers
                 req <- req |> httr2::req_headers("Cookie" = cookie)
                 req <- req |> httr2::req_headers("Accept" = "application/json")
                 # Automatically retry if the request fails
                 req <- req |> httr2::req_retry(max_tries = 5)
                 # Get the requested data by querying the API
                 resp <- httr2::req_perform(req)
                 # Read the returned data as a JSON file
                 page_data <- httr2::resp_body_json(resp)
                 # Add current page data to full list
                 api_data <- c(api_data, page_data$results)
                 cat(paste0('Number of API records = ', length(api_data), '\n'))

                 # Get the information about the next page in the API results
                 next_page <- page_data$`next`

                 cat(paste0(next_page, '\n'))
               }

               API_DATA <- api_data

               title <- unlist(lapply(API_DATA, function(x) x$display))
               display_dates <-  unlist(lapply(API_DATA, function(x) x$display_dates))
               display_dates <- gsub('&rarr;', "-", display_dates)

               df <- data.frame('title'=title,
                                'date'=display_dates
                                )
             }),



  tar_target(name = render_reports,
             command = {
               MPAs
               pillar_ecol_df
               Context
               flowerPalette
               odf
               N_Objectives
               Objectives_processed
               MPA_report_card
               collaborations

               mpas <- MPAs$NAME_E
               rmd_file <- system.file("data", "report.Rmd", package = "MarConsNetApp")
               output_dir <- file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","data", "reports")

               for (i in seq_along(mpas)) {
                 message(i)
                 state <- list()
                 params <- list()
                 input <- list()
                 state$mpas <- mpas[i]
                 params$mpas <- mpas[i]
                 input$mpas <- mpas[i]
                 output_file <- file.path(paste0(output_dir,"/", make.names(paste0(names=mpas[i], ".html"))))
                 render(input=rmd_file, output_file = output_file, output_format = "html_document", params = params, envir = new.env())
               }
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

               palette <- flowerPalette
               names(palette) <- c(100,75,50,25,0)
               #grades <- c(100, 50, 0)
               #palette <- c("#2C7BB6", "#FFFFBF","#D7191C")
               #names(palette) <- grades
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
                     k <- which(data_pillar_ecol_df$indicators == trimws(gsub("-", "", gsub("\n", "", O$objectives[i]))), "right")
                     O$tab[i] <- data_pillar_ecol_df$tab[k]
                     O$link[i] <- data_pillar_ecol_df$link[k]

                   }
                 } else {
                   O$tab[i] <- "tab_0"
                   O$link[i] <- "link_0"
                 }

               }
               O

             }),





  ############ data loading ############
  tar_target(name = data_MAR_cumulative_impacts,
             {
               temp_zip <- tempfile(fileext = ".zip")
               download.file("https://api-proxy.edh-cde.dfo-mpo.gc.ca/catalogue/records/37b59b8b-1c1c-4869-802f-c09571cc984b/attachments/Cumul_Impact_Maritimes.zip", temp_zip, mode = "wb")

               # Create temporary directory
               temp_dir <- tempfile()
               dir.create(temp_dir)

               # Extract ZIP contents
               unzip(temp_zip, exdir = temp_dir)

               # List files in the directory
               raster_files <- list.files(temp_dir, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)

               # Load the rasters
               rasters <- lapply(raster_files,stars::read_stars)

               # You can then work with the rasters
               names(rasters) <- basename(raster_files)
               rasters
               }
             ),

  tar_target(name = rawdata_obis_parquet_files,
             command = {
               # see https://obis.org/data/access/ for the latest version of their full "periodic" export
               url = "https://obis-open-data.s3.amazonaws.com/snapshots/obis_20250318_parquet.zip"
               dest_dir = file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","data","obis_data")

               # Set zip file path
               zip_path <- file.path(dest_dir, "obis_data.zip")

               # Download the file if it doesn't exist
               if (!file.exists(zip_path)) {
                 message("Downloading OBIS parquet dataset...")
                 utils::download.file(url, zip_path, mode = "wb")
               } else {
                 message("Using previously downloaded zip file.")
               }

               # Extract if needed
               extract_dir <- file.path(dest_dir, "extracted")
               if (!dir.exists(extract_dir) || length(list.files(extract_dir)) == 0) {
                 message("Extracting parquet files...")
                 utils::unzip(zip_path, exdir = extract_dir)
               } else {
                 message("Using previously extracted files.")
               }

               # Return the path to the extracted files
               return(extract_dir)

             },
             format = "file",

             cue = tar_cue("never") # Prevents re-downloading unless the file is deleted
  ),

  tar_target(name = rawdata_obis_by_region,
             command = {
               bbox <- st_bbox(regions) |> as.numeric()
               mid_x <- (bbox[1] + bbox[3]) / 2
               mid_y <- (bbox[2] + bbox[4]) / 2

               # Create coordinates for the 4 quadrants
               coords <- list(
                 nw = c(xmin = bbox[1], ymin = mid_y, xmax = mid_x, ymax = bbox[4]),
                 ne = c(xmin = mid_x, ymin = mid_y, xmax = bbox[3], ymax = bbox[4]),
                 sw = c(xmin = bbox[1], ymin = bbox[2], xmax = mid_x, ymax = mid_y),
                 se = c(xmin = mid_x, ymin = bbox[2], xmax = bbox[3], ymax = mid_y)
               )

               # Convert all to WKT in one step
               obis <- lapply(coords, function(subcoords) {
                 # browser()
                 class(subcoords) <- "bbox"
                 regionswkt <- subcoords |>
                   st_as_sfc(crs = st_crs(regions)) |>
                   st_as_text()

                 con <- dbConnect(duckdb::duckdb())
                 dbExecute(con, "SET memory_limit='16GB'")

                 message("extracting OBIS data for region: ", regions$NAME_E)

                 occ <- dbGetQuery(con,
                                    paste0("
    install spatial;
    load spatial;
    select * from read_parquet('",gsub("\\\\","/",rawdata_obis_parquet_files),"/occurrence/*.parquet')
    where ST_Intersects(geometry, ST_GeomFromText('",regionswkt,"'))
"))|>
                   st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
                            crs = 4326,
                            remove = FALSE) |>
                   st_filter(regions) |>
                   st_join(MPAs |> select(NAME_E), left=TRUE) |>
                   st_join(regions |> select(NAME_E), left=TRUE)
                 dbDisconnect(con, shutdown = TRUE)
                 occ
               }) |>
                 bind_rows()



                 # regionswkt <- regions |>
                 #   st_bbox() |>
                 #   st_as_sfc() |>
                 #   st_as_text()


               },
             pattern = map(regions),
             iteration = "list"),

  tar_target(
    name = data_obis,
    command = bind_rows(rawdata_obis_by_region)
  ),

  tar_target(name = data_MAR_biofouling_AIS,
             command = {
               # see https://open.canada.ca/data/en/dataset/8d87f574-0661-40a0-822f-e9eabc35780d
               # Create a temporary directory for the download
               temp_dir <- tempdir()
               zip_file <- file.path(temp_dir, "Downloads2023.gdb.zip")
               unzip_dir <- file.path(temp_dir, "extracted_fgdb")

               # Download the file
               download.file(
                 "https://api-proxy.edh-cde.dfo-mpo.gc.ca/catalogue/records/8d87f574-0661-40a0-822f-e9eabc35780d/attachments/Downloads2023.gdb.zip",
                 destfile = zip_file,
                 mode = "wb"
               )

               # Create directory for extraction
               dir.create(unzip_dir, showWarnings = FALSE)

               # Unzip the file
               unzip(zip_file, exdir = unzip_dir)
               list.files(unzip_dir, full.names = TRUE)

               layers <- st_layers(file.path(unzip_dir, "Downloads2023.gdb"))

               ais <- lapply(layers$name, function(x){
                 st_read(file.path(unzip_dir, "Downloads2023.gdb"),
                         layer = x)
               })
               names(ais) <- layers$name
               ais
             }
  ),


  tar_target(name = data_WORMS_species_distributions,
             command = {
               distributions <- map(unique(data_obis$aphiaid), function(aphiaID) {
                 url <- sprintf("https://www.marinespecies.org/rest/AphiaDistributionsByAphiaID/%d", aphiaID)
                 tryCatch({
                   jsonlite::fromJSON(url) |>
                     mutate(aphiaID = aphiaID)
                 }, error = function(e) {
                   NULL
                 })
               }) |>
                 bind_rows()
             }),

  tar_target(name = data_WORMS_species_distributions_polygons,
             {
               polygons <- data_WORMS_species_distributions |>
                 dplyr::select(locationID) |>
                 unique() |>
                 mutate(mrID = as.numeric(basename(locationID)),
                        geometry = map(mrID,function(id) {
                          print(id)
                          tryCatch({
                            browser()
                            gaz_geometry(id)
                          }, error = function(e) {
                            print("no polygon")
                            NA})
                        }) ) |>
                 filter(!is.na(geometry)) |>
                 rowwise() |>
                 mutate(geometry = st_sfc(st_combine(geometry))) |>
                 st_as_sf() |>
                 select(-mrID)
               polygons

               # data_WORMS_species_distributions |>
               #   left_join(polygons, by = "locationID")


             }
  ),

  tar_target(name = "data_musquash_benthic_infauna",
             command = {
               infauna <- arcpullr::get_spatial_layer("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/musquash_benthic_infauna/MapServer/1")
             }),

  tar_target(name = "data_musquash_eutrophication",
             command = {
               file <- paste0(file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","data"), "/ECW_MEM_COMPLETE Water Quality Data 2014 to 2024_2025.06.06_v1.xlsx")
               sheets <-  excel_sheets(file)
               final <- list()
               for (i in seq_along(sheets)) {
                 d <- read_excel(file, sheet=sheets[i], skip=19) # The Eutrophication starts at line 21
                 if (!(length(d) == 0)) {
                 d$year <- sheets[i]
                 } else {
                   d <- 1
                 }
                 final[[i]] <- d
               }

               names(final) <- sheets

               combined <- bind_rows(final[!sapply(final, function(x) identical(x, 1))])
               combined$year <- as.numeric(sub("-.*", "", combined$year))
               combined


             }),

  tar_target(data_musquash_coliform,
             command= {

               #lat <- c('45 11 863', '45 11 693', '45 11 686', '45 11 711', '45 11 195', '45 10 776','45 10 523',
               #             '45 10 136','45 9 300', '45 8 982','45 9 768','45 10 010') # From excel spread sheet
               #lon <- c("-66 19 400","-66 19 165","-66 16 743","-66 16 217", "-66 15 428","-66 14 191",
                # "-66 13 756", "-66 13 517", "-66 13 804", "-66 15 514", "-66 16 247","-66 15 275")


               #lat <- as.numeric(conv_unit(lat, from = "deg_min_sec", to = "dec_deg"))
               #lon <- as.numeric(conv_unit(lon, from = "deg_min_sec", to = "dec_deg"))

               #these were converted using the internet because everything I tried online did not work.

               lat <- c(
                 45.19772, 45.19488, 45.19477, 45.19518,
                 45.18658, 45.17960, 45.17538, 45.16893,
                 45.15500, 45.14970, 45.16280, 45.16683
               )

               lon <- c(-66.32333, -66.31942, -66.27905, -66.27028, -66.25713, -66.23652,
               -66.22927, -66.22528, -66.23007, -66.25857, -66.27078, -66.25458)

               file <- paste0(file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","data"), "/ECW_MEM_COMPLETE Water Quality Data 2014 to 2024_2025.06.06_v1.xlsx")
               sheets <-  excel_sheets(file)
               final <- list()
               coliform_data <- list()
               for (i in seq_along(sheets)) {
                 message(i)
                 d <- read_excel(file, sheet=sheets[i])

                 # Get dates
                 times <- as.character(unlist(d[which(grepl("SPRING", d))+1,]))
                 times <- as.POSIXct(sub(".* - ", "", times[-(which(is.na(times) | times == "Station"))]), format = "%Y/%m/%d")

                 d <- d[-c(1:(which(grepl("MPN",d))[1]-1)), ] # Find MPN
                 if (any(which(apply(d, 1, function(x) all(is.na(x)))))) {
                 d <- d[c(1:which(apply(d, 1, function(x) all(is.na(x))))[1]-1),]# Find first NA row
                 }

                 names <- as.character(unlist(d[1, ]))
                 names[is.na(names)] <- "X"

                 # Remove side latitude/longitude
                 if (any(names == "Site Latitude")) {
                 d <- d[, 1:(which(names == "Site Latitude") - 1)]
                 }
                 name_counts <- ave(names, names, FUN = seq_along)
                 unique_names <- paste0(names, "_", name_counts)

                 names(d) <- unique_names

                 Stations <- d$X_2
                 d <- d[-c(1:2)]

                 colifor <- list()

                 for (j in seq_along(times)) {
                   if (!(is.na(times[j]))) {
                   colifor[[j]] <- d[,c(which(grepl(j, names(d))))]
                   colifor[[j]]$Time <- times[j]
                   colifor[[j]]$Station <- Stations
                   colifor[[j]] <- colifor[[j]][-1,]
                   names(colifor[[j]]) <- gsub("_[0-9]+$", "", names(colifor[[j]]))
                   colifor[[j]]$MPN[which(grepl("<", colifor[[j]]$MPN))] <- 0
                   colifor[[j]]$MPN <- as.numeric(colifor[[j]]$MPN)
                   colifor[[j]]$Temperature <- as.numeric(colifor[[j]]$Temperature)
                   colifor[[j]]$Salinity <- as.numeric(colifor[[j]]$Salinity)
                   colifor[[j]]$latitude <- lat[as.numeric(colifor[[j]]$Station)]
                   colifor[[j]]$longitude <- lon[as.numeric(colifor[[j]]$Station)]
                   } else {
                     colifor[[j]] <- NULL
                   }
                 }

                 colifor <- colifor[!sapply(colifor, is.null)]

                 coliform_data[[i]] <- do.call(rbind, colifor)

               }
               coliform_data <- do.call(rbind, coliform_data)
               coliform_data$`Time Collected` <- format(as.POSIXct(as.numeric(coliform_data$`Time Collected`) * 86400, origin = "1899-12-30", tz = "UTC"), "%H:%M")
               coliform_data$year <- format(coliform_data$Time, "%Y")
               coliform_data

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

 tar_target(name = data_musquash_nekton_occurence,
            command = {
              # data from https://catalogue.ogsl.ca/en/dataset/ca-cioos_4c93ac96-0a9f-41d5-9505-80a3b24c30ae

              tmp <- tempfile()
              tmp2 <- tempfile()
              download.file("https://catalogue.ogsl.ca/data/ecw/ca-cioos_4c93ac96-0a9f-41d5-9505-80a3b24c30ae/ecw_nekton-project_occurrence_2019-2021.csv",
                            tmp, mode = "wb")
              download.file("https://catalogue.ogsl.ca/data/ecw/ca-cioos_4c93ac96-0a9f-41d5-9505-80a3b24c30ae/ecw_nekton-project_event_2019-2021.csv",
                            tmp2, mode = "wb")

              nekton <- read.csv(tmp)
              events <- read.csv(tmp2)

              nekton |>
                left_join(events, by = "eventID") |>
                st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326, remove = FALSE)
            }),

 tar_target(name = data_musquash_ECW_water_quality,
            command = {
              data_musquash_ECW_water_quality <- read_excel(file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","data","ECW_MEM_COMPLETE Water Quality Data 2014 to 2024_2025.06.06_v1.xlsx")) |>
                st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
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


 tar_target(control_polygons,
            command= {
              MPAs
              cp <- MPAs |>
                mutate(geoms = st_difference(st_buffer(geoms,20000),geoms)
                )
            }),

 ##### Indicators #####

 # tar_target(ind_placeholder_df,ind_placeholder(areas = all_areas)),
 tar_target(ind_placeholder_df,
            command = {
             process_indicator(data = NA,
                               indicator = "placeholder",
                               areas = MPAs,
                               plot_type = c('time-series','map'),
                               climate_expectation="FIXME",
                               indicator_rationale="FIXME",
                               bin_rationale="FIXME"
                               )
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
                               source="RV Survey",
                               project_short_title = "RV Survey",
                               areas = MPAs,
                               climate_expectation="FIXME",
                               indicator_rationale="FIXME",
                               bin_rationale="FIXME",
                               plot_type = c("violin", "map"),
                               plot_lm=FALSE)
              }
 ),


 #JAIM
 tar_target(data_gliders,
            command= {
              MPAs
              reDownload <- FALSE
              options(timeout = 700)
              dataDir <- tempdir()
              ftpUrl <- 'dontpush'
              dirs <- getURL(paste(ftpUrl,'', sep ="/"), ftp.use.epsv = FALSE, dirlistonly = TRUE)
              dirnames <- strsplit(dirs, "\r*\n")[[1]]
              okdir <- grepl('^GLD\\w+$', dirnames) # just in case something else gets put there
              glddir <- dirnames[okdir]
              # define subdirectory of which processed files to download
              subdir <- 'L0-timeseries-post'
              for(dir in glddir){
                message(which(glddir == dir))
                cat(paste('Check directory', dir), sep = '\n')
                # check that subdir exists
                pathcheck <- paste(ftpUrl, dir, '', sep = '/')
                glddircontents <- getURL(url = pathcheck, ftp.use.epsv = FALSE, dirlistonly = TRUE)
                glddircontents <- strsplit(glddircontents, "\r*\n")[[1]]
                hasSubdir <- any(grepl(pattern = subdir,
                                       x = glddircontents))
                if(hasSubdir){
                  path <- paste(ftpUrl,
                                dir,
                                subdir,
                                '', # to add '/' at end
                                sep = '/')
                  files <- getURL(url = path,
                                  ftp.use.epsv = FALSE, dirlistonly = TRUE)
                  filenames <- strsplit(files, "\r*\n")[[1]]
                  cat(paste('    Found', length(filenames), 'files'), sep = '\n')
                  if(length(filenames) != 0){ # meaning data was able to be processed
                    # should only be 1 file, but for completeness
                    for(f in filenames){
                      url <- paste0(path, f)
                      destfile <- paste(dataDir, f, sep = '/')
                      if(!file.exists(destfile) | reDownload){
                        cat(paste('        Downloading', f), sep = '\n')
                        downloadedFile <- try(download.file(url = url,
                                                            destfile = destfile,
                                                            mode = 'wb'), silent=TRUE)
                      } else {
                        cat(paste('        ', destfile, 'exists locally.'), sep = '\n')
                      }
                    } # closes f
                  } # closes length(filenames)
                } else { # closes hasSubDir
                  cat(paste('      ', subdir, 'does not exist.'), sep = '\n')
                }
              }

              # Making the netcdfs into a data frame for the app

              files <- list.files(path = dataDir, pattern = "\\.nc$", full.names = TRUE)
              glider_list <- vector("list", length(files))  # pre-allocate list

              for (i in seq_along(files)) {
                message(i)
                x <- try(read.glider.netcdf(file = files[i]), silent=TRUE)
                if (!(inherits(x, "try-error"))) {
                  glider_list[[i]] <- data.frame(
                    BBP700 = if (!is.null(x[["BBP700"]])) x[["BBP700"]] else NA,
                    CDOM = if (!is.null(x[["CDOM"]])) x[["CDOM"]] else NA,
                    CHLA = if (!is.null(x[["CHLA"]])) x[["CHLA"]] else NA,
                    CNDC = if (!is.null(x[["CNDC"]])) x[["CNDC"]] else NA,
                    CNDC2 = if (!is.null(x[["CNDC2"]])) x[["CNDC2"]] else NA,
                    DeadReckoning = if (!is.null(x[["DeadReckoning"]])) x[["DeadReckoning"]] else NA,
                    depth = if (!is.null(x[["depth"]])) x[["depth"]] else NA,
                    DOXY = if (!is.null(x[["DOXY"]])) x[["DOXY"]] else NA,
                    FLUORESCENCE_CHLA = if (!is.null(x[["FLUORESCENCE_CHLA"]])) x[["FLUORESCENCE_CHLA"]] else NA,
                    FREQUENCY_DOXY = if (!is.null(x[["FREQUENCY_DOXY"]])) x[["FREQUENCY_DOXY"]] else NA,
                    GLIDER_HEADING = if (!is.null(x[["GLIDER_HEADING"]])) x[["GLIDER_HEADING"]] else NA,
                    GLIDER_PITCH = if (!is.null(x[["GLIDER_PITCH"]])) x[["GLIDER_PITCH"]] else NA,
                    GLIDER_ROLL = if (!is.null(x[["GLIDER_ROLL"]])) x[["GLIDER_ROLL"]] else NA,
                    latitude = if (!is.null(x[["latitude"]])) x[["latitude"]] else NA,
                    LEGATO_CODA_CORR_PHASE = if (!is.null(x[["LEGATO_CODA_CORR_PHASE"]])) x[["LEGATO_CODA_CORR_PHASE"]] else NA,
                    longitude = if (!is.null(x[["longitude"]])) x[["longitude"]] else NA,
                    MFLUV1_NAPH_SCALED = if (!is.null(x[["MFLUV1_NAPH_SCALED"]])) x[["MFLUV1_NAPH_SCALED"]] else NA,
                    MFLUV1_PHE_SCALED = if (!is.null(x[["MFLUV1_PHE_SCALED"]])) x[["MFLUV1_PHE_SCALED"]] else NA,
                    MFLUV1_TMP = if (!is.null(x[["MFLUV1_TMP"]])) x[["MFLUV1_TMP"]] else NA,
                    MFLUV1_TRY_SCALED = if (!is.null(x[["MFLUV1_TRY_SCALED"]])) x[["MFLUV1_TRY_SCALED"]] else NA,
                    NavState = if (!is.null(x[["NavState"]])) x[["NavState"]] else NA,
                    oxygenConcentration = if (!is.null(x[["oxygenConcentration"]])) x[["oxygenConcentration"]] else NA,
                    PRES = if (!is.null(x[["PRES"]])) x[["PRES"]] else NA,
                    PRES2 = if (!is.null(x[["PRES2"]])) x[["PRES2"]] else NA,
                    profileDirection = if (!is.null(x[["profileDirection"]])) x[["profileDirection"]] else NA,
                    profileIndex = if (!is.null(x[["profileIndex"]])) x[["profileIndex"]] else NA,
                    PSAL = if (!is.null(x[["PSAL"]])) x[["PSAL"]] else NA,
                    PSAL2 = if (!is.null(x[["PSAL2"]])) x[["PSAL2"]] else NA,
                    salinity = if (!is.null(x[["salinity"]])) x[["salinity"]] else NA,
                    TEMP = if (!is.null(x[["TEMP"]])) x[["TEMP"]] else NA,
                    TEMP_DOXY = if (!is.null(x[["TEMP_DOXY"]])) x[["TEMP_DOXY"]] else NA,
                    TEMP2 = if (!is.null(x[["TEMP2"]])) x[["TEMP2"]] else NA,
                    time = if (!is.null(x[["time"]])) x[["time"]] else NA
                  )
                } else {
                  glider_list[[i]] <- NULL
                }
              }
              glider_combined <- do.call(rbind, glider_list)

              glider_combined





            }),









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
                               areas = MPAs[MPAs$region=="Maritimes",],
                               plot_type = c("violin", "map"),
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
                               source="RV Survey",
                               climate_expectation="FIXME",
                               indicator_rationale="FIXME",
                               bin_rationale="FIXME",
                               project_short_title = "RV Survey",
                               areas = MPAs,
                               plot_type = c("violin", "map"),
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
                               source="RV Survey",
                               climate_expectation="FIXME",
                               indicator_rationale="FIXME",
                               bin_rationale="FIXME",
                               project_short_title = "RV Survey",
                               areas = MPAs,
                               plot_type = c("violin", "map"),
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
                               source="AZMP",
                               climate_expectation="FIXME",
                               indicator_rationale="FIXME",
                               bin_rationale="FIXME",
                               project_short_title = "AZMP",
                               areas = MPAs,
                               plot_type=c('time-series','map'),
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
                               source="AZMP",
                               project_short_title = "AZMP",
                               climate = TRUE,
                               climate_expectation="FIXME",
                               indicator_rationale="FIXME",
                               bin_rationale="FIXME",
                               areas = MPAs,
                               plot_type=c('time-series','map'),
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
                               source="AZMP",
                               project_short_title = "AZMP",
                               climate = TRUE,
                               climate_expectation="FIXME",
                               indicator_rationale="Changes in nutrient levels can affect biological productivity of the ocean and lead to trophic cascades (e.g., Petersen et al. 2017; Thingstad 2020).",
                               bin_rationale="FIXME",
                               other_nest_variables="depth",
                               areas = MPAs,
                               plot_type = c('time-series','map'),
                               plot_lm=FALSE)
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
                               plot_lm=FALSE)
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
                               source="AZMP",
                               project_short_title = "AZMP",
                               other_nest_variables="depth",
                               areas = MPAs,
                               climate_expectation="FIXME",
                               indicator_rationale="Salinity changes can impact ocean biological functions and may produce community shifts including trophic cascades (e.g., Röthig et al. 2023). Changes in salinity can also adversely affect the temperature tolerance of aquatic organisms (e.g., Farias et al. 2024)",
                               bin_rationale="FIXME",
                               plot_type=c('time-series','map'),
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
                               source="AZMP",
                               project_short_title = "AZMP",
                               climate = TRUE,
                               other_nest_variables="depth",
                               areas = MPAs,
                               climate_expectation="FIXME",
                               indicator_rationale="Changes in temperature influence not only the distribution of species associated with particular water masses (e.g., Alvarez Perez and Santana 2022), but also affect growth and development rates, generation times and productivity of all species (e.g., Shoji et al. 2011; Szuwalski et al. 2021; Millington et al. 2022).",
                               bin_rationale="FIXME",
                               plot_type=c('time-series','map'),
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
                               source="AZMP",
                               project_short_title = "AZMP",
                               other_nest_variables="depth",
                               areas = MPAs,
                               climate_expectation="FIXME",
                               indicator_rationale="Chlorophyll a measurements are typically used as a proxy for primary production at the ocean surface, which, in turn, can influence ocean bottom conditions through benthic/pelagic coupling.",
                               bin_rationale="FIXME",
                               plot_type=c('time-series','map'),
                               plot_lm=FALSE)
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
                               plot_lm=FALSE)
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
                               plot_lm=FALSE)
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




             x <- process_indicator(data = data,
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
                               plot_lm=FALSE)


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
                               plot_lm=FALSE)
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
                               plot_lm=FALSE
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
                               plot_lm=FALSE)
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
                               plot_lm=FALSE)
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
                                 plot_lm=FALSE)
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
                               plot_lm=FALSE)
           }),

 tar_target(name = ind_musquash_infaunal_diversity,
            command = {
              data <- data_musquash_benthic_infauna |>
                group_by(scientificName_Nom_scientifique) |>
                reframe(geoms = st_make_valid(st_union(geoms))) |>
                st_as_sf()

              subclass <- NULL
              for (i in seq_along(data$scientificName_Nom_scientifique)) {
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

              process_indicator(data = data,
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
                                areas = MPAs[MPAs$NAME_E=="Musquash Estuary Marine Protected Area",],
                                plot_type='map-species',
                                plot_lm=FALSE)
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
                              plot_lm=FALSE)
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
                               project_short_title = NA,
                               climate = TRUE,
                               climate_expectation="FIXME",
                               indicator_rationale="FIXME",
                               bin_rationale="FIXME",
                               areas = MPAs[MPAs$NAME_E=="Musquash Estuary Marine Protected Area",],
                               plot_type=c('time-series-no-line','map'),
                               plot_lm=FALSE,
                               latitude='Lat',
                               longitude='Lon')

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
                               longitude='Lon')
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
                               indicator_rationale="FIXME",
                               bin_rationale="FIXME",
                               areas = MPAs[MPAs$NAME_E=="Musquash Estuary Marine Protected Area",],
                               plot_type=c('time-series-no-line','map'),
                               plot_lm=FALSE,
                               latitude='Lat',
                               longitude='Lon')
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
                               longitude='Lon')
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
                               plot_lm=FALSE)
           }
),




tar_target(ind_nitrate_inside_outside,
           command={

             control_polygons
             MPAs
             data <- data_azmp_Discrete_Occupations_Sections  |>
               dplyr::select(longitude, latitude, year, depth, nitrate)

             process_indicator(data = data,
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
                                    control_polygon=control_polygons)
           }),

tar_target(ind_musquash_phosphate_inside_outside,
           command={

             control_polygons
             MPAs
             data_musquash_eutrophication
             data <- data_musquash_eutrophication |>
               rename(phosphate= `tot P (mg/L)`) |>
               dplyr::select(Lon, Lat, phosphate, year)

             process_indicator(data = data,
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
                               control_polygon=control_polygons)
           }),

tar_target(ind_musquash_coliform_inside_outside,
           command={

             control_polygons
             MPAs
             data_musquash_coliform

             data <- data_musquash_coliform |>
               dplyr::select(latitude, longitude, MPN, year)

             process_indicator(data = data,
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
                               control_polygon=control_polygons)
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
                             weights_ratio=c(1,1,1),
                             weights_sum = 1,
                             ind_species_representation,
                             ind_musquash_infaunal_diversity,
                             ind_musquash_nekton_diversity

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
                             weights_ratio = c(1,1,1,0.5,1,1,1,1,1,1,1),
                             weights_sum = 1,
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
                             ind_musquash_coliform
                             )),
 tar_target(bin_habitat_KeyFishHabitat_df,
            aggregate_groups("bin",
                             "Key Fish Habitat",
                             weights_ratio = 1,
                             weights_sum = 1,
                             ind_temperature,
                             ind_SAR_CH_representation
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
                             ind_musquash_phosphate_inside_outside,
                             ind_musquash_coliform_inside_outside
            )),
 tar_target(bin_habitat_Uniqueness_df,
            aggregate_groups("bin",
                             "Uniqueness",
                             weights_ratio=1,
                             weights_sum = 1,
                             ind_QC_gulf_biogenic_habitat_representation,
                             ind_ebsa_representation
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
                             ind_phytoplankton
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

 tar_target(data_pillar_ecol_df,

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


          # Rearranging pillar_ecol

          areas <- unique(x$areaID)


          pillar_list <- split(x, x$areaID)

          mpa_list <- pillar_list[-which(names(pillar_list) %in% regions$NAME_E)]
          # Order based on score
          mpa_list <- lapply(mpa_list, function(ddff) {
            ddff[order(ddff$score, na.last = TRUE), ]
          })

          region_list <-  pillar_list[which(names(pillar_list) %in% regions$NAME_E)]
          region_list <- lapply(region_list, function(ddff) {
            ddff[order(ddff$score, na.last = TRUE), ]
          })
          # Order based on score

          for (i in seq_along(region_list)) {
            reg <- region_list[[i]]
            for (j in 1:nrow(reg)) {
              reg2 <- reg[j,]
              region_bin <- reg2$indicator
              keep <- which(names(mpa_list) == region_bin)
              mpa_list[[keep]] <- rbind(reg2, mpa_list[[keep]])
            }

          }


          # In r, there is a row for every indicator bin, for every mpa that makes up that region. E.g. if there is 3 mpas in that region there are 3*11 rows.

          do.call(rbind, mpa_list)


 }),

tar_target(name = pillar_ecol_df,
           command = select(data_pillar_ecol_df,-data,-plot)),


 tar_target(all_project_geoms,
             command = {

                data_pillar_ecol_df |>
                 filter(!map_lgl(data, is.null)) |>
                 mutate(data = map(data,function(x){
                   # browser()
                   if(!inherits(x,"sf")) x <- st_as_sf(x)

                   if("geoms" %in% names(x)) {
                     x <- x |>
                       mutate(geometry = geoms) |>
                       as.data.frame() |>
                       dplyr::select(-geoms) |>
                       st_as_sf()
                   }

                   if(!("year" %in% names(x))) {
                     x <- x |>
                       mutate(year = NA)
                   }

                   x <- x |>
                     mutate(year=as.numeric(year))

                   x |>
                     st_cast("GEOMETRY") |>
                     dplyr::select(year,geometry) |>
                     st_as_sf() |>
                     unique()
                 })
                 ) |>
                 dplyr::select(data,type, project_short_title, PPTID,areaID, source, climate_expectation, indicator_rationale, bin_rationale)|>
                 unnest(cols = data) |>
                 st_as_sf()

             }),


tar_target(plot_files,
            command = {
              allplotnames <- NULL
              for(i in 1:nrow(data_pillar_ecol_df)){
                message(i)
                if(!is.null(data_pillar_ecol_df$plot[[i]])){
                filename <-  file.path(Sys.getenv("OneDriveCommercial"),
                                       "MarConsNetTargets",
                                       "data", "plots",
                                       make.names(paste0("plot_",
                                                         data_pillar_ecol_df$areaID[i],
                                                         "_",
                                                         data_pillar_ecol_df$indicator[i],
                                                         ".png")))

                allplotnames <- c(allplotnames,filename)
                ggsave(filename,data_pillar_ecol_df$plot[[i]])
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


 ##### Pillar #####

tar_target(name = MPA_report_card,
           command = {
             left_join(MPAs,data_pillar_ecol_df |>
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
                            ' -P 22 "',
                            file.path(Sys.getenv("OneDriveCommercial"),'MarConsNetTargets','app_targets'),
                            '" "',
                            serveruser,
                            '@mar-spa.ent.dfo-mpo.ca:/home/rdaigle/MarConsNetTargets/'))

              # create data folder
              system(paste0('ssh -i ',
                            file.path(Sys.getenv("USERPROFILE"),'.ssh','id_rsa'),
                            ' -p 22 ',
                            serveruser,
                            '@mar-spa.ent.dfo-mpo.ca "mkdir -p /home/rdaigle/MarConsNetTargets/data"'))


              # copy data files
              datafiles <- list.files(file.path(Sys.getenv("OneDriveCommercial"),'MarConsNetTargets','data'),include.dirs = FALSE)
              datadirs <- list.dirs(file.path(Sys.getenv("OneDriveCommercial"),'MarConsNetTargets','data'),recursive = FALSE, full.names = FALSE)
              for(f in datafiles[!(datafiles %in% datadirs)]){
                system(paste0('scp -i ',
                              file.path(Sys.getenv("USERPROFILE"),'.ssh','id_rsa'),
                              ' -P 22 "',
                              file.path(Sys.getenv("OneDriveCommercial"),'MarConsNetTargets','data',f),
                              '" "',
                              serveruser,
                              '@mar-spa.ent.dfo-mpo.ca:/home/rdaigle/MarConsNetTargets/data/'))
              }


              # copy data directories
              for(f in datadirs[datadirs!="obis_data"]){
                system(paste0('scp -r -i ',
                              file.path(Sys.getenv("USERPROFILE"),'.ssh','id_rsa'),
                              ' -P 22 "',
                              file.path(Sys.getenv("OneDriveCommercial"),'MarConsNetTargets','data',f),
                              '" "',
                              serveruser,
                              '@mar-spa.ent.dfo-mpo.ca:/home/rdaigle/MarConsNetTargets/data/'))
              }





              TRUE
            },
            deployment = "main",
            priority = 0,
            cue = tar_cue(mode = "always"))


) |>
  unlist(recursive = FALSE)
