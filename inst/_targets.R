# Load packages required to define the pipeline:
if(!require(librarian)) install.packages("librarian")
pkgs <- c("sf",
          "targets",
          "viridis",
          "j-harbin/dataSPA",
          "arcpullr",
          "ArgoCanada/argoFloats",
          "casaultb/azmpdata",
          "raster",
          "dfo-mar-odis/TBSpayRates",
          "readxl",
          "ggplot2",
          "shinyBS",
          "Maritimes/Mar.datawrangling",
          "DT",
          "magrittr",
          "RColorBrewer",
          "dplyr",
          "tidyr",
          "stringr",
          "officer",
          "RColorBrewer",
          "car",
          "purrr",
          "dfo-mar-mpas/MarConsNetAnalysis",
          "dfo-mar-mpas/MarConsNetData",
          "dfo-mar-mpas/MarConsNetApp",
          "rnaturalearth",
          "DBI",
          "duckdb",
          "rmarkdown",
          "shiny",
          "measurements",
          "mregions2",
          "patchwork",
          "units",
          "dankelley/oceglider",
          "RCurl",
          "oce",
          "gsw",
          "leaflet",
          "rgbif",
          "qs",
          "qs2",
          "odbc",
          "rvest")
shelf(pkgs)

# Set target options here if they will be used in many targets, otherwise, you can set target specific packages in tar_targets below
tar_option_set(packages = basename(pkgs),
               format = "qs")

if(dir.exists("/srv/sambashare/MarConsNet/MarConsNetTargets/app_targets")){
  store = "/srv/sambashare/MarConsNet/MarConsNetTargets/app_targets"
} else if (dir.exists("//wpnsbio9039519.mar.dfo-mpo.ca/sambashare/MarConsNet/MarConsNetTargets/app_targets")) {
  # Accessing 'beast' via Windows
  store <- "//wpnsbio9039519.mar.dfo-mpo.ca/sambashare/MarConsNet/MarConsNetTargets/app_targets"
} else if(dir.exists(file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","app_targets"))){
  store = file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","app_targets")
} else if(dir.exists("//wpnsbio9039519.mar.dfo-mpo.ca/sambashare/MarConsNet/MarConsNetTargets/app_targets")){
  store = "//wpnsbio9039519.mar.dfo-mpo.ca/MarConsNet/MarConsNetTargets/app_targets"
} else {
  warning("MarConsNet data store not found. Please check the directory paths.")
  store = getwd()
}

#store <- Sys.getenv("MARCONSNET_TARGETS_PATH")
if (!nzchar(store)) stop("MARCONSNET_TARGETS_PATH is not set!")


tar_config_set(store = store)


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
               regions
               areas <- get_spatial_layer("https://maps-cartes.ec.gc.ca/arcgis/rest/services/CWS_SCF/CPCAD/MapServer/0",
                                 where="BIOME='M' AND MGMT_E='Fisheries And Oceans Canada'") |>
                 group_by(NAME_E, NAME_F) |>
                 summarise(geoms = st_make_valid(st_union(geoms)))

               centroids <- st_centroid(areas$geoms) |>
                 st_as_sf() |>
                 st_join(regions)

               areas$region <- centroids$NAME_E

               areas <- areas |>
                 filter(!is.na(region))


               bbox_coords <- matrix(c(
                 -171, 24,  # xmin, ymin (Hawaii/Alaska/Florida range)
                 -50,  24,  # xmax, ymin
                 -50,  84,  # xmax, ymax
                 -171, 84,  # xmin, ymax
                 -171, 24   # close the polygon
               ), ncol = 2, byrow = TRUE)

               bbox <- st_polygon(list(bbox_coords)) |>
                 st_sfc(crs = st_crs(areas)) |>
                 st_make_valid()

               # Step 4: Get union of all MPAs
               all_mpa_union <- st_union(areas$geoms)

               # Step 5: Compute difference (the "outside" area)
               outside_geom <- st_difference(bbox, all_mpa_union) |> st_make_valid()

               # Step 6: Create the Outside row
               outside_row <- tibble(
                 NAME_E = "Non_Conservation_Area",
                 NAME_F = "Extérieur",
                 geoms = outside_geom,
                 region = NA
               )

               # Step 7: Bind with existing areas
               areas_full <- bind_rows(areas, outside_row)

               areas_full

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
               home <- data.frame(flower="home", place="home", tab="tab_0", link="link_0")
               rbind(home, apptabs)
             }),

  tar_target(name=cookie,
           command={
             read.table(file.path(path_to_store(),"..","data","cookie.txt"))$V1

           }),

  tar_target(name = om,
             command = {
               cookie
               OM <- dataSPA::getData(type="om", age=3000, cookie=cookie,
                                      path = file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","data"))

               OM[-(which(OM$activity_type == "Other")),]

             }),

  tar_target(name=salary,
             command={
               SAL <- dataSPA::getData(type="salary", age=3000, cookie=cookie,
                                      path = file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","data"))

               SAL[-(which(SAL$activity_type == "Other")),]

             }),


  tar_target(name = collaborations,
             command = {
              cookie
               col <- dataSPA::getData(type='collaboration', cookie=cookie)
             }),

  tar_target(name = deliverables,
             command = {
               cookie
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
               cookie

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
               objective_tabs
               N_Objectives
               MPA_report_card
               collaborations
               creature_feature
               cost_of_mpas
               om
               objective_tabs
               regions
               Ecological
               all_project_geoms
               deliverables
               csas
               climate_change

               #TODO https://github.com/dfo-mar-mpas/MarConsNetApp/issues/184
               # mpas <- MPAs$NAME_E
               mpas <- unique(objective_tabs$area[objective_tabs$area %in% MPAs$NAME_E])
               rmd_file <- system.file("data", "report.Rmd", package = "MarConsNetApp")
               output_dir <- file.path(dirname(path_to_store()),"data","reports")

               for (i in seq_along(mpas)) {
                 message(i)
                 params <- list()
                 params$mpas <- mpas[i]
                 output_file <- paste0(output_dir,"/", make.names(paste0(names=mpas[i], ".html")))
                 render(input=rmd_file, output_file = output_file, output_format = "html_document", params = params, envir = new.env())
               }

               # network level
               rmd_file <- system.file("data", "network_report.Rmd", package = "MarConsNetApp")

               params <- list()
               params$mpas <- "Maritimes"
               output_file <- file.path(paste0(output_dir,"/", make.names(paste0(names=params$mpas, ".html"))))
               render(input=rmd_file, output_file = output_file, output_format = "html_document", params = params, envir = new.env())
             }),



  tar_target(name = areas,
             command = {
               MPAs$NAME_E
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

  tar_target(name = Context,
             command = {
               c <- lapply(areas, function(x) data_context(type="site", area=x))
               names(c) <- areas
               c <- c[which(unname(unlist(lapply(c, function(x) !(is.null(x))))))]
               c
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


  ############ data loading ############
  tar_target(name=data_otn_recievers,
             command = {
               geoserver_receivers <- readr::read_csv('https://members.oceantrack.org/geoserver/otn/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=otn:stations_receivers&outputFormat=csv', guess_max = 13579)
               geoserver_receivers
             }),



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
            ds_all,
            cue = tar_cue("never")),


 tar_target(rv_rawdata_env,{ # Environment
   # THIS TARGET NEEDS TO GET MADE ON WINDOWS (NOT LINUX)
   ds_all
   .pkgenv <- new.env(parent = emptyenv())

   get_pesd_dw_dir <- function() {
     file.path("C:", "DFO-MPO", "PESDData","MarDatawrangling")
   }

   get_ds_all <- function() {
     .pkgenv$ds_all
   }

   pwd <-  read.table("\\\\wpnsbio9039519.mar.dfo-mpo.ca\\sambashare\\MarConsNet\\MarConsNetTargets\\app_targets/objects/oracle.txt")$V1


   get_data('rv', extract_user = "DAIGLER", extract_computer = "WLNSBIO90210", cxn = DBI::dbConnect(odbc::odbc(), dsn = "PTRAN", uid = "DAIGLER", pwd = pwd), reextract.override = T, env=.pkgenv)
   .pkgenv
 }
 ),

 tar_target(rv_data,{
   # THIS TARGET NEEDS TO GET MADE ON WINDOWS (NOT LINUX)

   # for whatever reason, we need to run:
   # tar_invalidate(c("ds_all","rv_rawdata_env"))
   # before re-running this target

   temp <- rv_rawdata_env
   ds_all # mentioned here because otherwise it won't be available for self_filter


   GSINF <- temp$GSINF |>
     filter(!is.na(LONGITUDE),!is.na(LATITUDE)) |>
     st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
              crs = 4326,
              remove = FALSE) |>
     st_filter(regions[regions$NAME_E=="Maritimes",]) |>
     st_join(MPAs |> dplyr::select(NAME_E), left=TRUE) |>
     as.data.frame() |>
     dplyr::select(-geometry)


   self_filter(env=temp)

   summarize_catches('rv',env = temp)
 }
 ),

 tar_target(rv_data_det,{
   # for whatever reason, we need to run:
   # tar_invalidate(c("ds_all","rv_rawdata_env"))
   # before re-running this target
   temp <- rv_rawdata_env
   ds_all # mentioned here because otherwise it won't be available for self_filter

   temp$GSINF <- temp$GSINF |>
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
                filter(NAME_E!="Non_Conservation_Area") |>
                rowwise() |>
                mutate(geoms = st_difference(st_buffer(geoms,20000),geoms)
                )
            }),

             tar_target(name = data_MMMP_birds_raw, command = {
    # data from https://naturecounts.ca/nc/default/datasets.jsp?code=MMMP&sec=bmdr
    read.delim(
      file.path(
        Sys.getenv("OneDriveCommercial"),
        "MarConsNetTargets",
        "data",
        "birds",
        "naturecounts_request_257783_1752519752346",
        "mmmp_naturecounts_data.txt"
      ),
      header = TRUE
    )
  }),

  tar_target(name = data_musquash_MMMP_birds,
             command = {
    # data from https://naturecounts.ca/nc/default/datasets.jsp?code=MMMP&sec=bmdr
    data_MMMP_birds_raw |>
      filter(!is.na(DecimalLatitude), !is.na(DecimalLongitude)) |>
      st_as_sf(
        coords = c("DecimalLongitude", "DecimalLatitude"),
        crs = 4326,
        remove = FALSE
      ) |>
      st_filter(st_buffer(
        MPAs[MPAs$NAME_E == "Musquash Estuary Marine Protected Area", ],
        2000
      )) |>
      filter(
        ScientificName != "",
        !is.na(ScientificName)
      ) |>
      as.data.frame() |>
      mutate(
        DecimalLatitude = round(DecimalLatitude, 3),
        DecimalLongitude = round(DecimalLongitude, 3)
      ) |>
      group_by(
        DecimalLatitude,
        DecimalLongitude,
        YearCollected,
        MonthCollected,
        DayCollected,
        CollectorNumber,
        ScientificName
      ) |>
      reframe(n = n()) |>
      pivot_wider(
        names_from = ScientificName,
        values_from = n,
        values_fill = 0
      ) |>
      st_as_sf(
        coords = c("DecimalLongitude", "DecimalLatitude"),
        crs = 4326,
        remove = FALSE) |>
      select(
        -DecimalLatitude,
        -DecimalLongitude,
        -YearCollected,
        -MonthCollected,
        -DayCollected,
        -CollectorNumber
      )
  }),

tar_target(name = rawdata_inaturalist_download,
  command = {
    d <- 5000

    simplegeom <- st_simplify(st_buffer(MPAs[MPAs$NAME_E != "Non_Conservation_Area",],d),dTolerance = d) |>
      st_make_valid() |>
      st_union() |>
      st_as_text()

    occ_download(
      pred_within(simplegeom),
      pred_in("institutionCode", "iNaturalist"),
      pred("hasCoordinate", TRUE),
      pred("hasGeospatialIssue", FALSE),
      format = "SIMPLE_CSV"
    )

  }),

tar_target(name = data_inaturalist,
           command = {
             occ_download_wait(rawdata_inaturalist_download)
             x <- occ_download_get(rawdata_inaturalist_download)
             occ_download_import(x) |>
               st_as_sf(
                 coords = c("decimalLongitude", "decimalLatitude"),
                 crs = 4326,
                 remove = FALSE
               ) |>
               st_join(MPAs[, "NAME_E"], join = st_within)
           }),


  tar_target(name = creature_feature,
    command = {
      sptable <- data_inaturalist |>
        filter(!is.na(NAME_E)&NAME_E!="Non_Conservation_Area") |>
        as.data.frame() |>
        filter(!is.na(speciesKey),
               kingdom != "Plantae"|genus == "Zostera", # remove plants except eelgrass
               class!="Mammalia"|family %in% c("Phocidae","Otarioidea"), #remove mammals except seals
               class!="Testudines"|family %in% c("Cheloniidae","Dermochelyidae"),# remove reptiles except sea turtles
               phylum!="Basidiomycota",phylum!="Ascomycota", # remove mushrooms and lichens
               !(class %in% c("Insecta", "Arachnida", "Squamata","Amphibia")) # remove all insects, spiders, amphibians, and snakes
        ) |>
        group_by(speciesKey,scientificName,kingdom, phylum, class,NAME_E) |>
        reframe(n=n(),
                exampleurl = occurrenceID[1],
                rightsHolder = rightsHolder[1]) |>
        mutate(commonname = NA_character_,
               imageurl = NA_character_,
               image_column = NA_character_)


      conns_before <- showConnections()
      for(i in 1:nrow(sptable)){
        print(paste(i,sptable$scientificName[i]))
        sptable$imageurl[i] <- {
          url <- try(read_html(sptable$exampleurl[i]) |>
                       html_node('meta[property="og:image"]') |>
                       html_attr('content'))
          if (inherits(url, "try-error")) {
            NA_character_
          } else {
            url
          }
        }

        sptable$image_column[i] <- paste0(
          '<a href="', sptable$exampleurl[i], '" target="_blank">',
          '<img src="', sptable$imageurl[i], '" style="max-height: 100px; max-width: 100px; object-fit: cover;"/>',
          '<br>',sptable$rightsHolder[i],'</br>',
          '</a>'
        )



        if(is.na(sptable$commonname[i])){
          spnames <- try(name_usage(sptable$speciesKey[i], data = "vernacularNames")[[2]] )
          if(!inherits(spnames, "try-error")) {
            if(nrow(spnames)>0){
              cn <- spnames |>
                as.data.frame() |>
                filter(language == "eng") |>
                pull(vernacularName) |>
                table() |>
                sort(decreasing = TRUE) |>
                head(1) |>
                names()
              if(!is.null(cn)){
                sptable$commonname[sptable$speciesKey[i]==sptable$speciesKey] <- cn
              }
            }

          }
          conns_after <- showConnections()
          new_conns <- setdiff(rownames(conns_after), rownames(conns_before))
          for(conn in new_conns) {
            try(close(getConnection(as.integer(conn))), silent = TRUE)
          }
          Sys.sleep(1) # to avoid overloading the server
        }

      }


      sptable
      }),

tar_target(data_inseadistance_matrix,
           calc_in_sea_distance(cellsize = 5000,bioregion=st_union(regions),areas = MPAs[MPAs$NAME_E!="Non_Conservation_Area",])),

tar_target(data_protconn_EL_by_region,
           command = {
             mpas <- MPAs |>
               filter(NAME_E %in% colnames(data_inseadistance_matrix)) |>
               st_filter(st_buffer(regions,500000)) |>
               mutate(area=sf::st_area(geoms))

             edgelist <- map(c(regions$NAME_E,mpas$NAME_E[mpas$region==regions$NAME_E]),
                             function(x) {
                               ind_ProtConn(distkm = data_inseadistance_matrix[rownames(data_inseadistance_matrix)!=x,colnames(data_inseadistance_matrix)!=x],
                                            dkm=50,
                                            bioregion = regions,
                                            area = mpas[mpas$NAME_E!=x&mpas$region==regions$NAME_E,],
                                            returns = "EL") |>
                                 mutate(region = regions$NAME_E,
                                        areaID = if_else(regions$NAME_E==x,
                                                         NA,
                                                         x))
                             }) |>
               bind_rows()

             A <- as.numeric(sum(sf::st_area(regions)))
             prot <- as.numeric(sum(st_area(mpas[mpas$region==regions$NAME_E,])))
             PCregion <-as.numeric(100*sqrt(sum(edgelist$product[is.na(edgelist$areaID)]))/A)


             # for sites:
             summaryEL <- edgelist |>
               filter(!is.na(areaID)) |>
               group_by(areaID) |>
               mutate(PC = as.numeric(100*sqrt(sum(product))/A),
                 effect =(PCregion-PC)/PCregion) |>
               ungroup() |>
               mutate(score = cume_dist(effect))

             #adding the region
             summaryEL <- bind_rows(summaryEL,
                                    edgelist |>
                                      filter(is.na(areaID)) |>
                                    mutate(areaID = regions$NAME_E,
                                               PC = PCregion,
                                               effect = NA,
                                               score = PCregion/prot)) |>
               mutate(region = regions$NAME_E)

             summaryEL|>
               nest(data = c(names(edgelist)[names(edgelist)!="areaID"],"region"))


           },
           pattern = map(regions)),


 ##### Indicators #####

  tar_target(ind_placeholder_df,ind_placeholder(areas = MPAs)),
 # tar_target(ind_placeholder_df,
 #            command = {
 #             process_indicator(data = NA,
 #                               indicator = "placeholder",
 #                               areas = MPAs,
 #                               plot_type = c('time-series','map'),
 #                               climate_expectation="FIXME",
 #                               indicator_rationale="FIXME",
 #                               bin_rationale="FIXME"
 #                               )
 #            }),

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

tar_target(data_otn_tags,
           command={
             tags <- readr::read_csv('https://members.oceantrack.org/geoserver/otn/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=otn:animals&outputFormat=csv', guess_max = 13579)
             tags <- tags %>%
               group_by(catalognumber) %>%   # group by catalognumber
               slice(1) %>%                  # keep the first row in each group
               ungroup()                     # ungroup after slicing

             # 1. Subset obis for just Ocean Tracking Network
             otn_detection <- data_obis[which(data_obis$rightsHolder == "Ocean Tracking Network, Dalhousie University, Halifax, Nova Scotia otndc@dal.ca" & !(is.na(data_obis$decimalLatitude)) & !(is.na(data_obis$decimalLongitude))),]


             # 2. Remove all NA columns
             bad <- NULL
             message(1)
             for (i in seq_along(names(otn_detection))) {
               message(i)
               bad[i] <- all(is.na(otn_detection[[names(otn_detection)[i]]]))
             }

             oo <- otn_detection[, -which(bad)] # Removing all NA columns

             # 3. Further subset to only look at relevant columns
             ooo <- oo[, c("dataset_id", "id", "aphiaid", 'catalogNumber', 'collectionCode', 'datasetID', "datasetName", 'decimalLatitude', "decimalLongitude", "eventDate", 'eventID', "occurrenceID", 'organismID', 'organismName', 'rightsHolder', 'scientificName', "NAME_E.x", "geometry")]

             # 4.  Remove the ones that say 'release' and 'capture' in occurence ID
             ooo <- ooo[-which(grepl("release", ooo$occurrenceID)),]
             ooo <- ooo[-which(grepl("capture", ooo$occurrenceID)),]

             # 5. Only keep obis data that has tags$collectioncode in ooo$occurenceID (e.g. IBFS).
             ## This is to allow us to find the tag ID for all obis

             keeper <- list()
             message(2)
             for (i in seq_along(unique(tags$collectioncode))) {
               message(i)
               keeper[[i]] <- which(grepl(unique(tags$collectioncode)[i], ooo$occurrenceID))

             }

             keep <- unlist(keeper)
             ooo <- ooo[keep,]


             #Extracting 'tag' info from occurenceID
             ## (Trial and error)- tags$catalogNumber gives us the tag ID. It can be grepl in ooo$occurenceID


             pattern_dash <- "^.*?(\\d{4}-\\d{2}-\\d{2}T[^_]*_)"
             pattern_underscore <- "^.*?(\\d{4}_\\d{2}_\\d{2}T[^_]*_)"

             cut1 <- sapply(ooo$occurrenceID, function(x) {
               if (grepl(pattern_dash, x)) {
                 sub(pattern_dash, "", x)
               } else if (grepl(pattern_underscore, x)) {
                 sub(pattern_underscore, "", x)
               } else {
                 sub("^.*?_", "", x)  # remove everything before the first underscore
               }
             })


             cut2 <- sub("_.*$", "", cut1)

             bad_tags <- ooo$occurrenceID[which(!cut2 %in% tags$catalognumber)]
             bad <- which(!cut2 %in% tags$catalognumber) # FIXME: this could use more work.

             ## 6. Remove ones that could not get an associated tag
             ooo <- ooo[-bad,]
             ooo$tag_id <- cut2[which(cut2 %in% tags$catalognumber)]

             ooo
             #tracking <- split(ooo, ooo$tag_id)

             #tracking
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
                               objectives= c(
                                 "Maintain productivity of harvested species",
                                 "Help maintain healthy populations of species of Aboriginal, commercial, and/or recreational importance",
                                 "Allow sufficient escapement from exploitation for spawning",
                                 "Contribute to the recovery and conservation of depleted species"
                               ),
                               plot_lm=FALSE)
              }
 ),

 tar_target(data_gliders,
            command= {
              reDownload <- FALSE
              options(timeout = 700)
              dataDir <- file.path(dirname(path_to_store()),'data', 'gliders')
              ftpUrl <- read.table(file.path(dirname(path_to_store()),'data', 'gliders',"url.txt"))$V1
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
                glddircontents <- try(getURL(url = pathcheck, ftp.use.epsv = FALSE, dirlistonly = TRUE), silent=TRUE)
                glddircontents <- strsplit(glddircontents, "\r*\n")[[1]]
                path <- paste(ftpUrl,
                              dir,
                              subdir,
                              '', # to add '/' at end
                              sep = '/')
                files <- try(getURL(url = path,
                                    ftp.use.epsv = FALSE, dirlistonly = TRUE),silent = TRUE)
                if(!inherits(files, "try-error")){
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
                        Sys.sleep(15)
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

              message(paste0("files , " ,files))

              for (i in seq_along(files)) {
                message(i)
                x <- try(oceglider::read.glider.netcdf(file = files[i]), silent=TRUE)
                if (!(inherits(x, "try-error"))) {
                  # weird dates in the files due to a hardware issue with the gliders
                  #good_year <- names(which.max(table(as.numeric(format(x[['time']], "%Y")))))
                  #message(paste0("hi ", good_year))
                  x <- oceglider::subset(x, grepl(names(which.max(table(as.numeric(format(x[['time']], "%Y"))))), time))

                  # Remove profiles with profileIndex == 0 (inflecting/stalled)
                  x <- oceglider::subset(x, which(!(profileIndex == 0)))

                  # vertically binning and temporally averaging the data. For our analysis we bin it to 1dbar and hourly average.


                  # startTime and endTime will be numeric, that's OK b/c it will work for next calculation
                  # find which profiles are within the defined averaging time
                  # here it will be 1 hour.
                  dt <- 60 * 60 #sph
                  # define depth bins
                  dz <- 1 # size of depth bins
                  nz <- 600 # max depth of bins
                  z <- seq(1, nz, by = dz)

                  # initialize indices output
                  vars <- names(x[['data']][['payload1']]) # get all variables in glider file
                  # define breaks for splitting data
                  zbreaks <- c(0,z) + dz/2

                  dsubdata <- x[['data']][['payload1']]

                  dsubdata$mld <- NA

                  for (j in seq_along(unique(dsubdata$profileIndex))) {
                    message(j)
                    keep <- which(x[['profileIndex']] == unique(dsubdata$profileIndex)[j])

                    if (!all(is.na(dsubdata$PSAL[keep])) && !all(is.na(dsubdata$TEMP[keep])) && !all(is.na(dsubdata$depth[keep]))) {
                      # Use depth as a proxy for pressure (1 m ≈ 1 dbar)
                      approx_pres <- dsubdata$depth[keep]
                      SA <- gsw_SA_from_SP(dsubdata$PSAL[keep], approx_pres[keep],
                                           mean(dsubdata$longitude[keep], na.rm = TRUE),
                                           mean(dsubdata$latitude[keep], na.rm = TRUE))
                      CT <- gsw_CT_from_t(SA, dsubdata$TEMP[keep], approx_pres)
                      rho <- gsw_rho(SA, CT, approx_pres)

                      # Define MLD function using Δρ threshold
                      calc_mld <- function(depth, density, threshold = 0.03) {
                        ord <- order(depth)
                        depth <- depth[ord]
                        density <- density[ord]
                        surface_density <- density[which.min(depth)]
                        idx <- which(density - surface_density > threshold)
                        if (length(idx) == 0) {
                          return(max(depth, na.rm = TRUE))  # fallback if no stratification
                        } else {
                          return(depth[min(idx)])
                        }
                      }

                      dsubdata$mld[keep] <- calc_mld(dsubdata$depth[keep], rho)

                    } else {
                      dsubdata$mld[keep] <- NA
                    }

                  }

                  # Add mld into variable list:
                  vars <- c(vars, "mld")

                  # Above is to determine the MLD for each profile (upcast and downcast in the glider netcdf)


                  # split the data based on pressure breaks (zbreaks)
                  dsplit <- split(x = dsubdata, f = cut(dsubdata[['PRES']], breaks = zbreaks))
                  # now do the mean for each split
                  ## time is an issue for the `apply(..., FUN=mean)` so omit it
                  ## this is OK as we'll do the mean time over the entire profile
                  dmean <- lapply(dsplit, function(k) apply(X = k[,!names(k) %in% 'time'], MARGIN = 2, FUN = mean, na.rm = TRUE))
                  ## combine it together
                  dmeanall <- as.data.frame(do.call('rbind', dmean))
                  ### find which rows have all NA values
                  omitrows <- apply(X = dmeanall, MARGIN = 1, FUN = function(k) all(is.na(k)))
                  dmeanall <- dmeanall[!omitrows, ]
                  zprofile <- z[!omitrows]
                  ## create ctd object
                  ### get average values from all data in dsub for certain parameters
                  profileTime <- mean(dsubdata[['time']],na.rm = TRUE)
                  profileLongitude <- mean(dsubdata[['longitude']], na.rm = TRUE)
                  profileLatitude <- mean(dsubdata[['latitude']], na.rm = TRUE)
                  ### create ctd object
                  ctdadd <- oce::as.ctd(salinity = dmeanall[['PSAL']],
                                   temperature = dmeanall[['TEMP']],
                                   pressure = zprofile,
                                   conductivity = dmeanall[['CNDC']],
                                   longitude = profileLongitude,
                                   latitude = profileLatitude,
                                   time = profileTime)
                  ### add remaining variables,
                  ###     omit a few extra to avoid misleading user, these include :
                  ###         PRES2, depth
                  addvars <- vars[!vars %in% c('PSAL', 'TEMP', 'PRES', 'CNDC', 'longitude', 'latitude', 'time', 'PRES2', 'depth')]
                  for(addvar in addvars){
                      addvarname <- addvar

                    ctdadd <- oce::oceSetData(object = ctdadd, name = addvarname, value = dmeanall[[addvar]])
                  }
                  ### add metadata from original file
                  ctdadd@metadata <- x@metadata
                  ### have to re-set longitude, latitude, and time
                  ctdadd <- oce::oceSetMetadata(object = ctdadd,
                                           name = 'longitude',
                                           value = profileLongitude)
                  ctdadd <- oce::oceSetMetadata(object = ctdadd,
                                           name = 'latitude',
                                           value = profileLatitude)
                  ctdadd <- oce::oceSetMetadata(object = ctdadd,
                                           name = 'time',
                                           value = profileTime)
                  ### save ctd
                  x <- ctdadd

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
                    time = if (!is.null(x[["time"]])) x[["time"]] else NA,
                    mld = if (!is.null(x[["mld"]])) x[["mld"]] else NA
                  )

                } else {
                  glider_list[[i]] <- NULL
                }
              }

              glider_data <- do.call(rbind, glider_list)
              glider_data

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
                               indicator_rationale="FIXME",
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



tar_target(name = ind_sst,
           command = {
             ind_placeholder(ind_name="Sea Surface Temperature", areas = MPAs, readiness = "Readily Available", source = "AZMP", objectives = c("Protect continental shelf habitats and associated benthic and demersal communities"))
           }), # Environmental Representativity


tar_target(objectives_csv,
           command = "data_raw/objectives.csv",
           format = "file"
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

tar_target(objective_tabs,
           # FOR TABS, THERE ARE APPTABS, WHICH ARE FOR FLOWER, PILLAR_ECOL_DF WHICH ARE FOR INDICATORS, AND OBJECTIVE_TABS
           # WHICH ARE FOR OBJECTIVES
           command={
            ped <- pillar_ecol_df
           ot <- data.frame(objectives=objectives_df$Objective, tab=NA, area=objectives_df$Framework)

           start <- max(sort(as.numeric(sub(".*_", "", ped$tab))))+1
           end <- start+(length(ot$objectives)-1)
           tabs <- start:end
           ot$tab <- paste0("tab_", tabs)
           ot$link <- paste0("link_", tabs)
           ot

           }
           ),


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
                             weights_ratio = 1,
                             weights_sum = 1,
                             ind_species_representation,
                             ind_musquash_infaunal_diversity,
                             ind_musquash_nekton_diversity,
                             ind_musquash_birds_sample_coverage

            )
            ),
 tar_target(bin_habitat_Connectivity_df,
            aggregate_groups("bin",
                             "Connectivity",
                             weights_ratio=1,
                             weights_sum = 1,
                             ind_placeholder_df,
                             ind_otn_proportion_tags_detected_in_multiple_mpas
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
                             ind_sst
                             )),
 tar_target(bin_habitat_KeyFishHabitat_df,
            aggregate_groups("bin",
                             "Key Fish Habitat",
                             weights_ratio = 1,
                             weights_sum = 1,
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
                             ind_zooplankton_community_composition,
                             ind_phytoplankton
            )),
 tar_target(bin_productivity_StructureandFunction_df,
            aggregate_groups("bin",
                             "Structure and Function",
                             weights_ratio=1,
                             weights_sum = 1,
                             ind_stratification)),
 tar_target(bin_productivity_ThreatstoProductivity_df,
            aggregate_groups("bin",
                             "Threats to Productivity",
                             weights_ratio=1,
                             weights_sum = 1,
                             ind_placeholder_df
            )),


 ##### Themematic bins



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

 ##### Ecological Pillar and other end products #####

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

              ##### Make a filtered version for summarizing (excludes Non_Conservation_Area)
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


  # Process each objective separately
  tar_target(biodiversity_geoms, {
    ecol_obj_biodiversity_df |>
      filter(!map_lgl(data, is.null)) |>
      mutate(data = map(data, process_geom_data)) |>
      dplyr::select(data, type, project_short_title, PPTID, areaID, source,
                    climate_expectation, indicator_rationale, bin_rationale) |>
      unnest(cols = data) |>
      st_as_sf()
  }),

  tar_target(habitat_geoms, {
    ecol_obj_habitat_df |>
      filter(!map_lgl(data, is.null)) |>
      mutate(data = map(data, process_geom_data)) |>
      dplyr::select(data, type, project_short_title, PPTID, areaID, source,
                    climate_expectation, indicator_rationale, bin_rationale) |>
      unnest(cols = data) |>
      st_as_sf()
  }),

  tar_target(productivity_geoms, {
    ecol_obj_productivity_df |>
      filter(!map_lgl(data, is.null)) |>
      mutate(data = map(data, process_geom_data)) |>
      dplyr::select(data, type, project_short_title, PPTID, areaID, source,
                    climate_expectation, indicator_rationale, bin_rationale) |>
      unnest(cols = data) |>
      st_as_sf()
  }),

  # Combine only the processed results
  tar_target(all_project_geoms, {
    rbind(biodiversity_geoms, habitat_geoms, productivity_geoms)
  }),

  tar_target(name=project_widget_choices,
             command={
               distinct_rows <- unique(all_project_geoms[c("project_short_title", "PPTID", "source")])

               if (any(is.na(distinct_rows$project_short_title))) {
                 bad <- which(is.na(distinct_rows$project_short_title))
                 for (i in bad) {
                   message(paste0("for " , i, " source = ", distinct_rows$source[i]))
                   distinct_rows$project_short_title[i] <- distinct_rows$source[i]
                 }
               }

               unique(paste0(distinct_rows$project_short_title, " (", ifelse(is.na(distinct_rows$PPTID),distinct_rows$source,distinct_rows$PPTID), ")"))

             }),

tar_target(plot_files_biodiversity,
           command = save_plots(dplyr::select(ecol_obj_biodiversity_df,-data))),

tar_target(plot_files_habitat,
           command = save_plots(dplyr::select(ecol_obj_habitat_df,-data))),

tar_target(plot_files_productivity,
           command = save_plots(dplyr::select(ecol_obj_productivity_df,-data))),

tar_target(plot_files,
            command = c(plot_files_biodiversity,
                        plot_files_habitat,
                        plot_files_productivity)
            ),

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

tar_target(name = MPA_report_card,
           command = {

             mrc <- left_join(MPAs,pillar_ecol_df |>
                                            filter(indicator %in% MPAs$NAME_E,
                                                   areaID != "Non_Conservation_Area") |>
                                            calc_group_score(grouping_var = "indicator") |>
                                            mutate(grade = if_else(is.nan(score),
                                                                   NA,
                                                                   calc_letter_grade(score))),
                                          by=c("NAME_E"="indicator"))

             mrc

           }),


tar_target(cost_of_mpas,

           # This looks at the number of samples that were collected in our source dataset.
           # A 'sample' is considered a unique lat/lon and year. If multiple samples were
           # taken at the exact same location in the same year, this would only show up once.
           # Additionally, if there is no 'time' column, a 'sample' is simply considered each
           # unique lat and lon. This approach also assumes that we have pulled all data.
           # This should be considered when considering data sets such as AZMP.
           # It takes the number of samples overall and compares it to the number of samples in the MPA

           # Note that we still don't have any pricing information for 'polygon' type sampling.
           # This is because all of our indicators like this use external open data.

           command={
             project_costs <- om |>
               group_by(project_id) |>
               reframe(totalamount = sum(amount))

             all_project_geoms_single_obs_per_row <- all_project_geoms |>
               filter(!is.na(PPTID),
                      areaID!="Non_Conservation_Area") |>
               transform(gtype = as.character(st_geometry_type(geometry))) |>
               (\(x) {
                 rbind(
                   x[x$gtype == "MULTIPOINT", ] |> st_cast("POINT"),
                   x[x$gtype == "MULTILINESTRING", ] |> st_cast("LINESTRING"),
                   x[x$gtype == "MULTIPOLYGON", ] |> st_cast("POLYGON"),
                   x[!grepl("^MULTI", x$gtype), ]
                 )
               })() |>
               subset(select = -gtype) |>
               distinct()



               project_samples_total <- all_project_geoms_single_obs_per_row |>
                 group_by(PPTID) |>
                 reframe(totalsites=n()) |>
                 left_join(project_costs,by = c("PPTID"="project_id")) |>
                 mutate(price_per_station = totalamount/totalsites)

               # cost_of_mpas
               all_project_geoms_single_obs_per_row |>
                 group_by(PPTID,areaID ) |>
                 reframe(sites=n()) |>
                 tidyr::complete(PPTID, areaID = MPAs$NAME_E[MPAs$NAME_E!="Non_Conservation_Area"],fill=list(sites = 0)) |>
                 left_join(project_samples_total,by="PPTID") |>
                 mutate(percent_sites_in_mpa = sites/totalsites) |>
                 rename(area=areaID,
                        project_id=PPTID) |>
                 dplyr::select(project_id, area, percent_sites_in_mpa, price_per_station)

           }
),



tar_target(name = upload_all_data_to_shiny,
            command = {

              if(Sys.getenv("USERPROFILE")=="C:\\Users\\DaigleR"){
                serveruser="rdaigle"
              } else if(Sys.getenv("USERPROFILE")=="C:\\Users\\HarbinJ"){
                serveruser="jharbin"
              } else {
                return(TRUE)
              }

              # Upload the targets folder, and only certain objects needed by the app
              upload_objects <- c("APPTABS", "pillar_ecol_df", "all_project_geoms", "MPA_report_card",
                                  "MPAs", "areas", "regions", "flowerPalette", "indicatorFlower",
                                  "N_Objectives", "om", "Ecological", "Context", "collaborations",
                                  "deliverables", "csas", "climate", "cost_of_mpas","salary",
                                  "theme_table", "objective_tabs", "objective_indicators")


              # Add all targets folder subdirectories
              subdirs <- list.dirs(path_to_store(), full.names = TRUE, recursive = FALSE)


              system(paste0(
                'scp -r -i ',
                file.path(Sys.getenv("USERPROFILE"), '.ssh', 'id_rsa'),
                ' -P 22 ',
                paste(shQuote(subdirs[!grepl("objects",subdirs)]), collapse = ' '),
                ' "', serveruser, '@mar-spa.ent.dfo-mpo.ca:/home/rdaigle/MarConsNetTargets/app_targets" '
              ))

              # Add specific files from objects subdirectory
              system(paste0('ssh -i ',
                            file.path(Sys.getenv("USERPROFILE"),'.ssh','id_rsa'),
                            ' -p 22 ',
                            serveruser,
                            '@mar-spa.ent.dfo-mpo.ca "mkdir -p /home/rdaigle/MarConsNetTargets/app_targets/objects"'))


              object_files <- file.path(path_to_store(), "objects", upload_objects)

              system(paste0(
                'scp -i ',
                file.path(Sys.getenv("USERPROFILE"), '.ssh', 'id_rsa'),
                ' -P 22 ',
                paste(shQuote(object_files), collapse = ' '),
                ' "', serveruser, '@mar-spa.ent.dfo-mpo.ca:/home/rdaigle/MarConsNetTargets/app_targets/objects/" '
              ))

              # Create and upload data folder
              system(paste0('ssh -i ',
                            file.path(Sys.getenv("USERPROFILE"),'.ssh','id_rsa'),
                            ' -p 22 ',
                            serveruser,
                            '@mar-spa.ent.dfo-mpo.ca "mkdir -p /home/rdaigle/MarConsNetTargets/data"'))


              # copy data files
              datafiles <- list.files(file.path(dirname(path_to_store()),'data'),include.dirs = FALSE)
              datadirs <- list.dirs(file.path(dirname(path_to_store()),'data'),recursive = FALSE, full.names = FALSE)
              for(f in datafiles[!(datafiles %in% datadirs)]){
                system(paste0('scp -i ',
                              file.path(Sys.getenv("USERPROFILE"),'.ssh','id_rsa'),
                              ' -P 22 "',
                              file.path(dirname(path_to_store()),'data',f),
                              '" "',
                              serveruser,
                              '@mar-spa.ent.dfo-mpo.ca:/home/rdaigle/MarConsNetTargets/data/'))
              }


              # copy data directories
              for(f in datadirs[datadirs %in% c("reports","plots")]){
                system(paste0('scp -r -i ',
                              file.path(Sys.getenv("USERPROFILE"),'.ssh','id_rsa'),
                              ' -P 22 "',
                              file.path(dirname(path_to_store()),'data',f),
                              '" "',
                              serveruser,
                              '@mar-spa.ent.dfo-mpo.ca:/home/rdaigle/MarConsNetTargets/data/'))
              }





              TRUE
            },
            deployment = "main",
            priority = 0,
            cue = tar_cue(mode = "always")),

tar_target(windowsdependenttargets,
           command = {
             rv_data
             rv_data_det
             om
             csas
             deliverables
             collaborations
             salary
             return(TRUE)
           })


) |>
  unlist(recursive = FALSE)
