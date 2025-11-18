source("inst/set_up.R")


raw_data_targets <- list(

  # Areas ----

  tar_target(control_polygons,
             command= {
               MPAs
               # sf::sf_use_s2(FALSE)

               mpa_combined <- MPAs |>
                 filter(NAME_E != "Non_Conservation_Area") |>
                 st_union() |>
                 st_as_sf() |>
                 st_make_valid()

               cp <- MPAs |>
                 filter(NAME_E!="Non_Conservation_Area") |>
                 rowwise() |>
                 mutate(twenty_km = st_difference(st_buffer(geoms,20000),geoms),
                        forty_km = st_difference(st_buffer(geoms,40000), st_buffer(geoms, 20000)),
                        sixty_km=st_difference(st_buffer(geoms, 60000), st_buffer(geoms, 40000)),
                        eighty_km=st_difference(st_buffer(geoms, 80000), st_buffer(geoms, 60000))) |>
                 ungroup()|>
                 as.data.frame() |>
                 dplyr::select(-geoms) |>
                 pivot_longer(cols=ends_with("_km"),
                              names_to = "buffer_distance",
                              values_to = "geoms")|>
                 st_as_sf()|>
                 st_make_valid() |>
                 st_difference(mpa_combined)|>
                 st_make_valid()
             }),

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

  # DMAPPS ----

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

             },
             deployment = "worker"),

  tar_target(name=salary,
             command={
               SAL <- dataSPA::getData(type="salary", age=3000, cookie=cookie,
                                       path = file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","data"))

               SAL[-(which(SAL$activity_type == "Other")),]

             },
             deployment = "worker"),


  tar_target(name = collaborations,
             command = {
               cookie
               col <- dataSPA::getData(type='collaboration', cookie=cookie)
             },
             deployment = "worker"),

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
             },
             deployment = "worker"),

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
             },
             deployment = "worker"),


  # INDICATOR DATA ----

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
  },
  deployment = "worker"
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
  },
  deployment = "worker"
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

             })

)
