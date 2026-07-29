
#-----------------------------------
# KELP DATA TARGET

tar_target(name = kelp_data, command = {

  onedrive <- Sys.getenv("OneDriveCommercial")

  raster_folder <- file.path(
    onedrive,
    "Krumhansl, Kira (DFO_MPO)'s files - 2021 2024 Species Distribution Model Outputs"
  )

  l_digitata_current <- rast(
    file.path(
      raster_folder,
      "Laminaria digitata",
      "Laminaria_digitata_Bathy_rm4_20240223_avg_Binary.tif"
    )
  )

  s_latissima_current <- rast(
    file.path(
      raster_folder,
      "Saccharina latissima",
      "Saccharina_latissima_Bathy_rm2MinusRugosityAndProfile_20240223_avg_Binary.tif"
    )
  )

  l_digitata_poly <- as.polygons(
    l_digitata_current == 1,
    aggregate = TRUE
  ) |>
    st_as_sf() %>%
    mutate(species = "Laminaria digitata",
           suitable_habitat = Laminaria_digitata_Bathy_rm4_20240223_avg_Binary == 1,
           habitat_type = "kelp") |>
    # filter(Laminaria_digitata_Bathy_rm4_20240223_avg_Binary == 1) |>
    select(species,suitable_habitat,habitat_type)


  s_latissima_poly <- as.polygons(
    s_latissima_current == 1,
    aggregate = TRUE
  ) |>
    st_as_sf() %>%
    mutate(species = "Saccharina latissima",
           suitable_habitat = Saccharina_latissima_Bathy_rm2MinusRugosityAndProfile_20240223_avg_Binary == 1,
           habitat_type = "kelp") |>
    select(species,suitable_habitat,habitat_type)

  kelp <- bind_rows(s_latissima_poly,l_digitata_poly)

})

#----------------------------------
#  INDICATOR 1
#----------------------------------

tar_target(name = ind_distinctive_benthic_characteristics, command = {

  # data <- kelp %>%
  #   filter(suitable_habitat)

data <- kelp %>%
    filter(suitable_habitat) %>%
    select(suitable_habitat, habitat_type, geometry) %>%
    group_by(suitable_habitat, habitat_type) %>%
    summarise(geometry = st_union(geometry), .groups = "drop")

data$year_of_publication <- {

    ld_path <- file.path(
      onedrive,
      "Krumhansl, Kira (DFO_MPO)'s files - 2021 2024 Species Distribution Model Outputs",
      "Laminaria digitata",
      "Laminaria_digitata_Bathy_rm4_20240223_avg_Binary.tif"
    )

    sl_path <- file.path(
      onedrive,
      "Krumhansl, Kira (DFO_MPO)'s files - 2021 2024 Species Distribution Model Outputs",
      "Saccharina latissima",
      "Saccharina_latissima_Bathy_rm2MinusRugosityAndProfile_20240223_avg_Binary.tif"
    )

    # Get modified dates
    file_dates <- file.info(c(ld_path, sl_path))$mtime
    format(max(file_dates), "%Y")
}

data$min_target <- 30
 data$max_target <- 100
 data$plainname <- 'the total modelled kelp region'

mpas <- MPAs %>%
  st_filter(data) %>%
  filter(NAME_E != "Non_Conservation_Area")

  x <- process_indicator(
    data = data,
    indicator_var_name = "suitable_habitat",      ##This would replace the St Anns Bank placholder indicator
    indicator = "Diversity and community composition of the benthos and characteristics of surficial geology at selected sampling stations located in the identified distinctive seabed features of the AOI, plus abundance or biomass and size composition of the defining benthic taxa of those features",
    type = "model",
    units = "percent area",
    scoring = "coverage",
    PPTID = 1633,
    source = "OneDrive",
    project_short_title = "Species distribution models of kelp and non-indigenous macroalgae in the DFO Maritimes region",
    areas = mpas,
    climate_expectation = "FIXME",
    indicator_rationale = "Kelp forests support high biodiversity and productivity, and provide ecosystem services",
    bin_rationale = "FIXME",
    plot_type = "map",
    year = 'year_of_data_collection',
    objectives = c(
      "Protect unique, rare, or sensitive ecological features",
      "Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
      "Habitat required for all species, particularly priority species, is maintained and protected"
    ),
    theme = "Benthic Environment",
    externalData = NULL,
    scale = "region-site",
    SME = "Kira Krumhansl",
    control_polygon = NA,
    plot_lm = FALSE
  )

  save_plots(dplyr::select(x, -data, -adjacent_data))
  dplyr::select(x, -plot)


#----------------------------------
# PLOTS
#----------------------------------


 terra::plot(
    s_latissima_current,
    main = "Predicted Occurence of Saccharina latissima",
    xlab = "Longitude",
    ylab = "Latitude",
    col = c("white", "purple"),
    axes = TRUE,
    legend = TRUE
  )

  plot(
    sites,
    add = TRUE,
    col = NA,
    border = "black",
    lwd = 1.5
  )

})



#-----------------------------
# PRACTICE
#-----------------------------



library(terra)

##get the data
onedrive <- Sys.getenv("OneDriveCommercial")


raster_path <- file.path(
  onedrive,
  "Krumhansl, Kira (DFO_MPO)'s files - 2021 2024 Species Distribution Model Outputs",
  "Projections",
  "Laminaria_digitata_Bathy_FutureProjection_20240228_Binary.tif"
)

l_digitata_future <- rast(raster_path)


raster_path2 <- file.path(
  onedrive,
  "Krumhansl, Kira (DFO_MPO)'s files - 2021 2024 Species Distribution Model Outputs",
  "Laminaria digitata",
  "Laminaria_digitata_Bathy_rm4_20240223_avg_Binary.tif"
)

l_digitata_current<- rast(raster_path2)


raster_path3 <- file.path(
  onedrive,
  "Krumhansl, Kira (DFO_MPO)'s files - 2021 2024 Species Distribution Model Outputs",
  "Projections",
  "Saccharina_latissima_Bathy_FutureProjection_20240228_Binary.tif"
)

s_latissima_future <- rast(raster_path3)

raster_path4 <- file.path(
  onedrive,
  "Krumhansl, Kira (DFO_MPO)'s files - 2021 2024 Species Distribution Model Outputs",
  "Saccharina latissima",
  "Saccharina_latissima_Bathy_rm2MinusRugosityAndProfile_20240223_avg_Binary.tif"
)

s_latissima_current<- rast(raster_path4)



library(sf)
library(dplyr)

url <- "https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/draft_conservation_network_sites/MapServer/0/query?where=1=1&outFields=*&f=geojson"
conservation_network <- st_read(url)
#change this into a terra spatvector to be more compatible with the raster data
sites <- vect(conservation_network)



# CURRENT PREDICTED OCCURRENCE L. DIGITATA

##Ensure the same coordinate system
cur_ld <- project(sites, crs(l_digitata_current))

#get all raster values in each CS
cur_values_ld <- terra::extract(
  l_digitata_current,
  cur_ld
)

# Add the corresponding site names to the extracted values
cur_values_ld$SiteName_E <- cur_ld$SiteName_E[cur_values_ld$ID]

##calculates the summary stats for each site
cur_summary_ld <- cur_values_ld %>%
  group_by(SiteName_E) %>%
  summarise(
    cur_occurrence_cells_ld = sum(
      Laminaria_digitata_Bathy_rm4_20240223_avg_Binary == 1,
      na.rm = TRUE
    ),
    cur_valid_cells_ld = sum(
      !is.na(Laminaria_digitata_Bathy_rm4_20240223_avg_Binary)
    ),
    cur_occurrence_percent_ld = mean(
      Laminaria_digitata_Bathy_rm4_20240223_avg_Binary,
      na.rm = TRUE
    ) * 100
  )

## FUTURE PREDICTED OCCURRENCE L. DIGITATA

fut_ld <- project(sites, crs(l_digitata_future))

fut_values_ld <- terra::extract(
  l_digitata_future,
  sites
)

fut_values_ld$SiteName_E <- fut_ld$SiteName_E[fut_values_ld$ID]

fut_summary_ld <- fut_values_ld %>%
  group_by(SiteName_E) %>%
  summarise(
    fut_occurrence_cells_ld = sum(
      Laminaria_digitata_Bathy_FutureProjection_20240228_Binary == 1,
      na.rm = TRUE
    ),
    fut_valid_cells_ld = sum(
      !is.na(Laminaria_digitata_Bathy_FutureProjection_20240228_Binary)
    ),
    fut_occurrence_percent_ld = mean(
      Laminaria_digitata_Bathy_FutureProjection_20240228_Binary,
      na.rm = TRUE
    ) * 100
  )

###PERCENT CHANGE OVER TIME L. DIGITATA
percent_change_ld <- cur_summary_ld %>%
  select(
    SiteName_E,
    cur_occurrence_percent_ld
  ) %>%
  left_join(
    fut_summary_ld %>%
      select(
        SiteName_E,
        fut_occurrence_percent_ld
      ),
    by = "SiteName_E"
  ) %>%
  mutate(
    percent_change_ld = if_else(
      cur_occurrence_percent_ld == 0,
      NA_real_,
      (
        (fut_occurrence_percent_ld - cur_occurrence_percent_ld) /
          cur_occurrence_percent_ld
      ) * 100
    )
  )

# CURRENT PREDICTED OCCURRENCE S. LATISSIMA
cur_sl <- project(sites, crs(s_latissima_current))

cur_values_sl <- terra::extract(
  s_latissima_current,
  cur_sl
)

cur_values_sl$SiteName_E <- cur_sl$SiteName_E[cur_values_sl$ID]

options(scipen = 999)

cur_summary_sl <- cur_values_sl %>%
  group_by(SiteName_E) %>%
  summarise(
    cur_occurrence_cells_sl = sum(
      Saccharina_latissima_Bathy_rm2MinusRugosityAndProfile_20240223_avg_Binary == 1,
      na.rm = TRUE
    ),
    cur_valid_cells_sl = sum(
      !is.na(Saccharina_latissima_Bathy_rm2MinusRugosityAndProfile_20240223_avg_Binary)
    ),
    cur_occurrence_percent_sl = mean(
      Saccharina_latissima_Bathy_rm2MinusRugosityAndProfile_20240223_avg_Binary,
      na.rm = TRUE
    ) * 100
  )


## FUTURE PREDICTED OCCURRENCE S. LATISSIMA
fut_sl <- project(sites, crs(s_latissima_future))

fut_values_sl <- terra::extract(
  s_latissima_future,
  sites
)

fut_values_sl$SiteName_E <- fut_sl$SiteName_E[fut_values_sl$ID]

fut_summary_sl <- fut_values_sl %>%
  group_by(SiteName_E) %>%
  summarise(
    fut_occurrence_cells_sl = sum(
      Saccharina_latissima_Bathy_FutureProjection_20240228_Binary == 1,
      na.rm = TRUE
    ),
    fut_valid_cells_sl = sum(
      !is.na(Saccharina_latissima_Bathy_FutureProjection_20240228_Binary)
    ),
    fut_occurrence_percent_sl = mean(
      Saccharina_latissima_Bathy_FutureProjection_20240228_Binary,
      na.rm = TRUE
    ) * 100
  )

###PERCENT CHANGE OVER TIME S. LATISSIMA
percent_change_sl <- cur_summary_sl %>%
  select(
    SiteName_E,
    cur_occurrence_percent_sl
  ) %>%
  left_join(
    fut_site_summary_sl %>%
      select(
        SiteName_E,
        fut_occurrence_percent_sl
      ),
    by = "SiteName_E"
  ) %>%
  mutate(
    percent_change_sl = if_else(
      cur_occurrence_percent_sl == 0,
      NA_real_,
      (
        (fut_occurrence_percent_sl - cur_occurrence_percent_sl) /
          cur_occurrence_percent_sl
      ) * 100
    )
  )


combined_summary <- cur_summary_sl %>%
  dplyr::select(SiteName_E, cur_occurrence_percent_sl) %>%
  left_join(
    cur_summary_ld %>%
      dplyr::select(SiteName_E, cur_occurrence_percent_ld),
    by = "SiteName_E"
  )



######___________________________________________________________________

onedrive <- Sys.getenv("OneDriveCommercial")

raster_folder <- file.path(
  onedrive,
  "Krumhansl, Kira (DFO_MPO)'s files - 2021 2024 Species Distribution Model Outputs"
)

l_digitata_current <- rast(
  file.path(
    raster_folder,
    "Laminaria digitata",
    "Laminaria_digitata_Bathy_rm4_20240223_avg_Binary.tif"
  )
)

s_latissima_current <- rast(
  file.path(
    raster_folder,
    "Saccharina latissima",
    "Saccharina_latissima_Bathy_rm2MinusRugosityAndProfile_20240223_avg_Binary.tif"
  )
)


# 🔴 EDIT LOCATION 1:
# Convert binary SDM rasters into suitable habitat polygons

l_digitata_poly <- as.polygons(
  l_digitata_current == 1,
  aggregate = TRUE
) |>
  st_as_sf() %>%
  mutate(species = "Laminaria digitata",
         suitable_habitat = Laminaria_digitata_Bathy_rm4_20240223_avg_Binary == 1,
         habitat_type = "kelp") |>
  # filter(Laminaria_digitata_Bathy_rm4_20240223_avg_Binary == 1) |>
  select(species,suitable_habitat,habitat_type)




s_latissima_poly <- as.polygons(
  s_latissima_current == 1,
  aggregate = TRUE
) |>
  st_as_sf() %>%
  mutate(species = "Saccharina latissima",
         suitable_habitat = Saccharina_latissima_Bathy_rm2MinusRugosityAndProfile_20240223_avg_Binary == 1,
         habitat_type = "kelp") |>
  select(species,suitable_habitat,habitat_type)

kelp <- bind_rows(s_latissima_poly,l_digitata_poly)

url <- paste0(
  "https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/",
  "open_data_donnees_ouvertes/draft_conservation_network_sites/",
  "MapServer/0/query?",
  "where=1=1&outFields=*&f=geojson"
)

conservation_network <- st_read(url)


# 🔴 EDIT LOCATION 2:
# Keep sites as sf instead of converting to terra vect()
# ind_coverage() requires sf geometry

sites <- conservation_network



# 🔴 EDIT LOCATION 3:
# Replace extract_site_summary() with a polygon coverage function

calculate_sdm_coverage <- function(sdm_polygon, sites){

  sites %>%
    rowwise() %>%
    mutate(
      occurrence_percent = ind_coverage(
        x = sdm_polygon,
        y = st_sf(geometry = geometry),
        intersection = TRUE
      )
    ) %>%
    ungroup()
}



# 🔴 EDIT LOCATION 4:
# Calculate coverage scores from polygons

cur_ld <- calculate_sdm_coverage(
  l_digitata_poly,
  sites
) %>%
  select(
    SiteName_E,
    cur_occurrence_percent_ld = occurrence_percent
  )


cur_sl <- calculate_sdm_coverage(
  s_latissima_poly,
  sites
) %>%
  select(
    SiteName_E,
    cur_occurrence_percent_sl = occurrence_percent
  )



data <- cur_sl %>%
  left_join(
    cur_ld,
    by = "SiteName_E"
  )



data$year_of_publication <- {

  ld_path <- file.path(
    onedrive,
    "Krumhansl, Kira (DFO_MPO)'s files - 2021 2024 Species Distribution Model Outputs",
    "Laminaria digitata",
    "Laminaria_digitata_Bathy_rm4_20240223_avg_Binary.tif"
  )

  sl_path <- file.path(
    onedrive,
    "Krumhansl, Kira (DFO_MPO)'s files - 2021 2024 Species Distribution Model Outputs",
    "Saccharina latissima",
    "Saccharina_latissima_Bathy_rm2MinusRugosityAndProfile_20240223_avg_Binary.tif"
  )

  file_dates <- file.info(c(ld_path, sl_path))$mtime
  format(max(file_dates), "%Y")
}


onedrive <- Sys.getenv("OneDriveCommercial")

raster_folder <- file.path(
  onedrive,
  "Krumhansl, Kira (DFO_MPO)'s files - 2021 2024 Species Distribution Model Outputs"
)

l_digitata_current <- rast(
  file.path(
    raster_folder,
    "Laminaria digitata",
    "Laminaria_digitata_Bathy_rm4_20240223_avg_Binary.tif"
  )
)

s_latissima_current <- rast(
  file.path(
    raster_folder,
    "Saccharina latissima",
    "Saccharina_latissima_Bathy_rm2MinusRugosityAndProfile_20240223_avg_Binary.tif"
  )
)

url <- paste0(
  "https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/",
  "open_data_donnees_ouvertes/draft_conservation_network_sites/",
  "MapServer/0/query?",
  "where=1=1&outFields=*&f=geojson"
)

conservation_network <- st_read(url)
sites <- vect(conservation_network)

extract_site_summary <- function(raster, sites, raster_column, output_name) {

  sites_proj <- project(sites, crs(raster))
  values <- terra::extract(raster, sites_proj)
  values$SiteName_E <- sites_proj$SiteName_E[values$ID]

  summary <- values %>%
    group_by(SiteName_E) %>%
    summarise(
      !!output_name := mean(.data[[raster_column]], na.rm = TRUE) * 100
    )

  return(summary)
}

cur_ld <- extract_site_summary(
  l_digitata_current,
  sites,
  "Laminaria_digitata_Bathy_rm4_20240223_avg_Binary",
  "cur_occurrence_percent_ld"
)

cur_sl <- extract_site_summary(
  s_latissima_current,
  sites,
  "Saccharina_latissima_Bathy_rm2MinusRugosityAndProfile_20240223_avg_Binary",
  "cur_occurrence_percent_sl"
)

data <- cur_sl %>%
  left_join(
    cur_ld,
    by = "SiteName_E"
  )


data$year_of_publication <- {

  ld_path <- file.path(
    onedrive,
    "Krumhansl, Kira (DFO_MPO)'s files - 2021 2024 Species Distribution Model Outputs",
    "Laminaria digitata",
    "Laminaria_digitata_Bathy_rm4_20240223_avg_Binary.tif"
  )

  sl_path <- file.path(
    onedrive,
    "Krumhansl, Kira (DFO_MPO)'s files - 2021 2024 Species Distribution Model Outputs",
    "Saccharina latissima",
    "Saccharina_latissima_Bathy_rm2MinusRugosityAndProfile_20240223_avg_Binary.tif"
  )

  # Get modified dates
  file_dates <- file.info(c(ld_path, sl_path))$mtime
  format(max(file_dates), "%Y")
}

x <- process_indicator(
  data = data,
  indicator_var_name = "ind_distinctive_benthic_characteristics",      ##This would replace the St Anns Bank placholder indicator
  indicator = "Diversity and community composition of the benthos and characteristics of surficial geology at selected sampling stations located in the identified distinctive seabed features of the AOI, plus abundance or biomass and size composition of the defining benthic taxa of those features",
  type = "model",
  units = " ",
  scoring = "coverage",
  PPTID = NA,
  source = "OneDrive",
  project_short_title = "Species distribution models of kelp and non-indigenous macroalgae in the DFO Maritimes region",
  areas = MPAs,
  climate_expectation = "FIXME",
  indicator_rationale = "Kelp forests support high biodiversity and productivity, and provide ecosystem services",
  bin_rationale = "FIXME",
  plot_type = "map",
  year = 'year_of_data_collection',
  objectives = c(
    "Protect unique, rare, or sensitive ecological features",
    "Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
    "Habitat required for all species, particularly priority species, is maintained and protected"
  ),
  theme = "Benthic Environment",
  SME = "Unknown",
  control_polygon = control_polygons,
  plot_lm = FALSE
)

save_plots(dplyr::select(x, -data, -adjacent_data))
dplyr::select(x, -plot)



## PLOTS


levels(s_latissima_current) <- data.frame(
  value = c(0, 1),
  occurrence = c("Non-occurrence", "Predicted occurrence")
)

terra::plot(
  s_latissima_current,
  main = "Predicted Occurence of Saccharina latissima",
  xlab = "Longitude",
  ylab = "Latitude",
  col = c("white", "purple"),
  axes = TRUE,
  legend = TRUE
)

plot(
  sites,
  add = TRUE,
  col = NA,
  border = "black",
  lwd = 1.5
)











