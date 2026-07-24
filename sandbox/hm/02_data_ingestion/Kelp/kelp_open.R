
##Open Data (in situ)

library(tidyverse)
library(rvest)
library(stringr)

occurrence <- read_csv("https://api-proxy.edh-cde.dfo-mpo.gc.ca/catalogue/records/f1a022a4-b9bf-47d0-b641-2067ea568962/attachments/Occurrence.csv")
event <- read_csv("https://api-proxy.edh-cde.dfo-mpo.gc.ca/catalogue/records/f1a022a4-b9bf-47d0-b641-2067ea568962/attachments/Event.csv")
measurements <- read_csv("https://api-proxy.edh-cde.dfo-mpo.gc.ca/catalogue/records/f1a022a4-b9bf-47d0-b641-2067ea568962/attachments/extendedMeasurementOrFact.csv")


species_data <- occurrence %>%
  left_join(
    event %>%
      select(
        eventID,
        decimalLatitude,
        decimalLongitude
      ),
    by = "eventID"
  ) %>%
  left_join(
    measurements %>%
      filter(
        !is.na(occurrenceID),
        measurementType == "mean percent cover"
      ) %>%
      select(
        occurrenceID,
        measurementValue
      ) %>%
      rename(
        percent_cover = measurementValue
      ),
    by = "occurrenceID"
  )


depth_data <- measurements %>%
  filter(
    measurementType == "mean corrected depth",
    !is.na(eventID)
  ) %>%
  select(
    eventID,
    depth = measurementValue
  )

combined_data <- species_data %>%
  left_join(depth_data, by = "eventID")


combined_data <- combined_data %>%
  select(
    occurrenceID,
    occurrenceStatus,
    eventID,
    eventDate,
    scientificNameID,
    scientificName,
    decimalLatitude,
    decimalLongitude,
    percent_cover,
    depth
  )

##PROCESS INDICATOR

data <- combined_data
data$occurrence_id <-as.character(combined_data$occurrenceID)
data$occurrence_status <-as.character(combined_data$occurrenceStatus)
data$latitude <- as.numeric(combined_data$decimalLatitude)
data$longitude <- as.numeric(combined_data$decimalLongitude)
data$year_of_data_collection <- as.numeric(format(combined_data$eventDate, "%Y"))
data$percent_cover <- as.numeric(combined_data$percent_cover)
data$depth <- as.numeric(combined_data$depth)


data <- data[, c(
  "occurrence_id",
  "occurrence_status",
  "latitude",
  "longitude",
  "year_of_data_collection",
  "percent_cover",
  "depth"
)]

data$year_of_publication <- {

  url <- "https://open.canada.ca/data/en/dataset/f1a022a4-b9bf-47d0-b641-2067ea568962/resource/a47f5b93-9424-48a5-a65c-5fd5c6bc2b70"
  page <- read_html(url)
  page_text <- html_text2(page)

  page_text |>
    str_extract("(?<=Data last updated )\\w+ \\d{1,2}, \\d{4}") |>
    str_extract("\\d{4}")
}


x <- process_indicator(
  data = data,
  indicator_var_name = "ind_distinctive_benthic_characteristics",      ##This would replace the St Anns Bank placholder indicator
  indicator = "Diversity and community composition of the benthos and characteristics of surficial geology at selected sampling stations located in the identified distinctive seabed features of the AOI, plus abundance or biomass and size composition of the defining benthic taxa of those features",
  type = "in situ",
  units = " ",
  scoring = "coverage",
  source = "Open Data (DFO)",
  project_short_title = " Camera surveys of the Subtidal Flora",
  areas = MPAs,
  climate_expectation = "FIXME",
  indicator_rationale = "Kelp forests support high biodiversity and productivity, and provide ecosystem services",
  bin_rationale = "FIXME",
  plot_type = c("time-series", "map"),
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


##Fun Plots
library(ggplot2)

ggplot(data, aes(x = longitude, y = latitude)) +
  geom_point(aes(size = precent_cover, colour = occurrence_status)) +
  theme_minimal() +
  labs(
    title = "Distribution of Macroalgae in Nova Scotia and SW New Brunswick",
    x = "Longitude",
    y = "Latitude",
    size = "Percent cover",
    colour = "Occurrence status"
  )


###One Drive data (modeled)

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
cur_sites_ld <- project(sites, crs(l_digitata_current))

#get all raster values in each CS
cur_site_values_ld <- extract(
  l_digitata_current,
  cur_sites_ld
)

# Add the corresponding site names to the extracted values
cur_site_values_ld$SiteName_E <- cur_sites_ld$SiteName_E[cur_site_values_ld$ID]

##calculates the summary stats for each site
cur_site_summary_ld <- cur_site_values_ld %>%
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

fut_sites_ld <- project(sites, crs(l_digitata_future))

fut_site_values_ld <- extract(
  l_digitata_future,
  sites
)

fut_site_values_ld$SiteName_E <- fut_sites_ld$SiteName_E[fut_site_values_ld$ID]

fut_site_summary_ld <- fut_site_values_ld %>%
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
percent_change_ld <- cur_site_summary_ld %>%
  select(
    SiteName_E,
    cur_occurrence_percent_ld
  ) %>%
  left_join(
    fut_site_summary_ld %>%
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
cur_sites_sl <- project(sites, crs(s_latissima_current))

cur_site_values_sl <- extract(
  s_latissima_current,
  cur_sites_sl
)

cur_site_values_sl$SiteName_E <- cur_sites_sl$SiteName_E[cur_site_values_sl$ID]


cur_site_summary_sl <- cur_site_values_sl %>%
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
fut_sites_sl <- project(sites, crs(s_latissima_future))

fut_site_values_sl <- extract(
  s_latissima_future,
  sites
)

fut_site_values_sl$SiteName_E <- fut_sites_sl$SiteName_E[fut_site_values_sl$ID]

fut_site_summary_sl <- fut_site_values_sl %>%
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
percent_change_sl <- cur_site_summary_sl %>%
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















