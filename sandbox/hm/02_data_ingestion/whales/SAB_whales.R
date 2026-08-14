

#---------------------------
#  SIGHTINGS - COVERAGE
#---------------------------

wsdb_sightings <- read.csv(
  file.path(
    store,
    "..",
    "..",
    "MarConsNetTargets",
    "data",
    "whales",
    "WSDB_sightings_StAnnsBankMPA.csv"
  )
)

sightings <- wsdb_sightings
sightings$latitude <- as.numeric(wsdb_sightings$lat)
sightings$longitude <- as.numeric(wsdb_sightings$lon)
sightings <- sightings %>%
  rename(year_of_data_collection = ws_date) %>%
  mutate(year_of_data_collection = as.numeric(substr(year_of_data_collection, 1, 4)))
sightings <- sightings %>%
  filter(!is.na(scientific_name))
data <- st_as_sf(
  sightings,
  coords = c("longitude", "latitude"),
  crs = 4326
)
data <- data %>%
  rename(geoms = geometry,
         scientificName = scientific_name)

data_wide <- data %>%
  st_join(
    MPAs %>% select(NAME_E),
    join = st_intersects
  ) %>%
  st_drop_geometry() %>%
  count(NAME_E, year_of_data_collection, scientificName) %>%
  pivot_wider(
    id_cols = c(NAME_E, year_of_data_collection),
    names_from = scientificName,
    values_from = n,
    values_fill = 0
  )


# CALC_SAMPLE_COVERAGE

mpa_names <- unique(MPAs$NAME_E)

coverage_results <- data_wide %>%
  group_by(NAME_E) %>%
  group_modify(~ {

    mpa_data <- .x %>%
      arrange(year_of_data_collection) %>%
      select(-year_of_data_collection)

    result <- calc_sample_coverage(mpa_data)

    data.frame(
      sample_ID = result$N,           #the are 14 values for the 14 years of data
      SC = result$means
    )

  }) %>%
  ungroup()


# Final SC for each MPA
final_SC <- coverage_results %>%
  group_by(NAME_E) %>%
  slice_tail(n = 1) %>%
  ungroup()


#------------------------------
# SIGHTINGS - REPRESENTATION
#------------------------------

wsdb_sightings <- read.csv(
  file.path(
    store,
    "..",
    "..",
    "MarConsNetTargets",
    "data",
    "whales",
    "WSDB_sightings_StAnnsBankMPA.csv"
  )
)

sightings <- wsdb_sightings
sightings$latitude <- as.numeric(wsdb_sightings$lat)
sightings$longitude <- as.numeric(wsdb_sightings$lon)
sightings <- sightings %>%
  rename(year_of_data_collection = ws_date) %>%
  mutate(year_of_data_collection = as.numeric(substr(year_of_data_collection, 1, 4)))
sightings <- sightings %>%
  filter(!is.na(scientific_name))
data <- st_as_sf(
  sightings,
  coords = c("longitude", "latitude"),
  crs = 4326
)
data <- data %>%
  rename(geoms = geometry,
         scientificName = scientific_name)

file_info <- file.info(file.path(
  store,
  "..",
  "..",
  "MarConsNetTargets",
  "data",
  "whales",
  "WSDB_sightings_StAnnsBankMPA.csv"
))

year_of_publication <- format(file_info$mtime, "%Y")

data$year_of_publication <- as.numeric(format(file.info((file.path(
  store,
  "..",
  "..",
  "MarConsNetTargets",
  "data",
  "whales",
  "WSDB_sightings_StAnnsBankMPA.csv"
)))$mtime, "%Y"))



x <- process_indicator(
  data = data,
  indicator_var_name = "year_of_data_collection",
  indicator = "Cetacean presence and activity in the MPA, year-round",
  type = "in situ",
  units = NA,
  scoring = "desired state: increase",
  PPTID = NA,
  source = "GitHub",
  project_short_title = "WSDB Occurrences",
  bin_rationale = "FIXME",
  climate_expectation = "FIXME",
  indicator_rationale = "FIXME",
  SME = "Unknown",
  areas = MPAs,
  plot_type = 'map-species',
  plot_lm = FALSE,
  theme = "Marine Mammals and Other Top Predators",
  objectives = c(
    "Maintain biodiversity of individual species, communities and populations within the different ecotypes")
)



#---------------------------
#     PAM
#---------------------------

load(
  file.path(
    store,
    "..",
    "..",
    "MarConsNetTargets",
    "data",
    "whales",
    "whaleAcoustic2.RData"
  )
)

data <- whaleAcousticsdata2

data <- data %>%
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326
  )

data_wide <- data %>%
  st_join(
    MPAs %>% select(NAME_E),      #Unsure if this can be used for richness_sample_coverage as the different rows for St anns bank are not years, but different stations
    join = st_intersects,
    left = TRUE
  ) %>%
  st_drop_geometry()

x <- process_indicator(
  data = data,
  indicator_var_name = "n_days_present",
  indicator = "Cetacean presence and activity in the MPA, year-round",
  type = "remote sensing",
  units = NA,
  scoring = "",
  PPTID = NA,
  source = "GitHub",
  project_short_title = "PAM",
  bin_rationale = "FIXME",
  climate_expectation = "FIXME",
  indicator_rationale = "FIXME",
  SME = "Unknown",
  areas = MPAs,
  plot_type = 'map-species',
  plot_lm = FALSE,
  theme = "Marine Mammals and Other Top Predators",
  objectives = c(
    "Maintain biodiversity of individual species, communities and populations within the different ecotypes")
)



#---------------------------
#     PAM - COVERAGE
#---------------------------

am_results <- read.csv(
  file.path(
    store,
    "..",
    "..",
    "MarConsNetTargets",
    "data",
    "whales",
    "Acoustic_monitoring_results_StAnnsBankMPA_2024-03-01.csv"
  )
)

data <- am_results

data <- st_as_sf(
  data,
  coords = c("longitude", "latitude"),
  crs = 4326
)
data <- data %>%
  rename(geoms = geometry,
         scientificName = scientific_name)


year_of_publication <- format(file_info$mtime, "%Y")

data$year_of_publication <- as.numeric(format(file.info((file.path(
  store,
  "..",
  "..",
  "MarConsNetTargets",
  "data",
  "whales",
  "Acoustic_monitoring_results_StAnnsBankMPA_2024-03-01.csv"
)))$mtime, "%Y"))

x <- process_indicator(
  data = data,
  indicator_var_name = "n_days_present",
  indicator = "Cetacean presence and activity in the MPA, year-round",
  type = "remote sensing",
  units = NA,
  scoring = "",
  PPTID = NA,
  source = "GitHub",
  project_short_title = "PAM",
  bin_rationale = "FIXME",
  climate_expectation = "FIXME",
  indicator_rationale = "FIXME",
  SME = "Unknown",
  areas = MPAs,
  plot_type = 'map-species',
  plot_lm = FALSE,
  theme = "Marine Mammals and Other Top Predators",
  objectives = c(
    "Maintain biodiversity of individual species, communities and populations within the different ecotypes")
)















