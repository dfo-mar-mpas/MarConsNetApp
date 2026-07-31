

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

load(
  file.path(
    store,
    "..",
    "..",
    "MarConsNetTargets",
    "data",
    "whales",
    "whaledata2.RData"
  )
)


sightings <- wsdb_sightings
sightings$latitude <- as.numeric(wsdb_sightings$lat)
sightings$longitude <- as.numeric(wsdb_sightings$lon)
sightings <- sightings %>%
  rename(year_of_data_collection = ws_date) %>%
  mutate(year_of_data_collection = as.numeric(substr(year_of_data_collection, 1, 4)))








