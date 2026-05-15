

## Pulling the data from the Open Data platform
library(sf)

url <- "https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/grey_seal_pup_production_in_canadian_waters/MapServer/0/query?where=1=1&outFields=*&f=geojson"
seal_sf <- st_read(url)

seal_sf


## Raw data target
tar_target(name = seal_sf, command = {
  library(marea)
  data(seal_pups)
  return(seal_pups)
})

# data cleaning?
data <- seal_sf
data$year_of_data_collection <- as.numeric(seal_sf$Year_Année)
data$number_of_seals <- as.numeric(seal_sf$Pups_Count__Chiots__nombre_)
data$latitude <- as.numeric(seal_sf$Latitude)
data$longiude <- as.numeric(seal_sf$Longitude)
data <- data[, c(
  "year_of_data_collection",
  'latitude',
  'longitude',
  'number_of_seals'
)]

# Just the process_indicator function
x <- process_indicator(
  data = seal_sf,
  indicator_var_name = "number_of_seals",
  indicator = "Number of Seals",
  type = "remote sensing",
  units = " ",
  scoring = "desired state: increase",
  PPTID = 859,
  source = "Grey Seal Pup Production Survey",
  project_short_title = "Grey Seal Pup Production Survey",
  areas = MPAs,
  climate_expectation = "FIXME",
  indicator_rationale = "Seals feed on a variety of species, including groundfish",
  bin_rationale = "FIXME",
  plot_type = c("time-series", "map"),
  objectives = c("Protect unique, rare, or sensitive ecological features"),
  theme = "Marine Mammals and Other Top Predators",
  SME = "Unknown",
  control_polygon = control_polygons,
  plot_lm = FALSE
)

save_plots(dplyr::select(x, -data, -adjacent_data))
dplyr::select(x, -plot)
})





