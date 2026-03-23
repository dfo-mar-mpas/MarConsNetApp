library(sf)
library(dplyr)
tar_load(MPAs)

url <- "https://services.arcgis.com/6DIQcwlPy8knb6sg/arcgis/rest/services/SubmarineCables/FeatureServer/2/query?where=1=1&outFields=*&f=geojson"

cables <- st_read(url)

# make sure geometry column is active
mpas <- st_as_sf(MPAs)

# ensure same CRS
cables <- st_transform(cables, st_crs(mpas))

mps_counts <- mpas %>%
  mutate(
    cable_count = lengths(st_intersects(., cables))
  ) %>%
  st_drop_geometry()

data <- mps_counts[,c("NAME_E", "cable_count")]
