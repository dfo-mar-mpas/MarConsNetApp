library(Mar.datawrangling)
library(RODBC)
library(sf)
load_all("../MarConsNetData/")
#dir.create("C:/Users/HarbinJ/Documents/data/rv")
get_data('rv', data.dir="C:/Users/HarbinJ/Documents/data/rv")
GSSPECIES <- GSSPECIES[GSSPECIES$CODE == 11,]
self_filter(keep_nullsets = F)

test<- summarize_catches()

latitude <- round(test$LATITUDE,2)
longitude <- round(test$LONGITUDE,2)

bioregion <- data_bioregion()
MPAs <- data_CPCAD_areas(bioregion,  zones = FALSE)
webca <- MPAs$geoms[which(MPAs$NAME_E == "Western/Emerald Banks Conservation Area (Restricted Fisheries Zone)")]

coords <- cbind(longitude, latitude)

# Convert to sf object
points_sf <- st_as_sf(data.frame(coords), coords = c("longitude", "latitude"), crs = st_crs(4326))

points_within <- st_within(points_sf, webca, sparse = FALSE)
within_points <- points_sf[apply(points_within, 1, any), ]


leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addCircleMarkers(
    lng = st_coordinates(within_points)[, 1],
    lat = st_coordinates(within_points)[, 2],
    radius = 5,  # Radius of the circles
    color = "red",  # Border color of the circles
    fillColor = "orange",  # Fill color of the circles
    fillOpacity = 0.7  # Fill opacity of the circles
  ) %>%
  addPolygons(data = webca, color = "blue", weight = 2, opacity = 0.5)
