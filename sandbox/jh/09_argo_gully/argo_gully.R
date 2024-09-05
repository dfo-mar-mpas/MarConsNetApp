library(argoFloats)
library(MarConsNetData)
library(MarConsNetAnalysis)
ai <- getIndex(filename="core")

longitude <- ai[['longitude']]
latitude <- ai[['latitude']]

bad <- unique(c(which(is.na(longitude)), which(is.na(latitude))))
MPAs <- data_CPCAD_areas(data_bioregion("Scotian Shelf"),  zones = FALSE)
mpa <- MPAs$geoms[which(MPAs$NAME_E == "Gully Marine Protected Area"),]

longitude <- longitude[-bad]
latitude <- latitude[-bad]
coords <- cbind(longitude, latitude)

# Convert to sf object
points_sf <- st_as_sf(data.frame(coords), coords = c("longitude", "latitude"), crs = st_crs(4326))

points_within <- st_within(points_sf, mpa, sparse = FALSE)
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
  addPolygons(data = mpa, color = "blue", weight = 2, opacity = 0.5)
