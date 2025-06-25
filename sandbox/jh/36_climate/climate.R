as.data.frame(data_pillar_ecol_df[which(data_pillar_ecol_df$indicator == "Coliform"),])

data_sf <- st_as_sf(data,
                    coords = c("longitude", "latitude"),
                    crs = 4326)  # assuming WGS84

mpa <- MPAs$geoms[32]
control <- st_difference(st_buffer(st_transform(mpa, crs = 4326), 20000), st_transform(mpa, crs=4326))


leaflet() %>%
  addTiles() %>%
  addPolygons(data=control, color="purple") %>%
  addPolygons(data=mpa, color="red") %>%
  addCircleMarkers(lat=data$latitude, lng=data$longitude, color="black")


in_control <- st_within(data_sf, control, sparse = FALSE)[,1]
in_mpa <- st_within(data_sf, mpa, sparse = FALSE)[,1]

# Points inside either
in_either <- in_control | in_mpa

# Add result to your data
data$in_control <- in_control
data$in_mpa <- in_mpa
data$in_either <- in_either

leaflet() %>%
  addTiles() %>%
  addPolygons(data = control, color = "purple") %>%
  addPolygons(data = mpa, color = "red") %>%
  addCircleMarkers(data = data,
                   lng = ~longitude,
                   lat = ~latitude,
                   color = ~ifelse(in_control, "blue",
                                   ifelse(in_mpa, "orange", "black")),
                   label = ~paste("in_control:", in_control,
                                  "<br>in_mpa:", in_mpa))
