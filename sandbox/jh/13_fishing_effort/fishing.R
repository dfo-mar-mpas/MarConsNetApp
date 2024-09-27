library(raster)

planning_area <- data_planning_areas()
fishing <- data_commercial_fishing(planning_area)

webca <- MPAs$geoms[which(MPAs$NAME_E == "Western/Emerald Banks Conservation Area (Restricted Fisheries Zone)" )]

if (!st_crs(fishing) == st_crs(webca)) {
  fishing <- st_transform(fishing, st_crs(webca))
}

# Use st_within() to find which geometries in fishing$geoms are inside webca
within_indices <- st_within(fishing$geoms, webca)

# Convert to a logical vector if you want a TRUE/FALSE result for each geometry
within_logical <- lengths(within_indices) > 0

# Optionally, filter the fishing data frame to include only geometries within webca
fishing_within_webca <- fishing[within_logical, ]












## JAIM
radius_m <- sqrt(10234/ pi)*1000
# 1. Define a large buffer around webca (buffer outwards)
initial_buffer <- st_buffer(webca, dist = radius_m)  # The radius is an approximation

# 2. Exclude webca itself from the buffer using st_difference
buffer_outside_webca <- st_difference(initial_buffer, webca)

# 3. Calculate the area of the new buffer
buffer_area_km2 <- st_area(buffer_outside_webca) / 1e6  # Convert from m² to km²

# 4. Check if the buffer area is larger than 10,234 km², and adjust if necessary
target_area_km2 <- 10234

while (as.numeric(buffer_area_km2) > target_area_km2) {
  # Reduce the buffer distance and recalculate
  initial_buffer <- st_buffer(webca, dist = 57060 * target_area_km2 / as.numeric(buffer_area_km2))
  buffer_outside_webca <- st_difference(initial_buffer, webca)
  buffer_area_km2 <- st_area(buffer_outside_webca) / 1e6
}






fishing_in_buffer <- fishing[st_intersects(fishing$geoms, buffer_outside_webca, sparse = FALSE), ]
rp <- raster(extent(st_bbox(fishing_in_buffer)), res = 0.01)  # Adjust the resolution as needed
raster_buffer <- rasterize(fishing_in_buffer, rp, field = "weight", fun = mean)







# Optionally, filter the fishing data frame to include only geometries within webca
# radius_m <- sqrt(10234/ pi)*1000
# fishing_outside_webca <- fishing[!(within_logical), ]
#
# webca_centroid <- st_centroid(webca)

#
# # 3. Create a buffer around the centroid with the calculated radius
# webca_buffer <- st_buffer(webca_centroid, dist = radius_m)

# END JAIM




r <- raster(extent(st_bbox(fishing_within_webca)), res = 0.01)  # Adjust the resolution as needed

# Rasterize the sf object based on the 'weight' column
raster_fishing <- rasterize(fishing_within_webca, r, field = "weight", fun = mean)

pal <- colorNumeric(palette = "viridis", domain = values(raster_fishing), na.color = "transparent")


# Plot the raster on a Leaflet map
map <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = st_sf(geometry=buffer_outside_webca), color = "black", weight = 2, opacity = 1) %>%
  #addPolygons(data = st_sf(geometry=webca), color = "white", weight = 2, opacity = 1) %>%
  addRasterImage(raster_buffer, colors = pal, opacity = 0.8)%>%
  addRasterImage(raster_fishing, colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(raster_fishing),
            title = "Weight",  # Legend title
            labFormat = labelFormat(digits = 2),  # Customize label format
            opacity = 1)
map



# Analysis

webcaMean <- sum(as.numeric(fishing_within_webca$weight))/length(fishing_within_webca$OBJECTID)
bufferMean <- sum(as.numeric(fishing_in_buffer$weight))/length(fishing_in_buffer$weight)






