library(sf)
library(dplyr)
library(leaflet)
library(RColorBrewer)


apg <- all_project_geoms[-which(is.na(all_project_geoms$PPTID)),]
if (any(apg$areaID == "Non_Conservation_Area")) {
apg <- apg[-which(apg$areaID == "Non_Conservation_Area"),]
}
# Create a new geometry list with rounded coords
rounded_geoms <- st_sfc(
  lapply(st_geometry(apg), function(pt) {
    coords <- st_coordinates(pt)
    coords_rounded <- round(coords, 2)
    coords_vec <- as.numeric(coords_rounded[1, ])
    st_point(coords_vec)
  }),
  crs = st_crs(apg)
)

# Then assign back to the sf object properly
st_geometry(apg) <- rounded_geoms

# Now find unique geometries
apg_unique <- apg %>% distinct(geometry, .keep_all = TRUE)

# Getting lat and lng

coords_df <- do.call(rbind, lapply(st_geometry(apg_unique), function(pt) {
  c(lng = pt[1], lat = pt[2])
}))

# Add as columns to apg
apg_unique <- apg_unique %>%
  mutate(
    lng = coords_df[, "lng"],
    lat = coords_df[, "lat"]
  )

# Getting colors

names(mpa_colors) <- df$MPA
fill_colors <- mpa_colors[df$MPA]

# Replace any NA colors with grey if needed
fill_colors[is.na(fill_colors)] <- "grey"
fill_colors <- as.character(fill_colors)

money_mpas <- MPAs[which(MPAs$NAME_E %in% df$MPA),]

# GETTING PROJECTS
marker_color <- "lightblue"

# Assign FA icons for shapes
shape_icons <- c("circle", "square", "star", "triangle", "heart")

icons <- awesomeIcons(
  icon = shape_icons,
  iconColor = "black",
  library = "fa",
  markerColor = marker_color
)

# Legend work
pptids <- unique(apg_unique$PPTID)
shape_map <- setNames(rep(shape_icons, length.out = length(pptids)), pptids)

# Assuming you have a shape_map like this:
# shape_map <- setNames(c("circle", "star", "square-full"), pptids)

# Build legend items dynamically:
legend_items <- paste0(
  "<i class='fa fa-", shape_map[pptids], "' style='color: ", marker_color, ";'></i> ", pptids
)

# Combine into one HTML string
legend_html <- paste0(
  "<div style='background: white; padding: 8px; font-family: Arial;'>",
  "<b>Legend</b><br>",
  paste(legend_items, collapse = "<br>"),
  "</div>"
)


map <- leaflet(apg_unique) %>%
  addTiles() %>%
  addPolygons(
    data = money_mpas$geoms,
    fillColor = fill_colors,
    fillOpacity = 0.9,
    color = "grey30",
    weight = 3,
    opacity = 1
  ) %>%
  addAwesomeMarkers(
    ~lng, ~lat,
    icon = icons,
    popup = ~paste("Project ID:", PPTID)
  ) %>%
  addControl(
    html = legend_html,
    position = "bottomright"
  )

