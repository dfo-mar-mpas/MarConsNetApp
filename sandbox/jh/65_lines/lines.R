library(sf)
library(leaflet)

data <- data_azmp_Discrete_Occupations_Sections |>
  dplyr::select(longitude, latitude, year, depth, temperature, year_of_publication)

# Approximate Louisbourg Line endpoints
louisbourg_line <- st_sfc(
  st_linestring(
    matrix(
      c(
        -59.85, 45.825,  # Northwest end
        -57.527, 43.473  # Southeast end
      ),
      ncol = 2,
      byrow = TRUE
    )
  ),
  crs = 4326
)

# Project to metres, buffer by 15 km, then return to longitude/latitude
louisbourg_polygon <- louisbourg_line |>
  st_transform(32620) |>
  st_buffer(15000) |>
  st_transform(4326)


library(sf)
library(leaflet)

# Halifax Line station coordinates (longitude, latitude)
halifax_coords <- matrix(
  c(
    -63.4500, 44.4000,  # HL_01
    -63.3170, 44.2670,  # HL_02
    -62.8830, 43.8830,  # HL_03
    -62.7527, 43.7635,  # HL_3.3
    -62.4510, 43.4790,  # HL_04
    -62.0980, 43.1830,  # HL_05
    -61.8326, 42.9402,  # HL_5.5
    -61.7330, 42.8500   # HL_06
  ),
  ncol = 2,
  byrow = TRUE
)

halifax_line <- st_sfc(
  st_linestring(halifax_coords),
  crs = 4326
)

# Buffer by 15 km on either side
halifax_polygon <- halifax_line |>
  st_transform(32620) |>
  st_buffer(15000) |>
  st_transform(4326)



# Extended Halifax Line station coordinates
xhl_coords <- matrix(
  c(
    -61.4332, 42.4751,  # HL_07
    -61.3413, 42.3630,  # HL_08
    -61.1637, 42.1996,  # HL_09
    -61.0609, 42.0303,  # HL_10
    -60.9054, 41.7754,  # HL_11
    -60.6766, 41.4096   # HL_12
  ),
  ncol = 2,
  byrow = TRUE
)

xhl_line <- st_sfc(
  st_linestring(xhl_coords),
  crs = 4326
)

# Buffer by 15 km on either side
xhl_polygon <- xhl_line |>
  st_transform(32620) |>
  st_buffer(15000) |>
  st_transform(4326)



## PLOT

library(leaflet)
library(leaflet)

# Colours for each line
line_colours <- list(
  "Louisbourg Line" = "orange",
  "Halifax Line" = "blue",
  "Extended Halifax Line" = "purple"
)

leaflet(al) |>
  addTiles() |>
  addPolygons(
    fillColor = unname(unlist(line_colours[al$NAME_E])),
    fillOpacity = 0.3,
    color = unname(unlist(line_colours[al$NAME_E])),
    weight = 2,
    popup = paste0(
      "<b>", al$NAME_E, "</b><br>",
      al$NAME_F
    )
  ) |>
  addLegend(
    position = "bottomright",
    colors = unname(unlist(line_colours)),
    labels = names(line_colours),
    title = "AZMP Lines",
    opacity = 0.7
  ) |>
  fitBounds(
    lng1 = -64,
    lat1 = 41,
    lng2 = -57,
    lat2 = 46
  ) %>%
  addCircleMarkers(lat=data$latitude, lng=data$longitude, radius=2, color='gray')

