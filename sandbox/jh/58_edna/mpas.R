library(sf)
areas <- get_spatial_layer(
  "https://maps-cartes.ec.gc.ca/arcgis/rest/services/CWS_SCF/CPCAD/MapServer/0",
  where = "BIOME='M' AND MGMT_E='Fisheries And Oceans Canada'"
) |>
  group_by(NAME_E, NAME_F) |>
  summarise(geoms = st_make_valid(st_union(geoms)))

areas$region = 'Maritimes'

sf::sf_use_s2(FALSE)

areas$date_of_establishment <- 2017



## Adding non conservation areas
bbox_coords <- matrix(c(
  -171, 24,  # xmin, ymin (Hawaii/Alaska/Florida range)
  -50,  24,  # xmax, ymin
  -50,  84,  # xmax, ymax
  -171, 84,  # xmin, ymax
  -171, 24   # close the polygon
), ncol = 2, byrow = TRUE)

bbox <- st_polygon(list(bbox_coords)) |>
  st_sfc(crs = st_crs(areas)) |>
  st_make_valid()

# Step 4: Get union of all MPAs
all_mpa_union <- st_union(areas$geoms)

# Step 5: Compute difference (the "outside" area)
outside_geom <- st_difference(bbox, all_mpa_union) |> st_make_valid()

# Step 6: Create the Outside row
outside_row <- tibble(
  NAME_E = "Non_Conservation_Area",
  NAME_F = "Extérieur",
  geoms = outside_geom,
  region = NA
)

# Step 7: Bind with existing areas
areas_full <- bind_rows(areas, outside_row)

MPAs <- areas_full

