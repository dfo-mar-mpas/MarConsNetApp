library(terra)
tar_load(MPAs)
tar_load(data_epibenthic_communities_environmental)
mpas <- terra::vect(MPAs[c(42, 21), ])

spat <- data_epibenthic_communities_environmental[[3]] # Bottom temperature

spat_mpas <- terra::mask(
  terra::crop(spat, mpas),
  mpas
)

plot(spat_mpas, main = "MPAs 42 and 21")
plot(mpas, add = TRUE, border = "black", lwd = 2)

## Only keep SAB portion that is comparable to Laurential Channel.


spat <- data_epibenthic_communities_environmental[[3]]

mpas <- terra::vect(MPAs[c(42, 21), ])

# Mask raster to each MPA
temp_42 <- terra::mask(terra::crop(spat, mpas[1, ]), mpas[1, ])
temp_21 <- terra::mask(terra::crop(spat, mpas[2, ]), mpas[2, ])

# Get the temperature range in MPA 21
range_21 <- terra::global(temp_21, range, na.rm = TRUE)

# Keep MPA 42 cells within that range
temp_42_comparable <- terra::ifel(
  temp_42 >= range_21[1, 1] & temp_42 <= range_21[2, 1],
  temp_42,
  NA
)

# Plot
plot(
  temp_42_comparable,
  main = "Comparable Bottom Temperatures",
  xlim = terra::ext(mpas)[1:2],
  ylim = terra::ext(mpas)[3:4]
)
plot(temp_21, add = TRUE)
plot(mpas, add = TRUE, border = "black", lwd = 2)

## NOW get polygon

temp_42_polygon <- terra::as.polygons(
  temp_42_comparable,
  aggregate = TRUE,
  values = FALSE,
  na.rm = TRUE
) |>
  sf::st_as_sf()
