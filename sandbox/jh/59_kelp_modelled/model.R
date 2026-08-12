library(terra)
library(sf)
library(ggplot2)

musquash <- MPAs[
  MPAs[['NAME_E']] == "Musquash Estuary Marine Protected Area",
]

# Musquash in the same CRS as bathy
musquash <- st_transform(
  musquash,
  crs(shallow_bathymetry)
)

cols <- cellFromXY(
  bathy,
  cbind(
    c(st_bbox(musquash)$xmin, st_bbox(musquash)$xmax),
    c(st_bbox(musquash)$ymin, st_bbox(musquash)$ymax)
  )
)
musquash_ext <- ext(st_bbox(musquash))

bathy_musquash <- crop(bathy, musquash_ext)

# Convert raster to data frame
bathy_df <- as.data.frame(
  bathy_musquash,
  xy = TRUE,
  na.rm = TRUE
)

names(bathy_df)[3] <- "depth"

# Plot
ggplot(bathy_df) +
  geom_tile(
    aes(x = x, y = y, fill = depth)
  ) +
  geom_sf(
    data = musquash,
    inherit.aes = FALSE,
    fill = NA,
    color = "red",
    linewidth = 1
  ) +
  labs(
    title = "GEBCO 2026 bathymetry around Musquash",
    fill = "Depth (m)"
  ) +
  theme_minimal()


# Get suitable habitat within Musquash
kelp_musquash <- st_intersection(
  data,
  musquash
) |>
  filter(suitable_habitat == TRUE)

ggplot(bathy_df) +
  geom_tile(
    aes(x = x, y = y, fill = depth),
    color = "black"
  ) +
  geom_text(
    aes(
      x = x,
      y = y,
      label = round(depth)
    ),
    size = 3
  ) +
  geom_sf(
    data = kelp_musquash,
    fill = NA,
    color = "green",
    linewidth = 1
  ) +
  geom_sf(
    data = musquash,
    inherit.aes = FALSE,
    fill = NA,
    color = "red",
    linewidth = 1
  ) +
  labs(
    title = "Musquash: bathymetry and suitable kelp habitat",
    fill = "Depth (m)"
  ) +
  theme_minimal()


#### NEXT STEPS:
### Looking at cells that overlap less than 30 with suitable kelp habitat

# Keep bathymetry cells between 0 and 30 m depth
shallow_musquash <- bathy_musquash >= -30 & bathy_musquash < 0

shallow_poly <- as.polygons(
  shallow_musquash,
  values = TRUE,
  na.rm = TRUE
) |>
  st_as_sf()

shallow_poly <- shallow_poly |>
  filter(elevation == 1)

final_data <- st_intersection(
  kelp_musquash,
  shallow_poly
)

ggplot(bathy_df) +
  geom_tile(
    aes(x = x, y = y, fill = depth),
    color = "black"
  ) +
  geom_sf(
    data = final_data,
    fill = "green",
    alpha = 0.5,
    color = "darkgreen",
    linewidth = 0.8
  ) +
  geom_sf(
    data = musquash,
    fill = NA,
    color = "red",
    linewidth = 1
  ) +
  geom_text(
    aes(x = x, y = y, label = round(depth)),
    size = 3
  ) +
  labs(
    title = "Musquash: suitable kelp habitat ≤30 m",
    fill = "Depth (m)"
  ) +
  theme_minimal()

