library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(dplyr)


x$plot <- vector("list", nrow(x))
x$data <- vector("list", nrow(x))

for (i in seq_along(unique(x$areaID))) {
  message(i)
  name_of_interest <- unique(x$areaID)[i]

  k1 <- which(!unname(sapply(otn_areas_old, is.null)))
  k2 <- which(unname(sapply(otn_areas_old, function(xx) name_of_interest %in% xx)))
  keep <- intersect(k1, k2)

  if (length(keep) > 0) {
    # Combine tracking data
    df <- do.call(rbind, tracking[keep])
    df <- df[df$areaID != "Non_Conservation_Area", ]
    df$tag <- as.character(df$tag)
    df$eventDate <- as.POSIXct(df$eventDate)

    if (!inherits(df, "sf")) {
      df <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
    }

    # Land background
    land <- ne_countries(scale = "medium", returnclass = "sf")

    # Relevant MPAs
    mpas_sub <- MPAs$geoms[MPAs$NAME_E %in% unique(df$areaID), ]

    # Split tracks by tag
    subset_track <- split(df, df$tag_id)
    all_tags <- unique(unlist(lapply(subset_track, function(tt) tt$tag)))

    # Color palette
    pal <- viridis(length(all_tags))
    names(pal) <- all_tags

    # Extract coordinates
    df_coords <- df %>%
      mutate(
        X = st_coordinates(geometry)[,1],
        Y = st_coordinates(geometry)[,2]
      )

    df_path <- df_coords %>%
      arrange(tag, eventDate)

    # Determine plot limits based on MPAs or tracking data
    combined_geom <- c(st_geometry(mpas_sub), st_geometry(df))

    # Compute bounding box
    bbox <- st_bbox(st_sfc(combined_geom, crs = st_crs(df)))

    # Create ggplot
    map <- ggplot() +
      geom_sf(data = land, fill = "grey90", color = "white", size = 0.2) +
      geom_sf(data = mpas_sub, fill = "grey60", alpha = 0.3, color = "grey40") +
      geom_path(
        data = df_path,
        aes(x = X, y = Y, group = tag, color = tag),
        linewidth = 0.8
      ) +
      geom_point(
        data = df_coords,
        aes(x = X, y = Y, color = tag),
        size = 1.5
      ) +
      scale_color_manual(values = pal) +
      coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
               ylim = c(bbox["ymin"], bbox["ymax"])) +  # zoom to region
      theme_minimal() +
      theme(legend.position = "bottom") +
      labs(color = "Tag",
           x = "Longitude",
           y = "Latitude")

    # Store results
    x$plot[[i]] <- map
    x$data[[i]] <- tester[tester$areaID == name_of_interest, c("tag_id", "geometry")]

  }
}

