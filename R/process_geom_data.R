process_geom_data <- function(x) {
  if(!inherits(x, "sf")) x <- st_as_sf(x)

  if("geoms" %in% names(x)) {
    x <- x |>
      mutate(geometry = geoms) |>
      as.data.frame() |>
      dplyr::select(-geoms) |>
      st_as_sf()
  }

  if(!("year" %in% names(x))) {
    x <- x |> mutate(year = NA)
  }

  x |>
    mutate(year = as.numeric(year)) |>
    st_cast("GEOMETRY") |>
    dplyr::select(year, geometry) |>
    st_as_sf() |>
    unique()
}
