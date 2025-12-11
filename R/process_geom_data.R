#' Process Geometric Data
#'
#' @param x an sf object or data frame containing geometric data
#'
#' @returns an sf object with 'year' and 'geometry' columns
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' library(sf)
#' df <- data.frame(
#'  id = 1:3,
#'  year = c(2000, 2001, 2002),
#'  lon = c(-100, -101, -102),
#'  lat = c(40, 41, 42)
#'  )
#'  sf_data <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
#'  processed_data <- process_geom_data(sf_data)
#'
process_geom_data <- function(x) {
  if(!inherits(x, "sf")) x <- st_as_sf(x)

  if("geoms" %in% names(x)) {
    x <- x |>
      mutate(geometry = geoms) |>
      as.data.frame() |>
      dplyr::select(-geoms) |>
      st_as_sf()
  }

  if("Shape" %in% names(x)) {
    x <- x |>
      mutate(geometry = Shape) |>
      as.data.frame() |>
      dplyr::select(-Shape) |>
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
