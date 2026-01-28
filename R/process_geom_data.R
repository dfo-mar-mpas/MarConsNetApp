#' Standardize and clean geometry data for spatial processing
#'
#' Converts an input object to a valid \code{sf} object and
#' standardizes geometry and year fields to ensure consistent
#' downstream spatial operations. The function harmonizes
#' geometry column names, enforces a numeric \code{year} field,
#' and returns a simplified, unique geometry set.
#'
#' @param x
#' An object containing spatial data. Can be an \code{sf} object
#' or a data frame with a geometry-like column (e.g. \code{geoms}
#' or \code{Shape}).
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Converts \code{x} to an \code{sf} object if it is not
#'         already one.
#'   \item Renames alternative geometry columns (\code{geoms} or
#'         \code{Shape}) to the standard \code{geometry} column.
#'   \item Adds a \code{year} column if missing and coerces it to
#'         numeric.
#'   \item Casts geometries to \code{GEOMETRY} to ensure consistent
#'         geometry types.
#'   \item Retains only the \code{year} and \code{geometry} columns
#'         and removes duplicate geometries.
#' }
#'
#' This helper is primarily intended to prepare spatial layers
#' for mapping and spatial joins within the MarConsNet
#' application.
#'
#' @return
#' An \code{sf} object containing unique geometries with a numeric
#' \code{year} column.
#'
#' @importFrom sf st_as_sf st_cast
#' @importFrom dplyr mutate select
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
#'}
#' @export
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
