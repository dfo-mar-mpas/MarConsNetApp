#' Extract Latitude and Longitude from a Spatial Data Frame
#'
#' This function extracts the latitude and longitude coordinates from a spatial
#' data frame to display protected area polygons on the map in the app.
#'
#' @param sf argument of class 'sf', 'tbl_df', 'tbl', or 'data.frame'.
#' Object must contain spatial data with a geometry column and either a
#' 'NAME_E' or 'SiteName_E' column.
#' @importFrom sf st_sfc st_coordinates
#'
#'  @export
#'
#'  @examples
#'    \dontrun{
#'    polygons <- getLatLon(sf_data)
#'    print(polygons)
#'    }
#'  @return A list
getLatLon <- function(sf=NULL) {

  if (is.null(sf)) {
    stop("Must provide an sf argument of class 'sf' 'tbl_df' 'tbl' 'data.frame'")
  }
  multipolygon <- NULL
  if ("NAME_E" %in% names(sf)) {
  for (i in seq_along(sf$NAME_E)) {
    multipolygon[[i]] <- sf$geoms[i][[1]]
  }
  } else {
    for (i in seq_along(sf$SiteName_E)) {
      multipolygon[[i]] <- sf$geoms[i][[1]]
    }
  }

  # Convert to sfc object
  sfc_multipolygon <- lapply(multipolygon, function(x) sf::st_sfc(x, crs = 4326))

  if ("NAME_E" %in% names(sf)) {
  polygons <- vector("list", length(sf$NAME_E))
  names(polygons) <- sf$NAME_E
  } else {
    polygons <- vector("list", length(sf$SiteName_E))
    names(polygons) <- sf$SiteName_E
  }


  for (i in seq_along(sfc_multipolygon)) {
    polygons[[i]] <- list(lat = NULL, lng = NULL, color=NULL)
    lng <- sf::st_coordinates(sfc_multipolygon[i][[1]])[, "X"]
    lat <- sf::st_coordinates(sfc_multipolygon[i][[1]])[, "Y"]
    polygons[[i]]$lat <- lat
    polygons[[i]]$lng <- lng
    polygons[[i]]$color <- "hotpink"
  }

  return(polygons)
}
