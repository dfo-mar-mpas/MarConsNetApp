getLatLon <- function(sf=NULL) {

  if (is.null(sf)) {
    stop("Must provide an sf argument of class 'sf' 'tbl_df' 'tbl' 'data.frame'")
  }
  multipolygon <- NULL
  for (i in seq_along(sf$NAME_E)) {
    multipolygon[[i]] <- sf$geoms[i][[1]]
  }

  # Convert to sfc object
  sfc_multipolygon <- lapply(multipolygon, function(x) st_sfc(x, crs = 4326))

  polygons <- vector("list", length(sf$NAME_E))
  names(polygons) <- sf$NAME_E
  for (i in seq_along(sfc_multipolygon)) {
    polygons[[i]] <- list(lat = NULL, lng = NULL, color=NULL)
    lng <- st_coordinates(sfc_multipolygon[i][[1]])[, "X"]
    lat <- st_coordinates(sfc_multipolygon[i][[1]])[, "Y"]
    polygons[[i]]$lat <- lat
    polygons[[i]]$lng <- lng
    polygons[[i]]$color <- "hotpink"
  }

  return(polygons)
}
