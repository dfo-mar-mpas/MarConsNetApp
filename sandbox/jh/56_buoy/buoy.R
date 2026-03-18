library(dod)

arguments <- c("Banquereau Bank","East Scotian Slope", "Halifax", "Halifax DISCUS TriAx","Halifax Harbour",
"Laurentian Fan", "Port Hope", "Prince Edward Point", "Tail of the Bank")

# "Minas Basin"

dfs <- NULL

for (i in seq_along(arguments)) {
  message(i)
  destdir <- tempdir(check = TRUE)
  file <- dod.buoy("MEDS", arguments[i], destdir = destdir)
  col.names <- strsplit(readLines(file, 1), ",")[[1]]
  d <- read.csv(file, skip = 2, col.names = col.names)
  names(d) <- tolower(names(d))

  if ("vwh." %in% names(d)) {
    d$waveheight <- d$vwh.
  }

  if ('date' %in% names(d)) {
  d$date_revamped <- as.POSIXct(d$date, tz = "UTC", format = "%m/%d/%Y %H:%M")
  } else {
    d$date_revamped <- NA
  }

  if (all(is.na(d$waveheight)) && !(i == 4)) {
    browser()
  }

  if (all(is.na(d$date_revamped))) {
    browser()
  }

  dfs[[i]] <- d[,c("latitude", 'longitude', 'date_revamped', 'waveheight')]

  unlink(destdir, recursive = TRUE)
}

wave_height <- do.call(rbind, dfs)

wh <- wave_height[-(which(is.na(wave_height$waveheight))),]
wh$longitude <- -1*(wh$longitude)

## SECOND DATA SOURCE
arguments <- c('h1', 'halifax', 'hkb', 'saint_john', 'saint_johns')
dfs <- NULL

for (i in seq_along(arguments)) {
  message(i, " of ", length(arguments))
  destdir <- tempdir(check = TRUE)
  file <- dod.buoy("smartatlantic", arguments[i], destdir = destdir)
  col.names <- strsplit(readLines(file, 1), ",")[[1]]
  d <- read.csv(file, skip = 2, col.names = col.names)
  names(d) <- tolower(names(d))

  if ('lat' %in% names(d)) {
    d$latitude <- d$lat
    d$longitude <- d$lon
  }

  if ("wave_ht_sig" %in% names(d)) {
    d$waveheight <- d$wave_ht_sig
  } else {
    browser(1)
  }

  if ('timestamp' %in% names(d)) {
    d$date_revamped <- as.POSIXct(
      d$timestamp,
      tz = "UTC",
      format = "%Y-%m-%dT%H:%M:%SZ"
    )
    } else {
    browser(2)
    d$date_revamped <- NA
  }

  if (all(is.na(d$waveheight))) {
    browser()
  }

  if (all(is.na(d$date_revamped))) {
    browser()
  }

  dfs[[i]] <- d[,c("latitude", 'longitude', 'date_revamped', 'waveheight')]

  unlink(destdir, recursive = TRUE)
}

wave_height2 <- do.call(rbind, dfs)

wh2 <- wave_height2[-(which(is.na(wave_height2$waveheight))),]
wh2$latitude <- round(wh2$latitude,2)
wh2$longitude <- round(wh2$longitude,2)


final <- rbind(wh,wh2)



unique_pairs <- final %>%
  distinct(latitude, longitude)

leaflet() %>% addTiles() %>% addPolygons(data=MPAs$geoms[1:44]) %>% addCircleMarkers(lat=unique_pairs$latitude, lng=unique_pairs$longitude, col='red')

