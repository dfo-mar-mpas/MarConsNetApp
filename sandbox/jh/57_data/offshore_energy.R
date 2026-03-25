library(httr)
library(readxl)
library(leaflet)
library(targets)
tar_load(MPAs)

url <- "https://cnsopbdigitaldata.ca/geoviewer/dmc/public/dow-2025.xlsx"

tmp <- tempfile(fileext = ".xlsx")

GET(url, write_disk(tmp, overwrite = TRUE))

wells <- read_excel(tmp)
names(wells)[which(names(wells) == 'Longitude (W)')] <- 'longitude'
names(wells)[which(names(wells) == "Latitude       (N)")] <- 'latitude'

lat <- wells$latitude
lng <- wells$longitude

latitude_clean <- substr(lat, 1, nchar(lat) - 1)
latitude_clean <- sub("\"", "", latitude_clean)

longitude_clean <- substr(lng, 1, nchar(lng) - 1)
longitude_clean <- sub("\"", "", longitude_clean)

convert_dms <- function(x) {
  nums <- regmatches(x, gregexpr("[0-9.]+", x))
  sapply(nums, function(p) {
    d <- as.numeric(substr(p[1], 1, 2))   # degrees (first 2 digits)
    m <- as.numeric(substr(p[1], 3, 4))   # minutes (next 2)
    s <- as.numeric(p[2])                 # seconds
    d + m/60 + s/3600
  })
}
wells$latitude <- convert_dms(latitude_clean)
wells$longitude <- convert_dms(longitude_clean)*(-1)

leaflet() %>% addTiles %>%
  addPolygons(data=MPAs$geoms[1:44], col='gray') %>%
  addCircleMarkers(lat=wells$latitude, lng=wells$longitude, color='black', radius=0.5)

