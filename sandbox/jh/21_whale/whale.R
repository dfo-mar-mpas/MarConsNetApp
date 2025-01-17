library(readxl)
files <- list.files(file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","data", "whale"), full.names = TRUE)

#f1 <- read.csv(files[1])
f2 <- read.csv(files[2])
Outside <- st_transform(read_sf(system.file("data","WEBCA_10k_85k.shp", package = "MarConsNetAnalysis"))$geometry, crs=4326)

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lat=f2$latitude, lng=f2$longitude, fillColor="gray", color = "black") %>%
  addPolygons(data=MPAs[19,]$geoms) %>%
  addPolygons(data=Outside, color = "red")

year <- strsplit(f2$ws_date, "-")
year <- unlist(lapply(year, function(x) x[1]))
f2$year <- year

df <- split(f2, f2$year)

unname(unlist(lapply(df, function(x) length(x[['longitude']]))))

df <- data.frame(year=year, latitude=f2$latitude, longitude=f2$longitude, yearly_avg=1)
