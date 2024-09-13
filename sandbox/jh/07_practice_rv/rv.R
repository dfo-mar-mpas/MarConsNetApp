#library(Mar.datawrangling)
library(RODBC)
library(sf)
library(devtools)
library(leaflet)
load_all("../MarConsNetData/")
load_all("../Mar.datawrangling/")
#dir.create("C:/Users/HarbinJ/Documents/data/rv")
get_data('rv', data.dir="C:/Users/HarbinJ/Documents/data/rv")
GSSPECIES <- GSSPECIES[GSSPECIES$CODE == 11,]
self_filter(keep_nullsets = F)

test<- summarize_catches()

latitude <- round(test$LATITUDE,2)
longitude <- round(test$LONGITUDE,2)

bioregion <- data_bioregion()
MPAs <- data_CPCAD_areas(bioregion,  zones = FALSE)
webca <- MPAs$geoms[which(MPAs$NAME_E == "Western/Emerald Banks Conservation Area (Restricted Fisheries Zone)")]

coords <- cbind(longitude, latitude)

# Convert to sf object
points_sf <- st_as_sf(data.frame(coords), coords = c("longitude", "latitude"), crs = st_crs(4326))

points_within <- st_within(points_sf, webca, sparse = FALSE)
within_points <- points_sf[apply(points_within, 1, any), ]


leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addCircleMarkers(
    lng = st_coordinates(within_points)[, 1],
    lat = st_coordinates(within_points)[, 2],
    radius = 5,  # Radius of the circles
    color = "red",  # Border color of the circles
    fillColor = "orange",  # Fill color of the circles
    fillOpacity = 0.7  # Fill opacity of the circles
  ) %>%
  addPolygons(data = webca, color = "blue", weight = 2, opacity = 0.5)

# Now keeping haddock info
haddock <- test[which(points_within[,1]),]
# years <- sort(unique(haddock$YEAR))
#
# # Tows are different distances and there is a different number of tows each year
# abundance <- NULL
# for (i in seq_along(years)) {
#   k <- which(haddock$YEAR == years[i])
#   k2 <- which(haddock$COMM == "HADDOCK")
#   keep <- intersect(k,k2)
#   abundance[[i]] <- round(sum(as.numeric(haddock$TOTNO[keep]))/length(haddock$MISSION[keep]),0) # Average per tow
# }
# names(abundance) <- years
#
#
# plot(x=1:length(years), y=abundance, ylab="Average # of Haddock Per Tow", xlab="Year", type="o", pch=20, xaxt="n")
# axis(side = 1, at = seq_along(years), labels = years, las=2)


haddock <- haddock[which(haddock$COMM == 'HADDOCK'),]

haddock$Standardized_TOTAL <- (haddock$TOTNO / haddock$DIST) * 1.75

average_by_year <- round(aggregate(Standardized_TOTAL ~ YEAR, data = haddock, FUN = mean),2)


plot(x=1:length(average_by_year$YEAR), y=average_by_year$Standardized_TOTAL, ylab="Average # of Haddock Per Tow", xlab="Year", type="o", pch=20, xaxt="n")
axis(side = 1, at = seq_along(average_by_year$YEAR), labels = average_by_year$YEAR, las=2)
