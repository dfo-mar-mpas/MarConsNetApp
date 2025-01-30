# 1. bloom_df
script_lines <- readLines("https://raw.githubusercontent.com/BIO-RSG/PhytoFit/refs/heads/master/tools/tools_01a_define_polygons.R")

k1 <- which(grepl("poly\\$atlantic = list", script_lines))
k2 <- which(grepl("-61.1957, -61.1957, -59.54983, -59.54983, -61.1957", script_lines))
script <- script_lines[k1:k2]
poly <- list()
eval(parse(text=script))
DF <- poly$atlantic$AZMP$CSS_V02

coords <- matrix(c(DF$lon, DF$lat), ncol = 2, byrow = FALSE)
coords <- rbind(coords, coords[1,])
polygon_sf <- st_sfc(st_polygon(list(coords)))
st_crs(polygon_sf) <- 4326


df <- azmpdata::RemoteSensing_Annual_Broadscale
df <- df[which(df$area == "CSS_remote_sensing"),]
df$geom <- rep(polygon_sf)

DF <- df[,c("area", "year", "bloom_amplitude", "geom"),]
DF$type <- "Remote Sensing"
DF$units <- "(unit unknown)"

bloom_df <- DF


# 2. all_haddock
get_data('rv', data.dir = "C:/Users/HarbinJ/Documents/data/rv")

# All haddock
GSSPECIES = GSSPECIES[GSSPECIES$CODE %in% c(11),]
Mar.datawrangling::self_filter(keep_nullsets = F)
ah = Mar.datawrangling::summarize_catches()
names(ah)[which(names(ah) == "LATITUDE")] <- "latitude"
names(ah)[which(names(ah) == "LONGITUDE")] <- "longitude"
names(ah)[which(names(ah) == "SDATE")] <- "date"
names(ah)[which(names(ah) == "TOTNO")] <- "haddock_abundance"
ah$type <- "RV"
ah$units <- "(counts)"
names(ah)[which(names(ah) == "TOTWGT")] <- "haddock_biomass"
ah$year <- as.numeric(format(ah$date, "%Y"))

AH <- ah[,c("longitude", "latitude","year", "haddock_abundance", "type", "units"),]

all_haddock <- AH


## 2B.haddock_biomass
haddock_biomass <- ah[,c("longitude", "latitude","year", "haddock_biomass", "type"),]
haddock_biomass$units <- "kg"


# 3. gsdet
get_data('rv', data.dir = "C:/Users/HarbinJ/Documents/data/rv")

GSDET$latitude <- 0
GSDET$longitude <- 0
GSDET$year <- 0
missions <- unique(GSDET$MISSION)

GSINF <-GSINF[-which(is.na(GSINF$SDATE)),]
for (i in seq_along(missions)) {
  GSDET$latitude[which(GSDET$MISSION == missions[i])] <- GSINF$LATITUDE[which(GSINF$MISSION == missions[i])][1]
  GSDET$longitude[which(GSDET$MISSION == missions[i])]  <- GSINF$LONGITUDE[which(GSINF$MISSION == missions[i])][1]
  GSDET$year[which(GSDET$MISSION == missions[i])]  <- unique(as.numeric(substr(GSINF$SDATE[which(GSINF$MISSION == missions[i])],1,4)))
}
GSDET$type <- "RV Survey"
names(GSDET)[which(names(GSDET) == "FLEN")] <- "fish_length"
names(GSDET)[which(names(GSDET) == "FWT")] <- "fish_weight"


GS <- GSDET[,c("longitude", "latitude", "year", "fish_length", "type")]
GS$units <- "cm"

fish_length <- GS

## 3b. fish_weight
fish_weight <- GSDET[,c("longitude", "latitude", "year", "fish_weight", "type")]
fish_weight$units <- "g"

# 4. Zooplankton

df <- azmpdata::Zooplankton_Annual_Stations
sdf <- azmpdata::Derived_Occupations_Stations
df$latitude <- 0
df$longitude <- 0
for (i in seq_along(unique(df$station))) {
  if (unique(df$station)[i] == "HL2") { # ISSUE 53
    k <- 274
  } else {
    k <- 1
  }
  df$latitude[which(df$station == unique(df$station)[i])] <- sdf$latitude[which(sdf$station == unique(df$station)[i])][k]
  df$longitude[which(df$station == unique(df$station)[i])] <- sdf$longitude[which(sdf$station == unique(df$station)[i])][k]
}

df$type <- "Zooplankton AZMP"
df$Calanus_finmarchicus_biomass <- df$Calanus_finmarchicus_log10

DF <- df[c("latitude", "longitude", "type", "Calanus_finmarchicus_biomass", "year")]
DF$units <- "log_10"
zooplankton <- DF

# 5. surface_height

df <- azmpdata::Derived_Monthly_Stations
# Add latitude and longitude
df$latitude <- 0
df$longitude <- 0
type <- NULL
df$latitude[which(df$station == "Halifax")] <- 43.5475
df$longitude[which(df$station == "Halifax")] <- 63.5714

df$latitude[which(df$station == "Yarmouth")] <- 43.8377
df$longitude[which(df$station == "Yarmouth")] <- 66.1150

df$latitude[which(df$station == "North Sydney")] <- 46.2051
df$longitude[which(df$station == "North Sydney")] <- 60.2563
df$units <- "m"
df$type <- "derived (AZMP)"
df <- df[,c("latitude", "longitude", "year", "units", "type", "sea_surface_height")]

surface_height <- df

# 6A.Discrete_Occupations_Sections (FIXME)
df <- Discrete_Occupations_Sections
df$year <- as.numeric(format(df$date, "%Y"))
df$type <- "AZMP"

DF <- df[,c("latitude", "longitude", "year", "nitrate", "type", "depth")]
DF$units <- "mmol/m3"

nitrate <- DF

# 6B. salinity
salinity <- df[,c("latitude", "longitude", "year", "salinity", "type", "depth")]
salinity$units <- "psu"

# 6C temperature
temperature <- df[,c("latitude", "longitude", "year", "temperature", "type", "depth")]
temperature$units <- "C"

# 6D. chlorophyll
chlorophyll <- df[,c("latitude", "longitude", "year", "temperature", "type", "depth")]
chlorophyll$units <- "ug/L"
chlorophyll$type <- "In situ AZMP"

# 7. whale_sighting
whale_biodiversity <- project_whale_biodiversity()

#' Plot Indicator Trends
#'
#' This function plots the trends of indicators. It also provides the values
#' of indicators for each year that exists
#'
#' @param mpa an object of class "sf", likely from data_CPCAD_areas that contains
#' the name and geoms of the protected areas
#' @param df data frame either from a getFunction specified in dataTable or from
#' a tar_load that includes latitude, longitude, year, type, and the parameter
#' to average
#' @param area area of interest (from the NAME_E column of MPAs)
#' @param type surface or bottom
#' @param dataframe FALSE a Boolean indicating if a data frame is returned or not
#' @param parameter a character indicating which parameter to measure
#' @param outside a Boolean indicating if an outside comparison is happening
#' @param map a Boolean indicating if the latitude, longitude, boundary, and outside boundary
#' should be returned. This is likely used for plotting purposes
#' @importFrom azmpdata Discrete_Occupations_Sections
#' @importFrom sf st_within st_as_sf
#' @importFrom dplyr slice_max ungroup group_by
#' @return
#' @export
#'
#' @examples
# plot_trend_status <- function(df=NULL, mpa=NULL, area="Western/Emerald Banks Conservation Area (Restricted Fisheries Zone)", type=NULL,
#                               dataframe=FALSE, outside=FALSE, map=FALSE) {
#
#   # Derived_Monthly_Stations
#   multipolygon <- mpa$geoms[which(mpa$NAME_E == area)]
#
#   if (area == "Western/Emerald Banks Conservation Area (Restricted Fisheries Zone)") {
#     Outside <- st_transform(read_sf(system.file("data","WEBCA_10k_85k.shp", package = "MarConsNetAnalysis"))$geometry, crs=4326)
#   } else {
#     stop("Must code in outside buffer for outside comparison in this area")
#   }
#
#   outside_exclusive_multipolygon <- sf::st_difference(Outside, multipolygon)
#
#   if (!("geom" %in% names(df))) {
#     points_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
#     inside <- sf::st_within(points_sf, multipolygon, sparse = FALSE)
#
#     # Filter points that are inside the polygon
#     points_inside <- points_sf[inside, ]
#   } else {
#     polys <- unique(df$geom)
#     if (outside) {
#     overlap <- lapply(polys, function(x) st_overlaps(x, Outside))
#     } else {
#       overlap <- lapply(polys, function(x) st_overlaps(x, multipolygon))
#
#     }
#     yon <- NULL # 1 means overlap
#     for (i in seq_along(overlap)) {
#       yon[[i]] <- overlap[[i]][[1]]
#     }
#     polyKeep <- polys[which(yon == 1)]
#
#     keepers <- vector("list", length(df$geom))
#     for (i in seq_along(df$geom)) {
#       dfk <- unlist(df$geom[i])
#         for (j in seq_along(polyKeep)) {
#           pg <- unlist(polyKeep[[j]])
#           if (identical(dfk,pg)) {
#             keepers[[i]][[j]] <- i
#           }
#         }
#       }
#
#     keepers <- unlist(keepers)
#
#     inside <-  matrix(FALSE, nrow = length(df$type), ncol = 1)
#     inside[,1][unlist(keepers)] <- TRUE
#   }
#
#   # OUTSIDE BUFFER
#   if (outside) {
#     if (!("geom" %in% names(df))) {
#     inside <- sf::st_within(points_sf, outside_exclusive_multipolygon, sparse = FALSE)
#     if (!(any(inside[,1]) == 0)) {
#       # Filter points that are inside the polygon
#       points_inside <- points_sf[inside, ]
#     } else {
#       stop("No outside comparison available.")
#     }
#     }
#   }
#     if (any(inside[,1])) {
#       keep <- df[which(inside[,1]),]
#     } else {
#       if (!("geom" %in% names(df))) {
#       # None in, but find the closest
#       latitude <- df$latitude
#       longitude <- df$longitude
#       points <- sf::st_as_sf(data.frame(latitude,longitude),
#                              coords = c("longitude", "latitude"),
#                              crs = 4326)  # WGS84 CRS
#       distances <- st_distance(points, multipolygon)
#       closest_index <- apply(distances, 1, which.min)
#       closest_distance <- apply(distances, 1, min)
#       closest_point <- unique(points[closest_index, ])
#       closest_coordinates <- st_coordinates(closest_point)
#       latKeep <- which(df$latitude %in% unname(closest_coordinates[,2]) & df$longitude %in% unname(closest_coordinates[,1]))
#       keep <- df[(latKeep),]
#       }
#     }
#
#   keep <- keep[order(keep$year),]
#
#   if (!(is.null(type))) {
#     if (type=="surface" && "depth" %in% names(df)) {
#       keep <- keep[which(keep$depth < 5),]
#     } else if (type == "bottom") {
#       keep <- keep %>%
#         dplyr::group_by(date) %>%
#         dplyr::slice_max(depth, n = 1, with_ties = FALSE) %>%
#         dplyr::ungroup()
#     }
#   }
#
#
#   NAMES <- names(df)
#   parameter <- NAMES[which(!(NAMES %in% c('latitude', "longitude", "type", "geom", "year", "units", "species_name", "area", "depth")))]
#
#   grouped_list <- split(keep, keep$year)
#   if (!("whale_biodiversity" %in% names(df))) {
#   yearly_avg <- sapply(grouped_list, function(df) mean(df[[parameter]], na.rm=TRUE))
#   } else {
#     yearly_avg <- sapply(grouped_list, function(df) length(unique(df$whale_biodiversity)))
#
#   }
#
#   names(yearly_avg) <- names(grouped_list)
#   yearly_avg <- unlist(yearly_avg)
#
#   # Convert to a data frame for plotting
#   if (!(dataframe)) {
#   plot_data <- data.frame(
#     year = as.numeric(names(yearly_avg)),  # Convert year to numeric
#     avg_parameter = unname(yearly_avg),
#     parameter_name=rep(parameter),
#     type=ifelse(!(is.null(type)), rep(type), ""),
#     units=unique(df$units)
#   )
#   } else {
#     plot_data <- keep
#   }
#
#   if (dataframe & (!(map))) {
#     return(plot_data)
#   }
#   # Base R plot
#
#   if (!(dataframe) & !(map)) {
#     plot(
#       plot_data$year, plot_data$avg_parameter,
#       type = "b",                    # Line and points
#       col = "blue",                  # Line color
#       pch = 19,                      # Point style (solid circle)
#       xlab = "Year",                 # Label for x-axis
#       ylab = ifelse(is.null(type), paste0("Average ", parameter, " (", unique(plot_data$units), ")"), paste0(ifelse(type=="surface", paste0("Average Surface ",parameter), paste0("Average Bottom ", parameter)), " (", unique(plot_data$units, ")")))
#     )
#   }
#
#   if (map) {
#     if (!("geom" %in% names(df))) {
#       inside2 <- which(sf::st_within(points_sf, outside_exclusive_multipolygon, sparse = FALSE)[,1])
#       insideKeep <- unique(c(inside2, which(inside[,1])))
#
#       latitude <- df$latitude[insideKeep]
#       longitude <- df$longitude[insideKeep]
#
#
#
#       if (length(df$latitude[insideKeep]) > 1000) {
#         latitude <- round(latitude,1)
#         longitude <- round(longitude,1)
#         coord <- data.frame(latitude, longitude)
#
#         # Get unique pairs
#         unique_coords <- unique(coord)
#         latitude <- unique_coords$latitude
#         longitude <- unique_coords$longitude
#
#       }
#
#       mapdf <- list(
#         latitude=latitude,
#         longitude=longitude,
#         area=multipolygon,
#         outside=outside_exclusive_multipolygon
#       )
#     } else {
#       st_crs(df$geom) <- 4326
#       geometry <- st_transform(df$geom, st_crs(multipolygon))
#       geometry <- st_make_valid(geometry)
#       overlaps_poly <- st_intersects(geometry, multipolygon, sparse = FALSE)
#
#
#       sf_df <- st_sf(df, geometry = geometry)
#       geometry <- unique(geometry[which(overlaps_poly[,1])])[[1]]
#
#       geometry_sf <- st_sfc(geometry)
#       multipolygon_sf <- st_sfc(multipolygon)
#       outside_sf <- st_sfc(outside_exclusive_multipolygon)
#
#       mapdf <- list(
#         geom=geometry_sf,
#         area=multipolygon_sf,
#         outside=outside_sf
#       )
#     }
#
#     return(mapdf)
#
#   }
# }
