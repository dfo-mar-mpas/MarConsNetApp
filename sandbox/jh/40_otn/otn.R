proj_start <- ymd("20190101")
proj_end <- ymd("20230909")
proj_long_upp <- -40.00
proj_long_low <- -70.00
proj_lat_upp <- 60.00
proj_lat_low <- 40.00

tar_load(MPAs)
MPAs <- MPAs[-which(MPAs$NAME_E == "Non_Conservation_Area"),]

geoserver_receivers <- readr::read_csv('https://members.oceantrack.org/geoserver/otn/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=otn:stations_receivers&outputFormat=csv', guess_max = 13579)

geoserver_tag_releases <- readr::read_csv('https://members.oceantrack.org/geoserver/otn/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=otn:animals&outputFormat=csv', guess_max = 13579)

geoserver_tag_releases <- geoserver_tag_releases %>%
  filter(yearcollected > 2018)

geoserver_projects <- readr::read_csv('https://members.oceantrack.org/geoserver/otn/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=otn:otn_resources_metadata_points&outputFormat=csv', guess_max = 13579)


otn_stations <- geoserver_receivers %>%
  # Remove rows with missing deploy_date values
  filter(!is.na(deploy_date)) %>%

  # Select stations that fall within the project timeframe defined above
  filter((deploy_date > proj_start & deploy_date < proj_end) |
           (recovery_date < proj_end & recovery_date > proj_start) |
           (deploy_date < proj_end & is.na(recovery_date) & deploy_date > proj_start - duration(18, 'months')) |

           # Select specific models within certain date ranges
           (grepl('VR3', model) & deploy_date < proj_end & is.na(recovery_date) & deploy_date > proj_start - duration(4, 'years')) |
           (grepl('VR4', model) & deploy_date < proj_end & is.na(recovery_date) & deploy_date > proj_start - duration(6, 'years'))) %>%

  # Filter stations based on latitude and longitude bounds
  filter(stn_lat >= proj_lat_low & stn_lat <= proj_lat_upp &
           stn_long >= proj_long_low & stn_long <= proj_long_upp)


columns_to_keep <- c("FID", "institutioncode", "datacenter_reference", "scientificname", "vernacularname","longitude", "latitude", "basisofrecord", "yearcollected", "collector", "classname")

# Filter and select the desired columns
otn_animals <- geoserver_tag_releases %>%
  filter(longitude >= proj_long_low & longitude <= proj_long_upp &
           latitude >= proj_lat_low & latitude <= proj_lat_upp) %>%
  dplyr::select(all_of(columns_to_keep))

# Define the columns to keep
columns_to_keep_projects <- c("FID", "resource_full_name", "ocean", "seriescode", "status", "collaborationtype")

# Filter and select the desired columns
otn_projects <- geoserver_projects %>%
  filter(seriescode == "DFOCanada") %>%
  dplyr::select(all_of(columns_to_keep_projects))


# Create a Leaflet map for receiver stations
map <- leaflet(otn_stations) %>%
  # Add default basemap
  addTiles() %>%
  # Add MPA polygons
  addPolygons(data = MPAs,
              color = "yellow",
              weight = 0,
              fillOpacity = 1,
              label = ~NAME_E) %>%

  # Add circle markers for each station
  addCircleMarkers(
    lng = ~stn_long,
    lat = ~stn_lat,
    label = ~paste(seriescode, instrumenttype, sep = " - "),
    radius = 0.5,  # Adjust the circle marker size
    color = ~ifelse(seriescode == "DFOCanada", "red", "blue"),  # Marker color
    fillOpacity = 0.8  # Adjust fill opacity
  ) %>%

  # Add bounding box based on filter
  addRectangles(
    lng1 = proj_long_low,      # Left longitude
    lat1 = proj_lat_low,       # Lower latitude
    lng2 = proj_long_upp,      # Right longitude
    lat2 = proj_lat_upp,       # Upper latitude
    weight = 2.5,              # Border width
    color = "darkorange",      # Border color
    fill = FALSE               # Don't fill the rectangle
  ) %>%

  # Add legend for all elements of the plot
  addLegend(
    position = "bottomleft",
    colors = c("blue", "red", "darkorange", "yellow"),
    labels = c("All OTN", "OTN-DFO", "Data filter", "Marine Protected Areas"),
    title = "Legend"
  ) %>%

  # Add filter control displaying the range date
  addControl(
    html = sprintf("<strong>Filter:</strong> %s - %s", proj_start, proj_end)
  )


#Calculate the total number of receivers for each seriescode (OTN vs OTN-DFO).
summary_table <- otn_stations %>%
  group_by(seriescode) %>%
  summarize(
    Total_Receivers = n()
  ) %>%
  # Calculate the percentage relative to the total and round it.
  mutate(
    Percentage_Relative_To_Total = paste(round((Total_Receivers / sum(Total_Receivers)) * 100, 0), "%"))

# Rename columns
colnames(summary_table)[colnames(summary_table) == "Total_Receivers"] <- "Total # of Receivers"
colnames(summary_table)[colnames(summary_table) == "Percentage_Relative_To_Total"] <- "% Receivers relative to total"

# Print the summary table as a Markdown table.
table_receivers <- knitr::kable(
  summary_table,
  format = "markdown",
  align = "l",
  width = "100%"
)




# NEWER TEST
tar_load(data_obis)
#In some cases, tags does not have unique collectioncode (e.g. PBSM-1513746-2022-07-18).
# This is because of the 'vernacularname' (e.g. Atlantic mackerel to mackerel). I therefore subset tags for the first occurrence of the unique catalognumberID.
tags <- readr::read_csv('https://members.oceantrack.org/geoserver/otn/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=otn:animals&outputFormat=csv', guess_max = 13579)
tags <- tags %>%
  group_by(catalognumber) %>%   # group by catalognumber
  slice(1) %>%                  # keep the first row in each group
  ungroup()                     # ungroup after slicing

# 1. Subset obis for just Ocean Tracking Network
otn_detection <- data_obis[which(data_obis$rightsHolder == "Ocean Tracking Network, Dalhousie University, Halifax, Nova Scotia otndc@dal.ca" & !(is.na(data_obis$decimalLatitude)) & !(is.na(data_obis$decimalLongitude))),]


# 2. Remove all NA columns
bad <- NULL
for (i in seq_along(names(otn_detection))) {
  bad[i] <- all(is.na(otn_detection[[names(otn_detection)[i]]]))
}

oo <- otn_detection[, -which(bad)] # Removing all NA columns

# 3. Further subset to only look at relevant columns
ooo <- oo[, c("dataset_id", "id", "aphiaid", 'catalogNumber', 'collectionCode', 'datasetID', "datasetName", 'decimalLatitude', "decimalLongitude", "eventDate", 'eventID', "occurrenceID", 'organismID', 'organismName', 'rightsHolder', 'scientificName', "NAME_E.x", "geometry")]

# 4.  Remove the ones that say 'release' and 'capture' in occurence ID
ooo <- ooo[-which(grepl("release", ooo$occurrenceID)),]
ooo <- ooo[-which(grepl("capture", ooo$occurrenceID)),]

# 5. Only keep obis data that has tags$collectioncode in ooo$occurenceID (e.g. IBFS).
## This is to allow us to find the tag ID for all obis

keeper <- list()
for (i in seq_along(unique(tags$collectioncode))) {
  keeper[[i]] <- which(grepl(unique(tags$collectioncode)[i], ooo$occurrenceID))

}

keep <- unlist(keeper)
ooo <- ooo[keep,]


#Extracting 'tag' info from occurenceID
## (Trial and error)- tags$catalogNumber gives us the tag ID. It can be grepl in ooo$occurenceID


pattern_dash <- "^.*?(\\d{4}-\\d{2}-\\d{2}T[^_]*_)"
pattern_underscore <- "^.*?(\\d{4}_\\d{2}_\\d{2}T[^_]*_)"

cut1 <- sapply(ooo$occurrenceID, function(x) {
  if (grepl(pattern_dash, x)) {
    sub(pattern_dash, "", x)
  } else if (grepl(pattern_underscore, x)) {
    sub(pattern_underscore, "", x)
  } else {
    sub("^.*?_", "", x)  # remove everything before the first underscore
  }
})


cut2 <- sub("_.*$", "", cut1)

bad_tags <- ooo$occurrenceID[which(!cut2 %in% tags$catalognumber)]
bad <- which(!cut2 %in% tags$catalognumber) # FIXME: this could use more work.

## 6. Remove ones that could not get an associated tag
ooo <- ooo[-bad,]
ooo$tag_id <- cut2[which(cut2 %in% tags$catalognumber)]

tracking <- split(ooo, ooo$tag_id)

## plotting

ddff <- tracking[[128]]


# leaflet map with points and line
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = ddff, radius = 4, color = "red")



