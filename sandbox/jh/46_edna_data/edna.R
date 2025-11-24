root <- "../eDNA-for-MPAs/data/"

# Get all CSVs that contain 'GOTeDNA' anywhere in the full path
eDNA_csv <- list.files(
  path = root,
  pattern = "GOTeDNA.*\\.csv$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

paths <- sub(".*eDNA-for-MPAs/data/", "", eDNA_csv)
cruise <- sub("/.*", "", paths)
grouped <- split(eDNA_csv, cruise)


big_df <- list()
counter <- 1


for (i in seq_along(grouped)) { # GROUP BY CRUISE MISSION
  message("i= ", i)

  METADATA <- grouped[[i]][which(grepl("Metadata", grouped[[i]], ignore.case=TRUE))]

  DATA <- grouped[[i]][which(!grepl("Metadata", grouped[[i]], ignore.case=TRUE))]

  location <- sub("^[^/]+/[^/]+/[^/]+/[^/]+/([^/]+).*", "\\1", METADATA)

  if (any(grepl("GOTeDNA", location, ignore.case=TRUE))) {
    # FORMATTED INCORRECT (NJ)
    bad_location <- which(grepl("GOTeDNA", location, ignore.case=TRUE))

    location_options <- c("ESI", "SAB")

    hits <- location_options[ sapply(location_options, function(x)
      grepl(x, location[bad_location], ignore.case = TRUE)
    ) ]

    if (length(hits) == 0) {
      # COULD NOT FIND LOCATION NAME IN METADATA, NOW LOOKING IN DATA NAME
      hits <- location_options[ sapply(location_options, function(x)
        grepl(x, DATA[bad_location], ignore.case = TRUE)
      ) ]

      if (length(hits) == 0) {
        stop("There was a new location added, and formatting is incorrect.")
      }
    }

    location[bad_location] <- hits

  }

  for (j in seq_along(location)) { # CYCLE THROUGH ALL LOCATIONS ON CRUISE MISSION
    message("j= ", j)
    keepj <- which(grepl(location[j], DATA, ignore.case=TRUE))

    for (k in seq_along(keepj)) { # CYCLE THROUGH ALL SAMPLE TYPES AT EACH LOCATION FOR EACH MISSION

      ## FIXME: SHOULD LOOK INTO THIS
      message("k= ", k)

      name_of_data <- DATA[keepj][k]
      data <- read.csv(name_of_data)
      metadata <- read.csv(METADATA[j])

      if (!("materialSampleID" %in% names(metadata))) {
        # NOT CONSISTENT (NJ) - METADATA FILES USUALLY MATERIALSAMPLEID
        names(metadata)[which(names(metadata) == "SampleID")] <- "materialSampleID"
      }
      metadata$materialSampleID <- gsub("-", ".", metadata$materialSampleID)
      # ABOVE NOT CONSISTENT (NJ) - SOMETIMES MATERIALSAMPLEID _ AND SOMETIMES .


      HIT_IDs <- names(data)[which(names(data) %in% metadata$materialSampleID)]

      if (length(HIT_IDs) == 0) {
        message("Naming convention change")
        names(data) <- sub("^X", "", names(data)) # Sometimes there are leading X's
        HIT_IDs <- names(data)[which(names(data) %in% metadata$materialSampleID)]

      }

      if (length(HIT_IDs) == 0) {
        message("Naming convention change")
        message("Special case for i = 4, j=1,k=1 using both materialSampleID and eDNATube. Not consistent NJ")

        metadata$materialSampleID <- gsub("_", ".", metadata$materialSampleID)
        metadata$eDNA_Tube <- gsub("_", ".", metadata$eDNA_Tube)

        metadata$materialSampleID[which(!(grepl("AZMP", metadata$materialSampleID)))] <- metadata$eDNA_Tube[which(!(grepl("AZMP", metadata$materialSampleID)))]

        HIT_IDs <- names(data)[which(names(data) %in% metadata$materialSampleID)]



      }

      if (length(HIT_IDs) < 5) {
        # ONLY 5 SAMPLES MATCH. THIS IS A BANDAID FIX
        #length_5_keep <- which(!(metadata$materialSampleID) %in% names(data))
        length_5_keep <- which(!(metadata$materialSampleID %in% names(data)))

        metadata$materialSampleID[length_5_keep] <- gsub("_", ".", metadata$materialSampleID[length_5_keep])
        HIT_IDs <- names(data)[which(names(data) %in% metadata$materialSampleID)]

      }




      df <- data.frame(ID= HIT_IDs,
                       date= as.Date(rep(NA, length(HIT_IDs))),
                       latitude=NA,
                       longitude=NA,
                       species_richness=NA,
                       method=NA,
                       location=NA)

      for (l in seq_along(df$ID)) { ## CYCLE THROUGH EACH SAMPLE FOR EACH SAMPLE TYPE
        message("l= ", l)


        keep <- which(metadata$materialSampleID == df$ID[l])

        if (!(length(keep) == 0)) {
          df$date[l] <- as.Date(metadata$eventDate[keep], "%m/%d/%Y")

          if ("decimalLatitude" %in% names(metadata)) {
            df$latitude[l] <- metadata$decimalLatitude[keep]
            df$longitude[l] <- metadata$decimalLongitude[keep]
          } else {
            df$latitude[l] <- metadata$Lat[keep]
            df$longitude[l] <- metadata$Long[keep]
          }



          ACTUAL_DATA <- data[[df$ID[l]]]

          # NOT CONSISTENT NAMING (v6 NJ)

          if (!("Species" %in% names(data))) {
            names(data)[which(names(data) == "species")] <- "Species"

            if (!("Species" %in% names(data))) {
              names(data)[which(names(data) == "V6")] <- "Species"
            }

            if (!("Species" %in% names(data))) {
              browser()
            }
          }
          df$species_richness[l] <- length(unique(data$Species[which(!(ACTUAL_DATA == 0))]))
          df$method[l] <- sub(".*eDNA-for-MPAs/data/[^/]+/[^/]+/([^/]+).*", "\\1", name_of_data)
          df$location[l] <- location[j]


          ## and add everything to main mega table.
          ## tweak formatting of . - _


        } else {
          browser()
        }

      }

      big_df[[counter]] <- df
      counter <- counter + 1

    }
  }
}

final_df <- do.call(rbind, big_df)
final_df$latitude <- round(as.numeric(final_df$latitude), 2)
final_df$longitude <- round(as.numeric(final_df$longitude), 2)

b1 <- which(is.na(final_df$latitude))
b2 <- which(is.na(final_df$longitude))

final_df <- final_df[-intersect(b1,b2),]
final_df$year <- as.numeric(format(final_df$date, "%Y"))

## INCONSISTENT: SOME LONGITUDES ARE IN THE POSITIVES - NJ

final_df$longitude[which(final_df$longitude > 0)] <- final_df$longitude[which(final_df$longitude > 0)]*-1

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    lat = final_df$latitude,
    lng = final_df$longitude,
    popup = paste0("Lat: ", final_df$latitude,
                   "<br>Lon: ", final_df$longitude)
  )

