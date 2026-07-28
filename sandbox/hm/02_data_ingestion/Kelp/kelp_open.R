
##Open Data (in situ)

library(tidyverse)
library(rvest)
library(stringr)

occurrence <- read_csv("https://api-proxy.edh-cde.dfo-mpo.gc.ca/catalogue/records/f1a022a4-b9bf-47d0-b641-2067ea568962/attachments/Occurrence.csv")
event <- read_csv("https://api-proxy.edh-cde.dfo-mpo.gc.ca/catalogue/records/f1a022a4-b9bf-47d0-b641-2067ea568962/attachments/Event.csv")
measurements <- read_csv("https://api-proxy.edh-cde.dfo-mpo.gc.ca/catalogue/records/f1a022a4-b9bf-47d0-b641-2067ea568962/attachments/extendedMeasurementOrFact.csv")


species_data <- occurrence %>%
  left_join(
    event %>%
      select(
        eventID,
        decimalLatitude,
        decimalLongitude
      ),
    by = "eventID"
  ) %>%
  left_join(
    measurements %>%
      filter(
        !is.na(occurrenceID),
        measurementType == "mean percent cover"
      ) %>%
      select(
        occurrenceID,
        measurementValue
      ) %>%
      rename(
        percent_cover = measurementValue
      ),
    by = "occurrenceID"
  )


depth_data <- measurements %>%
  filter(
    measurementType == "mean corrected depth",
    !is.na(eventID)
  ) %>%
  select(
    eventID,
    depth = measurementValue
  )

combined_data <- species_data %>%
  left_join(depth_data, by = "eventID")


combined_data <- combined_data %>%
  select(
    occurrenceID,
    occurrenceStatus,
    eventID,
    eventDate,
    scientificNameID,
    scientificName,
    decimalLatitude,
    decimalLongitude,
    percent_cover,
    depth
  )

##PROCESS INDICATOR

data <- combined_data
data$occurrence_id <-as.character(combined_data$occurrenceID)
data$occurrence_status <-as.character(combined_data$occurrenceStatus)
data$latitude <- as.numeric(combined_data$decimalLatitude)
data$longitude <- as.numeric(combined_data$decimalLongitude)
data$year_of_data_collection <- as.numeric(format(combined_data$eventDate, "%Y"))
data$percent_cover <- as.numeric(combined_data$percent_cover)
data$depth <- as.numeric(combined_data$depth)


data <- data[, c(
  "occurrence_id",
  "occurrence_status",
  "latitude",
  "longitude",
  "year_of_data_collection",
  "percent_cover",
  "depth"
)]

data$year_of_publication <- {

  url <- "https://open.canada.ca/data/en/dataset/f1a022a4-b9bf-47d0-b641-2067ea568962/resource/a47f5b93-9424-48a5-a65c-5fd5c6bc2b70"
  page <- read_html(url)
  page_text <- html_text2(page)

  page_text |>
    str_extract("(?<=Data last updated )\\w+ \\d{1,2}, \\d{4}") |>
    str_extract("\\d{4}")
}


x <- process_indicator(
  data = data,
  indicator_var_name = "ind_distinctive_benthic_characteristics",      ##This would replace the St Anns Bank placholder indicator
  indicator = "Diversity and community composition of the benthos and characteristics of surficial geology at selected sampling stations located in the identified distinctive seabed features of the AOI, plus abundance or biomass and size composition of the defining benthic taxa of those features",
  type = "in situ",
  units = " ",
  scoring = "coverage",
  source = "Open Data (DFO)",
  project_short_title = " Camera surveys of the Subtidal Flora",
  areas = MPAs,
  climate_expectation = "FIXME",
  indicator_rationale = "Kelp forests support high biodiversity and productivity, and provide ecosystem services",
  bin_rationale = "FIXME",
  plot_type = c("time-series", "map"),
  year = 'year_of_data_collection',
  objectives = c(
    "Protect unique, rare, or sensitive ecological features",
    "Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
    "Habitat required for all species, particularly priority species, is maintained and protected"
    ),
  theme = "Benthic Environment",
  SME = "Unknown",
  control_polygon = control_polygons,
  plot_lm = FALSE
)

save_plots(dplyr::select(x, -data, -adjacent_data))
dplyr::select(x, -plot)


##Fun Plots
library(ggplot2)

ggplot(data, aes(x = longitude, y = latitude)) +
  geom_point(aes(size = precent_cover, colour = occurrence_status)) +
  theme_minimal() +
  labs(
    title = "Distribution of Macroalgae in Nova Scotia and SW New Brunswick",
    x = "Longitude",
    y = "Latitude",
    size = "Percent cover",
    colour = "Occurrence status"
  )












