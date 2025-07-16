library(targets); library(dplyr); library(argoFloats); library(azmpdata); library(raster); library(shiny); library(leaflet)
library(dplyr); library(sf); library(shinyjs); library(viridis); library(arcpullr); library(devtools)
library(MarConsNetAnalysis);library(MarConsNetData);
library(TBSpayRates); library(dataSPA); library(readxl)
library(ggplot2); library(shinyBS); library(Mar.datawrangling); library(DT); library(rmarkdown)
library(stringr); library(tidyr); library(officer); library(RColorBrewer); library(rvest); library(httr2); library(httr);
library(tidyverse); library(dplyr)


tar_load(c("APPTABS","pillar_ecol_df","all_project_geoms","MPA_report_card","MPAs","areas","regions","odf","flowerPalette","indicatorFlower","Objectives_processed","N_Objectives","om","Ecological", "Context", "collaborations", "deliverables", "csas", "climate"))
source("R/app.R");app()

tar_make(c("all_project_geoms", "MPA_report_card", "odf", "pillar_ecol_df", "plot_files"),
         script=tar_paths("inst"),
         store=tar_paths())
tar_load(c("all_project_geoms", "MPA_report_card", "odf", "pillar_ecol_df", "plot_files"))


# TEST 1 (large legend)
tar_load(MPAs)
tar_load(data_musquash_benthic_infauna)
data <- data_musquash_benthic_infauna |>
  group_by(scientificName_Nom_scientifique) |>
  reframe(geoms = st_make_valid(st_union(geoms))) |>
  st_as_sf()

subclass <- NULL
for (i in seq_along(data$scientificName_Nom_scientifique)) {
  message(i)
  result <- try(worrms::wm_records_name(data$scientificName_Nom_scientifique[i]), silent=TRUE)
  if (inherits(result, "try-error")) {
    subclass[i] <- NA
  } else {
    aphia_id <- result$AphiaID[1]  # Use the first match, or refine if needed
    classification <- worrms::wm_classification(id = aphia_id)
    subclass[i] <- ifelse(length(classification$scientificname[which(classification$rank == "Subclass")]) == 0, NA, classification$scientificname[which(classification$rank == "Subclass")])
  }
}

data$subclass <- subclass

x <- process_indicator(data = data,
                  indicator_var_name = "scientificName_Nom_scientifique",
                  indicator = "Infaunal Diversity",
                  type = "Observations",
                  units = NA,
                  scoring = "representation: cumulative distribution with regional thresholds",
                  PPTID = 827,
                  source="Open Data (DFO)",
                  climate_expectation="FIXME",
                  indicator_rationale="FIXME",
                  other_nest_variables = "subclass",
                  bin_rationale="FIXME",
                  project_short_title = "Musquash benthic monitoring",
                  areas = MPAs[MPAs$NAME_E=="Musquash Estuary Marine Protected Area",],
                  plot_type='map-species',
                  plot_lm=FALSE)

# TEST 2 (violin haddock)
tar_load(rv_data)
data = rv_data |>
  filter(COMM %in% c("HADDOCK")) |>
  mutate(longitude = LONGITUDE,
         latitude = LATITUDE,
         haddock_counts = TOTNO,
         year = YEAR)  |>
  dplyr::select(longitude, latitude, year, haddock_counts)

x <- process_indicator(data = data,
                  indicator_var_name = "haddock_counts",
                  indicator = "Haddock Number per Tow",
                  type = "Ecosystem Trawl Survey",
                  units = "Number per Tow",
                  scoring = "desired state: increase",
                  PPTID = 726,
                  source="RV Survey",
                  climate_expectation="FIXME",
                  indicator_rationale="FIXME",
                  bin_rationale="FIXME",
                  project_short_title = "RV Survey",
                  areas = MPAs,
                  plot_type = c("violin", "map"),
                  plot_lm=FALSE)

# TEST 3 (inside vs outside)
tar_load(data_azmp_Discrete_Occupations_Sections)
tar_load(control_polygons)
data <- data_azmp_Discrete_Occupations_Sections  |>
  dplyr::select(longitude, latitude, year, depth, nitrate)

x <- process_indicator(data = data,
                  indicator_var_name = "nitrate",
                  indicator = "Nutrient Conditions (Nitrate) Inside Outside Comparison",
                  type = "Discrete Occupations Sections",
                  units = "mmol/m3",
                  scoring = "control site linear trend: less inside",
                  PPTID = 579,
                  source="AZMP",
                  project_short_title = "AZMP",
                  climate_expectation="FIXME",
                  indicator_rationale="FIXME",
                  bin_rationale="FIXME",
                  climate = TRUE,
                  other_nest_variables="depth",
                  areas = MPAs,
                  plot_type = c('outside-comparison','map'),
                  plot_lm=FALSE,
                  control_polygon=control_polygons)
# TEST 4: Phosphate
tar_load(data_musquash_eutrophication)
data <- data_musquash_eutrophication |>
  rename(phosphate= `tot P (mg/L)`) |>
  dplyr::select(Lon, Lat, phosphate, year)

x <- process_indicator(data = data,
                  indicator_var_name = "phosphate",
                  indicator = "Nutrient Conditions (Phosphate)",
                  type = "Discrete Occupations Sections",
                  units = "mg/L",
                  scoring = "desired state: increase",
                  PPTID = NA,
                  source="Eastern Charlotte Waterways",
                  project_short_title = NA,
                  climate = TRUE,
                  climate_expectation="FIXME",
                  indicator_rationale="FIXME",
                  bin_rationale="FIXME",
                  areas = MPAs[MPAs$NAME_E=="Musquash Estuary Marine Protected Area",],
                  plot_type=c('time-series-no-line','map'),
                  plot_lm=FALSE,
                  latitude='Lat',
                  longitude='Lon')

