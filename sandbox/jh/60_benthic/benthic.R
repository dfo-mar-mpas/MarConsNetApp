library(httr2)
library(readxl)
library(terra)

## BIOLOGICAL DATA

# url <- "https://data.mendeley.com/public-files/datasets/n8yk8rds9y/files/301c6642-352b-4290-abc9-90339ec22eea/file_downloaded"
#
# file <- tempfile(fileext = ".csv")
#
# request(url) |>
#   req_perform() |>
#   resp_body_raw() |>
#   writeBin(file)
#
# biological_data1 <- read.csv(file) # Occurence data of 99 epibenthic invertebrate species selected for community analysis

## Now data2:
url2 <- 'https://data.mendeley.com/public-files/datasets/n8yk8rds9y/files/c2e8c6d9-f07f-4f89-804a-871d6512e487/file_downloaded'
file2 <- tempfile(fileext = ".xlsx")

request(url2) |>
  req_perform() |>
  resp_body_raw() |>
  writeBin(file2)

biological_data2 <- read_excel(file2) # Occurrence data of 317 epifaunal taxa found on the Scotian Shelf and Gulf of Maine/ Bay pf Fundy during summer RV surveys

# Reshaping the data
# Identify the species columns
species_start <- which(names(biological_data2) == "Abietinaria_abietina")
species_cols <- species_start:ncol(biological_data2)

# Create output data frame
df <- data.frame(
  ID = paste0(biological_data2$Mission, biological_data2$Set),
  latitude = biological_data2$`Start Latitude`,
  longitude = biological_data2$`Start Longitude`,
  species = NA_character_,
  detections = NA_character_,
  year_of_data_collection = 2017,
  stringsAsFactors = FALSE
)

# Cycle through each sample
for (l in seq_len(nrow(biological_data2))) {

  # Get species and their detections for this sample
  species_values <- biological_data2[l, species_cols]

  keep <- which(as.numeric(species_values) > 0)

  if (length(keep) > 0) {
    species_names <- names(species_values)[keep]
    detection_values <- as.numeric(species_values[keep])

    # Species
    df$species[l] <- paste0(
      species_names,
      collapse = ", "
    )

    # Detections
    df$detections[l] <- paste0(
      detection_values,
      collapse = ", "
    )
  }
}

df <- df %>%
  separate_rows(species, detections, sep = ",\\s*") %>%
  mutate(detections = as.numeric(detections))

df$species <- clean_species_names(df$species)
df$class <- NA
df$common_name <- NA
df$subclass <- NA

for (i in seq_along(unique(df$species))) {
  message(paste0("For loop ", i, " of ", length(unique(df$species))))
  df$subclass[which(df$species == unique(df$species)[i])] <- taxize_species(unique(df$species)[i], level="Subclass")
  df$class[which(df$species == unique(df$species)[i])] <- taxize_species(unique(df$species)[i], level='Class')
  #df$common_name[which(df$species == unique(df$species)[i])] <- taxize_species(unique(df$species)[i], level='common_name')
}

df <- add_assumptions(
  df,
  caveats='Benthic data collected with trawling net (not great catchability for benthic organisms, so what is collected may not represent what is on the bottom'
)

df <- st_as_sf(
  df,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

df$stagnant_source <- TRUE
df$year_of_publication <- as.numeric(format(file.info(file2)$atime, "%Y"))



## ENVIRONMENTAL DATA

env_urls <- c(
  bottom_current_mean =
    "https://data.mendeley.com/public-files/datasets/n8yk8rds9y/files/0b49f9aa-f6f1-4c74-bb8b-91cfd4058941/file_downloaded",
  bottom_salinity_mean =
    "https://data.mendeley.com/public-files/datasets/n8yk8rds9y/files/dfbe8bdd-1168-4e08-844c-8830a2451013/file_downloaded",
  bottom_temperature_mean =
    "https://data.mendeley.com/public-files/datasets/n8yk8rds9y/files/64db951f-047c-4956-822a-105a2600400f/file_downloaded",
  depth =
    "https://data.mendeley.com/public-files/datasets/n8yk8rds9y/files/6470aecf-f963-4c71-8606-6889d193bd59/file_downloaded",
  fishing_effort_mobile =
    "https://data.mendeley.com/public-files/datasets/n8yk8rds9y/files/1973d23d-319a-4d7d-be56-951b977edece/file_downloaded",
  sediment_grain_size =
    "https://data.mendeley.com/public-files/datasets/n8yk8rds9y/files/d40463cb-3683-425a-87ab-a95ba09cd617/file_downloaded",
  slope =
    "https://data.mendeley.com/public-files/datasets/n8yk8rds9y/files/060bda2f-5a86-4e40-8129-a4d44b16622c/file_downloaded"
)

# Temporary directory for the TIFFs
env_dir <- tempfile("mendeley_environmental_")
dir.create(env_dir)

# Download files
env_files <- vapply(names(env_urls), function(x) {
  file <- file.path(env_dir, paste0(x, ".tif"))

  request(env_urls[[x]]) |>
    req_perform() |>
    resp_body_raw() |>
    writeBin(file)

  file
}, character(1))

# Load as terra rasters
env_rasters <- lapply(env_files, rast)
names(env_rasters) <- names(env_urls)

env_rasters$depth

## INDICATOR 1: Number of species per trophic level within each habitat type
## INDICATOR 1 IS HERE.

data_epibenthic_communities_biological <- df

   trophic_levels <- read_excel(paste0(dirname(path_to_store()), "/data/AI_trophic_groups.xlsx"))
   data_epibenthic_communities_biological$ai_trophic_level <- NA
   for (i in seq_along(unique(data_epibenthic_communities_biological$class))) {
     data_epibenthic_communities_biological$ai_trophic_level[which(data_epibenthic_communities_biological$class == unique(data_epibenthic_communities_biological$class)[i])] <- trophic_levels$trophic_group[which(trophic_levels$class == unique(data_epibenthic_communities_biological$class)[i])]
   }

   data_epibenthic_communities_biological$min_target <- 0
   data_epibenthic_communities_biological$max_target <- 30
   data_epibenthic_communities_biological$plainname <- 'the total region samplled by the RV survey'

   mpas <- MPAs %>%
     st_filter(data_epibenthic_communities_biological[which(data_epibenthic_communities_biological$ai_trophic_level == 'Predator'),]) %>%
     filter(NAME_E != "Non_Conservation_Area")

   x <- process_indicator(
     data = data_epibenthic_communities_biological[which(data_epibenthic_communities_biological$ai_trophic_level == 'Predator'),],
     readiness = "Ready",
     indicator_var_name = "detections",
     indicator = "Benthic species per trophic level within each habitat type",
     type = "in situ",
     units = NA, # FIXME
     scoring = "representation: regional relative ranking", # protection coverage (# this says how well each ara represents teh benthic biodiversity found in the broadrer region)
     PPTID = 395,
     source = "RV",
     project_short_title = "Mapping biodiversity and ecosystem services of benthic communities",
     bin_rationale = "FIXME",
     climate = FALSE,
     SME = "Javier Murillo Perez",
     indicator_rationale = "Direct biodiversity measure",
     areas = mpas,
     plot_type = c('detections'),
     plot_lm = FALSE,
     theme = "Trophic Structure and Function",
     objectives = c("Maintain biodiversity of individual species, communities and populations within the different ecotypes"),
     SME_validated = TRUE,
     other_nest_variables = c("species","ID", "year_of_data_collection", 'ai_trophic_level', 'min_target', 'max_target', 'stagnant_source', 'subclass', 'class', 'detections', 'latitude', 'longitude', 'common_name'),
     scale='region-site'
   )
# save_plots(dplyr::select(x, -data, -adjacent_data))
# dplyr::select(x, -plot)


## END OF INDICATOR 1





#tar_target(name=data_epibenthic_communities,
#           command={
#}),


# tar_map(
   #   values = tibble::tibble(trophic = sort(unique(data_edna_data$ai_trophic_level))),
   #   names = trophic,
#  tar_target(name = ind_species_per_trophic_edna, command = {
# }))),
