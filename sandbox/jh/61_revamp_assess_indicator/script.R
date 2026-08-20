data <- DATA2
x <- process_indicator(
  data = data[which(data$class == "Teleostei"), ],
  readiness = "Ready",
  indicator_var_name = "detections",
  indicator = "Diversity of the benthos",
  type = "in situ",
  units = "read number",
  scoring = "desired trend: no decrease",
  PPTID = 480,
  source = "eDNA",
  project_short_title = "Animal Acoustic Tagging",
  bin_rationale = "FIXME",
  climate = FALSE,
  SME = "Ryan Stanley and Nick Jeffery",
  indicator_rationale = "Direct biodiversity measure",
  areas = MPAs,
  plot_type = c("map", "community_composition"),
  plot_lm = FALSE,
  theme = "Benthic Environment",
  objectives = c(
    "Protect Vazella pourtalesi glass sponges",
    "Protect continental shelf habitats and associated benthic and demersal communities",
    "Conserve and protect marine areas of high biodiversity at the community, species, population and genetic levels within the MPA"
  ),
  SME_validated = TRUE,
  other_nest_variables = c("species", "year_of_data_collection", "ID", "date", "species_richness", "method", "location", "year_of_publication", "subclass", "common_name", "class")
)



## Zooplankton (desired trend increase)
data <- data_azmp_zooplankton_annual_stations |>
  mutate(Calanus_finmarchicus_biomass = Calanus_finmarchicus_log10) |>
  dplyr::select(longitude, latitude, year, Calanus_finmarchicus_biomass)

names(data)[which(names(data) == 'year')] <- 'year_of_data_collection'
data$year_of_publication <- 2025

x <- process_indicator(
  data = data,
  indicator = "Biomass of Zooplankton (Calanus finmarchicus)",
  indicator_var_name = "Calanus_finmarchicus_biomass",
  type = "in situ",
  units = "log10 of abundance",
  scoring = "desired trend: increase",
  PPTID = 579,
  source = "AZMP",
  climate_expectation = "FIXME",
  control_polygon = control_polygons,
  indicator_rationale = "FIXME",
  bin_rationale = "FIXME",
  project_short_title = "AZMP",
  areas = MPAs,
  plot_type = c('time-series', 'map'),
  plot_lm = FALSE,
  theme = "Secondary Production",
  SME = "Unknown",
  objectives = c(
    "Conserve and protect biological productivity across all trophic levels so that they are able to fulfill their ecological role in the ecosystems of the MPA",
    "Maintain/promote ecosystem structure and functioning",
    "Maintain Functional Biodiversity",
    "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"
  )
)


## AIS BIOFOULING REPRESENTATION

data <- data_MAR_biofouling_AIS$AIS_AllSpecies_2021_PA |>
  filter(cover_index == 1) |>
  group_by(species_name) |>
  reframe(geoms = st_make_valid(st_union(Shape))) |>
  st_as_sf()
data$year_of_publication <- 2021

x <- process_indicator(
  data = data,
  indicator_var_name = "species_name",
  indicator = "Biofouling AIS representation",
  type = "in situ",
  units = NA,
  scoring = 'representation: regional relative ranking',
  direction = "inverse",
  PPTID = NA,
  climate_expectation = "FIXME",
  indicator_rationale = "FIXME",
  SME = "Unknown",
  bin_rationale = "FIXME",
  source = "Open Data (DFO)",
  project_short_title = "Biofouling AIS",
  areas = MPAs <- MPAs |>
    dplyr::filter(NAME_E != "Non_Conservation_Area"),
  plot_type = 'map',
  plot_lm = FALSE,
  theme = "Anthropogenic Pressure and Impacts",
  objectives = c(
    "Minimize unintended introduction and transmission of invasive species",
    "Prevent and Mitigate Invasive Alien Species"
  ))


## WELL PROXIMITY (point representation)

data <- data_offshore_energy_wells
data$year_of_publication <- 2025

data <- data[, c(
  "latitude",
  'longitude',
  'year_of_publication',
  'Well Name'
)]
names(data)[which(names(data) == 'Well Name')] <- 'well_name'

data <- data |>
  dplyr::filter(!is.na(longitude), !is.na(latitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

names(data)[which(names(data) == 'geometry')] <- 'geoms'
st_geometry(data) <- "geoms"

x <- process_indicator(
  data = data,
  indicator_var_name = "well_name",
  indicator = "Number of Offshore Energy Wells",
  type = "in situ",
  units = NA,
  scoring = 'representation: regional relative ranking',
  direction = "inverse",
  PPTID = NA,
  climate_expectation = "FIXME",
  indicator_rationale = "Exposure to petroleum can cause biological effects, changes in behavior and modify benthic communities.",
  SME = "Unknown",
  bin_rationale = "FIXME",
  source = "Open Data (DFO)",
  project_short_title = "Offshore Wells",
  areas = MPAs[MPAs$region == "Maritimes", ],
  plot_type = 'map',
  plot_lm = FALSE,
  theme = "Anthropogenic Pressure and Impacts",
  objectives = c(
    "Minimize the disturbance of seafloor habitat and associated benthic communities caused by human activities",
    "Manage the disturbance of benthic habitat that supports juvenile and adult haddock and other groundfish species",
    "Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
    "conserve and protect benthic (seabed) habitats"
  )
)

# Testing the new coverage:
#ind_distinctive_benthic_characteristics_kelp

data <- data_kelp_modelled  %>%
  filter(suitable_habitat) %>%
  select(suitable_habitat, habitat_type, geometry) %>%
  group_by(suitable_habitat, habitat_type) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

onedrive <- Sys.getenv("OneDriveCommercial")


data$year_of_publication <- {

  ld_path <- file.path(
    onedrive,
    "Krumhansl, Kira (DFO_MPO)'s files - 2021 2024 Species Distribution Model Outputs",
    "Laminaria digitata",
    "Laminaria_digitata_Bathy_rm4_20240223_avg_Binary.tif"
  )

  sl_path <- file.path(
    onedrive,
    "Krumhansl, Kira (DFO_MPO)'s files - 2021 2024 Species Distribution Model Outputs",
    "Saccharina latissima",
    "Saccharina_latissima_Bathy_rm2MinusRugosityAndProfile_20240223_avg_Binary.tif"
  )

  # Get modified dates
  file_dates <- file.info(c(ld_path, sl_path))$mtime
  format(max(file_dates), "%Y")
}

data$min_target <- 30
data$max_target <- 100
data$plainname <- 'the total modelled kelp region that is less than 30 m'

mpas <- MPAs %>%
  st_filter(data) %>%
  filter(NAME_E != "Non_Conservation_Area")

musquash <- st_transform(
  mpas,
  crs(shallow_bathymetry)
)

cols <- cellFromXY(
  bathy,
  cbind(
    c(st_bbox(musquash)$xmin, st_bbox(musquash)$xmax),
    c(st_bbox(musquash)$ymin, st_bbox(musquash)$ymax)
  )
)
musquash_ext <- ext(st_bbox(musquash))

bathy_musquash <- crop(bathy, musquash_ext)

# Only include data that is in 30 m or less
shallow_poly <- as.polygons(
  bathy_musquash,
  values = TRUE,
  na.rm = TRUE
) |>
  st_as_sf() |>
  filter(elevation == 1)

data <- st_intersection(
  data |> st_make_valid(),
  shallow_poly
) |>
  filter(suitable_habitat == TRUE)

x <- process_indicator(
  data = data,
  indicator_var_name = "suitable_habitat",
  indicator = "Diversity and community composition of the benthos and characteristics of surficial geology at selected sampling stations located in the identified distinctive seabed features of the AOI, plus abundance or biomass and size composition of the defining benthic taxa of those features",
  type = "model",
  units = "percent area",
  scoring = "representation: protection coverage",
  PPTID = c(1633, 2576),
  source = "OneDrive",
  project_short_title = c('Development and application of high throughput community monitoring','Predicting and assessing interannual change in kelp forest habitat'),
  areas = mpas,
  climate_expectation = "FIXME",
  indicator_rationale = "Kelp forests support high biodiversity and productivity, and provide ecosystem services",
  bin_rationale = "FIXME",
  plot_type = "map",
  year = 'year_of_data_collection',
  objectives = c(
    "Protect unique, rare, or sensitive ecological features",
    "Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes",
    "Habitat required for all species, particularly priority species, is maintained and protected"
  ),
  theme = "Benthic Environment",
  externalData = NULL,
  scale = "region-site",
  SME = "Kira Krumhansl",
  control_polygon = NA,
  SME_validated = TRUE,
  plot_lm = FALSE
)


## Some things to change:
## All desired states need to be trends
## Any representation is now represention: "representation: regional relative ranking"
## For representation, it's important to only run it on areas that we expect to see data.
## community composition is now community retention



## NEXT
data <- data_edna_data
x <- process_indicator(
           data = data[which(data$class == "Teleostei"), ],
           readiness = "Ready",
           indicator_var_name = "detections",
           indicator = "Community Composition of the benthos",
           type = "in situ",
           units = "read number",
           scoring = "community retention",
           PPTID = 480,
           source = "eDNA",
           project_short_title = "Animal Acoustic Tagging",
           bin_rationale = "FIXME",
           climate = FALSE,
           SME = "Ryan Stanley and Nick Jeffery",
           indicator_rationale = "Direct biodiversity measure",
           areas = MPAs,
           plot_type = c("map", "community_composition"),
           plot_lm = FALSE,
           theme = "Benthic Environment",
           objectives = c(
             "Protect Vazella pourtalesi glass sponges",
             "Protect continental shelf habitats and associated benthic and demersal communities",
             "Conserve and protect marine areas of high biodiversity at the community, species, population and genetic levels within the MPA"
           ),
           SME_validated = TRUE,
           other_nest_variables = c("species", "year_of_data_collection", 'method')
         )

