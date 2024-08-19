

# Load packages required to define the pipeline:
library(targets)

tar_config_set(store = file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","app_targets"))


# Set target options:
tar_option_set(
  packages = c("arcpullr"), # Packages that your targets need for their tasks.
  format = "qs" # Optionally set the default storage format. qs is fast.

)

# Run the R scripts in the R/ folder with your custom functions:
#tar_source("https://raw.githubusercontent.com/dfo-mar-mpas/MarConsNetData/main/R/data_CPCAD_areas.R")
source("https://raw.githubusercontent.com/dfo-mar-mpas/MarConsNetData/main/R/data_CPCAD_areas.R")
source("https://raw.githubusercontent.com/dfo-mar-mpas/MarConsNetData/main/R/data_bioregion.R")
source("R/getLatLon.R")

# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  tar_target(
    name = MPAs,
    command = data_CPCAD_areas(data_bioregion("Scotian Shelf"),  zones = FALSE)
  ),
  tar_target(
    name = subarea_coords,
    command = getLatLon(MPAs)
  )
)
