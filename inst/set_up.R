if(!require(librarian)) install.packages("librarian")
pkgs <- c("sf",
          "targets",
          "viridis",
          "j-harbin/dataSPA",
          "arcpullr",
          "ArgoCanada/argoFloats",
          "casaultb/azmpdata",
          "raster",
          "dfo-mar-odis/TBSpayRates",
          "readxl",
          "ggplot2",
          "shinyBS",
          "Maritimes/Mar.datawrangling",
          "DT",
          "magrittr",
          "RColorBrewer",
          "dplyr",
          "tidyr",
          "stringr",
          "officer",
          "RColorBrewer",
          "car",
          "purrr",
          "dfo-mar-mpas/MarConsNetAnalysis",
          "dfo-mar-mpas/MarConsNetData",
          "dfo-mar-mpas/MarConsNetApp",
          "rnaturalearth",
          "DBI",
          "duckdb",
          "rmarkdown",
          "shiny",
          "measurements",
          "mregions2",
          "patchwork",
          "units",
          "dankelley/oceglider",
          "RCurl",
          "oce",
          "gsw",
          "leaflet",
          "rgbif",
          "qs",
          "qs2",
          "odbc",
          "rvest",
          "tarchetypes")
shelf(pkgs)

# Set target options here if they will be used in many targets, otherwise, you can set target specific packages in tar_targets below
if (dir.exists(Sys.getenv("OneDriveCommercial"))){
  deployment <- "worker"
} else {
  deployment <- "main"
}

tar_option_set(packages = basename(pkgs),
               format = "qs",
               deployment = deployment)

store <- path_to_store()

if (!nzchar(store)) stop("MARCONSNET_TARGETS_PATH is not set!")


tar_config_set(store = store)
