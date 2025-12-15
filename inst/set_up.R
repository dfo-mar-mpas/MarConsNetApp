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

conservation_targets <- read.csv(file.path(dirname(path_to_store()),"data","target_values.csv")) |>
  rename(filter_type = filter)

# NOTE: This can't be a target, because it requires tar_glimpse, which does not work
# inside tar_make

# Checking if code was made within the last 6 months:
if(as.Date(file.info(paste0(dirname(path_to_store()), '/data/unique_table_cost.rda'))$mtime) <= Sys.Date() - 180) {

  tar_load(pillar_ecol_df)
  tar_load(MPAs)

  ## Below cannot be a target, because we need tar_glimpse
  unique_table <- pillar_ecol_df %>%
    distinct(indicator, source) %>%
    left_join(
      pillar_ecol_df %>%
        select(indicator, source, target_name) %>%
        group_by(indicator, source) %>%
        summarize(target_names = paste(unique(target_name), collapse = ", "), .groups = "drop"),
      by = c("indicator", "source")
    )

  unique_table <- unique_table[-which(unique_table$indicator %in% MPAs$NAME_E),]
  unique_table$code <- NA
  unique_table <- unique_table %>%
    mutate(plot = vector("list", n()))

  for (i in seq_along(unique_table$target_names)) {
    message(paste0(i, "of ", length(unique_table$indicator)))

    indicator_clicked <- trimws(unique_table$target_names[i], "both")
    workflow <- tar_glimpse(names =  !!sym(indicator_clicked), script = "inst/_targets.R")

    # get relevant targets
    relevant_targets <- workflow$x$nodes$name

    # get manifest
    manifest <- tar_manifest(script = "inst/_targets.R") |>
      filter(name %in% relevant_targets) |>
      # the rest of this pipe is just to make the code  in the table look nice
      rowwise() |>
      mutate(cleancommand = if_else(startsWith(command, "{"),
                                    paste(strsplit(command, "\n")[[1]][-c(1, length(strsplit(command, "\n")[[1]]))], collapse="\n"),
                                    command))
    # print command for target
    unique_table$code[i] <- paste0(manifest$cleancommand, collapse='\n\n')
    unique_table$plot[[i]] <- workflow

  }

  save(unique_table, file = paste0(dirname(path_to_store()), '/data/unique_table_cost.rda'))
}
