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
          "tarchetypes",
          "magick")
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



  ## GETTING CODE FOR ALL INDICATORS
  ## Find code for relevant targets, without using manifest
  target_files <- list.files("inst/", full.names = TRUE)
  target_files <- target_files[-which(grepl("set_up", target_files))]

  target_names <- list()

  for (i in seq_along(target_files)) { # Looping through files
    lines <- readLines(target_files[i])

    tar_idx <- grep("tar_target", lines, fixed=TRUE)

    # Optional: add one past the last line for splitting convenience
    tar_idx_end <- c(tar_idx[-1] - 1, length(lines))

    # Split into a list of character vectors
    chunks <- Map(function(start, end) lines[start:end], tar_idx, tar_idx_end)
    first_lines <- sapply(chunks, `[`, 1)

    if (any(which(trimws(first_lines) == "tar_target("))) {
      # Some target names are on the second line
      second <- which(trimws(first_lines) == "tar_target(")
      second_lines <- sapply(chunks, `[`, 2)[which(trimws(first_lines) == "tar_target(")]
      first_lines[second] <- paste0(first_lines[second], second_lines)
    }
    first_lines <- gsub(" ", "", first_lines)
    first_lines <- gsub("name=", "", first_lines)
    first_lines <- gsub("name=", "", first_lines)
    first_lines <-  gsub('["\']', '', first_lines)


    if (any(!(grepl(",", first_lines)))) {
      stop("Check 1: There is not a comma for ",  i)
    }

    target_names[[i]] <- data.frame(indicator=sub(".*tar_target\\(([^,]+),.*", "\\1", first_lines))
    target_names[[i]]$code <- NA

    # Finding code for each indicator

    for (j in seq_along(target_names[[i]]$indicator)) {
      full_text <- paste(chunks[[j]], collapse = "\n")
      code_inside <- sub("(?s).*?\\{(.*)\\}.*", "\\1", full_text, perl = TRUE)
      target_names[[i]]$code[j] <- code_inside
    }
  }

  targets_and_code <- do.call(rbind, target_names)

  # END GETTING CODE FOR ALL INDICATORS


  for (i in seq_along(unique_table$target_names)) {
    message(paste0(i, " of ", length(unique_table$indicator)))

    indicator_clicked <- trimws(unique_table$target_names[i], "both")
    workflow <- tar_glimpse(names =  !!sym(indicator_clicked), script = "inst/_targets.R")
    relevant_targets <- workflow$x$nodes$name

    # This puts them in order
    manifest <- tar_manifest(script = "inst/_targets.R") |>
         filter(name %in% relevant_targets)

    ordered_rows <- match(manifest$name, targets_and_code$indicator)
    targets_and_code_ordered <- targets_and_code[ordered_rows, ]

    unique_table$code[i] <- paste0(targets_and_code_ordered$code, collapse="\n\n")
    unique_table$plot[[i]] <- workflow
  }
  save(unique_table, file = paste0(dirname(path_to_store()), '/data/unique_table_cost.rda'))
}
