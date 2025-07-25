# install.packages("../MarConsNetData/", repos = NULL, type="source")
# install.packages("../MarConsNetAnalysis/", repos = NULL, type="source")
# install.packages("../MarConsNetApp/", repos = NULL, type="source")
remotes::install_github("dfo-mar-mpas/MarConsNetAnalysis", upgrade = "never")
remotes::install_github("dfo-mar-mpas/MarConsNetData", upgrade = "never")
remotes::install_github("dfo-mar-mpas/MarConsNetApp", upgrade = "never")


require(MarConsNetApp)
require(MarConsNetAnalysis)
require(MarConsNetData)
require(targets)
require(shiny)
require(targets)
require(RColorBrewer)
require(dataSPA)
require(DT)
require(sf)
require(ggplot2)
require(dplyr)
require(tidyr)
require(purrr)


# tar_load(c("MPAs","regions","data_obis"))
# tar_load(c("MPAs","regions"))
#
# data_obis <- data_OBIS(MPAs[32,])
#
#
# library(worrms)
# library(dplyr)
# library(purrr)
#
# # Assuming data_obis contains your OBIS dataset with aphiaID column
# # Extract unique aphiaIDs
# # sampleobis <- round(runif(1000,1,nrow(data_obis)))
# unique_aphia_ids <- unique(data_obis$aphiaID)
# unique_aphia_ids <- unique_aphia_ids[!is.na(unique_aphia_ids)]
#
# test <- unique_aphia_ids |> wm_attr_data_()
#
# # Function to safely retrieve attributes for a single aphiaID
# get_attributes_safe <- function(id) {
#   tryCatch({
#     attrs <- wm_attr_data(id)
#     # Add a short pause to be nice to the API
#     Sys.sleep(0.2)
#     return(attrs)
#   }, error = function(e) {
#     message("Error processing ID ", id, ": ", e$message)
#     return(NULL)
#   })
# }
#
# # Process aphiaIDs - using map to iterate through each ID individually
# attributes_list <- map(unique_aphia_ids, get_attributes_safe)
#
# # Filter out NULL results and combine into one dataframe
# valid_attributes <- attributes_list[!sapply(attributes_list, is.null)]
# all_attributes <- bind_rows(valid_attributes)
#
#
#
# library(worms)
# worms_info <- worms_records(scientificname = unique(data_obis$scientificName))
#
# library(rfishbase)
# species_list <- c("Gadus morhua", "Clupea harengus")
# diet_data <- diet(species_list)
# ecology_data <- ecology(species_list)

# tar_invalidate(c("ind_species_representation","bin_biodiversity_SpeciesDiversity_df",,"ecol_obj_biodiversity_df","pillar_ecol_df"))

tar_load(c(
  MPAs,
  pillar_ecol_df,
  Context,
  flowerPalette,
  odf,
  N_Objectives,
  Objectives_processed,
  MPA_report_card,
  collaborations
))

tar_make(
  c(
    "plot_files",
    "MPA_report_card",
    "odf",
    "pillar_ecol_df",
    "all_project_geoms"
  ),
  script = "inst/_targets.R"
)

tar_make("data_musquash_MMMP_birds", script = "inst/_targets.R")

tar_visnetwork(script = "inst/_targets.R")
# tar_make("upload_all_data_to_shiny",script="inst/_targets.R")
# tar_make("indicator_to_plot",script = "inst/_targets.R")
# tar_invalidate("bin_habitat_EnvironmentalRepresentativity_df")
# tar_load(names=mani[grepl("ecol_obj",mani)])
# tar_load("MPAs")
# tar_prune_list(script="inst/_targets.R")
#
# tar_make(names = "pillar_ecol_df_new",script = "inst/_targets.R")
# tar_glimpse(names = "pillar_ecol_df_new",script = "inst/_targets.R")
# tar_make("APPTABS",script = "inst/_targets.R")
# mani <- tar_manifest(script = system.file("_targets.R", package = "MarConsNetApp"),
#                      fields = "name") |>
#   unlist(use.names = FALSE)
# tar_invalidate(mani[grepl("ind_",mani)])
# tar_load(names=mani[!grepl("rv_data",mani)])
# mani <- tar_manifest(script = "inst/_targets.R",
#                      fields = "name") |>
#   unlist(use.names = FALSE)
# tar_load(names=mani[!grepl("data",mani)&!grepl("obis",mani)&!grepl("rv_data",mani)&!grepl("ind_",mani)&!grepl("bin_",mani)&!grepl("ecol_obj",mani)])
# tar_invalidate(names=c("ds_all","rv_rawdata_env",mani[grepl("ind_",mani)]))
# tar_make(c("APPTABS"),script = "inst/_targets.R")
# tar_load(names=mani[grepl("ind_",mani)])
# tar_load(names=mani[grepl("ecol_obj",mani)])
# tar_load(names="pillar_ecol_df_data")
# tar_load_everything()
# pillar_ecol_df <- pillar_ecol_df_data |> select(-data)
# rm(indicator_to_plot)
# pillar_ecol_df <- pillar_ecol_df|> select(-data,-plot)

tar_load(c(
  "APPTABS",
  "pillar_ecol_df",
  "all_project_geoms",
  "MPA_report_card",
  "MPAs",
  "areas",
  "regions",
  "odf",
  "flowerPalette",
  "indicatorFlower",
  "Objectives_processed",
  "N_Objectives",
  "om",
  "Ecological"
))
source("R/app.R")
app()


# get workflow
workflow <- tar_glimpse(
  names = c("fish_weight_per_1.75kn_tow"),
  script = "inst/_targets.R"
)
# get relevant targets
relevant_targets <- workflow$x$nodes$name
# get manifest
manifest <- tar_manifest(script = "inst/_targets.R") |>
  filter(name %in% relevant_targets) |>
  # the rest of this pipe is just to make the code  in the table look nice
  rowwise() |>
  mutate(
    cleancommand = if_else(
      startsWith(command, "{"),
      paste(
        strsplit(command, "\n")[[1]][
          -c(1, length(strsplit(command, "\n")[[1]]))
        ],
        collapse = "\n"
      ),
      command
    )
  )
# print command for target
manifest$command[manifest$name == "fish_weight_per_1.75kn_tow"]
# or as a rmd code chunk
knitr::asis_output(paste(
  "```r\n",
  unlist(manifest$command[manifest$name == "fish_weight_per_1.75kn_tow"]),
  "\n```",
  sep = ""
))
