library(targets); library(dplyr); library(argoFloats); library(azmpdata); library(raster); library(shiny); library(leaflet)
library(dplyr); library(sf); library(shinyjs); library(viridis); library(arcpullr); library(devtools)
library(MarConsNetAnalysis);library(MarConsNetData);
library(TBSpayRates); library(dataSPA); library(readxl)
library(ggplot2); library(shinyBS); library(Mar.datawrangling); library(DT); library(rmarkdown)
library(stringr); library(tidyr); library(officer); library(RColorBrewer); library(rvest); library(httr2); library(httr);
library(tidyverse); library(dplyr); library(oceglider); library(RCurl); library(oce); library(marea); library(rgbif);library(qs2)


tar_load(c("APPTABS","pillar_ecol_df","all_project_geoms","MPA_report_card","MPAs","areas","regions","odf","flowerPalette","indicatorFlower","Objectives_processed","N_Objectives","om","Ecological", "Context", "collaborations", "deliverables", "csas", "climate", "cost_of_mpas", "salary"))
source("R/app.R");app()

tar_make(c("all_project_geoms", "MPA_report_card", "odf", "pillar_ecol_df", "plot_files"),
         script=tar_paths("inst"),
         store=tar_paths())
tar_load(c("all_project_geoms", "MPA_report_card", "odf", "pillar_ecol_df", "plot_files"))


# TEST
library(gsw)
glider_df <- data_gliders

# Want stratification to be decreasing.
# Decreasing MLD	Stronger stratification
# Increasing MLD	Weaker stratification

