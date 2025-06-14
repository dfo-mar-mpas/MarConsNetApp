library(targets); library(dplyr); library(argoFloats); library(azmpdata); library(raster); library(shiny); library(leaflet)
library(dplyr); library(sf); library(shinyjs); library(viridis); library(arcpullr); library(devtools)
library(MarConsNetAnalysis);library(MarConsNetData);
library(TBSpayRates); library(dataSPA); library(readxl)
library(ggplot2); library(shinyBS); library(Mar.datawrangling); library(DT); library(rmarkdown)
library(stringr); library(tidyr); library(officer); library(RColorBrewer); library(rvest); library(httr2); library(httr);
library(tidyverse); library(dplyr)


tar_load(c("APPTABS","pillar_ecol_df","all_project_geoms","MPA_report_card","MPAs","areas","regions","odf","flowerPalette","indicatorFlower","Objectives_processed","N_Objectives","om","Ecological", "Context", "collaborations", "deliverables", "csas"))
source("R/app.R");app()


# 1. MPAs
MPAs <- data_CPCAD_areas(data_bioregion("Scotian Shelf"),  zones = FALSE)
subarea_coords <- getLatLon(MPAs)


EBM <- data.frame(grouping=rep(c("Ecological",
                                 "Economic",
                                 "Governance",
                                 "Social & Cultural"),
                               times=c(3,3,3,4)),
                  labels=c("Habitat",
                           "Biodiversity",
                           "Productivity",
                           "Economic Effiency",
                           "Economic Equity",
                           "Economic Sustainability",
                           "Governance Outcomes",
                           "Governance Structure & Processes",
                           "Legal Obligations & Other Commitments",
                           "Culture",
                           "Ethical & Just Activities",
                           "Health & Well-being",
                           "Sustainable Communities"),
                  score=runif(13,55,100)) |>
  group_by(grouping) |>
  mutate(weight=1/n()) |>
  ungroup()

Ecological <- data.frame(grouping=rep(c("Biodiversity",
                                        "Habitat",
                                        "Productivity"),
                                      times=c(3,5,3)),
                         labels=c("Genetic Diversity",
                                  "Species Diversity",
                                  "Functional Diversity",

                                  "Environmental Representativity",
                                  "Key Fish Habitat",
                                  "Connectivity",
                                  "Uniqueness",
                                  "Threats to Habitat",

                                  "Biomass Metrics",
                                  "Structure and Function",
                                  "Threats to Productivity"),
                         score=runif(11,55,100)) |>
  # group_by(grouping) |>
  # mutate(weight=1/n()) |>
  mutate(weight=runif(11,1,10)) |>
  ungroup()|>
  mutate(angle=(cumsum(weight)-weight/2)/sum(weight)*360)

grade <- function(percent){
  cutoffs=c(0, seq(60, 100, by = 10/3))
  letters=c("F", paste0(toupper(rep(letters[4:1], each = 3)), rep(c("-","","+"),4)))
  cut(percent,cutoffs,letters)
}


# flowerTabs
# Create then filter out (odf, N_Objectives, binned_indicators)

ftabs <- data.frame(flower=unique(c(Ecological$grouping, Ecological$labels)))
ftabs$place <- tolower("Scotian_Shelf")

MYTABS <- NULL

for (i in seq_along(MPAs$NAME_E)) {
  df <- ftabs
  df$place <- MPAs$NAME_E[i]
  MYTABS[[i]] <- df
}

MYTABS <- do.call(rbind, MYTABS)
APPTABS <- rbind(ftabs, MYTABS)
APPTABS$tab <- paste0("tab_", seq_along(1:length(APPTABS$flower)))
APPTABS$link <- paste0("link_", seq_along(1:length(APPTABS$flower)))
home <- data.frame(flower="home", place="home", tab="tab_0", link="link_0")
APPTABS <- rbind(home, APPTABS)
APPTABS <- NAME_to_tag(APPTABS)


# 2. Project Titles
load(file.path(system.file(package="MarConsNetData"),"data", "dataTable.rda"))

# 3. dataSPA om data
# FIXME, A COOKIE WILL NEED TO BE GIVEN SOMEWHERE
if (exists("om")) {
  om <- om
} else {
  om <- dataSPA::getData(type="om", age=3000, cookie="cookie")
}

om <- om[-(which(om$activity_type == "Other")),]

# 4. Objectives
areas <- c("st_Anns_Bank_MPA", "musquash_MPA", "gully_MPA", "WEBCA") # Only including Maritimes
#areas <- c("stAnnsBank", "musquash", "laurentianChannel", "gully", "gilbert", "eastport",
#           "basinHead", "bancsDesAmericains")

objectives <- lapply(areas, function(x) data_objectives(type="site", area=x))
Objectives <- vector(mode="list", length(objectives))
for (i in seq_along(objectives)) {
  O <- objectives[[i]]
  for (j in seq_along(O)) {
    Objectives[[i]][[j]] <- newLine(O[j])
  }
}
Objectives <- lapply(Objectives, unlist)
names(Objectives) <- areas


# Obtaining network objectives
Nobjectives <- data_objectives(type="network")
N_Objectives <- vector(mode="list", length(Nobjectives))
for (i in seq_along(Nobjectives)) {
  O <- Nobjectives[[i]]

  N_Objectives[[i]] <- newLine(O)


}
N_Objectives <- unlist(N_Objectives)

odf <- data.frame(
  objectives = c(0, unlist(Objectives, use.names = FALSE), N_Objectives)
)


#odf$tab <- c("tab_0", paste0("tab_", 1:(length(odf$objectives)-1)))
#odf$link <- c("link_0", paste0("link_", 1:(length(odf$objectives)-1)))

# 5. Project Data
# COMMENT
# FIXME TEMPORARY:
dataTable <- dataTable[-which(dataTable$id == 579),] #JAIM

projectData <- NULL
for (i in seq_along(dataTable$id)) {
  message("i = ", i)
  func_name <- dataTable$get_function[i]
  func <- get(func_name)  # Get the function object
  arguments <- names(formals(func))
  if (dataTable$package[i] == "MarConsNetData" && "taxize" %in% arguments) {
    pd <- get_project_data(ids=dataTable$id[i], taxize=FALSE)
  } else {
    default_args <- formals(func)
    default_args[is.null(default_args)] <- NA

    # If you want to handle this gracefully:
    filled_args <- lapply(default_args, function(arg) {
      if (is.null(arg)) return(NA)
      else return(arg)
    })

    # Use do.call to call the function with filled arguments
    pd <- do.call(func, filled_args)
  }

  projectData[[i]] <- pd
}

names(projectData) <- dataTable$id

# 7. Indicators
indicators <- data_indicators()


# 8. Context
Context <- lapply(areas, function(x) data_context(type="site", area=x))


# 9. Flower plot site level CO to indicator bin
# FIXME
fp <- read.csv("../../MarConsNetAnalysis/sandbox/JH/02_meta_names/metaframework.csv")

# Add network objectives to odf


# Link flower plot to odf
odf$flower_plot <- 0
odf$area <- 0
get_first_four_words <- function(texts) {
  lapply(texts, function(text) {
    words <- strsplit(text, " ")[[1]] # Split each string into words
    first_four_words <- paste(words[1:min(4, length(words))], collapse = " ") # Concatenate the first four words (or fewer if there are not enough words)
    return(first_four_words)
  })
}

for (i in seq_along(odf$objectives)) {
  ob <- gsub("[-\n]", "", odf$objectives[i])
  if (!(odf$objectives[i] == "0")) {
    keep <- which(tolower(get_first_four_words(fp$label_Objective)) == tolower(get_first_four_words(ob)[[1]]))
    if (!(length(keep) == 0)) {
    odf$flower_plot[i] <- fp$Flowerplot_connection[keep]
    odf$area[i] <- fp$label_Framework[keep]
    } else {
      message("i is also wrong ", i)
    }
  } else {
    odf$flower_plot[i] <- "flower_0"
    odf$area[i] <= "area_0"
  }
}

grades <- c("F", paste0(toupper(rep(letters[4:1], each = 3)), rep(c("-","","+"),4)))
flowerPalette <- colorRampPalette(brewer.pal(11,"RdBu"))(length(grades))
names(flowerPalette) <- grades



# 10. Getting indicator bins
binned_indicators <- read_excel("../../MarConsNetAnalysis/data/indicator_binning.xlsx")
#binned_indicators <- read_excel(file.path(system.file(package="MarConsNetAnalysis"),"data", "indicator_binning.xlsx"))

## Giving indicator links
binned_indicators$tab<- paste0("tab_", length(APPTABS$flower)+(1:length(binned_indicators$indicators)))
binned_indicators$link <- paste0("link_", length(APPTABS$flower)+(1:length(binned_indicators$indicators)))



# TABS AND LINKS
odf$tab <- 0
odf$link <- 0
for (i in seq_along(odf$objectives)) {
  message("i = ", i)
  if (!(i == 1)) {
  if (!(grepl("Indicator", odf$flower_plot[i]))) {
  k1 <- which(gsub("\\.","", APPTABS$place) == tolower(sub("_CO$", "", odf$area[i]))) # SAME AREA AND FLOWER
  k2 <- which(APPTABS$flower == odf$flower_plot[i])
  keep <- intersect(k1,k2)
  odf$tab[i] <- APPTABS$tab[keep]
  odf$link[i] <- APPTABS$link[keep]
  } else {
    k <- which(binned_indicators$indicators == trimws(gsub("-", "", gsub("\n", "", odf$objectives[i]))), "right")
    odf$tab[i] <- binned_indicators$tab[k]
    odf$link[i] <- binned_indicators$link[k]

  }
  } else {
    odf$tab[i] <- "tab_0"
    odf$link[i] <- "link_0"
  }

}


# 11. Flower Plot
tar_load(store = file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","pipeline_targets"),"pillar_ecol_df")
pillar_ecol_df <- pillar_ecol_df %>%
  mutate(angle=(cumsum(weight)-weight/2)/sum(weight)*360)


calc_letter_grade <- function(percent, min_score=0, max_score=100){
  scalerange <- max_score-min_score
  cutoffs=c(min_score, seq(max_score-scalerange*.4, max_score, by = 10/3/100*scalerange))
  grades=c("F", paste0(toupper(rep(letters[4:1], each = 3)), rep(c("-","","+"),4)))
  cut(percent,cutoffs,grades)
}


# 12. Calculating indicators
get_data('rv', data.dir="C:/Users/HarbinJ/Documents/data/rv") #JAIM
species <- c("COD(ATLANTIC)", "HADDOCK")

RV_ABUNDANCE <- vector(mode="list", length(areas))

for (i in seq_along(areas)) {
  for (j in seq_along(species)) { # This has up to
    message("i = ", i, " and j = ", j)
    RV_ABUNDANCE[[i]][[j]] <- ind_rv_abundance(species = species[j], area=areas[i], MPAs=MPAs, data=GSSPECIES)
  }
}


names(RV_ABUNDANCE) <- areas
for (i in seq_along(RV_ABUNDANCE)) {
  names(RV_ABUNDANCE[[i]]) <- species
}



# BOTTOM ASSIGNING PLOTTING (FIXME)
# Linking indicators to plots
plotindy <- c(binned_indicators$indicators)

indicator_to_plot <- data.frame(indicator=unique(plotindy), plot=rep(0), type="plot", status=rep(0), trend=rep(0))
indicator_to_plot$plot[which(indicator_to_plot$indicator == "Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock")] <- "plot_rv_abundance(RV_ABUNDANCE[[which(names(RV_ABUNDANCE) == 'WEBCA')]][[which(species == 'HADDOCK')]])"


planning_area <- data_planning_areas()
fishing <- data_commercial_fishing(planning_area)

indicator_to_plot$plot[which(indicator_to_plot$indicator == "Fishing effort")] <- "plot_fishing_abundance(area='WEBCA', CPCAD=MPAs)"
indicator_to_plot$type[which(indicator_to_plot$indicator == "Fishing effort")] <- "leaflet"

source("results_app.R")

