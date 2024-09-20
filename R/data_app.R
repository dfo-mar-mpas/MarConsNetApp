library(sf)
library(targets)
library(viridis)
library(dataSPA)
library(arcpullr)
library(dplyr)
library(argoFloats)

# 1. MPAs
MPAs <- data_CPCAD_areas(data_bioregion("Scotian Shelf"),  zones = FALSE)
subarea_coords <- getLatLon(MPAs)

#tar_load(c("MPAs", "subarea_coords"))

# 2. Project Titles
load(file.path(system.file(package="MarConsNetData"),"data", "dataTable.rda"))

# 3. dataSPA om data
# FIXME, A COOKIE WILL NEED TO BE GIVEN SOMEWHERE
if (exists("om")) {
  om <- om
} else {
  om <- getData(type="om", age=3000, cookie="cookie")
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

odf$tab <- c("tab_0", paste0("tab_", 1:(length(odf$objectives)-1)))
odf$link <- c("link_0", paste0("link_", 1:(length(odf$objectives)-1)))



# 5. Project Data
# COMMENT
# FIXME TEMPORARY:
dataTable <- dataTable[-which(dataTable$id == 579),]

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

# 10. Getting indicator bins
binned_indicators <- read_excel(file.path(system.file(package="MarConsNetAnalysis"),"data", "indicator_binning.xlsx"))

## Giving indicator links
binned_indicators$tab<- paste0("tab_", length(odf$objectives)+(1:length(binned_indicators$indicators)))
binned_indicators$link <- paste0("link_", length(odf$objectives)+(1:length(binned_indicators$indicators)))


# 11. Flower Plot
tar_load(store = file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","pipeline_targets"),"pillar_ecol_df")


# 12. Calculating indicators
get_data('rv', data.dir="C:/Users/HarbinJ/Documents/data/rv")
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

# Linking indicators to plots
indicator_to_plot <- data.frame(indicator=c(unique(binned_indicators$indicators), odf$objective[which(grepl("indicator", odf$flower_plot, ignore.case=TRUE))]))
indicator_to_plot$plot <- 0
# FIXME: Likely do this elsewhere (manually) this is a placeholder and will need to be changed
indicator_to_plot$plot[length(indicator_to_plot$indicator)] <- "plot_rv_abundance(RV_ABUNDANCE[[4]][[2]])"


