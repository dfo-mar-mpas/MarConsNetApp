library(sf)
#library(targets)
library(viridis)
library(dataSPA)
library(arcpullr)
library(dplyr)


# 1. MPAs
MPAs <- data_CPCAD_areas(data_bioregion("Scotian Shelf"),  zones = FALSE)
subarea_coords <- getLatLon(MPAs)

#tar_load(c("MPAs", "subarea_coords"))

# 2. Project Titles
load(file.path(system.file(package="MarConsNetData"),"data", "dataTable.rda"))

# 3. dataSPA om data
if (exists("om")) {
  om <- om
} else {
  om <- getData(type="om", age=3000, cookie="cookie")
}

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

odf <- data.frame(
  objectives = c(0, do.call(c,unname(Objectives)))
)

odf$tab <- c("tab_0", paste0("tab_", 1:(length(odf$objectives)-1)))
odf$link <- c("link_0", paste0("link_", 1:(length(odf$objectives)-1)))

# Obtaining network objectives

Nobjectives <- data_objectives(type="network")
N_Objectives <- vector(mode="list", length(Nobjectives))
for (i in seq_along(Nobjectives)) {
  O <- Nobjectives[[i]]

  N_Objectives[[i]] <- newLine(O)


}
N_Objectives <- unlist(N_Objectives)



# 5. Project Data
# COMMENT
# projectData <- NULL
# for (i in seq_along(dataTable$id)) {
#   message("i = ", i)
#   func_name <- dataTable$get_function[i]
#   func <- get(func_name)  # Get the function object
#   arguments <- names(formals(func))
#   if ("taxize" %in% arguments) {
#     pd <- get_project_data(ids=dataTable$id[i], taxize=FALSE)
#   } else {
#     #pd <- get_project_data(ids=dataTable$id[i], type="all")
#   }
#
#   projectData[[i]] <- pd
# }
#
# names(projectData) <- dataTable$id

# 6. Theme
my_theme <- bslib::bs_theme(
  bg = "#ecf0f1",
  # Light grey text
  fg = "#2c3e50",
  # Greyish blue background
  primary = "#2980b9",
  # Blue primary color for buttons
  primary_hover = "#3498db" # Lighter blue on hover for buttons
)

# 7. Indicators
indicators <- data_indicators()


# 8. Context
Context <- lapply(areas, function(x) data_context(type="site", area=x))


# 9. Flower plot site level CO to indicator bin
# FIXME
fp <- read.csv("../../MarConsNetAnalysis/sandbox/JH/02_meta_names/metaframework.csv")

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


# odf$link <- 0
# odf$tab <- 0
# for (i in seq_along(odf$flower_plot)) {
#   if (!(odf$objectives[i] == "0")) {
#     L <- which(unique(odf$flower_plot) %in% odf$flower_plot[i])
#     odf$link[i] <- paste0("link_", L)
#     odf$tab[i] <- paste0("tab_", L)
#   } else {
#     odf$link[i] <- "link_0"
#     odf$tab[i] <- "tab_0"
#   }
# }

