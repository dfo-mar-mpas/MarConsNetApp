library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(shinyjs)
library(viridis)
library(dataSPA)
library(arcpullr)

# 1. MPAs
MPAs <- data_CPCAD_areas(data_bioregion(),  zones = FALSE)
subarea_coords <- getLatLon(MPAs)

# 2. Project Titles
load(file.path(system.file(package="MarConsNetData"),"data", "dataTable.rda"))

# 3. dataSPA om data
if (exists("om")) {
  om <- om
} else {
  om <- getData(type="om", age=3000, cookie="cookie")
}

# 4. Objectives
areas <- c("stAnnsBank", "musquash", "laurentianChannel", "gully", "gilbert", "eastport",
           "basinHead", "bancsDesAmericains")

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
# projectData <- NULL
# for (i in seq_along(dataTable$id)) {
#   message("i = ", i)
#   pd <- get_project_data(ids=dataTable$id[i], taxize=FALSE)
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




