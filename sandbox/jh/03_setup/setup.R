# Install required packages
install.packages(c("shiny", "leaflet", "dplyr", "sf", "shinyjs", "devtools"))
# Load required packages
library(devtools)
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(shinyjs)
# Load info from packages that are not on GitHub (will explain)
load_all("../MarConsNetData/")
load_all("../dataSPA")
source("R/getLatLon.R")
source("R/newLine.R")
# Define a cookie. This tells R that you have the credentials to look at the project planning tool
cookie <- "csrftoken=hallestoken; sessionid=hallesid"
om <- dataSPA::getData(type="om", age=0, cookie=cookie, keep=TRUE, path="../Github") # Gets O&M information from PPT and saves it in your GitHub repository
