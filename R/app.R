library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(shinyjs)


# Getting the data
# 1. MPAs
MPAs <- data_CPCAD_areas(data_bioregion(),  zones = FALSE)
subarea_coords <- getLatLon(MPAs)

# 2. Project Titles
load(file.path(system.file(package="MarConsNetData"),"data", "dataTable.rda"))



# Theme
my_theme <- bslib::bs_theme(
  bg = "#ecf0f1",
  # Light grey text
  fg = "#2c3e50",
  # Greyish blue background
  primary = "#2980b9",
  # Blue primary color for buttons
  primary_hover = "#3498db" # Lighter blue on hover for buttons
)

# Define UI
ui <- fluidPage(
  titlePanel("Maritimes Connectivity Network App"),
  theme = my_theme,
  sidebarLayout(
    sidebarPanel(
      selectInput("mpas","Select Marine Protected Area:",choices = c("All", MPAs$NAME_E)),
      selectInput("projects", "Select project(s):", choices=paste0(dataTable$title, " (", dataTable$id,")"), multiple=TRUE)
      ),
    mainPanel(leafletOutput("map"))
  )
)


# Define server logic
server <- function(input, output) {
  # Render the map with selected coordinates
  output$map <- renderLeaflet({
    coords <- subarea_coords[[input$mpas]]
    map <- leaflet() %>%
      addTiles()

    if (!(is.null(input$mpas)) && !(input$mpas == "All")) {
      map <- map %>% addPolygons(
        lng = coords$lng,
        lat = coords$lat,
        fillColor = coords$color,
        fillOpacity = 0.5,
        weight = 2
      )
    } else if (input$mpas == "All") {
      for (c in seq_along(subarea_coords)) {
        coord <- subarea_coords[[c]]
        map <- map %>%
          addPolygons(lat = coord$lat, lng = coord$lng, fillColor = coord$color, fillOpacity = 0.5, weight = 2)
      }
    }

    map
  })


}

# Run the application
shinyApp(ui = ui, server = server)
