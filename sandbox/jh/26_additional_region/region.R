library(MarConsNetData)
library(leaflet)
library(magrittr)
bio <- data_bioregion(bioregion="Gulf of Saint Lawrence")
mp <- data_CPCAD_areas(bioregion=bio)

map <- leaflet() %>%
  addTiles()

for (i in seq_along(mp$NAME_E)) {
  map <- addPolygons(map=map, data=mp$geoms[i], color="gray")
}
map

regionNames <- c("Scotian Shelf", "Gulf Of Saint Lawrence")
Scotian_Shelf <- MPAs
Gulf_Of_Saint_Lawrence <- mp


ui <- fluidPage(
  # Multi-select region dropdown
  selectInput("region", "Select a region:",
              choices = regionNames,
              multiple = TRUE,
              selected = "Scotian Shelf"),
  selectInput("mpas", "Select an MPA:", choices = NULL)
)

server <- function(input, output, session) {

  # Reactive expression to update MPA choices based on selected regions
  mpas_choices <- reactive({
    selected_regions <- input$region
    choices_list <- list()

    for (region in selected_regions) {
      var_name <- gsub(" ", "_", region)  # Convert region name to match variable name
      region_data <- get(var_name)$NAME_E  # Retrieve the NAME_E column dynamically

      # Add "All" for Scotian Shelf, and add the region name for others
      if (region == "Scotian Shelf") {
        choices_list[region] <- list(c("All", region_data))
      } else {
        choices_list[region] <- list(c(region, region_data))
      }
    }

    return(choices_list)
  })

  # Update the mpas dropdown dynamically
  observe({
    updateSelectInput(session, "mpas", choices = mpas_choices())
  })
}

shinyApp(ui, server)
