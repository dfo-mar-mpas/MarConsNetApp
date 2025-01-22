
# UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      uiOutput("legendUI") # Output for the legend
    )
  )
)

# Server
server <- function(input, output) {
  output$legendUI <- renderUI({
    # Generate legend items
    legendItems <- lapply(names(flowerPalette), function(name) {
      div(
        style = paste0(
          "display: flex; align-items: center; margin-right: 20px;"
        ),
        div(
          style = paste0(
            "width: 20px; height: 20px; background-color: ",
            flowerPalette[name],
            "; margin-right: 5px; border: 1px solid black;"
          )
        ),
        span(name) # Label
      )
    })
    # Wrap the items in a horizontal flex container
    div(
      style = "display: flex; flex-wrap: wrap; align-items: center;",
      legendItems
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
