library(shiny)

# Define the UI
ui <- fluidPage(
  # Button to open the HTML file
  tags$a(
    href = "/htmlfiles/dataSPA.html",
    target = "_blank",
    class = "btn btn-primary",
    "Report"
  )
)

# Define the Server
server <- function(input, output, session) {
  # Map the file directory to a URL path
  shiny::addResourcePath("htmlfiles", "C:/Users/HarbinJ/Documents/GitHub/dataSPA/vignette")
}

# Run the app
shinyApp(ui = ui, server = server)
