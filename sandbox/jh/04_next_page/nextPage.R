library(shiny)

# Define UI for the application
ui <- fluidPage(
  # Application title
  # Main page content
  mainPanel(
    h3("Welcome to the Main Page"),
    tags$a(href = "#page2", "Go to Page 2")
  ),

  # Second page content
  conditionalPanel(
    condition = "window.location.hash == '#page2'",
    h3("Welcome to Page 2"),
    tags$a(href = "#", "Go back to Main Page")
  )
)

# Define server logic required
server <- function(input, output) {}

# Run the application
shinyApp(ui = ui, server = server)
