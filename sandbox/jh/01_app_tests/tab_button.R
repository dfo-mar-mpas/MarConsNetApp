library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("Shiny App with Side Panel Buttons"),
  sidebarLayout(
    sidebarPanel(
      # First fluidRow with two action buttons
      fluidRow(
        column(6, actionButton("button1", "Button 1")),
        column(6, actionButton("button2", "Button 2"))
      ),
      # Second fluidRow with two action buttons
      fluidRow(
        column(6, actionButton("button3", "Button 3")),
        column(6, actionButton("button4", "Button 4"))
      )
    ),
    mainPanel(
      # Any other content for the main panel
      h3("Main Panel Content")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # You can define server logic here if needed
}

# Run the application
shinyApp(ui = ui, server = server)
