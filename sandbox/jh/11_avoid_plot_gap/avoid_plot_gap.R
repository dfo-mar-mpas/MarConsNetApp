library(shiny)

ui <- fluidPage(
  titlePanel("Conditional Plot Display with renderUI"),

  sidebarLayout(
    sidebarPanel(
      selectInput("plot_choice", "Choose a plot to display:",
                  choices = c("None", "Plot 1", "Plot 2"))
    ),

    mainPanel(
      uiOutput("plot_ui")  # Dynamically rendered UI for the plots
    )
  )
)

server <- function(input, output) {
  # Conditionally render UI for plots
  output$plot_ui <- renderUI({
    if (input$plot_choice == "Plot 1") {
      plotOutput("plot1")
    } else if (input$plot_choice == "Plot 2") {
      plotOutput("plot2")
    } else {
      NULL  # Return NULL to not render anything, avoiding the gap
    }
  })

  # Define Plot 1
  output$plot1 <- renderPlot({
    plot(cars, main = "Plot 1: Speed vs Stopping Distance")
  })

  # Define Plot 2
  output$plot2 <- renderPlot({
    plot(pressure, main = "Plot 2: Temperature vs Pressure")
  })
}

shinyApp(ui, server)
