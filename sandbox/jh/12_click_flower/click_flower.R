require(ggplot2)
require(shiny)
require(dplyr)
require(MarConsNetAnalysis)

# Shiny example with two plots: one responding to clicks, the other not

ui <- fluidPage(
  plotOutput('flowerPlot', click = "flower_click"),  # Clickable plot
  plotOutput('fakePlot'),                            # Static plot with no click event
  textOutput('cut')
)

server <- function(input, output, session) {
  # First plot with click interaction
  output$flowerPlot <- renderPlot({
    plot_flowerplot(Ecological, title="test")
  })

  # Static plot without click interaction
  output$fakePlot <- renderPlot({
    ggplot(mtcars, aes(wt, mpg)) +
      geom_point() +
      ggtitle("Static Plot - No Click Interaction")
  })

  # Respond to clicks only for the flower plot
  output$cut <- renderText({
    req(input$flower_click)  # Only listens to clicks on the flowerPlot

    xscale <- 0.5
    yscale <- 205/2

    x <- (input$flower_click$x - xscale) / xscale
    y <- (input$flower_click$y + 50 - yscale) / yscale

    clickangle <- 90 - atan2(y, x) * 180 / pi
    if (clickangle < 0) clickangle <- 360 + clickangle

    if (sqrt(x^2 + y^2) > 0.75) {
      paste(Ecological$grouping[which.min(abs(Ecological$angle - clickangle))])
    } else {
      paste(Ecological$labels[which.min(abs(Ecological$angle - clickangle))])
    }
  })
}

shinyApp(ui, server)
