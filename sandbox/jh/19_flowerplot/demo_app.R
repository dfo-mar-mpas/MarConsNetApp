ui <- fluidPage(
  titlePanel("Flower Plot Interaction"),
  sidebarLayout(
    sidebarPanel(
      h3("Click Info"),
      verbatimTextOutput("click_info")
    ),
    mainPanel(
      plotOutput("flower_plot", click = "plot_click")
    )
  )
)

server <- function(input, output, session) {
  # Example data
  df <- data.frame(
    grouping = c("HABITAT", "HABITAT", "HABITAT", "HABITAT", "HABITAT",
                 "PRODUCTIVITY", "PRODUCTIVITY", "BIODIVERSITY", "BIODIVERSITY", "BIODIVERSITY"),
    labels = c("ENVIRONMENTAL (REPRESENTATIVITY)", "UNIQUENESS", "CONNECTIVITY",
               "KEY FISH HABITAT", "THREATS TO HABITAT", "BIOMASS METRICS",
               "STRUCTURE AND FUNCTION", "SPECIES DIVERSITY", "FUNCTIONAL DIVERSITY", "GENETIC DIVERSITY"),
    score = c(0.587, 0, 0, 0.176, 0, 1.1, 0, 0, 0, 0),
    weight = c(0.264, 0.0345, 0.0402, 0.0977, 0.218, 0.115, 0.046, 0.109, 0.0632, 0.0115),
    pos = c(0.132, 0.282, 0.319, 0.388, 0.546, 0.713, 0.793, 0.871, 0.957, 0.994),
    bg = c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white")
  )

  # Render the flower plot
  output$flower_plot <- renderPlot({
    plot_flowerplot(df, title="hi")
  })

  # Detect clicks on the plot
  output$click_info <- renderPrint({
    req(input$plot_click)  # Ensure a click event exists
    click <- input$plot_click

    # Determine closest label and grouping based on click coordinates
    closest <- df[which.min((df$pos - click$x)^2 + (df$score - click$y)^2), ]
    list(
      Grouping = closest$grouping,
      Label = closest$labels,
      Score = closest$score
    )
  })
}

shinyApp(ui = ui, server = server)
