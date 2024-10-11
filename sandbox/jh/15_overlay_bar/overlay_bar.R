library(shiny)
library(ggplot2)

# Sample data for demonstration
odf <- data.frame(
  objectives = c(
    "Protect unique, rare, or sensitive ecological features",
    "Protect representative examples of identified ecosystems and habitat types",
    "Help maintain ecosystem structure, functioning, and resilience (including resilience to climate change)",
    "Contribute to the recovery and conservation of depleted species",
    "Help maintain healthy populations of species of Aboriginal, commercial, and/or recreational importance"
  ),
  link = c("link1", "link2", "link3", "link4", "link5"),
  stringsAsFactors = FALSE
)

# UI
ui <- fluidPage(
  # Container for objectives and charts
  fluidRow(
    uiOutput("networkObjectiveText")  # Output for dynamic objectives
  )
)

# Server
server <- function(input, output, session) {

  # Generate random bar chart data for each objective
  output$networkObjectiveText <- renderUI({
    textN <- odf$objectives  # Get the objectives text

    # Create a list of divs for objectives and bar charts
    objectiveDivs <- lapply(seq_along(textN), function(i) {
      # Objective Container
      tags$div(
        style = "position: relative; height: 100px; width: 400px; margin-bottom: 20px;",

        # Bar chart
        tags$div(
          plotOutput(paste0("bar", i), height = "100px", width = "400px"),
          style = "position: absolute; top: 0; left: 0; z-index: 1; opacity: 0.7;"
        ),

        # Action link (Objective)
        tags$div(
          actionLink(inputId = odf$link[i], label = textN[i]),
          style = "position: absolute; top: 30px; left: 10px; z-index: 2; font-weight: bold; color: white;"
        )
      )
    })

    # Return the list of objective divs
    do.call(tagList, objectiveDivs)
  })

  # Render bar charts
  for (i in 1:nrow(odf)) {
    local({
      id <- i
      output[[paste0("bar", id)]] <- renderPlot({
        data <- data.frame(x = paste0("Objective ", id), y = runif(1, 0, 1))  # Random value between 0 and 1
        ggplot(data, aes(x = x, y = y)) +
          geom_bar(stat = "identity", fill = "lightcoral") +  # Light red color
          ylim(0, 1) +
          theme_void() +
          coord_flip()
      })
    })
  }
}

# Run the app
shinyApp(ui = ui, server = server)
