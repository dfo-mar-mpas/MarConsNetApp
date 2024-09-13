library(shiny)
library(htmltools)

ui <- fluidPage(
  uiOutput("dynamicText")
)

server <- function(input, output, session) {
  # Example data
  binned_indicators <- data.frame(
    indicators = c("Indicator 1", "Indicator 2"),
    link = c("link1", "link2"),
    stringsAsFactors = FALSE
  )

  keepind <- 1:nrow(binned_indicators)

  # Generate ind_links with lapply and combine using tagList
  ind_links <- tagList(lapply(seq_along(binned_indicators$indicators[keepind]), function(i) {
    tags$a(
      href = paste0("#", binned_indicators$link[keepind][i]),
      binned_indicators$indicators[keepind][i],
      onclick = sprintf(
        "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
        binned_indicators$link[keepind][i],
        binned_indicators$indicators[keepind][i]
      )
    )
  }))

  CO_label <- "CO Label"
  objective <- "Objective Description"
  area <- "Area Description"
  indicator_label <- "Indicator Label"
  binned_ind <- c("Binned Indicator 1", "Binned Indicator 2")
  formatted_projects <- c("Project 1", "Project 2")

  output$dynamicText <- renderUI({
    HTML(
      paste(
        "<p><strong>", CO_label, "</strong></p>",
        "<p>", objective, "</p>",
        "<p><strong>Area:</strong></p>",
        "<p>", area, "</p>",
        "<p><strong>", indicator_label, "</strong></p>",
        "<p>", ind_links, "</p>",
        "<p><strong>Indicator Bin:</strong></p>",
        "<p>", paste0(binned_ind, collapse = "<br>"), "</p>",
        "<p><strong>Projects:</strong></p>",
        "<p>", paste0(formatted_projects, collapse = "<br>"), "</p>"
      )
    )
  })
}

shinyApp(ui, server)
