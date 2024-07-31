library(shiny)

ui <- fluidPage(
  actionLink(inputId = "tab_1", label = "This is the label I want"),
  tabsetPanel(id = "tabs",
              tabPanel("This is the label I want", value = "tab1", "Content of Tab 1"),
              tabPanel("This I dont want", value = "tab2", "Content of Tab 2")
  )
)

server <- function(input, output, session) {
  observeEvent(input$tab_1, {
    # Get the label of the action link
    clicked_label <- "This is the label I want"

    # Find the tab value that matches the label
    tab_value <- NULL
    tabset <- c("tab1" = "This is the label I want", "tab2" = "This I dont want")

    for (value in names(tabset)) {
      if (tabset[[value]] == clicked_label) {
        tab_value <- value
        break
      }
    }

    # Update the tabset panel to switch to the tab with the matching value
    if (!is.null(tab_value)) {
      updateTabsetPanel(session, "tabs", selected = tab_value)
    }
  })
}

shinyApp(ui = ui, server = server)
