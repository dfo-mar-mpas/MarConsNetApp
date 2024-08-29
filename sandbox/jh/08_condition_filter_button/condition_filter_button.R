library(shiny)

ui <- fluidPage(
  # Tabs for navigation
  tabsetPanel(
    id = "tabs",
    tabPanel("Tab 1",
             selectInput("mpa", "Select MPA", choices = c("None", "MPA1", "MPA2")),
             selectInput("project", "Select Project", choices = c("None", "Project1", "Project2")),
             uiOutput("filter_button_ui")
    ),
    tabPanel("Tab 2",
             # Placeholder for another tab's content
             h3("Content of Tab 2")
    )
  )
)

server <- function(input, output, session) {
  # Reactive values to maintain the state of the button
  rv <- reactiveValues(button_label = "See all data")

  # Reactive expression to check if both MPA and Project are selected
  is_button_visible <- reactive({
    input$mpa != "None" && input$project != "None"
  })

  # Update the button label when clicked
  observeEvent(input$filter_button, {
    rv$button_label <- ifelse(rv$button_label == "See all data", "Filter project data", "See all data")
  })

  # Render the action button UI
  output$filter_button_ui <- renderUI({
    if (is_button_visible()) {
      actionButton("filter_button", rv$button_label)
    }
  })

  # Ensure the button is correctly displayed when navigating tabs
  observe({
    output$filter_button_ui <- renderUI({
      if (is_button_visible()) {
        actionButton("filter_button", rv$button_label)
      }
    })
  })
}

shinyApp(ui, server)
