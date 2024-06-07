library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  uiOutput("pageContent")
)

server <- function(input, output, session) {
  current_page <- reactiveVal("home")

  observeEvent(input$go_to_page2, {
    current_page("page2")
  })

  observeEvent(input$go_to_home, {
    current_page("home")
  })

  output$pageContent <- renderUI({
    if (current_page() == "home") {
      fluidRow(
        column(12,
               h1("Welcome to the Home Page"),
               actionButton("go_to_page2", "Go to Page 2")
        )
      )
    } else if (current_page() == "page2") {
      fluidRow(
        column(12,
               h1("Welcome to Page 2"),
               actionButton("go_to_home", "Go to Home")
        )
      )
    }
  })
}

shinyApp(ui = ui, server = server)
