library(shiny)

# Your vector of character class
objectives <- c("Objective1", "Objective2")

# Define UI
ui <- fluidPage(
  # CSS to hide the tab headers
  tags$style(HTML("
    .nav-tabs { display: none; }
  ")),

  tabsetPanel(
    id = "tabs",
    tabPanel("Home",
             uiOutput("objectives_links")
    ),
    tabPanel("Objective1",
             h2("This is the Objective1 page"),
             p("Content for Objective1 goes here."),
             actionButton("toHome1", "Back to Home")
    ),
    tabPanel("Objective2",
             h2("This is the Objective2 page"),
             p("Content for Objective2 goes here."),
             actionButton("toHome2", "Back to Home")
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  output$objectives_links <- renderUI({
    # Create a list of links
    links <- lapply(objectives, function(obj) {
      actionLink(inputId = obj, label = obj)
    })
    # Create a navigation container
    tags$ul(lapply(links, tags$li))
  })

  observeEvent(input$Objective1, {
    updateTabsetPanel(session, "tabs", selected = "Objective1")
  })

  observeEvent(input$Objective2, {
    updateTabsetPanel(session, "tabs", selected = "Objective2")
  })

  observeEvent(input$toHome1, {
    updateTabsetPanel(session, "tabs", selected = "Home")
  })

  observeEvent(input$toHome2, {
    updateTabsetPanel(session, "tabs", selected = "Home")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
