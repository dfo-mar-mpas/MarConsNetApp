library(shiny)

# Example top-level tabs (dynamic)
main_tabs <- c("tab_0", "tab_1", "tab_2")

ui <- fluidPage(

  # Modern Dashboard CSS
  tags$head(
    tags$style(HTML("
      body { font-family: 'Inter', sans-serif; background-color: #f0f2f5; margin: 0; padding: 0; }
      .top-navbar { background: linear-gradient(90deg, #1e3a8a 0%, #3b82f6 100%); color: white; padding: 15px 30px; display: flex; justify-content: space-between; align-items: center; position: sticky; top: 0; z-index: 1000; }
      .app-title { font-size: 24px; font-weight: 700; margin: 0; }
      .sidebar-panel { background: #fff; border-right: 1px solid #e5e7eb; padding: 20px; height: calc(100vh - 70px); overflow-y: auto; position: fixed; top: 70px; left: 0; width: 25%; z-index: 900; }
      .main-content { padding: 25px; margin-left: 25%; background: #f0f2f5; }
      .integrated-tabs-card { background: white; border-radius: 12px; box-shadow: 0 1px 3px rgba(0,0,0,0.08); padding: 15px; margin-bottom: 20px; }
    "))
  ),

  # Top Navigation Bar
  div(class = "top-navbar",
      div(h1(class = "app-title", "Mini Dashboard")),
      div("Top Nav Buttons Here")
  ),

  # Sidebar
  div(class = "sidebar-panel",
      h4("Sidebar"),
      p("Filters or info can go here")
  ),

  # Main content
  div(class = "main-content",
      # Dynamic tabs
      div(class = "integrated-tabs-card",
          uiOutput("mytabs")
      ),

      # Indicator info
      uiOutput("indicatorText"),

      # Debug
      verbatimTextOutput("debug")
  )
)

server <- function(input, output, session) {

  # Dynamically generate the tabset
  output$mytabs <- renderUI({
    tabs <- lapply(main_tabs, function(tabname) {
      if (tabname == "tab_0") {
        tabPanel(
          "tab_0",
          tabsetPanel(
            id = "tab0_subtabs",
            tabPanel("Subtab A", p("Content A")),
            tabPanel("Subtab B", p("Content B"))
          )
        )
      } else {
        tabPanel(tabname, p(paste("Content of", tabname)))
      }
    })

    do.call(tabsetPanel, c(tabs, id = "tabs"))
  })

  # Reactive to inspect top-level tab and subtabs
  calculated_info <- reactive({
    req(input$tabs)
    # Only trigger browser if not on the default tab
    if (input$tabs != "tab_0") browser()

    list(
      top = input$tabs,
      sub = input$tab0_subtabs,
      CO_label = paste("CO for", input$tabs),
      objective = paste("Objective for", input$tabs),
      area = paste("Area for", input$tabs),
      indicator_label = paste("Indicator for", input$tabs),
      flower = paste("Flower plot for", input$tabs),
      formatted_projects = paste("Project", 1:3, "for", input$tabs)
    )
  })

  # Force reactive to run
  observe({
    info <- calculated_info()
    cat("Top:", info$top, " Sub:", info$sub, "\n")
  })

  # Shortened indicatorText UI
  output$indicatorText <- renderUI({
    info <- calculated_info()
    req(info)
    HTML(
      paste(
        "<p><strong>", info$CO_label, "</strong></p>",
        "<p>", info$objective, "</p>",
        "<p><strong>Area:</strong> ", info$area, "</p>",
        "<p><strong>", info$indicator_label, "</strong></p>",
        "<p>", info$flower, "</p>",
        "<p>", paste0(info$formatted_projects, collapse = "<br>"), "</p>"
      )
    )
  })

  # Debug
  output$debug <- renderPrint({
    list(
      top = input$tabs,
      sub = input$tab0_subtabs
    )
  })
}

shinyApp(ui, server)
