# library(shiny)
# library(shinyjs)
#
# ui <- fluidPage(
#   useShinyjs(),  # Initialize shinyjs
#   tabsetPanel(id = "tabs",
#               tabPanel("Tab 1",
#                        textInput("text1", "Text Input 1", value = "Default text"),
#                        actionButton("save1", "Save")
#               ),
#               tabPanel("Tab 2",
#                        textInput("text2", "Text Input 2", value = "Default text"),
#                        actionButton("save2", "Save")
#               )
#   )
# )
#
# server <- function(input, output, session) {
#   # Create reactive values to store input states
#   state <- reactiveValues(
#     text1 = NULL,
#     text2 = NULL
#   )
#
#   observeEvent(input$save1, {
#     state$text1 <- input$text1
#     shinyjs::runjs("alert('State saved for Tab 1!')")
#   })
#
#   observeEvent(input$save2, {
#     state$text2 <- input$text2
#     shinyjs::runjs("alert('State saved for Tab 2!')")
#   })
#
#   observe({
#     if (!is.null(state$text1)) {
#       updateTextInput(session, "text1", value = state$text1)
#     }
#
#     if (!is.null(state$text2)) {
#       updateTextInput(session, "text2", value = state$text2)
#     }
#   })
# }
#
# shinyApp(ui, server)






library(shiny)

ui <- fluidPage(
  selectInput("mpa", "Select MPA", choices = c("All MPAs", "MPA 1", "MPA 2"), selected = "All MPAs"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  # Reactive value to store the current selection
  current_mpa <- reactiveVal("All MPAs")

  observeEvent(input$mpa, {
    current_mpa(input$mpa)
  })

  output$plot <- renderPlot({
    mpa <- current_mpa()
    # Your plot code here, based on the selected MPA
    plot(1:10, main = paste("Plot for", mpa))
  })

  observe({
    updateSelectInput(session, "mpa", selected = current_mpa())
  })
}

shinyApp(ui, server)



