# objectives <- c("o1", "o2", "o3", "o4")
# indicators <- c("i1", "i2", "i3","i4")
#
# ui  <- fluidPage(
#     mainPanel(
#       uiOutput('mytabs')
#     ))
#
#   server <-  function(input, output, session){
#     output$mytabs = renderUI({
#       nTabs = length(objectives)
#       myTabs = lapply(paste('Tab', 1: nTabs), tabPanel)
#       do.call(tabsetPanel, myTabs)
#     })
#   }
#
# shinyApp(ui = ui, server = server)


library(shiny)

objectives <- c("o1", "o2", "o3", "o4")
indicators <- c("i1", "i2", "i3", "i4")

ui <- fluidPage(
  mainPanel(
    uiOutput('mytabs'),
    textOutput('selected_indicator')
  )
)

server <- function(input, output, session) {
  output$mytabs <- renderUI({
    nTabs <- length(objectives)
    myTabs <- lapply(seq_len(nTabs), function(i) {
      tabPanel(
        title = objectives[i],
        value = objectives[i],
      )
    })
    do.call(tabsetPanel, c(myTabs, id = "tabs"))
  })

  output$selected_indicator <- renderText({
    req(input$tabs)  # Ensure the input is available
    selected_tab <- input$tabs
    indicator_index <- match(selected_tab, objectives)
    indicators[indicator_index]
  })
}

shinyApp(ui = ui, server = server)



