objectives <- c("o1", "o2", "o3", "o4")

ui  <- fluidPage(
  tags$style(HTML("
    .nav-tabs { display: none; }
  ")),
    mainPanel(
      uiOutput('mytabs')
    ))

  server <-  function(input, output, session){
    output$mytabs = renderUI({
      nTabs = length(objectives)
      myTabs = lapply(paste('Tab', 1: nTabs), tabPanel)
      do.call(tabsetPanel, myTabs)
    })
  }

shinyApp(ui = ui, server = server)


