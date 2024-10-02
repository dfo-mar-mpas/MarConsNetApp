library(shiny)
library(DT)

# Sample data for ind_links
ind_links <- c(
  '<a href="#link_352" onclick="Shiny.setInputValue(&#39;link_352&#39;, &#39;Fishing effort&#39;, {priority: &#39;event&#39;}); $(&#39;#yourTabsetId a[data-value=&quot;tab_link_352&quot;]&#39;).tab(&#39;show&#39;);">Fishing effort</a>',
  '<a href="#link_353" onclick="Shiny.setInputValue(&#39;link_353&#39;, &#39;climate change&#39;, {priority: &#39;event&#39;}); $(&#39;#yourTabsetId a[data-value=&quot;tab_link_353&quot;]&#39;).tab(&#39;show&#39;);">climate change</a>'
)

# Define UI
ui <- fluidPage(
  titlePanel("Interactive DataTable with Links"),
  DTOutput("table")
)

# Define server logic
server <- function(input, output) {
  output$table <- renderDT({
    datatable(
      data.frame(
        Indicator = ind_links,
        Status = rep(NA, length(ind_links)),
        Trend = rep(NA, length(ind_links)),
        stringsAsFactors = FALSE
      ),
      escape = FALSE,  # Allow HTML rendering
      options = list(pageLength = 100)
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
