library(shiny)
library(dplyr)

# ---- fake data ----
pillar_ecol_df_base <- tibble(
  objective = "Obj",
  bin = "Bin",
  areaID = c("A1", "A2", "A3", "A1"),
  region = "Region",
  indicator = "Ind",
  score = 1,
  PPTID = c("P1", "P2", "P3", "P4"),
  weight = 1,
  type = c("Physical", "Biological", "Physical", "Biological"),
  units = "unit",
  scoring = "method",
  project_short_title = "Project",
  climate = "No",
  design_target = "Target",
  status_statement = "Status",
  trend_statement = "Trend",
  quality_statement = "Quality",
  source = "Source",
  climate_expectation = "None",
  indicator_rationale = "Rationale",
  objectives = "Objectives",
  bin_rationale = "Bin rationale",
  readiness = "Ready",
  scale = c("Local", "Local", "Regional", "Regional"),
  target_name = "Target",
  pillar = "Ecological",
  tab = "Tab"
)

# ---- UI ----
ui <- fluidPage(

  checkboxGroupInput(
    "filter_type",
    "Filter by Type",
    choices = unique(pillar_ecol_df_base$type),
    selected = unique(pillar_ecol_df_base$type)
  ),

  selectInput(
    "project_id",
    "Select Project (PPTID)",
    choices = NULL
  ),

  tableOutput("preview")
)

# ---- server ----
server <- function(input, output, session) {

  # 1️⃣ Reactive filtered data
  pillar_ecol_df <- reactive({
    req(input$filter_type)

    pillar_ecol_df_base |>
      filter(type %in% input$filter_type)
  })

  # 2️⃣ Reactive project choices derived from filtered data
  project_choices <- reactive({
    sort(unique(pillar_ecol_df()$PPTID))
  })

  # 3️⃣ Update project selector when data changes
  observe({
    updateSelectInput(
      session,
      "project_id",
      choices = project_choices(),
      selected = project_choices()
    )
  })

  # 4️⃣ Something downstream that depends on filtered data
  output$preview <- renderTable({
    pillar_ecol_df()
  })
}

shinyApp(ui, server)
