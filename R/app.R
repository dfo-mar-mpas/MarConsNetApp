#' MarConsNetApp Conserved and Protected Area App
#'
#' This function creates a shiny app for the Maritimes region.
#' It addressed the following goals:
#' Site-Level Goals
#' - Report on what scientific work is occurring, and resources
#' allocated for this
#' - Report on how the scientific work being done is contrifbuting to
#' indicators and therefore conservation objectives (CO)
#' - Report on the status of the sites, based on existing data
#' Network-level Goals
#' - How are individual sites contributing to network objectives
#' - How are science projects contributing to network objectives
#' - Provide scientific data to support status claims
#' @importFrom shiny fluidRow tags titlePanel uiOutput sidebarLayout
#'  sidebarPanel mainPanel plotOutput column reactiveValues reactive
#'   observeEvent renderUI selectInput actionButton actionLink showModal
#'    modalDialog HTML updateTabsetPanel tagList
#'     showNotification shinyApp br
#' @importFrom shinyjs useShinyjs
#' @importFrom DT renderDT dataTableOutput datatable formatStyle styleEqual formatRound
#' @importFrom leaflet leafletOutput renderLeaflet leaflet addTiles addPolygons
#'  addCircleMarkers  addLegend
#' @importFrom MarConsNetAnalysis plot_flowerplot calc_group_score calc_letter_grade
#' @importFrom shinyBS bsCollapse bsCollapsePanel
#' @importFrom viridis viridis
#' @importFrom sf st_as_sf st_within st_coordinates st_crs
#' @importFrom dataSPA subsetSPA
#' @importFrom stringr str_extract_all
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange if_else filter select mutate left_join distinct group_by summarize reframe ungroup sym rowwise row_number
#' @importFrom targets tar_load
#' @importFrom readxl read_excel
#'
#'
#' @export
#' @examples
#' \dontrun{
#' app()
#' }

# Define UI

app <- function() {

  rv <- reactiveValues(
    plotted_projects = character()  # keeps track of already plotted projects
  )

  # find the path to the targets data store
  STORE <- path_to_store()

  reportpath <- file.path(dirname(STORE), "data", "reports")
  shiny::addResourcePath("htmlfiles", reportpath)

  # load targets if necessary
  if(!exists("APPTABS")){
    tar_load(c("APPTABS","pillar_ecol_df","all_project_geoms","MPA_report_card","MPAs","regions","flowerPalette","indicatorFlower","N_Objectives","om","Ecological", "Context", "collaborations", "deliverables", "csas", "climate_change", "cost_of_mpas", "salary", "theme_table", "objective_tabs", "objective_indicators","map_palette","labels","all_indicator_project_geoms","conservation_targets_target"),
             store = STORE)
  }
  load(paste0(dirname(path_to_store()), '/data/unique_table_cost.rda'))

  condition <- paste0('input.tabs === "tab_0"')

  ## FILTERING FOR

  old_pillar_ecol_df <- pillar_ecol_df
  old_all_project_geoms <- all_project_geoms
  pillar_ecol_df <- pillar_ecol_df[-which(pillar_ecol_df$areaID == "Non_Conservation_Area"),]
  #all_project_geoms <- all_project_geoms[-which(all_project_geoms$areaID == "Non_Conservation_Area"),]

  obj <- paste0(dirname(STORE),"/data/objectives.xlsx")
  obj_excel <-read_excel(obj)


  ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),

    # Modern Dashboard CSS
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        /* Base styling with dark sidebar */
        body {
          font-family: 'Inter', 'Helvetica Neue', Arial, sans-serif;
          background-color: #f0f2f5;
          margin: 0;
          padding: 0;
        }

       /* Reset default margins and padding */
        * {
          margin: 0;
          padding: 0;
          box-sizing: border-box;
        }

        html, body {
          margin: 0;
          padding: 0;
          width: 100%;
          height: 100%;
        }

        /* Top Navigation Bar */
        .top-navbar {
          background: linear-gradient(90deg, #1e3a8a 0%, #3b82f6 100%);
          color: white;
          padding: 15px 30px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.15);
          position: fixed;
          top: 0;
          left: 0;
          right: 0;
          width: 100%;
          z-index: 1000;
          display: flex;
          justify-content: space-between;
          align-items: center;
        }

        .app-title {
          font-size: 24px;
          font-weight: 700;
          margin: 0;
          letter-spacing: -0.5px;
        }

        .app-subtitle {
          font-size: 13px;
          opacity: 0.9;
          margin: 0;
        }

        .top-nav-buttons {
          display: flex;
          gap: 10px;
          align-items: center;
        }

        /* Integrated Tab Styling */
        .integrated-tabs-card {
          background: white;
          border-radius: 12px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.08);
          border: 1px solid #e5e7eb;
          margin-bottom: 20px;
          overflow: hidden;
        }

        #tab0_subtabs {
          margin-bottom: 0;
        }

        #tab0_subtabs > .nav.nav-tabs {
          border: none;
          border-bottom: 1px solid #e5e7eb;
          display: flex;
          gap: 0;
          padding: 0;
          margin: 0;
          background: #f9fafb;
        }

        #tab0_subtabs > .nav.nav-tabs > li {
          flex: 1;
          margin: 0;
        }

        #tab0_subtabs > .nav.nav-tabs > li > a {
          border: none;
          border-radius: 0;
          color: #6b7280;
          font-weight: 600;
          font-size: 14px;
          padding: 16px 20px;
          margin: 0;
          text-align: center;
          transition: all 0.2s ease;
          background: transparent;
          border-bottom: 3px solid transparent;
          display: block;
        }

        #tab0_subtabs > .nav.nav-tabs > li > a:hover {
          background: #f3f4f6;
          color: #374151;
        }

        #tab0_subtabs > .nav.nav-tabs > li.active > a,
        #tab0_subtabs > .nav.nav-tabs > li.active > a:hover,
        #tab0_subtabs > .nav.nav-tabs > li.active > a:focus {
          background: white;
          color: #3b82f6;
          border: none;
          border-bottom: 3px solid #3b82f6;
        }

        #tab0_subtabs > .tab-content {
          padding: 25px;
          border: none;
          background: white;
          min-height: 200px;
        }

        /* Sidebar styling with fixed positioning */
        .sidebar-panel {
          background: #ffffff;
          border-right: 1px solid #e5e7eb;
          padding: 0;
          height: calc(100vh - 70px);
          overflow-y: auto;
          box-shadow: 2px 0 10px rgba(0,0,0,0.05);
          position: fixed;
          top: 70px;
          left: 0;
          width: 25%;
          z-index: 900;
        }

        .sidebar-section {
          padding: 20px;
          border-bottom: 1px solid #e5e7eb;
        }

        .sidebar-section h4 {
          font-size: 12px;
          text-transform: uppercase;
          letter-spacing: 1px;
          color: #6b7280;
          margin: 0 0 15px 0;
          font-weight: 600;
        }

        /* Main content area with left margin to accommodate fixed sidebar */
        .main-content {
          padding: 25px;
          background: #f0f2f5;
          margin-left: 25%;
        }

        /* Card components */
        .dashboard-card {
          background: white;
          border-radius: 12px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.08);
          padding: 25px;
          margin-bottom: 20px;
          border: 1px solid #e5e7eb;
          transition: box-shadow 0.3s ease;
        }

        .dashboard-card:hover {
          box-shadow: 0 4px 12px rgba(0,0,0,0.12);
        }

        .card-header {
          display: flex;
          justify-content: space-between;
          align-items: center;
          margin-bottom: 20px;
          padding-bottom: 15px;
          border-bottom: 2px solid #e5e7eb;
        }

        .card-title {
          font-size: 18px;
          font-weight: 600;
          color: #1f2937;
          margin: 0;
        }

        /* Content section within tabs */
        .tab-section {
          margin-bottom: 30px;
        }

        .tab-section:last-child {
          margin-bottom: 0;
        }

        .tab-section-header {
          display: flex;
          justify-content: space-between;
          align-items: center;
          margin-bottom: 20px;
          padding-bottom: 12px;
          border-bottom: 2px solid #e5e7eb;
        }

        .tab-section-title {
          font-size: 18px;
          font-weight: 600;
          color: #1f2937;
          margin: 0;
        }

        /* Button styles */
        .btn {
          border-radius: 8px;
          font-weight: 500;
          font-size: 14px;
          padding: 10px 20px;
          border: none;
          transition: all 0.2s ease;
          box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }

        .btn-primary {
          background: linear-gradient(135deg, #3b82f6 0%, #2563eb 100%);
          color: white;
        }

        .btn-primary:hover {
          background: linear-gradient(135deg, #2563eb 0%, #1d4ed8 100%);
          transform: translateY(-1px);
          box-shadow: 0 4px 8px rgba(59, 130, 246, 0.3);
        }

        .btn-secondary {
          background: #f3f4f6;
          color: #374151;
        }

        .btn-secondary:hover {
          background: #e5e7eb;
        }

        .btn-icon {
          padding: 8px 12px;
          border-radius: 8px;
          background: rgba(255,255,255,0.2);
          color: white;
          border: 1px solid rgba(255,255,255,0.3);
          font-size: 14px;
        }

        .btn-icon:hover {
          background: rgba(255,255,255,0.3);
        }

        /* Context Button */
        .btn-context-custom {
          background: linear-gradient(135deg, #f59e0b 0%, #f97316 100%);
          color: white;
          border: none;
          border-radius: 8px;
          padding: 12px 18px;
          font-weight: 600;
          font-size: 14px;
          box-shadow: 0 4px 12px rgba(245, 158, 11, 0.3);
          transition: all 0.3s ease;
          text-transform: uppercase;
          letter-spacing: 0.5px;
        }

        .btn-context-custom:hover {
          background: linear-gradient(135deg, #f97316 0%, #ea580c 100%);
          transform: translateY(-2px);
          box-shadow: 0 6px 16px rgba(245, 158, 11, 0.4);
        }

        .btn-context-custom:active {
          transform: translateY(0);
          box-shadow: 0 2px 8px rgba(245, 158, 11, 0.3);
        }

        /* Filter Button */
        .btn-filter-custom {
          background: linear-gradient(135deg, #8b5cf6 0%, #7c3aed 100%);
          color: white;
          border: none;
          border-radius: 8px;
          padding: 12px 18px;
          font-weight: 600;
          font-size: 14px;
          box-shadow: 0 4px 12px rgba(139, 92, 246, 0.3);
          transition: all 0.3s ease;
          text-transform: uppercase;
          letter-spacing: 0.5px;
        }

        .btn-filter-custom:hover {
          background: linear-gradient(135deg, #7c3aed 0%, #6d28d9 100%);
          transform: translateY(-2px);
          box-shadow: 0 6px 16px rgba(139, 92, 246, 0.4);
        }

        .btn-filter-custom:active {
          transform: translateY(0);
          box-shadow: 0 2px 8px rgba(139, 92, 246, 0.3);
        }

        /* Icon styling */
        .btn-context-custom i,
        .btn-filter-custom i {
          margin-right: 6px;
        }

        /* Select inputs */
        .selectize-input {
          border: 1px solid #d1d5db;
          border-radius: 8px;
          padding: 10px 12px;
          font-size: 14px;
          background: white;
          box-shadow: 0 1px 2px rgba(0,0,0,0.05);
        }

        .selectize-input:focus {
          border-color: #3b82f6;
          box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1);
        }

        .selectize-dropdown {
          border-radius: 8px;
          border: 1px solid #d1d5db;
          box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        }

        /* Legend styling */
        .legend-box {
          background: #f9fafb;
          border-radius: 8px;
          padding: 15px;
          border: 1px solid #e5e7eb;
        }

        .legend-title {
          font-size: 12px;
          font-weight: 600;
          text-transform: uppercase;
          color: #6b7280;
          margin-bottom: 12px;
          letter-spacing: 0.5px;
        }

        .legend-items {
          display: flex;
          flex-wrap: wrap;
          gap: 12px;
        }

        .legend-item {
          display: flex;
          align-items: center;
          gap: 8px;
          font-size: 13px;
          color: #374151;
        }

        .legend-color-box {
          width: 20px;
          height: 20px;
          border-radius: 4px;
          border: 1px solid rgba(0,0,0,0.1);
          box-shadow: 0 1px 2px rgba(0,0,0,0.05);
        }

        /* Map container */
        .map-container {
          border-radius: 12px;
          overflow: hidden;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
          border: 1px solid #e5e7eb;
          height: 550px;
        }

        /* DataTables */
        .dataTables_wrapper {
          padding: 0;
        }

        table.dataTable {
          border-collapse: separate;
          border-spacing: 0;
          width: 100%;
        }

        table.dataTable thead th {
          background: #f9fafb;
          color: #374151;
          font-weight: 600;
          font-size: 13px;
          text-transform: uppercase;
          letter-spacing: 0.5px;
          padding: 15px 12px;
          border-bottom: 2px solid #e5e7eb;
        }

        table.dataTable tbody td {
          padding: 12px;
          border-bottom: 1px solid #f3f4f6;
          font-size: 14px;
          color: #374151;
        }

        table.dataTable tbody tr:hover {
          background-color: #f9fafb;
        }

        /* Radio button toggle */
        .radio-toggle {
          background: #f3f4f6;
          padding: 3px;
          border-radius: 8px;
          display: inline-flex;
          gap: 3px;
        }

        .radio-toggle label {
          padding: 6px 16px;
          border-radius: 6px;
          cursor: pointer;
          transition: all 0.2s;
          font-size: 13px;
          font-weight: 500;
          color: #6b7280;
          background: transparent;
          border: none;
          margin: 0;
        }


        .radio-toggle input[type='radio'] {
          display: none;
        }

        .radio-toggle input[type='radio']:checked + span {
          background: white;
          color: #3b82f6;
          box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }

        /* Notifications */
        .shiny-notification {
          background: white;
          border-left: 4px solid #f59e0b;
          border-radius: 8px;
          box-shadow: 0 4px 12px rgba(0,0,0,0.15);
          padding: 16px 20px;
        }

        /* Section headers */
        h3 {
          font-size: 20px;
          font-weight: 600;
          color: #1f2937;
          margin: 0 0 15px 0;
        }

        h4 {
          font-size: 16px;
          font-weight: 600;
          color: #374151;
          margin: 0 0 10px 0;
        }

        /* Links */
        a {
          color: #3b82f6;
          text-decoration: none;
          transition: color 0.2s;
        }

        a:hover {
          color: #2563eb;
          text-decoration: underline;
        }

        /* Grid layout for objectives */
        .objectives-grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
          gap: 20px;
        }

        .objective-card {
          background: #f9fafb;
          border-radius: 10px;
          padding: 20px;
          border: 1px solid #e5e7eb;
          transition: all 0.2s;
        }

        .objective-card:hover {
          box-shadow: 0 4px 12px rgba(0,0,0,0.1);
          transform: translateY(-2px);
        }

        /* Scrollbar styling */
        ::-webkit-scrollbar {
          width: 8px;
          height: 8px;
        }

        ::-webkit-scrollbar-track {
          background: #f3f4f6;
        }

        ::-webkit-scrollbar-thumb {
          background: #d1d5db;
          border-radius: 4px;
        }

        ::-webkit-scrollbar-thumb:hover {
          background: #9ca3af;
        }

        /* Hide default tabs */
        #mytabs > .tabbable > .nav.nav-tabs {
          display: none;
        }

        /* Responsive adjustments */
        @media (max-width: 768px) {
          .sidebar-panel {
            position: relative;
            width: 100%;
            height: auto;
          }

          .main-content {
            margin-left: 0;
          }

          .top-navbar {
            flex-direction: column;
            align-items: flex-start;
            gap: 10px;
          }

          .objectives-grid {
            grid-template-columns: 1fr;
          }
        }

        /* Plot container styling */
        .plot-container {
          width: 100%;
          overflow-x: auto;
          overflow-y: auto;
          max-height: none;
          padding: 15px 0;
        }

        /* Tab content must have sufficient height */
        #tab0_subtabs > .tab-content {
          padding: 25px;
          border: none;
          background: white;
          min-height: auto;
          max-height: none;
        }

        /* Image display styling */
        .image-display-wrapper {
          display: block;
          width: 100%;
          margin: 20px 0;
          overflow-x: auto;
        }

        /* Individual image styling */
        .image-display-wrapper img {
          max-width: 100%;
          height: auto;
          display: block;
          margin: 10px 0;
        }

        /* Integrated tabs card should not restrict height */
        .integrated-tabs-card {
          background: white;
          border-radius: 12px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.08);
          border: 1px solid #e5e7eb;
          margin-bottom: 20px;
          overflow: visible;
        }

        /* Main content should have proper overflow handling */
        .main-content {
          padding: 25px;
          background: #f0f2f5;
          margin-left: 25%;
          overflow-y: auto;
        }
      "))
    ),

    # Top Navigation Bar
    div(class = "top-navbar",
        div(
          h1(class = "app-title", "Maritimes Conservation Network"),
          p(class = "app-subtitle", "Marine Protected Areas Monitoring Dashboard")
        ),
        div(class = "top-nav-buttons",
            shiny::uiOutput("gohome"),
            actionButton("about", label = NULL, icon = icon("question-circle"),
                         class = "btn-icon", title = "User Guide"),
            tags$a(
              href = "https://github.com/dfo-mar-mpas/MarConsNetApp",
              target = "_blank",
              class = "btn-icon",
              icon("github"),
              title = "View on GitHub"
            )
        )
    ),

    # Hide tabs CSS
    tags$style(HTML("#mytabs > .tabbable > .nav.nav-tabs { display: none; }")),

    # Main layout
    shiny::sidebarLayout(
      # Sidebar Panel (now fixed)
      shiny::sidebarPanel(
        width = 3,
        class = "sidebar-panel",
        style = "padding: 0;",

        # Flower plot section
        div(class = "sidebar-section",
            shiny::uiOutput("conditional_ind_Flower")
        ),

        # Legend section
        div(class = "sidebar-section",
            div(class = "legend-box",
                div(class = "legend-title", "LEGEND"),
                div(class = "legend-items",
                    shiny::uiOutput("legendUI")
                )
            )
        ),

        # Filters section
        div(class = "sidebar-section",
            h4("FILTERS"),
            shiny::uiOutput("region"),
            shiny::uiOutput("mpas"),
            shiny::uiOutput("projects")
        ),

        # buttons section
        div(class = "sidebar-section",
            div(shiny::uiOutput("contextButton"),
                shiny::uiOutput("filter_button_ui", inline = TRUE)
            )
        ),

        # indicator selection section
        div(class = "sidebar-section",
            h4("INDICATOR SELECTION"),
            shiny::uiOutput("filter_ind_type_ui", inline = TRUE),
            shiny::uiOutput("filter_ind_scale_ui", inline = TRUE
            )
        ),

      ),

      # Main Panel
      shiny::mainPanel(
        width = 9,
        class = "main-content",

        # Map card
        conditionalPanel(
          condition = "input.tabs == 'tab_0'",
          div(class = "dashboard-card",
              div(class = "card-header",
                  h3(class = "card-title", "Geographic Overview"),
                  shiny::uiOutput("score_disclaimer")
              ),
              div(class = "map-container",
                  id = "mapContainer",
                  leafletOutput("map", height = "100%")
              ))
        ),

        # Integrated tabs card with all content inside
        div(class = "integrated-tabs-card",
            shiny::uiOutput('mytabs'),
            # Shared content
            shiny::uiOutput("indicatorText"),
            shiny::uiOutput("DT_ui"),
            shiny::uiOutput("conditionalPlot"),
            shiny::uiOutput("conditionalIndicatorMap"),
            shiny::uiOutput("whaleDisclaimer"),
        )
      )
    )
  )


  # Define server logic
  server <- function(input, output, session) {
    state <- shiny::reactiveValues(
      region = "Maritimes",
      mpas = "Maritimes",
      projects = NULL,
      fundingSource = NULL,
      theme = NULL,
      functionalGroup = NULL,
      section = NULL,
      division = NULL
      #report = NULL
    )

    output$ddff_display_tbl <- DT::renderDT({

      if (any(c(is.null(input$filter_ind_type), is.null(input$filter_ind_scale)))) {
      ddff <- old_pillar_ecol_df[0,]
      names(ddff) <- toupper(names(ddff))
        } else {
      ddff <- ddff_display_r()
        }
      if (length(ddff$INDICATOR) == 0) {
        if (input$tabs == "tab_0") {
        showModal(modalDialog(
          title = "No indicators Available",
          "There are no indicators that match your filter. Try adding more selections in the 'Type' filter.",
          easyClose = TRUE,
          footer = NULL
        ))
        }
        return(NULL)
      } else {
        dt <- datatable(
          ddff,
          rownames = FALSE,
          selection = "single",
          extensions = "RowGroup",
          escape = FALSE,
          options = list(
            rowGroup = list(dataSrc = 0),
            columnDefs = list(
              list(visible = FALSE, targets = 0),
              list(
                visible = FALSE,
                targets = which(names(ddff) == "PPTID") - 1
              )
            ),
            pageLength = 100
          )
        )
        return(dt)

      }
    }
   )

    output$network_column <- renderUI({
      req(state$mpas)

      # Check condition
      in_list <- state$mpas %in% MPAs$NAME_E

      # If ANY selected MPA is in the list → width = 6
      width_val <- if (any(in_list)) 6 else 12

      column(
        width = width_val,
        br(),
        style = "border-left: 1px solid #ccc; border-right: 1px solid #ccc;",
        uiOutput("networkObjectiveText")
      )
    })



    rv <- shiny::reactiveValues(button_label = "See All Project Data")

    is_button_visible <- shiny::reactive({
      req(state$mpas)
      req(input$projects)
      length(state$mpas) > 0 && length(state$projects) > 0 && input$tabs == "tab_0" && !(state$mpas %in% unique(pillar_ecol_df$region))
    })

    observe({
      showModal(modalDialog(
        title = "Disclaimer",
        "This app is under development. The scores shown do not represent a finalized or authoritative assessment of status. Apparent data gaps may reflect datasets that have not yet been incorporated into the app, and do not necessarily indicate that data were not collected.",
        easyClose = TRUE,
        footer = modalButton("I Understand")
      ))
    })


    # Reactive expression to update MPA choices based on selected regions
    mpas_choices <- reactive({
      mpas <- pillar_ecol_df[pillar_ecol_df$region %in% state$region,c("areaID","region")] |>
        unique()

      split(mpas$areaID, mpas$region, drop = TRUE)
    })


    input_ids <- c("mpas", "region", "projects", "fundingSource", "theme", "functionalGroup", "section", "division") # THE SAME AS STATE

    lapply(input_ids, function(id) {
      shiny::observeEvent(input[[id]], {
        state[[id]] <- input[[id]]
      })
    })


    output$mytabs <- renderUI({

      myTabs <- lapply(c(APPTABS$tab, pillar_ecol_df$tab, objective_tabs$tab), function(tabname) {
        if (tabname == "tab_0") {
          tabPanel(
            "tab_0",
            tabsetPanel(
              id = "tab0_subtabs",

              # Management Effectiveness Tab
              tabPanel("Management Effectiveness",
                       p("This tab considers only the effectiveness indicators that directly inform objectives that the site is managed for"),
                       hr(),

                       # Management Effectiveness specific
                       hr(),
                       fluidRow(
                         uiOutput("network_column"),
                         column(6, shiny::uiOutput("objectives"))
                       )
              ),

              # Effectiveness Contributions Tab
              tabPanel("Effectiveness Contributions",
                       p("This tab considers only the effectiveness indicators that inform objectives for which the site is NOT managed. These are considered additional contributions of the site."),
                       hr(),

                       # Effectiveness Contributions specific
                       hr(),
                       div(class = "tab-section-header",
                           h3(class = "tab-section-title", "Conservation Framework")
                       ),
                       div(class = "objectives-grid",
                           div(class = "objective-card", shiny::uiOutput('gbf_objectives')),
                           div(class = "objective-card", shiny::uiOutput("ebm_objectives")),
                           div(class = "objective-card", shiny::uiOutput('network_design'))
                       )
              ),

              # Ecosystem Overview Tab
              tabPanel("Ecosystem Overview",
                       p(
                         "Readiness Score Explained: ",
                         strong("Ready"), " – Code is developed and data is integrated into the app, ",
                         strong("Readily Available"), " – Data is being collected and stored but requires some work to integrate into the app, ",
                         strong("Not currently collected"), " – Data is not yet being collected, ",
                         strong("Conceptual"), " – There is no means of collecting this type of data. ",
                         strong("Unknown"), " – More work needed to determine readiness score."
                       ),
                       hr(),
                       div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 20px; padding-bottom: 12px; border-bottom: 2px solid #e5e7eb;",
                           h3(style = "margin: 0; font-size: 18px; font-weight: 600; color: #1f2937;", "Ecosystem Overview"),
                           div(style = "display: flex; align-items: center; gap: 10px;",
                               span(style = "font-size: 13px; color: #6b7280; font-weight: 500;", "View:"),
                               uiOutput('indicator_mode')
                           )
                       ),
                       fluidRow(
                         column(5,
                                conditionalPanel(
                                  condition = "input.tab0_subtabs == 'Ecosystem Overview'",
                                  shiny::uiOutput("conditionalFlower")
                                )
                         ),
                         column(7,
                                # Try rendering the table directly
                                DT::DTOutput("ddff_display_tbl") # KYLO
                         )
                       ),
                       br(),
                       shiny::uiOutput("threats_home_table")
              ),

              # Threats Tab
              tabPanel("Threats",
                       p("This tab shows threat indicators and their status across the network."),
                       hr(),
                       div(class = "tab-section-header",
                           h3(class = "tab-section-title", "Threats Assessment")
                       ),
                       DT::DTOutput("ecosystem_table")
              )
            )
          )
        } else {
          tabPanel(tabname)
        }
      })

      do.call(tabsetPanel, c(myTabs, id = "tabs"))
    })


    output$indicator_mode <- renderUI({
      req(state$mpas)
      req(input$tabs)

      if (input$tabs == "tab_0" && input$tab0_subtabs == "Ecosystem Overview") {
        div(class = "radio-toggle", style = "margin: 0;",
            radioButtons(
              inputId = "indicator_mode",
              label = "Select table view:",
              choices = list("EBM Framework" = "ebm",
                             "Ecological Themes" = "themes"),
              selected = "ebm",
              inline = TRUE
            )
        )
      }
    })

    output$score_disclaimer <- renderUI({
      req(input$tabs)
      if (input$tabs == "tab_0") {
        renderText("* Polygons on map are color-coded according to overall ecological score")
      }
    })


    output$ecosystem_table <- renderDT({ # JAIM (show modal showing up when it shouldn't)
      if (any(c(is.null(input$filter_ind_type), is.null(input$filter_ind_scale)))) {
        ddff <- old_pillar_ecol_df[0,]
        names(ddff) <- toupper(names(ddff))
      } else {
        ddff <- ddff_display_r()
      }
      if (length(ddff$INDICATOR) == 0) {
        showModal(modalDialog(
          title = "No Indicators Available",
          "There are no indicators that match your filter. Try adding more selections in the 'Type' filter.",
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)

      } else {
      datatable(
        ddff,
        rownames = FALSE,
        selection='single',
        extensions = "RowGroup",
        escape=FALSE,
        options = list(
          rowGroup = list(dataSrc = 0),                        # group by GROUPING
          columnDefs = list(
            list(visible = FALSE, targets = 0),               # hide GROUPING column
            list(visible = FALSE, targets = (which(names(ddff_display_r()) == "PPTID") - 1))
          ),
          pageLength = 100
        )
      )
      }
    })

    ddff_display_r <- reactive({
      req(input$tab0_subtabs)
      req(input$tabs)
      if (input$tabs == "tab_0" &&
          input$tab0_subtabs %in% c("Ecosystem Overview", "Threats")) {

        if (input$tab0_subtabs == "Ecosystem Overview") {
          req(input$indicator_mode)
        }

        if ((input$tab0_subtabs == "Ecosystem Overview" && input$indicator_mode == "ebm") | input$tab0_subtabs == "Threats") {
          if (!(is.null(input$filter_ind_type)) | !(is.null(input$filter_ind_scale))) {

          # Ecological Overview

          if (state$mpas %in% regions$NAME_E) {
            k1 <- which(!(filtered_pillar_ecol_df()$areaID %in% regions$NAME_E))
            k2 <- which(grepl("Network design", filtered_pillar_ecol_df()$indicator))
            table_ped <- filtered_pillar_ecol_df()[sort(c(k1,k2)),]
            if (!(length(k2) == 0)) { # This is now due to if we remove network targets from indicator type
            table_ped <- table_ped[-(which(grepl("Network design", table_ped$indicator) & (!(table_ped$areaID %in% regions$NAME_E)))),]
            }

          } else {
            table_ped <- filtered_pillar_ecol_df()[which(filtered_pillar_ecol_df()$areaID == state$mpas),]
          }
          if (any(table_ped$indicator == "placeholder") | any(is.na(table_ped$indicator))) {
            table_ped <- table_ped[-which(table_ped$indicator == 'placeholder' | is.na(table_ped$indicator)),]
          }


          table_ped <- table_ped[,c("bin", "indicator", "source", "score", "weight", "PPTID", "areaID", 'readiness', 'quality_statement')]

          ddff <- table_ped %>%
            left_join(
              Ecological %>% dplyr::select(labels, grouping),
              by = c("bin" = "labels")   # bin in table_ped matches labels in Ecological
            ) %>%
            # Add placeholders for readiness, quality, cost
            mutate(
              #readiness = NA_real_,
              cost      = NA_real_
            ) %>%
            dplyr::select(grouping, bin, indicator, source, score, readiness, quality_statement, cost, PPTID, areaID) %>%
            dplyr::arrange(grouping, bin) %>%
            setNames(toupper(names(.)))

          if (state$mpas %in% regions$NAME_E) {
            # FIXME!!!!!
            ddff_unique <- ddff %>%
              rowwise() %>%
              mutate(
                SCORE = if_else(
                  grepl("Network design", INDICATOR),

                  # 🔹 WHEN TRUE → use actual score
                  table_ped$score[
                    table_ped$indicator == INDICATOR &
                      table_ped$bin == BIN
                  ][1],

                  # 🔹 WHEN FALSE → weighted mean (your existing logic)
                  weighted.mean(
                    x = table_ped$score[
                      table_ped$indicator == INDICATOR &
                        table_ped$bin == BIN
                    ],
                    w = table_ped$weight[
                      table_ped$indicator == INDICATOR &
                        table_ped$bin == BIN
                    ],
                    na.rm = TRUE
                  )
                )
              ) %>%
              ungroup() %>%
              distinct(GROUPING, BIN, INDICATOR, SOURCE, SCORE, READINESS, QUALITY_STATEMENT, COST, PPTID)

          } else {
            ddff_unique <- ddff
          }
          } else {
          # EVERYTHING IS DE-SELECTED KYLO
            ddff_display <- old_pillar_ecol_df[0, ]  # empty df
            names(ddff_display) <- toupper(names(ddff_display))
            ddff_unique <- ddff_display


        }
        } else {

          ## THEME
          if (!(is.null(input$filter_ind_type)) | !(is.null(input$filter_ind_scale))) {

          if (state$mpas %in% regions$NAME_E) {
            table_ped <- theme_table[which(!(theme_table$areaID %in% regions$NAME_E)),]

          } else {
            table_ped <- theme_table[which(theme_table$areaID == state$mpas),]
          }
          if (any(table_ped$indicator == "placeholder") | any(is.na(table_ped$indicator))) {
            table_ped <- table_ped[-which(table_ped$indicator == 'placeholder' | is.na(table_ped$indicator)),]
          }

          table_ped <- table_ped[,c("theme", "indicator", "source", "score", "weight", "PPTID", 'readiness', 'quality_statement')]



          ddff <- table_ped %>%
            # Add placeholders for readiness, quality, cost
            mutate(
              #readiness = NA_real_,
              cost      = NA_real_
            ) %>%
            # Select columns in the desired order
            dplyr::select(theme, indicator, source, score, readiness, quality_statement, cost, PPTID) %>%
            # Arrange by theme and indicator
            dplyr::arrange(theme, indicator) %>%
            # Capitalize column names
            setNames(toupper(names(.)))

          if (state$mpas %in% regions$NAME_E) {
            # FIXME!!!!!

            ddff_unique <- ddff %>%
              rowwise() %>%
              mutate(
                SCORE = weighted.mean(
                  x = table_ped$score[table_ped$indicator == INDICATOR & table_ped$theme == THEME],
                  w = table_ped$weight[table_ped$indicator == INDICATOR & table_ped$theme == THEME],
                  na.rm = TRUE
                )
              ) %>%
              ungroup() %>%
              distinct(THEME,INDICATOR, SOURCE, SCORE, READINESS, QUALITY_STATEMENT, COST, PPTID)

          } else {
            ddff_unique <- ddff
          }

          } else {
            # EVERYTHING IS DE-SELECTED KYLO
            ddff_display <- old_pillar_ecol_df[0, ]  # empty df
            names(ddff_display) <- toupper(names(ddff_display))
            ddff_unique <- ddff_display


          }

        }

        if (!(length(ddff_unique$PPTID) == 0)) {
          for (i in seq_along(unique(ddff_unique$PPTID))) {
            ppt <- unique(ddff_unique$PPTID)[i]
            if (is.na(ppt)) {

              isna <- which(is.na(ddff_unique$PPTID))

              for (j in isna) {
                if (!(ddff_unique$READINESS[j] == "Ready")) {
                  ddff_unique$COST[j] <- "Unknown at this time"
                } else {
                  ddff_unique$COST[j] <- "External"
                }

              }
            } else {
              ddff_unique$COST[which(ddff_unique$PPTID == unique(ddff_unique$PPTID)[i])] <- paste0("$", round(unique(cost_of_mpas$price_per_station[which(cost_of_mpas$project_id == ppt)]),2), "/ source sample")

            }
          }

          if (input$tab0_subtabs == "Threats") {
            ddff_unique <- ddff_unique[which(grepl("Threats", ddff_unique$BIN)),]

          }
          if (!(input$tab0_subtabs == "Threats")) {
            if (input$indicator_mode == "ebm") {
              ddff_display <- ddff_unique %>%
                dplyr::arrange(GROUPING, SOURCE) %>%                # make sure sources are together within grouping
                group_by(GROUPING, SOURCE) %>%
                mutate(COST = ifelse(row_number() == 1, COST, "")) %>%  # only first row of each SOURCE shows COST
                ungroup()
            } else {
              ddff_display <- ddff_unique %>%
                dplyr::arrange(THEME, SOURCE) %>%                # make sure sources are together within grouping
                group_by(THEME, SOURCE) %>%
                mutate(COST = ifelse(row_number() == 1, COST, "")) %>%  # only first row of each SOURCE shows COST
                ungroup()
            }
          } else {
            # Threats
            ddff_display <- ddff_unique %>%
              dplyr::arrange(GROUPING, SOURCE) %>%                # make sure sources are together within grouping
              group_by(GROUPING, SOURCE) %>%
              mutate(COST = ifelse(row_number() == 1, COST, "")) %>%  # only first row of each SOURCE shows COST
              ungroup()

          }
        }


        ddff_display$SCORE <- round(ddff_display$SCORE,2)

        ddff_display <- ddff_display %>%
          mutate(
            SOURCE = paste0(
              '<a href="#" class="source-link">', SOURCE, '</a>'  # ••• Make SOURCE look like a link
            )
          )

        ddff_display
      }

    })

    observeEvent(input$ecosystem_table_cell_clicked, {

      info <- input$ecosystem_table_cell_clicked
      req(info$row, info$col)

      if (colnames(ddff_display_r())[info$col + 1] == "SOURCE") {
        indicator_clicked <- ddff_display_r()$INDICATOR[info$row]
        source_clicked <- gsub("<[^>]+>", "", ddff_display_r()$SOURCE[info$row])
        k1 <- which(unique_table$indicator == indicator_clicked)
        k2 <- which(unique_table$source == source_clicked)
        keep <- intersect(k1,k2)

        showModal(
          modalDialog(
            title = "Code for Indicator",
            tagList(
              unique_table$plot[[keep]],

              # Hidden textarea used for copying
              tags$textarea(
                id = "code_to_copy",
                unique_table$code[keep],
                style = "position:absolute; left:-9999px;"
              ),

              # Visible formatted code
              tags$pre(htmltools::htmlEscape(unique_table$code[keep]))
            ),
            size = "xl",
            easyClose = TRUE,
            footer = tagList(
              tags$button(
                "Copy",
                class = "btn btn-primary",
                onclick = "
  var el = document.getElementById('code_to_copy');
  el.style.display = 'block';
  el.select();
  document.execCommand('copy');
  el.style.display = 'none';
"
              ),
              modalButton("Close")
            )
          )
        )

      }
    })


    output$flowerType <- renderUI({
      req(state$mpas)
      req(input$tabs)
      if (input$tabs == "tab_0") {
        if (state$mpas %in% unique(pillar_ecol_df$areaID) || tolower(state$mpas) %in% tolower(MPAs$NAME_E)) {
          choices <- c("Status","Network Design Targets", "Level of Certainty", "Models", "In Situ Measurements", "Time Since Last in Situ Measurement")
          selectInput("flowerType", "Select Score Type", choices=choices, selected = "Status")
        } else {
          shiny::showModal(shiny::modalDialog(
            title = "No data provided for this area."
          ))
        }
      }

    })

    output$legendUI <- renderUI({
      req(state$mpas)
      req(input$tabs)
      # Generate legend items
      if (input$tabs == "tab_0") {
        PALETTE <- append(flowerPalette,list("NA" = "#EDEDED"))
      } else {
        PALETTE <- indicatorFlower
      }
      legendItems <- lapply(names(PALETTE), function(name) {
        div(
          style = paste0(
            "display: flex; align-items: center; margin-right: 20px;"
          ),
          div(
            style = paste0(
              "width: 20px; height: 20px; background-color: ",
              PALETTE[name],
              "; margin-right: 5px; border: 1px solid black;"
            )
          ),
          span(name) # Label
        )
      })
      # Wrap the items in a horizontal flex container
      div(
        style = "display: flex; flex-wrap: wrap; align-items: center;",
        legendItems
      )
    })

    observeEvent(input$about, {
      req(input$about)
      showModal(
        modalDialog(
          title = "About this App",
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Close"),

          tags$p(
            "This app uses the ",
            tags$b("Ecosystem-based Management (EBM) framework"),
            " to report on the effectiveness of Marine Protected Areas."
          ),

          tags$p(
            "Learn more here: ",
            tags$a(
              href = "https://dfo-mar-mpas.shinyapps.io/EBMapp/",
              "Ecosystem-based Management Framework",
              target = "_blank"
            )
          ),

          tags$p("The app reports on:"),

          tags$ol(
            tags$li("The Management Effectiveness of network- and site-level objectives"),
            tags$li("Effectiveness contributions to objectives that sites are not explicitly managed for"),
            tags$li("Overall ecosystem health"),
            tags$li("Threats to the ecosystem")
          )
        )
      )

    })


    output$mpas <- shiny::renderUI({
      req(input$tabs)
      if (input$tabs == "tab_0") {
        shiny::selectInput("mpas","Select Protected/Conserved Area:",choices = mpas_choices(), selected=state$mpas)
      }
    })

    output$region <- shiny::renderUI({
      req(input$tabs)
      if (input$tabs == "tab_0") {
        regionlist <- unique(pillar_ecol_df$region)
        shiny::selectInput("region","Select Region(s)",choices = regionlist[!is.na(regionlist)], selected=state$region, multiple=TRUE)
      }
    })

    output$projects <- shiny::renderUI({
      req(input$tabs)
      if (input$tabs == "tab_0") {
        shiny::selectInput("projects", "Select Project(s):", choices=project_choices(), multiple=TRUE, selected=NULL)
      }
    })

    # Reactive project choices derived from filtered data
    project_choices <- reactive({
      req(input$filter_ind_scale)
      req(input$filter_ind_type)
      req(filtered_pillar_ecol_df())


      filter_ppt <- unique(filtered_pillar_ecol_df()$PPTID)
      filter_ppt <- filter_ppt[-(which(grepl(";", filter_ppt)))]
      filtered_labels <- labels[
        grepl(
          paste0("\\(", paste(filter_ppt, collapse = "|"), "\\)"),
          labels
        )
      ]

      filtered_labels

    })

    observe({
      updateSelectInput(
        session,
        "projects",
        choices = project_choices(),
        selected = input$projects
      )
    })




    # Check if the static HTML file exists
    observe({
      req(state$mpas)
      static_file_path <- file.path(reportpath, make.names(paste0(state$mpas,".html")))
      if (file.exists(static_file_path)) {
        # Show a link to the existing file
        output$report_button_ui <- renderUI({
          tags$a(
            href = paste0("htmlfiles/",make.names(paste0(state$mpas, ".html"))),
            target = "_blank",
            class = "btn btn-primary",
            "Report"
          )
        })
      } else {
        # Show a button to generate a new report
        output$report_button_ui <- renderUI({
          actionButton("report", "Report", class = "btn btn-primary")
        })
      }
    })

    # Logic to generate the report
    observeEvent(input$report, {
      req(state$mpas)
      if (!(state$mpas %in% unique(pillar_ecol_df$region))) {
        # Define the Rmd file to render
        rmd_file <- system.file("data", "report.Rmd", package = "MarConsNetApp")
      } else {
        rmd_file <- system.file("data", "network_report.Rmd", package = "MarConsNetApp")
      }
      if (file.exists(rmd_file)) {
        params <- list(
          mpas = state$mpas
        )
        output_dir <- file.path(dirname(STORE),"data", "reports")
        output_file <- file.path(paste0(output_dir,"/", make.names(paste0(names=state$mpas, ".html"))))
        render(rmd_file, output_file = output_file, output_format = "html_document", params = params, envir = new.env())
        output$report_ui <- renderUI({
          tags$iframe(src = "report.html", width = "100%", height = "600px")
        })

      }
    })


    output$objectives <- shiny::renderUI({ # JAIM
      req(input$tabs)
      if (input$tabs == "tab_0" && !(is.null(state$mpas)) && input$tab0_subtabs == "Management Effectiveness") {
        if (!(state$mpas %in% regions$NAME_E)) {
          keepO <- which(objective_tabs$area %in% input$mpas)
          #keepO <- which(objective_tabs$area %in% MPAs$NAME_E)
          if (!(length(keepO) == 0)) {
            textO <- objective_tabs$objectives[keepO]
            #textO <- trimws(substr(gsub("\n", "", textO), 2, nchar(gsub("\n", "", textO))), 'both')
            # Create UI elements for objectives with bar charts

            filtered_odfS <- objective_tabs[objective_tabs$objectives %in% textO, ]

            links <- character(length(filtered_odfS$objectives))
            site_grades <- NULL
            site_color <- NULL

            for (i in seq_along(filtered_odfS$objectives)) {
              links[i] <- sprintf(
                '<a href="#%1$s" style="color: black; font-weight: bold; text-decoration: underline"
            onclick="Shiny.setInputValue(&#39;%1$s&#39;, &#39;%2$s&#39;, {priority: &#39;event&#39;});
                     $(\'#yourTabsetId a[data-value=&quot;tab_%1$s&quot;]\').tab(&#39;show&#39;);">
             %2$s
         </a>',
                filtered_odfS$tab[i],           # tab id
                filtered_odfS$objectives[i]     # objective text
              )

              KEEP <- filtered_pillar_ecol_df()[which(filtered_pillar_ecol_df()$areaID == state$mpas),]
              KEEP$score[which(!(grepl(textO[i], KEEP$objectives, fixed=TRUE)))] <- NA

              KEEP_df <- calc_group_score(df=KEEP,grouping_var = "bin",
                                          score_var = "score",
                                          weight_var = "weight")
              site_grades[i] <- as.character(calc_letter_grade(weighted.mean(x=KEEP_df$score, w=KEEP_df$weight, na.rm=TRUE)))


              if (!(site_grades[i] == "NA")) {
                site_color[i] <- unname(flowerPalette[which(names(flowerPalette) == site_grades[i])])
              } else {
                site_color[i] <- "#EDEDED"
              }
            }

            dt_data <- data.frame(
              Objective = links,
              Grade=site_grades,
              stringsAsFactors = FALSE
            )

            # Render header + DT table
            tagList(
              h3("Site Conservation Objectives"),  # 🔴 header
              DT::datatable(
                dt_data,
                escape = FALSE,      # 🔴 render HTML links
                rownames = FALSE,
                options = list(dom = 't', paging = FALSE)
              ) %>%
                DT::formatStyle(
                  columns = names(dt_data),
                  target = "row",
                  backgroundColor = DT::styleEqual(dt_data$Grade, site_color))
            )

          }
        }
      }
    })

    # Update the button label when clicked
    shiny::observeEvent(input$filter_button, {
      rv$button_label <- ifelse(rv$button_label == "See All Project Data", "Filter Project Data", "See All Project Data")
    })


    # Ensure the button is correctly displayed when navigating tabs
    observe({
      output$filter_button_ui <- shiny::renderUI({
        if (is_button_visible()) {
          shiny::actionButton(
            "filter_button",
            label = HTML(paste("<i class='fa fa-sliders'></i> ", rv$button_label)),
            class = "btn btn-filter-custom"
          )
        }
      })
    })

    output$contextButton <- shiny::renderUI({
      req(input$tabs)
      if (input$tabs == "tab_0" && !(is.null(state$mpas))) {
        string <- state$mpas
        keepO <- which(MPAs$NAME_E == string)
        if (!(length(keepO) == 0)) {
          shiny::actionButton(
            inputId = "contextButton",
            label = HTML("<i class='fa fa-info-circle'></i> Area Context"),
            class = "btn btn-context-custom"
          )
        }
      }
    })

    shiny::observeEvent(input$contextButton, {
      string <- state$mpas
      keepC <- which(MPAs$NAME_E == string)
      textC <- Context[MPAs$NAME_E[keepC]]
      textC <- unlist(lapply(textC, function(x) paste(x, "\n\n")))
      shiny::showModal(shiny::modalDialog(
        title = "Marine Protected Area Context",
        shiny::HTML(textC)
      ))
    })

    output$filter_ind_type_ui <- shiny::renderUI({
      choices <- unique(pillar_ecol_df$type)

      shiny::checkboxGroupInput(
        inputId = "filter_ind_type",
        label = "Indicator Type:",
        choices = choices,
        selected = choices
      )

      # selectInput(
      #   inputId = "filter_ind_type",
      #   label = "Indicator Type:",
      #   choices = choices,
      #   selected = choices,  # default selection
      #   multiple = TRUE
      # )
    })

    output$filter_ind_scale_ui <- shiny::renderUI({
      choices <- unique(pillar_ecol_df$scale)
      shiny::checkboxGroupInput(
        inputId = "filter_ind_scale",
        label = "Indicator Scale:",
        choices = choices,
        selected = choices
      )

    })


    filtered_pillar_ecol_df <- reactive({

      # Doesn't react when NULL. Never going to hit this for modal.
      req(input$filter_ind_scale)
      req(input$filter_ind_type)

      filterTypes <- input$filter_ind_type
      filterScales <- input$filter_ind_scale
      filterTypes[filterTypes == ""] <- NA
      filterScales[filterScales == ""] <- NA

      pillar_ecol_df <- old_pillar_ecol_df |>
        filter(type %in% filterTypes &
                 scale %in% filterScales &
                 areaID != "Non_Conservation_Area")

      return(pillar_ecol_df)
    })


    # Dynmaically coding in which actionLink is selected will update the tab
    for (i in 0:max(sort(as.numeric(gsub("\\D", "", c(pillar_ecol_df$tab, APPTABS$tab, objective_tabs$tab)))))) {
      local({
        tab_id <- paste0("tab_", i)
        #print(paste0(tab_id, " IS THE TAB ID"))
        shiny::observeEvent(input[[tab_id]], {
          if (tab_id %in% APPTABS$tab) {
            selected_tab <- unique(APPTABS$tab[which(APPTABS$tab == tab_id)])
          } else if (tab_id %in% pillar_ecol_df$tab) {
            selected_tab <- unique(pillar_ecol_df$tab[which(pillar_ecol_df$tab == tab_id)])

          } else {
            selected_tab <- unique(objective_tabs$tab[which(objective_tabs$tab == tab_id)])
          }
          shiny::updateTabsetPanel(session, "tabs", selected = selected_tab)
        })
      })
    }

    # Dynmaically coding in which actionLink is will paste indicators
    calculated_info <- shiny::reactive({
      req(input$tabs)
      tab_id <- input$tabs
      if (input$tabs %in% c(APPTABS$tab, pillar_ecol_df$tab, objective_tabs$tab)) {
        if (!(input$tabs == "tab_0")) {
          if (input$tabs %in% pillar_ecol_df$tab) {
            objective <- " "
            flower <- pillar_ecol_df$bin[which(pillar_ecol_df$tab == tab_id)]
            area <- gsub("_", " ", gsub("_CO$", "", pillar_ecol_df$areaID[which(pillar_ecol_df$tab == tab_id)]))

          }

          if (!(input$tabs %in% objective_tabs$tab)) {
            objective <- " "
            flower <- APPTABS$flower[which(APPTABS$tab == tab_id)]
            area <- gsub("_", " ", gsub("_CO$", "", APPTABS$place[which(APPTABS$tab == tab_id)]))

            if (!(length(flower) == 0)) {
              if (flower %in% c("Productivity", "Habitat", "Biodiversity")) {
                labels <- Ecological$labels[which(Ecological$grouping == flower)]

                flowerBins <- NULL
                for (i in seq_along(labels)) {
                  flowerBins[[i]] <- which(grepl(labels[i], gsub("\\(|\\)", "", filtered_pillar_ecol_df()$bin), ignore.case = TRUE))
                }

                ki1 <- sort(unique(unlist(flowerBins)))

              } else {
                ki1 <- which(grepl(flower, gsub("\\(|\\)", "", filtered_pillar_ecol_df()$bin), ignore.case = TRUE))
              }
              if (!(state$mpas %in% unique(pillar_ecol_df$region))) {
                #2024/12/31 Issue 7
                #ki2 <- which(tolower(pillar_ecol_df$applicability) %in% tolower(c(gsub(" MPA", "", area), "coastal", "offshore", "all")))
                ki2 <- which(tolower(filtered_pillar_ecol_df()$areaID) == tolower(state$mpas))
              } else {
                ki2 <- which(filtered_pillar_ecol_df()$region %in% state$mpas)
              }

              keepind <- intersect(ki1, ki2)
              keepind <- keepind[!(is.na(filtered_pillar_ecol_df()$indicator[keepind]))]
              keepind <- keepind[filtered_pillar_ecol_df()$indicator[keepind]!="placeholder"]
            } else {
              keepind <- which(filtered_pillar_ecol_df()$tab == tab_id)
            }
          } else {
            keep_name <- names(objective_indicators)[which(names(objective_indicators) == objective_tabs$objectives[which(objective_tabs$tab == input$tabs)])]
            area <- state$mpas

            if (area %in% MPAs$NAME_E) {
              keep1 <- which(grepl(keep_name, filtered_pillar_ecol_df()$objectives, fixed=TRUE)) # NOTE: The ones above can remain pillar_ecol because they won't change regardless
              keep2 <- which(filtered_pillar_ecol_df()$areaID == area)
              keepind <- intersect(keep1,keep2)

            } else {
              keepind <- which(grepl(keep_name, filtered_pillar_ecol_df()$objectives, fixed=TRUE))
            }
          }

          if (input$tabs %in% pillar_ecol_df$tab) {
            keepind <- which(filtered_pillar_ecol_df()$tab == input$tabs)
          }
          binned_ind <- gsub("^[0-9]+\\. ", "", gsub("Indicator [0-9]+: ", "", pillar_ecol_df$indicator[keepind]))
          areaID <- pillar_ecol_df$areaID[keepind]

          if (length(keepind) == 0) {
            return(pillar_ecol_df[0, c(
              "indicator",
              "status_statement",
              "trend_statement",
              "score",
              "source",
              "PPTID",
              "scoring",
              "indicator_rationale"
            )])
          }

          ind_tabs <- shiny::tagList(lapply(seq_along(pillar_ecol_df$indicator[keepind]), function(i) {
            tab_id <- gsub("^[0-9]+\\. ", "", gsub("Indicator [0-9]+: ", "", pillar_ecol_df$tab[keepind][i]))
            shiny::tags$a(
              href = paste0("#", tab_id),
              gsub("^[0-9]+\\. ", "", gsub("Indicator [0-9]+: ", "", pillar_ecol_df$indicator[keepind][i])),
              style = "color: black; font-weight: bold; TEXT-DECORATION: underline",
              onclick = sprintf(
                "Shiny.setInputValue('%s', '%s', {priority: 'event'}); $('#yourTabsetId a[data-value=\"%s\"]').tab('show');",
                tab_id,
                gsub("^[0-9]+\\. ", "", gsub("Indicator [0-9]+: ", "", pillar_ecol_df$indicator[keepind][i])),
                paste0('tab_', tab_id)
              )
            )
          }))

          PPTProjects <- pillar_ecol_df$PPTID[keepind]

          #PPTProjects <- sort(unique(om$project_id[which(grepl(area, om$tags, ignore.case = TRUE) & grepl(flower, om$tags, ignore.case = TRUE))]))
          PPTtitles <- unlist(lapply(PPTProjects, function(x) unique(om$project_title[which(om$project_id == x)])))

          if (exists("flower")) {
            indicator_label <- ifelse(flower %in% c("Biodiversity", "Productivity", "Habitat"),
                                      "Ecosystem Based Management Objective:",
                                      "Indicator Bin:")
            indicator_bin_label <- ifelse(grepl("Indicator", flower, ignore.case = TRUE), "\n\n", "Indicators:")

            binned_indicator_label <- ifelse(grepl("Indicator", flower, ignore.case = TRUE), "\n\n",
                                             paste0(binned_ind, collapse = "<br>"))

          } else {
            indicator_label <- " "
            objective <- keep_name
            flower <- " "
            indicator_bin_label <- " "

          }
          CO_label <- "Objective"


          if (!(length(PPTProjects) == 0) & !all(is.na(PPTProjects)|PPTProjects=="NA")) {
            urls <- paste0("https://dmapps/en/ppt/projects/", PPTProjects, "/view/")
            formatted_urls <- sapply(seq_along(PPTProjects), function(i) {
              paste0('<strong><a href="', urls[i], '" target="_blank">Project ', PPTProjects[i], '</a></strong>')
            })
            formatted_projects <- paste0(formatted_urls, " - ", PPTtitles)

            activityType <- unlist(lapply(PPTProjects, function(x){
              if(is.na(x)|x=="NA"){
                NA
              } else if (grepl(";",x)) {
                lapply(unlist(strsplit(x, ";")), function(y) {
                  y <- trimws(y)
                  if(y=="NA"){
                    NA
                  } else
                    unique(om$activity_type[which(om$project_id == y)])
                }) |>
                  paste(collapse = "; ")
              } else {
                unique(om$activity_type[which(om$project_id == x)])
              }
              unique(om$activity_type[which(om$project_id == x)])
            }))

            if(length(activityType) == 0){
              activityData <- list(formatted_projects)
            } else {
              activityData <- split(formatted_projects, activityType)
            }

            formatted_projects_grouped <- shiny::tagList(lapply(names(activityData), function(activity) {
              shiny::tags$div(
                shiny::tags$h4(activity),  # Activity Type Header
                shiny::tags$ul(lapply(activityData[[activity]], function(proj) {
                  shiny::tags$li(shiny::HTML(proj))  # Each project as a list item
                }))
              )
            }))

            return(list(
              CO_label = CO_label,
              objective = objective,
              area = area,
              areaID=areaID,
              indicator_label = indicator_label,
              flower = flower,
              indicator_bin_label = indicator_bin_label,
              indicator_names = pillar_ecol_df$indicator[keepind],
              keep = keepind,
              ind_tabs = ind_tabs,
              indicatorStatus = pillar_ecol_df$status_statement[keepind],
              indicatorTrend = pillar_ecol_df$trend_statement[keepind],
              indicatorGrade = pillar_ecol_df$score[keepind],
              Source=pillar_ecol_df$source[keepind],
              indicatorProject = pillar_ecol_df$PPTID[keepind],
              indicatorScore = pillar_ecol_df$scoring[keepind],
              Rationale=pillar_ecol_df$indicator_rationale[keepind]
            ))
          } else {
            return(list(
              CO_label = CO_label,
              objective = objective,
              area = area,
              areaID=areaID,
              indicator_label = indicator_label,
              flower = flower,
              indicator_bin_label = indicator_bin_label,
              indicator_names = pillar_ecol_df$indicator[keepind],
              keep = keepind,
              ind_tabs = ind_tabs,
              indicatorStatus = pillar_ecol_df$status_statement[keepind],
              indicatorTrend = pillar_ecol_df$trend_statement[keepind],
              indicatorGrade = pillar_ecol_df$score[keepind],
              Source=pillar_ecol_df$source[keepind],
              indicatorProject = pillar_ecol_df$PPTID[keepind],
              indicatorScore = pillar_ecol_df$scoring[keepind],
              Rationale=pillar_ecol_df$indicator_rationale[keepind]
            ))
          }
        }
      }

    })


    output$indicatorText <- shiny::renderUI({
      info <- calculated_info()
      req(info)  # Ensure the info is available
      shiny::HTML(
        paste(
          "<p><strong>", info$CO_label, "</strong></p>",
          "<p>", info$objective, "</p>",
          "<p><strong>Area:</strong></p>",
          "<p>", info$area, "</p>",
          "<p><strong>", info$indicator_label, "</strong></p>",
          "<p>", info$flower, "</p>",
          "<p>", paste0(info$formatted_projects, collapse = "<br>"), "</p>",
          "<p><strong><span style='background-color: yellow;'>",
          "Click on each source to see the code to produce that specific indicator",
          "</span></strong></p>"
        )
      )
    })

    dfdt_r <- reactive({
      req(input$tabs)
      info <- calculated_info()
      if (length(info$indicator_names) == 0) {
        ddff <- as.data.frame(
          setNames(
            replicate(9, character(0), simplify = FALSE),
            c(
              "Indicator",
              "Rationale",
              "areaID",
              "Status",
              "Trend",
              "Projects",
              "Source",
              "Score",
              "Method"
            )
          )
        )
        return(ddff)
      }
      req(info)  # Ensure the info is available
      indicatorProject <- as.numeric(info$indicatorProject)
      Projects <- NULL
      for (i in seq_along(indicatorProject)) {
        if (is.na(indicatorProject[i])) {
          Projects[i] <- NA
        } else if (indicatorProject[i] == "project") {
          Projects[i] <- NA
        } else {
          Projects[i] <- paste0(unique(om$project_title[which(om$project_id == as.numeric(indicatorProject[i]))]), " : ", '<a href=\"http://glf-proxy:8018/mar-spa/reports/',indicatorProject[i],'.html" target="_blank" rel="noopener noreferrer" style="color: black; font-weight: bold; TEXT-DECORATION: underline">',indicatorProject[i],'</a>')
        }
      }

      if (!(length(info$indicator_names) == 1 && "<a href=" %in% info$ind_tabs)) {
        # The below line puts the links in the proper format to direct us to the relevant tab when it is clicked on.
        formatted_indicators <- trimws(gsub("\n", "", paste0("<a href=", unlist(strsplit(as.character(info$ind_tabs), "<a href="))[nzchar(unlist(strsplit(as.character(info$ind_tabs), "<a href=")))])), "both")

        good <- which(!(info$areaID %in% regions$NAME_E))

        dfdt <- data.frame(
          Indicator = formatted_indicators[good],
          Rationale=info$Rationale[good],
          areaID=info$areaID[good],
          Status = info$indicatorStatus[good],
          Trend = info$indicatorTrend[good],
          Projects = Projects[good],
          Source=info$Source[good],
          Score=info$indicatorGrade[good],
          Method=info$indicatorScore[good],
          stringsAsFactors = FALSE
        )

        if (any(dfdt$Status == "No features are represented.",na.rm=TRUE)) {
          dfdt$Status[dfdt$Status == "No features are represented."] <- paste0(
            "No features are represented. ** Note: a score will still be assigned if multiple MPAs are 'tied' for the lowest rank. ",
            "For example, if no features are represented but an MPA has a score of 50, this indicates that 50% of MPAs share the 'least represented' status."
          )

        }

      }

      if (input$tabs %in% c(APPTABS$tab, pillar_ecol_df$tab, objective_tabs$tab)) {
        if (!(input$tabs == "tab_0")) {
          if (!(length(info$indicator_names) == 1 && info$indicator_names == "<a href=")) {
            # Assuming dfdt is your data frame, and indicatorGrade corresponds to the grade in 'Status' column

            dfdt$Grade <- sapply(dfdt$Score, calc_letter_grade)
            # NEW 2025/07/07
            if (any(is.na(dfdt$Trend))) {
              dfdt <- dfdt[-(which(is.na(dfdt$Trend))),]
            }

          } else {
            # MODAL
            showModal(modalDialog(
              title = "Indicator Information",
              paste("There were no indicators for this indicator bin in this region."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }

        } else {
          NULL
        }
        return(dfdt)
      } else {
        NULL
      }
    })

    output$DT <- DT::renderDT({
      if (length(dfdt_r()$Indicator) == 0) {
        DT::datatable(dfdt_r())
      } else {
      DT::datatable(dfdt_r(),
                    escape = FALSE,
                    options = list(
                      pageLength = 100,
                      columnDefs = list(
                        list(targets = ncol(dfdt_r()), visible = FALSE)  # Hide the Grade column (last column)
                      )
                    )) %>%
        DT::formatStyle(
          columns = colnames(dfdt_r()),  # Apply styling to all columns
          target = 'row',  # Target the entire row for styling
          backgroundColor = DT::styleEqual(
            names(flowerPalette),  # Map based on letter grades
            flowerPalette[names(flowerPalette)]  # Apply corresponding colors
          )
        )
      }


    })

    observeEvent(input$DT_cell_clicked, {
      info <- input$DT_cell_clicked
      req(info$row, info$col)
      # Only trigger when SOURCE column is clicked
      if (colnames(dfdt_r())[info$col] == "Source") {


        source_clicked <- gsub("<[^>]+>", "", dfdt_r()$Source[info$row])
        indicator_clicked <- gsub("<[^>]+>", "", dfdt_r()$Indicator[info$row])

        k1 <- which(unique_table$indicator == indicator_clicked)
        k2 <- which(unique_table$source == source_clicked)
        keep <- intersect(k1,k2)

        showModal(
          modalDialog(
            title = "Code for Indicator",
            tagList(
              unique_table$plot[[keep]],

              # Hidden textarea used for copying
              tags$textarea(
                id = "code_to_copy",
                unique_table$code[keep],
                style = "position:absolute; left:-9999px;"
              ),

              # Visible formatted code
              tags$pre(htmltools::htmlEscape(unique_table$code[keep]))
            ),
            size = "xl",
            easyClose = TRUE,
            footer = tagList(
              tags$button(
                "Copy",
                class = "btn btn-primary",
                onclick = "
  var el = document.getElementById('code_to_copy');
  el.style.display = 'block';
  el.select();
  document.execCommand('copy');
  el.style.display = 'none';
"
              ),
              modalButton("Close")
            )
          )
        )
      }
    })


    output$objective_flower <- shiny::renderPlot({
      req(input$tabs)
      req(input$mpas)
      if (input$tabs %in% objective_tabs$tab) {
        if (state$mpas %in% MPAs$NAME_E) {
          ind_ped <- pillar_ecol_df[which(pillar_ecol_df$areaID == state$mpas),]
        } else {
          ind_ped <- pillar_ecol_df[which(pillar_ecol_df$areaID %in% MPAs$NAME_E),]
        }
        OB <- names(objective_indicators)[[which(names(objective_indicators) == objective_tabs$objectives[which(objective_tabs$tab == input$tabs)])]]
        ind_ped$score[which(!grepl(OB, ind_ped$objectives, fixed=TRUE))] <- NA
        ind_ped$score[which(!(ind_ped$type %in% input$filter_ind_type))] <- NA
        #browser()

        if (!(all(is.na(unique(ind_ped$indicator)))) | !(length(ind_ped$indicator) == 0)) {
          MarConsNetAnalysis::plot_flowerplot(ind_ped,
                                              grouping = "objective",
                                              labels = "bin",
                                              score = "score",
                                              max_score=100,
                                              min_score=0,
                                              title=" ")

        } else {
          NULL
        }
      }

    })

    output$conditional_ind_Flower <- shiny::renderUI({
      req(state$mpas)
      req(input$tabs)
      if (input$tabs %in% objective_tabs$tab) {
        plotOutput("objective_flower")
      } else {
        NULL
      }
    })



    output$DT_ui <- shiny::renderUI({
      req(input$tabs)
      if (!(input$tabs == "tab_0")) {
        if (input$tabs %in% c(APPTABS$tab, pillar_ecol_df$tab, objective_tabs$tab)) {
          DT::dataTableOutput("DT")
        } else {
          NULL
        }
      } else {
        NULL
      }
    })

    observe({
      req(input$tabs)
      if (input$tabs == "tab_0") {
        shinyjs::show("mapContainer")
      } else {
        shinyjs::hide("mapContainer")
      }
    })

    # Create reactive image_files that's accessible to all outputs
    image_files <- reactive({
      req(input$tabs)

      if (input$tabs %in% c(APPTABS$tab, pillar_ecol_df$tab)) {
        currentInd <- pillar_ecol_df$indicator[which(pillar_ecol_df$tab == input$tabs)]

        if (!(length(currentInd) == 0)) {

          image_folder <- file.path(dirname(STORE),
                                    "data",
                                    "plots")
          image_files_list <- list.files(image_folder, full.names = TRUE)

          if (length(image_files_list) == 0) {
            image_files_list <- list.files(file.path(dirname(STORE), "data", "plot"), full.names = TRUE)
          }

          k2 <- which(grepl(make.names(pillar_ecol_df$indicator[which(pillar_ecol_df$tab == input$tabs)]), image_files_list, ignore.case=TRUE)) # Which ones have the correct indicator name

          if (!(state$mpas %in% regions$NAME_E)) {
            k1 <- which(grepl(make.names(state$mpas), image_files_list, ignore.case=TRUE)) # Which are from the correct area
          } else {
            k1 <- which(grepl(make.names(pillar_ecol_df$areaID[which(pillar_ecol_df$tab == input$tabs)]), image_files_list))
          }

          KEEP <- intersect(k1, k2)
          image_files_list <- image_files_list[KEEP]

          if (length(image_files_list) > 1) {
            if (!(grepl("Inside", pillar_ecol_df$indicator[which(pillar_ecol_df$tab == input$tabs)], ignore.case=TRUE))) {
              image_files_list <- image_files_list[-which(grepl("Inside", image_files_list))]
            }
          }

          return(image_files_list)
        }
      }

      return(NULL)
    })

    output$conditionalPlot <- shiny::renderUI({
      req(input$tabs)
      req(state$mpas)
      if (input$tabs == "tab_0") {
        # shiny::div(
        #   style = "display: block;",
        #   leafletOutput("map")
        # )
      } else if (input$tabs %in% c(APPTABS$tab, pillar_ecol_df$tab)) {
        files <- image_files()  # Call the reactive

        if (!(is.null(files)) && length(files) > 0) {
          # Create outputs for each image
          lapply(seq_along(files), function(i) {
            output[[paste0("image_display_", i)]] <<- renderImage({
              list(
                src = normalizePath(files[i], winslash = "/"),
                contentType = "image/jpeg",
                width = "100%",
                height = "auto"
              )
            }, deleteFile = FALSE)
          })

          # Return the UI wrapper
          shiny::div(
            class = "plot-container",
            style = "width: 100%; overflow-x: auto; overflow-y: auto; max-height: 100%;",
            shiny::uiOutput("indicatorPlot")
          )
        }
      } else {
        NULL
      }
    })

    output$whaleDisclaimer <- shiny::renderUI({
      req(input$tabs)
      currentInd <- pillar_ecol_df$indicator[which(pillar_ecol_df$tab == input$tabs)]
      if (length(currentInd) > 0 && currentInd == "presence of species that \"use\" productivity, e.g. whales") {
        # Show the modal dialog with a Close button
        shiny::showModal(
          shiny::modalDialog(
            title = "Disclaimer",
            tags$ul(
              tags$li("The data may contain some erroneous or duplicate records."),
              tags$li("The certainty of species identification and number of animals is sometimes unknown. Until May 2022, best count could have been interpreted using count ranges specified in the archival field ‘confidence level’. Many sightings could not be identified to species but are listed to the smallest taxonomic group possible. Many sightings were collected on an opportunistic basis and may come from contributors with different levels of experience. Accuracy will vary with visibility, sea state, weather conditions, and interpretation."),
              tags$li("Sighting coordinates most often refer to the location of the observer and not the animal. There are observations from shore, but these should not be interpreted as sightings on land."),
              tags$li("Most sightings have been gathered from vessel-based platforms. The inherent problems with negative or positive reactions by cetaceans to the approach of such platforms have not been factored into the data."),
              tags$li("Effort associated with collection of sightings has not been quantified in this database and cannot be used to estimate true species density or abundance for an area. Effort is not consistent among months, years, and areas. Lack of sightings within a particular area/time does not necessarily represent lack of species present but could reflect lack of or limited effort. Seasonal and distribution information should not be considered definitive."),
              tags$li("Comments originally submitted in French were translated using Google Translate, and so may not be accurate."),
              tags$li("Animal Condition is recorded in WSDB as provided. If sighter’s comments do not indicate the animal is ‘alive’ or ‘dead’ this field will be left blank.")
            ),
            easyClose = TRUE, # Allow the user to close the modal by clicking outside it
            footer = tagList(
              shiny::modalButton("Close") # Adds a Close button to the footer
            )
          )
        )
      }
      NULL
    })

    # modal for map link clicking
    # shiny::observeEvent(input$projects, {
    #   if (length(input$projects) == 1) {
    #     showModal(modalDialog(
    #       title = "Project Investment URL",
    #       paste("To see the investment for the selected project(s), please click on a sampling location on the map to display a popup with a hyperlink to a financial report."),
    #       easyClose = TRUE,
    #       footer = modalButton("Close")
    #     ))
    #   }
    # })

    output$indicatorPlot <- renderUI({
      files <- image_files()  # Call the reactive
      req(files)
      req(length(files) > 0)

      img_outputs <- lapply(seq_along(files), function(i) {
        image_id <- paste0("image_display_", i)
        div(
          class = "image-display-wrapper",
          style = "width: 100%; display: block; margin: 20px 0;",
          imageOutput(image_id, height = "auto", width = "100%")
        )
      })

      do.call(tagList, img_outputs)
    })


    output$conditionalFlower <- shiny::renderUI({
      req(state$mpas)
      req(input$tabs)
      if (input$tabs == "tab_0" & input$tab0_subtabs == "Ecosystem Overview" & input$indicator_mode == 'ebm') { # ROXANNE
        plotOutput("flowerPlot",click="flower_click")
      } else {
        NULL
      }
    })


    output$ebm_objectives <- renderUI({
      req(state$mpas)
      req(input$tabs)
      if (input$tabs == "tab_0" && !(is.null(input$mpas)) && input$tab0_subtabs == "Effectiveness Contributions") {

        emb_targets <- c("Control unintended incidental mortality for all species",
                         "Distribute population component mortality in relation to component biomass",
                         "Minimize unintended introduction and transmission of invasive species",
                         "Control introduction and proliferation of disease/pathogens",
                         "Minimize aquaculture escapes",
                         "Maintain Species Biodiversity",
                         "Maintain Functional Biodiversity",
                         "Maintain Ecosystem Resistance",
                         "Maintain Habitat Diversity",
                         "Keep fishing and other forms of mortality moderate",
                         "Allow sufficient escapement from exploitation for spawning",
                         "Limit disturbing activity in important reproductive areas/seasons",
                         "Control alteration of nutrient concentrations affecting primary production",
                         "Maintain/promote ecosystem structure and functioning",
                         "Habitat required for all species, particularly priority species, is maintained and protected",
                         "Fish habitat that has been degraded is restored",
                         "Pollution is prevented and reduced"
        )

        tagList(
          h3("EBM Objectives"),
          DT::datatable(
            data.frame(Objective = emb_targets),
            rownames = FALSE,
            options = list(
              pageLength = 10,
              autoWidth = TRUE
            )
          )
        )
      }

    })

    output$gbf_objectives <- renderUI({
      req(state$mpas)
      req(input$tabs)

      if (input$tabs == "tab_0" && !(is.null(state$mpas)) && input$tab0_subtabs == "Effectiveness Contributions") {
        gbf_targets <- c(
          "Plan and Manage all Areas To Reduce Biodiversity Loss",
          "Restore 30% of all Degraded Ecosystems",
          "Conserve 30% of Land, Waters, and Seas",
          "Ensure Sustainable Use of Wild Species",
          "Reduce Pollution Risks to Biodiversity",
          "Prevent and Mitigate Invasive Alien Species",
          "Enhance Biodiversity in Agriculture, Aquaculture, and Forestry",
          "Increase the Area of Protected Areas and Other Effective Area-Based Conservation Measures",
          "Integrate Biodiversity into Decision-Making",
          "Ensure Equitable Sharing of Benefits from Genetic Resources",
          "Increase Financial Resources for Biodiversity",
          "Enhance Capacity for Biodiversity Implementation",
          "Ensure Access to and Use of Biodiversity Data",
          "Ensure Gender Equality in Biodiversity Decision-Making",
          "Ensure Participation of Indigenous Peoples and Local Communities",
          "Ensure Full, Equitable, Inclusive, and Gender-Responsive Representation",
          "Ensure Access to Justice and Information Related to Biodiversity",
          "Ensure Recognition of Rights of Indigenous Peoples and Local Communities",
          "Ensure Recognition of Traditional Knowledge",
          "Ensure Recognition of Rights over Genetic Resources",
          "Ensure Recognition of Rights over Digital Sequence Information",
          "Ensure Recognition of Rights over Traditional Knowledge Associated with Genetic Resources",
          "Ensure Recognition of Rights over Traditional Knowledge Associated with Digital Sequence Information"
        )

        tagList(
          h3("Global Biodiversity Targets"),  # This adds the title above the table
          DT::datatable(
            data.frame(Target = gbf_targets),
            rownames = FALSE,
            options = list(
              pageLength = 10,
              autoWidth = TRUE
            )
          )
        )
      }
    })

    output$ebm_objectives <- renderUI({
      req(state$mpas)
      req(input$tabs)
      if (input$tabs == "tab_0" && !(is.null(input$mpas)) && input$tab0_subtabs == "Effectiveness Contributions") {

        emb_targets <- c("Control unintended incidental mortality for all species",
                         "Distribute population component mortality in relation to component biomass",
                         "Minimize unintended introduction and transmission of invasive species",
                         "Control introduction and proliferation of disease/pathogens",
                         "Minimize aquaculture escapes",
                         "Maintain Species Biodiversity",
                         "Maintain Functional Biodiversity",
                         "Maintain Ecosystem Resistance",
                         "Maintain Habitat Diversity",
                         "Keep fishing and other forms of mortality moderate",
                         "Allow sufficient escapement from exploitation for spawning",
                         "Limit disturbing activity in important reproductive areas/seasons",
                         "Control alteration of nutrient concentrations affecting primary production",
                         "Maintain/promote ecosystem structure and functioning",
                         "Habitat required for all species, particularly priority species, is maintained and protected",
                         "Fish habitat that has been degraded is restored",
                         "Pollution is prevented and reduced"
        )

        tagList(
          h3("EBM Objectives"),
          DT::datatable(
            data.frame(Objective = emb_targets),
            rownames = FALSE,
            options = list(
              pageLength = 10,
              autoWidth = TRUE
            )
          )
        )
      }

    })

    output$network_design <- renderUI({
      req(state$mpas)
      req(input$tabs)
      req(input$region)

      if (input$tabs == "tab_0" && !(is.null(state$mpas)) && input$tab0_subtabs == "Effectiveness Contributions" && input$region == "Maritimes") {

        tagList(
          h3("Network Design Targets"),  # This adds the title above the table
          DT::datatable(
            data.frame(Type=conservation_targets_target$type, Target = conservation_targets_target$plainname),
            rownames = FALSE,
            options = list(
              pageLength = 10,
              autoWidth = TRUE
            )
          )
        )
      }
    })

    output$flowerPlot <- shiny::renderPlot({
      req(state$mpas)
      req(input$tabs)
      if (input$tabs == "tab_0") {
        #browser()
        # JAIM - filtered_pillar_ecol_df()
        NAME <- state$mpas
        ind_ped <- pillar_ecol_df
        flower_df <- ind_ped[which(ind_ped$areaID == NAME),]
        if (NAME %in% regions$NAME_E) {
          # Removing design targets in the plot because they already exist in the site 'indicators'
          # (see issue 219)
          flower_df <- flower_df[which(is.na(flower_df$scale)),]
        } else {
        flower_df$score[which(!(flower_df$type %in% input$filter_ind_type))] <- NA
        }


        MarConsNetAnalysis::plot_flowerplot(flower_df,
                                            grouping = "objective",
                                            labels = "bin",
                                            score = "score",
                                            max_score=100,
                                            min_score=0,
                                            title=NAME
        )
      }


    })

    shiny::observeEvent(input$flower_click, {
      req(state$mpas)
      req(input$flower_click)
      req(input$tabs)
      xscale <- 0.5
      yscale <- 205/2

      x <- (input$flower_click$x-xscale)/xscale
      y <- (input$flower_click$y+50-yscale)/yscale

      ped <- pillar_ecol_df |>
        filter(areaID == state$mpas) |>
        group_by(bin, objective, weight) |>
        reframe(weight = sum(weight,na.rm = TRUE)) |>
        dplyr::arrange(objective,bin) |>
        mutate(angle = (cumsum(weight)-weight/2)/sum(weight)*360)

      clickangle <- 90-atan2(y,x)*180/pi
      if(clickangle<0) clickangle <- 360+clickangle

      if(sqrt(x^2+y^2)>0.75){
        wording <- tolower(ped$objective[which.min(abs(ped$angle-clickangle))])
      } else {
        wording <- tolower(ped$bin[which.min(abs(ped$angle-clickangle))])
      }

      if (wording == "environmental (representativity)") {
        wording <- "environmental representativity"
      }

      string <- state$mpas
      k1 <- which(APPTABS$place == string)
      k2 <- which(tolower(APPTABS$flower) == wording)
      updatedTab <- APPTABS$tab[intersect(k1,k2)]
      shiny::updateTabsetPanel(session, "tabs", selected=updatedTab)
    })

    output$networkObjectiveText <- shiny::renderUI({
      req(input$tabs)

      if (input$tabs == "tab_0" && !(is.null(state$mpas)) && "Maritimes" %in% state$region && input$tab0_subtabs == "Management Effectiveness") {
        n_objectives <- trimws(substr(gsub("\n", "", N_Objectives), 2, nchar(gsub("\n", "", N_Objectives))), 'both')
        filtered_odf <- objective_tabs[which(objective_tabs$objectives %in% n_objectives),]

        links <- character(length(filtered_odf$objectives))
        grades <- NULL
        grade_colors <- NULL

        for (i in seq_along(filtered_odf$objectives)) {
          #message(i)
          links[i] <- sprintf(
            '<a href="#%1$s" style="color: black; font-weight: bold; text-decoration: underline"
            onclick="Shiny.setInputValue(&#39;%1$s&#39;, &#39;%2$s&#39;, {priority: &#39;event&#39;});
                     $(\'#yourTabsetId a[data-value=&quot;tab_%1$s&quot;]\').tab(&#39;show&#39;);">
             %2$s
         </a>',
            filtered_odf$tab[i],
            gsub("\n", "<br>", N_Objectives[i])
          )

          # GRADES
          if (state$mpas %in% MPAs$NAME_E) {
            KEEP <- filtered_pillar_ecol_df()[which(filtered_pillar_ecol_df()$areaID == state$mpas),]
          } else {
            KEEP <- filtered_pillar_ecol_df()[which(filtered_pillar_ecol_df()$areaID %in% MPAs$NAME_E),]
          }
          KEEP$score[which(!(grepl(n_objectives[i], KEEP$objectives, fixed=TRUE)))] <- NA

          KEEP_df <- calc_group_score(df=KEEP,grouping_var = "bin",
                                      score_var = "score",
                                      weight_var = "weight")
          grades[i] <- as.character(calc_letter_grade(weighted.mean(x=KEEP_df$score, w=KEEP_df$weight, na.rm=TRUE)))
          if (!(all(is.na(KEEP$score)))) {
            grade_colors[i] <- unname(flowerPalette[which(names(flowerPalette) == grades[i])])
          } else {
            grade_colors[i] <- "#EDEDED"
          }
        }



        dt_data <- data.frame(
          Objective = links,
          Grade = grades,
          stringsAsFactors = FALSE
        )
        tagList(
          h3("Network Conservation Objectives"),
          DT::datatable(
            dt_data,
            escape = FALSE,
            rownames = FALSE,
            options = list(dom = 't', paging = FALSE)
          ) %>%
            DT::formatStyle(
              columns = names(dt_data),
              target = "row",
              backgroundColor = DT::styleEqual(dt_data$Grade, grade_colors)
            )
        )
      }
    })

    output$gohome <- shiny::renderUI({
      req(input$tabs)
      req(state$mpas)
      if (!(input$tabs == "tab_0")) {
        shiny::actionButton(inputId = "gohome", "Go Home")
      }
    })

    shiny::observeEvent(input$gohome, {
      shiny::updateTabsetPanel(session, "tabs", selected = "tab_0")
    })

    output$network <- shiny::renderUI({
      req(input$tabs)
      if (input$tabs == "tab_0" && !(is.null(state$mpas))) {
        string <- "Scotian_Shelf_CO"
        textN <- N_Objectives
        links <- lapply(seq_along(textN), function(i) {
          shiny::actionLink(inputId = objective_tabs$tab[which(objective_tabs$objectives == textN[[i]])], label = textN[[i]])
        })
      }
    })

    # Render the map with selected coordinates

    output$map <- leaflet::renderLeaflet({
      map <- leaflet() %>%
        addTiles()
      if (!(is.null(state$mpas)) && !(state$mpas %in% unique(pillar_ecol_df$region))) {
        selected <- which(MPA_report_card$NAME_E == state$mpas)
        map <- map %>% leaflet::addPolygons(
          data=MPA_report_card[selected,]$geoms,
          fillColor=ifelse(!is.na(MPA_report_card$grade[selected]), flowerPalette[MPA_report_card$grade[selected]], "#EDEDED"),
          opacity=1,
          fillOpacity = 1,
          weight = 1,
          color=ifelse(!is.na(MPA_report_card$grade[selected]), "black", "lightgrey")
        )

      } else if (state$mpas %in% unique(pillar_ecol_df$region)) {
        selected <- which(MPA_report_card$region %in% state$mpas)
        map <- map %>%
          leaflet::addPolygons(data=MPA_report_card$geoms[selected],
                               fillColor = unname(if_else(!is.na(MPA_report_card$grade[selected]), flowerPalette[MPA_report_card$grade[selected]], "#EDEDED")),
                               opacity=1,
                               fillOpacity = 1,
                               weight = 1,
                               color = if_else(!is.na(MPA_report_card$grade[selected]), "black", "lightgrey"),
                               popup = MPA_report_card$NAME_E[selected])
      }

      message(">>> renderLeaflet ran again <<<")

      map

    })


    # ---- Incremental project plotting ----

    # Track plotted projects globally
    plotted_projects <- reactiveVal(character(0))
    last_tab <- reactiveVal(NULL)
    last_mpa <- reactiveVal(NULL)
    # Ensure the map is fully rendered

    observeEvent(list(input$projects, input$tabs, input$mpas), {
      req(input$tabs)

      tab_changed <- !identical(last_tab(), input$tabs)
      mpa_changed <- !identical(last_mpa(), input$mpas)

      last_tab(input$tabs)        # update memory
      last_mpa(input$mpas)

      projects <- input$projects
      if (is.null(projects)) projects <- character(0)

      triggered_by_tab <- input$tab

      if (tab_changed || mpa_changed) {
        old_projects <- character(0)
        new_projects <- projects
        removed_projects <- plotted_projects()
      } else {
        old_projects <- plotted_projects()
        new_projects <- setdiff(projects, old_projects)
        removed_projects <- setdiff(old_projects, projects)
      }

      message("old_projects = ", old_projects)
      message("new_projects = ", new_projects)
      message("removed_projects = ", removed_projects)

      # ---- Remove deselected project layers ----
      if (input$tabs == "tab_0") {
        req(input$map_bounds)
        invalidateLater(1000, session)

        message("ONLY SHOW UP ON TAB_0")
        proxy <- leafletProxy("map")

        if (length(removed_projects) > 0) {
          for (proj in removed_projects) {
            proxy <- proxy %>% clearGroup(proj)
          }
        }

        # ---- Add newly selected project layers ----
        if (length(new_projects) > 0) {
          for (proj in new_projects) {

            proj_id <- sub("^.*\\(([^)]*)\\).*", "\\1", proj)
            proj_short <- sub(" \\(.*", "", proj)

            if (!(rv$button_label == "Filter Project Data") && !(state$mpas %in% "Maritimes")) {
              k1 <- which(all_project_geoms$areaID == state$mpas)
              k2 <- which(all_project_geoms$project_short_title %in% proj_short)
              keep_projects <- intersect(k1, k2)
            } else {
              keep_projects <- which(all_project_geoms$project_short_title %in% proj_short)
            }

            APJ_filtered <- all_project_geoms[keep_projects, ]

            if (!(proj_id == "NA")) {
              if (suppressWarnings(is.na(as.numeric(proj_id)))) {
                k1 <- which(
                  APJ_filtered$PPTID %in%
                    unique(pillar_ecol_df$PPTID[
                      which(pillar_ecol_df$type == proj_id)
                    ])
                )
              } else {
                k1 <- which(APJ_filtered$PPTID == proj_id)
              }
            } else {
              k1 <- which(is.na(APJ_filtered$PPTID))
            }

            k2 <- which(APJ_filtered$project_short_title == proj_short)
            keep <- intersect(k1, k2)
            APJ <- APJ_filtered[keep, ]

            message("proj=", proj)
            message("nrow(APJ)=", nrow(APJ))
            message("point color = ", map_palette$Color[map_palette$Project == proj])

            type <- APJ$type
            popupContent <- mapply(
              function(type_val, proj_id) {
                paste0(
                  type_val,
                  "<br>",
                  "<a href='http://glf-proxy:8018/mar-spa/reports/",
                  proj_id,
                  ".html' target='_blank' rel='noopener noreferrer'>",
                  "View Investment: ",
                  proj_id,
                  "</a>"
                )
              },
              type,
              proj_id,
              SIMPLIFY = FALSE
            ) |> unlist()

            geom <- APJ$geometry

            # --- Add polygons ---
            if (any(st_geometry_type(geom) == "POLYGON")) {
              polygon_keep <- which(st_geometry_type(geom) == "POLYGON")
              sf_polygons <- st_as_sf(APJ[polygon_keep, ], sf_column = "geometry")

              proxy <- proxy %>%
                addPolygons(
                  data = sf_polygons,
                  color = map_palette$Color[which(map_palette$Project == proj)],
                  popup = unique(popupContent[polygon_keep]),
                  weight = 0.5,
                  fillOpacity = 0.3,
                  group = proj
                )
            }

            # --- Add points ---
            point_keep <- which(st_geometry_type(geom) %in% c("POINT", "MULTIPOINT"))
            if (length(point_keep) > 0) {

              APJ_sub <- APJ[point_keep, ]
              multipoints <- APJ_sub %>% filter(st_geometry_type(.) == "MULTIPOINT")
              points <- APJ_sub %>% filter(st_geometry_type(.) == "POINT")
              multipoints_expanded <- st_cast(multipoints, "POINT")
              APJ_points <- bind_rows(points, multipoints_expanded)

              proxy <- proxy %>%
                addCircleMarkers(
                  data = APJ_points,
                  radius = 3,
                  color = map_palette$Color[which(map_palette$Project == proj)],
                  popup = unname(popupContent[point_keep]),
                  group = proj
                )
            }
          }
        }

        # ---- Update legend ----
        if (length(projects) > 0) {
          proxy %>%
            clearControls() %>%
            addLegend(
              "bottomright",
              colors = map_palette$Color[map_palette$Project %in% projects],
              labels = projects,
              opacity = 1,
              layerId = "projectLegend"
            )
        } else {
          proxy %>% clearControls()
        }

        # ---- Save the current state ----
        plotted_projects(projects)
      }
    }, ignoreNULL = FALSE)


    observeEvent(input$tabs, {
      updateQueryString(paste0("?tab=", input$tabs), mode = "push")
      #cat("Switched to:", input$tabs, "at", format(Sys.time(), "%M:%S"), "\n")
    })

    # Set initial tab based on URL
    observe({
      query <- parseQueryString(session$clientData$url_search)
      if (!is.null(query[['tab']])) {
        updateTabsetPanel(session, "tabs", selected = query[['tab']])
      }
    })

    # Display current URL (for demonstration)
    output$current_url <- renderText({
      paste0("Current URL: ", session$clientData$url_protocol, "//",
             session$clientData$url_hostname, ":",
             session$clientData$url_port,
             session$clientData$url_pathname,
             session$clientData$url_search)
    })
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}
