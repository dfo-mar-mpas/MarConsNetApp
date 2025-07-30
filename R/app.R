#' MarConsNetApp Conserved and Protected Area App
#'
#' This function creates a shiny app for the Maritimes region.
#' It addressed the following goals:
#' Site-Level Goals
#' - Report on what scientific work is occurring, and resources
#' allocated for this
#' - Report on how the scientific work being done is contributing to
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
#'     showNotification shinyApp
#' @importFrom shinyjs useShinyjs
#' @importFrom DT renderDT dataTableOutput datatable formatStyle styleEqual
#' @importFrom leaflet leafletOutput renderLeaflet leaflet addTiles addPolygons
#'  addCircleMarkers  addLegend
#' @importFrom MarConsNetAnalysis plot_flowerplot
#' @importFrom shinyBS bsCollapse bsCollapsePanel
#' @importFrom ggplot2 ggplot geom_bar ylim theme_void coord_flip guides
#' @importFrom viridis viridis
#' @importFrom sf st_as_sf st_within st_coordinates st_crs
#' @importFrom dataSPA subsetSPA
#' @importFrom stringr str_extract_all
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange
#'
#' @export
#' @examples
#' \dontrun{
#' app()
#' }

# Define UI

app <- function() {
  if(dir.exists(Sys.getenv("OneDriveCommercial"))){
    onedrive <- file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets")
  } else {
    onedrive <- "MarConsNetTargets"
    if(!exists("APPTABS")){
      tar_load(c("APPTABS","pillar_ecol_df","all_project_geoms","MPA_report_card","MPAs","areas","regions","odf","flowerPalette","indicatorFlower","Objectives_processed","N_Objectives","om","Ecological"),
               store = "MarConsNetTargets/app_targets")
    }
  }

  #regions_js_array <- paste0('[',
  #                           paste0('"', unique(pillar_ecol_df$region), '"', collapse = ','),
  #                           ']')
  #condition <- paste0('!', regions_js_array, '.includes(input.mpas) && input.tabs === "tab_0"')
  condition <- paste0('input.tabs === "tab_0"')


ui <- shiny::fluidPage(
  shinyjs::useShinyjs(),
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      .shiny-notification {
        background-color: yellow;
      }
    "))),
  shiny::titlePanel("Maritimes Conservation Network App"),
  fluidRow(
    shiny::column(2, shiny::uiOutput("gohome"))
    ),
  #Makes the tabs hide
  shiny::tags$style(shiny::HTML("
    .nav-tabs { display: none; }
  ")),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::fluidRow(
      shiny::column(2, actionButton(inputId="about", label="User Guide")),
      shiny::column(2, offset = 1, shiny::uiOutput("contextButton")),
      shiny::column(2, shiny::uiOutput("filter_button_ui"))
      ),
      br(), br(),
      shiny::uiOutput("legendUI"),
      br(),
      shiny::uiOutput("region"),
      shiny::uiOutput("mpas"),
      shiny::uiOutput("projects"),
      shiny::conditionalPanel(
        condition = condition,
      shiny::uiOutput("report_button_ui")),
      uiOutput("report_ui"),
      br(),
      tags$hr(style = "border-top: 2px solid #000; margin-top: 10px; margin-bottom: 10px;"),
      br(),
      shiny::uiOutput("networkObjectiveText")
    ),
     shiny::mainPanel(
       shiny::uiOutput("indicatorText"),
       shiny::uiOutput("DT_ui"),
       shiny::uiOutput('mytabs'),
       shiny::uiOutput("conditionalPlot"),
       shiny::uiOutput("conditionalIndicatorMap"),
       shiny::uiOutput("whaleDisclaimer"),
       shiny::fluidRow(shiny::column(width=6, align="left",
                                     br(),
                                     shiny::uiOutput("siteObjectiveText"),
                                     shiny::uiOutput("objectives")),
                       shiny::column(width=6, align="right",
                                     br(),
                                     shiny::uiOutput("conditionalFlower"))
                       ),
       shiny::fluidRow(shiny::column(width=6, offset=6, align="right", br(), shiny::uiOutput("flowerType")))

     ) #MAIN
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

  rv <- shiny::reactiveValues(button_label = "See All Project Data")

  is_button_visible <- shiny::reactive({
    req(state$mpas)
    req(input$projects)
    length(state$mpas) > 0 && length(state$projects) > 0 && input$tabs == "tab_0" && !(state$mpas %in% unique(pillar_ecol_df$region))
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
  output$mytabs = shiny::renderUI({
    nTabs = length(APPTABS$flower)+length(pillar_ecol_df$indicator)
    myTabs = lapply(c(APPTABS$tab, pillar_ecol_df$tab), tabPanel)
    do.call(tabsetPanel, c(myTabs, id = "tabs"))
  })

  output$flowerType <- renderUI({
    req(state$mpas)
    req(input$tabs)
    if (input$tabs == "tab_0") {
      if (state$mpas %in% unique(pillar_ecol_df$areaID) || tolower(state$mpas) %in% tolower(areas)) {
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
    showModal(modalDialog(
      title = "User Guide",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      fluidRow(
        column(12,
               p("This app is designed to support the following goals:"),
               tags$ul(
                 tags$li("Site-level Goals"),
                 tags$ul(
                   tags$li("1. Report on what scientific work is occurring, and resources allocated for this"),
                   tags$li("2. Report on how the scientific work being done is contributing to indicators and therefore conservation objectives (CO)"),
                   tags$li("3. Report on the status of the sites, based on existing data")
                 ),
                 tags$li("Network-level Goals"),
                 tags$ul(
                   tags$li("1. How are individual sites contributing to network conservation objectives"),
                   tags$li("2. How are science projects contributing to network conservation objectives"),
                   tags$li("3. Provide scientific data to support status claims")
                 )
               ),
               p("To navigate the app:"),
               tags$ol(
                 tags$li("Choose a Region"),
                 tags$ul(
                   tags$li("Focus on the Scotian Shelf network or select an individual protected area to assess its contribution to broader conservation goals.")
                 ),
                 tags$li("Select a Project"),
                 tags$ul(
                   tags$li("Pick the project(s) you're interested in to view sampling locations."),
                   tags$li("Click on any point to see sample types and financial details.")
                 ),
                 tags$li("Explore the Data (Read Left to Right)"),
                 tags$ul(
                   tags$li("Left Panel: Status of network-level conservation objectives"),
                   tags$li("Center: Site-level objectives, evaluating progress at individual locations."),
                   tags$li("Right Panel: The flower plot with 'Indicator Bins', summarizing key scientific indicators for the selected region. These bins are clickable.")
                 ),
                 tags$li("Drill Down into Indicators"),
                 tags$ul(
                   tags$li("Click an Indicator Bin to see the scientific indicators that inform it."),
                   tags$li("Click an individual indicator to view trends over time and sampling locations on a map.")
                 ),
                 tags$li("Generate Reports"),
                 tags$ul(
                   tags$li("Click 'Report' to create a reproducible summary of your selected data.")
                 )
               ),
               br(),
               p("1. Status"),
               p("Trend Method:"),
               tags$ul(
                 tags$li("The score of an area of the network is calculated in the following way:"),
                 tags$ul(
                   tags$li("When a trend is statistically significant AND matches the desired direction for the indicator, a score of 100 is assigned."),
                   tags$li("If there is no significant change, a score of 50 is assigned."),
                   tags$li("If a trend is statistically significant but going in the opposite direction, a score of 0 is assigned.")
                 )
               ),
               p("Stable Method:"),
               tags$ul(
                 tags$li("If the desired direction of the indicator is stable:"),
                 tags$ul(
                   tags$li("If the trend of the indicator has no significant change, an A is assigned."),
                   tags$li("If there is a significant change, an F is assigned.")
                 )
               ),
               renderImage({
                 # Path to your image outside the 'www' folder
                 list(
                   src = paste0(onedrive, "/data/visual_flow.png"),
                   contentType = "image/png",
                   width = "580px",
                   height = "auto"
                 )
               }, deleteFile = FALSE),
               p("From: ", a("http://127.0.0.1:3803/", href = "http://127.0.0.1:3803/"))
        )
      )
    ))
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
    distinct_rows <- unique(all_project_geoms[c("project_short_title", "PPTID", "source")])
    if (input$tabs == "tab_0") {
      shiny::selectInput("projects", "Select Project(s):", choices=unique(paste0(distinct_rows$project_short_title, " (", ifelse(is.na(distinct_rows$PPTID),distinct_rows$source,distinct_rows$PPTID), ")")), multiple=TRUE, selected=state$projects)
    }
  })

  if(onedrive != "MarConsNetTargets"){
    reporturl <- "/htmlfiles/"
  } else {
    reporturl <- paste0("/",basename(getwd()),"/htmlfiles/")
  }
  shiny::addResourcePath("htmlfiles", file.path(onedrive,"data", "reports"))


  # Check if the static HTML file exists
  observe({
    req(state$mpas)
    static_file_path <- paste0(file.path(onedrive,"data", "reports"), "/", make.names(paste0(state$mpas,".html")))
    if (file.exists(static_file_path)) {
      # Show a link to the existing file
      output$report_button_ui <- renderUI({
        tags$a(
          href = paste0(reporturl,make.names(paste0(state$mpas, ".html"))),
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
        output_dir <- file.path(onedrive,"data", "reports")
        output_file <- file.path(paste0(output_dir,"/", make.names(paste0(names=state$mpas, ".html"))))
        render(rmd_file, output_file = output_file, output_format = "html_document", params = params, envir = new.env())
        output$report_ui <- renderUI({
          tags$iframe(src = "report.html", width = "100%", height = "600px")
        })
      # } else {
      #
      #
      #
      #
      #   showNotification("The required Rmd file does not exist.", type = "error")
      # }
    }
  })

  output$siteObjectiveText <- shiny::renderUI({
    req(input$tabs)
    req(state$mpas)
    if (input$tabs == "tab_0" && !(is.null(state$mpas))) {
      keepO <- which(areas == state$mpas)
      if (!(length(keepO) == 0)) {
        shiny::tags$b("Site Level Conservation Objectives")
      }
    }
  })

  output$objectives <- shiny::renderUI({ #JAIM
    req(input$tabs)
    if (input$tabs == "tab_0" && !(is.null(state$mpas))) {
        keepO <- which(names(Objectives_processed) == state$mpas)
      if (!(length(keepO) == 0)) {
        textO <- Objectives_processed[[keepO]]
        # Create UI elements for objectives with bar charts
        objectiveDivs <- lapply(seq_along(textO), function(i) {
          shiny::tags$div(
            style = "position: relative; height: 100px; width: 400px; margin-bottom: 20px;",

            # Bar chart
            shiny::tags$div(
              plotOutput(paste0("site_bar", i), height = "100px", width = "400px"),
              style = "position: absolute; top: 0; left: 0; z-index: 1; opacity: 0.7;"
            ),

            # Action link for the objective
            shiny::tags$div(
              actionLink(
                inputId = odf$tab[which(odf$objectives == textO[[i]])],
                label = textO[[i]]
              ),
              style = "position: absolute; top: 30px; left: 10px; z-index: 2; font-weight: bold; color: white;"
            )
          )
        })

        # Display objectives directly without collapsing
        return(shiny::tagList(objectiveDivs))
      }
    }

    return(NULL)
  })


  # Update the button label when clicked
  shiny::observeEvent(input$filter_button, {
    rv$button_label <- ifelse(rv$button_label == "See All Project Data", "Filter Project Data", "See All Project Data")
  })

  # Render the action button UI
  output$filter_button_ui <- shiny::renderUI({
    if (is_button_visible()) {
      shiny::actionButton("filter_button", rv$button_label)
    }
  })

  # Ensure the button is correctly displayed when navigating tabs
  observe({
    output$filter_button_ui <- shiny::renderUI({
      if (is_button_visible()) {
        shiny::actionButton("filter_button", rv$button_label)
      }
    })
  })

  output$contextButton <- shiny::renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0" && !(is.null(state$mpas))) {
      string <- state$mpas
      keepO <- which(areas == string)
      if (!(length(keepO) == 0)) {
        shiny::actionButton(inputId="contextButton", label="Context")
      }
    }
  })

  shiny::observeEvent(input$contextButton, {
    string <- state$mpas
    keepC <- which(areas == string)
    textC <- Context[areas[keepC]]
    textC <- unlist(lapply(textC, function(x) paste(x, "\n\n")))
    shiny::showModal(shiny::modalDialog(
      title = "Marine Protected Area Context",
      shiny::HTML(textC)
    ))
  })

  # Dynmaically coding in which actionLink is selected will update the tab
  for (i in 0:(length(unique(APPTABS$tab))+length(pillar_ecol_df$indicator))) {
    local({
      tab_id <- paste0("tab_", i)
      shiny::observeEvent(input[[tab_id]], {
        selected_tab <- unique(APPTABS$tab[which(APPTABS$tab == tab_id)])
        if (length(selected_tab) == 0) {
          selected_tab <- unique(pillar_ecol_df$tab[which(pillar_ecol_df$tab == tab_id)])
        }
        shiny::updateTabsetPanel(session, "tabs", selected = selected_tab)
      })
    })
  }

  # Dynmaically coding in which actionLink is will paste indicators
  calculated_info <- shiny::reactive({
    req(input$tabs)
    tab_id <- input$tabs
    if (input$tabs %in% c(APPTABS$tab, pillar_ecol_df$tab)) {
      if (!(input$tabs == "tab_0")) {
        if (input$tabs %in% odf$tab) {
        objective <- gsub("\n", "", odf$objectives[which(odf$tab == tab_id)])
        flower <- odf$flower_plot[which(odf$tab == tab_id)]
        area <- gsub("_", " ", gsub("_CO$", "", odf$area[which(odf$tab == tab_id)]))

        } else if (input$tabs %in% pillar_ecol_df$tab) {
          objective <- "This flower plot subset is not associated with any network or site level objectives for this location subset."
          flower <- pillar_ecol_df$bin[which(pillar_ecol_df$tab == tab_id)]
          area <- gsub("_", " ", gsub("_CO$", "", pillar_ecol_df$areaID[which(pillar_ecol_df$tab == tab_id)]))

        } else {
          objective <- "This flower plot subset is not associated with any network or site level objectives for this location subset."
          flower <- APPTABS$flower[which(APPTABS$tab == tab_id)]
          area <- gsub("_", " ", gsub("_CO$", "", APPTABS$place[which(APPTABS$tab == tab_id)]))
        }

        if (flower %in% c("Productivity", "Habitat", "Biodiversity")) {
          labels <- Ecological$labels[which(Ecological$grouping == flower)]

          flowerBins <- NULL
          for (i in seq_along(labels)) {
            flowerBins[[i]] <- which(grepl(labels[i], gsub("\\(|\\)", "", pillar_ecol_df$bin), ignore.case = TRUE))
          }

          ki1 <- sort(unique(unlist(flowerBins)))

        } else {
        ki1 <- which(grepl(flower, gsub("\\(|\\)", "", pillar_ecol_df$bin), ignore.case = TRUE))
        }
        if (!(state$mpas %in% unique(pillar_ecol_df$region))) {
          #2024/12/31 Issue 7
         #ki2 <- which(tolower(pillar_ecol_df$applicability) %in% tolower(c(gsub(" MPA", "", area), "coastal", "offshore", "all")))
          ki2 <- which(tolower(pillar_ecol_df$areaID) == tolower(state$mpas))
        } else {
          ki2 <- which(pillar_ecol_df$region %in% state$mpas)
        }

        keepind <- intersect(ki1, ki2)
        keepind <- keepind[!(is.na(pillar_ecol_df$indicator[keepind]))]
        keepind <- keepind[pillar_ecol_df$indicator[keepind]!="placeholder"]

        if (input$tabs %in% pillar_ecol_df$tab) {
          keepind <- which(pillar_ecol_df$tab == input$tabs)
        }
        binned_ind <- gsub("^[0-9]+\\. ", "", gsub("Indicator [0-9]+: ", "", pillar_ecol_df$indicator[keepind]))
        areaID <- pillar_ecol_df$areaID[keepind]

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

        indicator_label <- ifelse(flower %in% c("Biodiversity", "Productivity", "Habitat"),
                                  "Ecosystem Based Management Objective:",
                                  "Indicator Bin:")
        CO_label <- ifelse(area %in% unique(pillar_ecol_df$region),
                           "Network Level Conservation Objective:",
                           "Site Level Conservation Objective:")
        indicator_bin_label <- ifelse(grepl("Indicator", flower, ignore.case = TRUE), "\n\n", "Indicators:")
        binned_indicator_label <- ifelse(grepl("Indicator", flower, ignore.case = TRUE), "\n\n",
                                         paste0(binned_ind, collapse = "<br>"))

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
            indicatorScore = pillar_ecol_df$scoring[keepind]
            #formatted_projects = formatted_projects_grouped
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
            indicatorScore = pillar_ecol_df$scoring[keepind]
            #formatted_projects = "There are no projects for this selection."
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
        "<p>", paste0(info$formatted_projects, collapse = "<br>"), "</p>"
      )
    )
  })

  output$DT <- DT::renderDT({
    req(input$tabs)
    info <- calculated_info()
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
        areaID=info$areaID[good],
        Status = info$indicatorStatus[good],
        Trend = info$indicatorTrend[good],
        Projects = Projects[good],
        Source=info$Source[good],
        Score=info$indicatorGrade[good],
        Method=info$indicatorScore[good],
        stringsAsFactors = FALSE
      )
    }

    if (input$tabs %in% c(APPTABS$tab, pillar_ecol_df$tab)) {
      if (!(input$tabs == "tab_0")) {
        if (!(length(info$indicator_names) == 1 && info$indicator_names == "<a href=")) {
        # Assuming dfdt is your data frame, and indicatorGrade corresponds to the grade in 'Status' column

        dfdt$Grade <- sapply(dfdt$Score, calc_letter_grade)

        # NEW 2025/07/07
        if (any(is.na(dfdt$Trend))) {
        dfdt <- dfdt[-(which(is.na(dfdt$Trend))),]
        }

        DT::datatable(dfdt, escape = FALSE, options = list(
          pageLength = 100,
          columnDefs = list(
            list(targets = ncol(dfdt), visible = FALSE)  # Hide the Grade column (last column)
          )
        )) %>%
          DT::formatStyle(
            columns = colnames(dfdt),  # Apply styling to all columns
            target = 'row',  # Target the entire row for styling
            backgroundColor = DT::styleEqual(
              names(flowerPalette),  # Map based on letter grades
              flowerPalette[names(flowerPalette)]  # Apply corresponding colors
            )
          )

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
    } else {
      NULL
    }
  })


  output$DT_ui <- shiny::renderUI({
    req(input$tabs)
    if (!(input$tabs == "tab_0")) {
      if (input$tabs %in% c(APPTABS$tab, pillar_ecol_df$tab)) {
      DT::dataTableOutput("DT")
      } else {
        NULL
      }
    } else {
      NULL
    }
  })



  output$conditionalPlot <- shiny::renderUI({
    req(input$tabs)
    req(state$mpas)
    if (input$tabs == "tab_0") {
      leaflet::leafletOutput("map")
    } else if (input$tabs %in% c(APPTABS$tab, pillar_ecol_df$tab)) {
        currentInd <- pillar_ecol_df$indicator[which(pillar_ecol_df$tab == input$tabs)]
        if (!(length(currentInd) == 0)) {

          image_folder <- file.path(onedrive,
                                    "data",
                                    "plots")
          image_files <- list.files(image_folder, full.names = TRUE)


          k2 <- which(grepl(make.names(pillar_ecol_df$indicator[which(pillar_ecol_df$tab == input$tabs)]), image_files, ignore.case=TRUE)) # Which ones have the correct indicator name

          if (!(state$mpas %in% regions$NAME_E)) {
            k1 <- which(grepl(make.names(state$mpas), image_files, ignore.case=TRUE)) # Which are from the correct area

          } else {
            k1 <- which(grepl(make.names(pillar_ecol_df$areaID[which(pillar_ecol_df$tab == input$tabs)]), image_files))
          }

          KEEP <- intersect(k1,k2)

          image_files <- image_files[KEEP]

          shiny::uiOutput("indicatorPlot")

          lapply(seq_along(image_files), function(i) {
            output[[paste0("image_display_", i)]] <- renderImage({
              list(
                src = normalizePath(image_files[i], winslash = "/"),
                contentType = "image/jpeg",
                width = "75%"
              )
            }, deleteFile = FALSE)
          })
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
  shiny::observeEvent(input$projects, {
    if (length(input$projects) == 1) {
      showModal(modalDialog(
        title = "Project Investment URL",
        paste("To see the investment for the selected project(s), please click on a sampling location on the map to display a popup with a hyperlink to a financial report."),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }
  })

  output$indicatorPlot <- renderUI({
    img_outputs <- lapply(seq_along(image_files), function(i) {
      image_id <- paste0("image_display_", i)
      div(
        imageOutput(image_id),
        style = "display: inline-block; margin: 10px; width: 15px; height: 10px; overflow: hidden;"
      )
    })
    do.call(tagList, img_outputs)
  })

  output$indicatorLeaflet <- leaflet::renderLeaflet({
    req(input$tabs)
    currentInd <- pillar_ecol_df$indicator[which(pillar_ecol_df$tab == input$tabs)]
    if (!(length(currentInd) == 0)) {
      indy <- odf$objectives[which(odf$tab == input$tabs)]
      if (length(indy) == 0) {
        indy <- pillar_ecol_df$indicator[which(pillar_ecol_df$tab == input$tabs)]
      }
      plot <- pillar_ecol_df$plot[which(pillar_ecol_df$indicator == indy)]
      if (pillar_ecol_df$type[which(pillar_ecol_df$indicator == currentInd)] == "leaflet") {
        plot2 <- eval(parse(text = plot))
      }
    }
  })

  output$conditionalFlower <- shiny::renderUI({
    req(state$mpas)
    req(input$tabs)
    if (input$tabs == "tab_0") {
      plotOutput("flowerPlot",click="flower_click")
    } else {
      NULL
    }

  })

  output$flowerPlot <- shiny::renderPlot({
    req(state$mpas)
    req(input$tabs)
    if (input$tabs == "tab_0") {
        NAME <- state$mpas
        MarConsNetAnalysis::plot_flowerplot(pillar_ecol_df[which(pillar_ecol_df$areaID == NAME),],
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
      dplyr::select(bin, objective, weight) |>
      unique() |>
      arrange(objective,bin) |>
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
    if (input$tabs == "tab_0" && !(is.null(state$mpas)) && "Maritimes" %in% state$region) {
      filtered_odf <- odf[odf$objectives %in% gsub("<br>", "", N_Objectives), ]
      # Create a div for filtered objectives and bar charts
      objectiveDivs <- lapply(seq_along(filtered_odf$objectives), function(i) {
        # Objective
        shiny::tags$div(
          style = "position: relative; height: 100px; width: 400px; margin-bottom: 20px;",

          # Bar chart
          shiny::tags$div(
            plotOutput(paste0("bar", i), height = "100px", width = "400px"),
            style = "position: absolute; top: 0; left: 0; z-index: 1; opacity: 0.7;"
          ),

          # Action link (Objective with new lines handled by HTML)
          shiny::tags$div(
            actionLink(
              inputId = filtered_odf$tab[i],
              label = shiny::HTML(gsub("\n", "<br>", N_Objectives[i]))
            ),
            style = "position: absolute; top: 30px; left: 10px; z-index: 2; font-weight: bold; color: white;"
          )
        )
      })
      shiny::tagList(
        shiny::h3("Maritimes Network Conservation Objectives"),
        do.call(tagList, objectiveDivs)
      )

    }
  })

  shiny::observeEvent(input$tabs, {
    req(input$tabs)
    if (input$tabs == "tab_0") {
      # Ensure filtered_odf is created inside this condition
      filtered_odf <- odf[odf$objectives %in% N_Objectives, ]
      for (fo in seq_along(filtered_odf$objectives)) {
        local({
          id <- fo
          output[[paste0("bar", id)]] <- renderPlot({
            # Ensure bar chart is rendered only when tab_0 is active
            if (input$tabs == "tab_0") {
              req(state$mpas %in% unique(pillar_ecol_df$areaID))
              if (state$mpas == "Maritimes") {
                c1 <- 1:length(pillar_ecol_df$areaID)
              } else {
                c1 <- which(pillar_ecol_df$areaID == state$mpas)
              }

              c2 <- which(tolower(pillar_ecol_df$bin) == tolower(odf$flower_plot[which(odf$objectives == N_Objectives[id])]))
              KEEP <- intersect(c1,c2)
              ymax <- pillar_ecol_df$score[KEEP]
              weight <- pillar_ecol_df$weight[KEEP]
              if (length(ymax) == 0) {
                # This means it's a ecological objective (i.e. biodiversity, productivity, habitat)
                c2 <- which(tolower(pillar_ecol_df$objective) == tolower(odf$flower_plot[which(odf$objectives == N_Objectives[id])]))
                KEEP <- intersect(c1,c2)
                ymax <- pillar_ecol_df$score[KEEP]
                weight <- pillar_ecol_df$weight[KEEP]
              }

              #ymax <- ymax[-which(ymax == 0)]
              ymax <- weighted.mean(ymax, weight, na.rm=TRUE)


              # Create data frame for plotting
              data <- data.frame(
                x = paste0("Objective ", id),
                y = ymax
              )

              clc <- as.character(calc_letter_grade(data$y))
              finalCol <- unname(flowerPalette[which(names(flowerPalette) == clc)])

              if (!(length(finalCol) == 0)) {

              ggplot2::ggplot(data, aes(x = x, y = y)) +  # Use calc_letter_grade to map y values
                ggplot2::geom_bar(stat = "identity", fill=finalCol) +  # No need to specify fill color here, as it's dynamically set above
                ggplot2::ylim(0, 100) +
                ggplot2::theme_void() +
                ggplot2::coord_flip()+
                ggplot2::guides(fill = "none")
              }
            }
          })
        })
      }
    }
  })

  shiny::observeEvent(state$mpas, {
    req(input$tabs)
    req(state$mpas)
    if (input$tabs == "tab_0" & !(state$mpas == "Maritimes")) {
      # Ensure filtered_odf is created inside this condition
      keepO <- state$mpas
      if (!(length(keepO) == 0)) {
      S_Objectives <- Objectives_processed[[keepO]]


      filtered_odfS <- odf[odf$objectives %in% S_Objectives, ]
      for (fo in seq_along(filtered_odfS$objectives)) {
        local({
          id <- fo
          output[[paste0("site_bar", id)]] <- renderPlot({
            # Ensure bar chart is rendered only when tab_0 is active
            if (input$tabs == "tab_0") {
              req(state$mpas %in% c("Maritimes", unique(pillar_ecol_df$areaID)))
              # Ensure ymax is properly filtered and has a single value
              if (state$mpas == "Maritimes") {
                c1 <- 1:length(pillar_ecol_df$areaID)
              } else {
                c1 <- which(pillar_ecol_df$areaID == state$mpas)
              }

              if (tolower(odf$flower_plot[which(odf$objectives == S_Objectives[id])]) == "environmental (representativity)") {
                c2 <- which(tolower(pillar_ecol_df$bin) == "environmental representativity")

              } else {
                c2 <- which(tolower(pillar_ecol_df$bin) == tolower(odf$flower_plot[which(odf$objectives == S_Objectives[id])]))
              }


              KEEP <- intersect(c1,c2)
              ymax <- pillar_ecol_df$score[KEEP]
              weight <- pillar_ecol_df$weight[KEEP]

              # Handling empty or multiple ymax cases
              if (length(ymax) == 0) {
                # This means it's a ecological objective (i.e. biodiversity, productivity, habitat)
                c2 <- which(tolower(pillar_ecol_df$objective) == tolower(odf$flower_plot[which(odf$objectives == N_Objectives[id])]))
                KEEP <- intersect(c1,c2)
                ymax <- pillar_ecol_df$score[KEEP]
                weight <- pillar_ecol_df$weight[KEEP]
              }
              ymax <- weighted.mean(ymax, weight, na.rm=TRUE)
              #ymax <- weighted.mean(ymax, na.rm=TRUE)
              # Create data frame for plotting
              data <- data.frame(
                x = paste0("Objective ", id),
                y = ymax
              )
              clc <- as.character(calc_letter_grade(data$y))
              finalCol <- unname(flowerPalette[which(names(flowerPalette) == clc)])
              #print(finalCol)
              #print(clc)

              if (!(length(finalCol) == 0)) {

                ggplot2::ggplot(data, aes(x = x, y = y)) +  # Use calc_letter_grade to map y values
                  ggplot2::geom_bar(stat = "identity", fill=finalCol) +  # No need to specify fill color here, as it's dynamically set above
                  ggplot2::ylim(0, 100) +
                  ggplot2::theme_void() +
                  ggplot2::coord_flip()+
                  ggplot2::guides(fill = "none")
              }
            }
          })
        })
      }
    } # HERE
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
        shiny::actionLink(inputId = odf$tab[which(odf$objectives == textN[[i]])], label = textN[[i]])
      })
    }
  })

  # Render the map with selected coordinates
  output$map <- leaflet::renderLeaflet({
    req(input$tabs)
    req(state$mpas)
    req(state$region)

    palette <- viridis::viridis(length(input$projects))

    if (input$tabs == "tab_0") {
      if (!(is.null(state$mpas))) {
        map <- leaflet::leaflet() %>%
          leaflet::addTiles()

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

        if (!(is.null(input$projects))) {
          if (!(rv$button_label == "Filter Project Data") && !(state$mpas %in% "Maritimes")) { # We want it filtered
            k1 <- which(all_project_geoms$areaID == state$mpas)
            k2 <- which(all_project_geoms$project_short_title %in% sub("\\s*\\(.*", "", input$projects))
            keep_projects <- intersect(k1,k2)
          } else {
          keep_projects <- which(all_project_geoms$project_short_title %in% sub("\\s*\\(.*", "", input$projects))
          }
          APJ_filtered <- all_project_geoms[keep_projects,]
          projectIds <- sub(".*\\((.*)\\).*", "\\1", input$projects) # The sub is because input$projects is snowCrabSurvey (1093)

          for (i in seq_along(input$projects)) {
            if (!(projectIds[i] == "NA")) {
            k1 <- which(APJ_filtered$PPTID == projectIds[i])
            } else {
              k1 <- which(is.na(APJ_filtered$PPTID))
            }
            k2 <- which(APJ_filtered$project_short_title ==  sub(" \\(.*", "", input$projects[i]))
            keep <- intersect(k1,k2)

            APJ <- APJ_filtered[keep,]

            type <- APJ$type
            popupContent <- mapply(
              function(type_val, proj_id) {
                paste0(
                  type_val,
                  "<br>",  # Line break
                  "<a href='http://glf-proxy:8018/mar-spa/reports/", proj_id, ".html' target='_blank'  rel='noopener noreferrer'>",
                  "View Investment: ", proj_id,
                  "</a>"
                )
              },
              type,  # Your vector of types
              projectIds[i],  # Your vector of project IDs
              SIMPLIFY = FALSE
            ) |>
              unlist()
            geom <- APJ$geometry
            if (any(st_geometry_type(geom) == "POLYGON")) {
              # Remote Sensing
              point_keep <- which(st_geometry_type(geom) == "POINT")
              polygon_keep <- which(st_geometry_type(geom) == "POLYGON")
              sf_polygons <- st_as_sf(APJ[polygon_keep,], sf_column = "geometry")

              map <- map %>%
                leaflet::addPolygons(data=sf_polygons, color=palette[i],
                                     popup=unique(popupContent[polygon_keep]), weight=0.5, fillOpacity = 0.3,)
            } else {
              point_keep <- 1:length(APJ$geometry)

            }
            #browser()

            if (!("sfc_GEOMETRY" %in% class(APJ$geometry[point_keep]))) {
              if (!(length(point_keep) == 0)) {
            latitude <- st_coordinates(APJ$geometry[point_keep])[, "Y"]
            longitude <- st_coordinates(APJ$geometry[point_keep])[, "X"]

            map <- map %>%
              leaflet::addCircleMarkers(longitude, latitude, radius=3, color=palette[i],
                                        popup=unname(popupContent[point_keep]))
              }
            } else {
              if (!(length(point_keep) == 0)) {
              APJ_sub <<- APJ[point_keep, ]
              multipoints <- APJ_sub %>% filter(st_geometry_type(.) == "MULTIPOINT")
              points <- APJ_sub %>% filter(st_geometry_type(.) == "POINT")
              multipoints_expanded <- st_cast(multipoints, "POINT")
              APJ_points <- bind_rows(points, multipoints_expanded)
              map <- map %>%
                addCircleMarkers(data=APJ_points, radius = 3, color=palette[i],
                                 popup=unname(popupContent[point_keep]))
              }
            }
          }

          map <- map %>%
            leaflet::addLegend(
              "bottomright",
              colors = palette,
              labels = input$projects,
              opacity = 1
            )
        }
        map

      }
    }
  })

  observeEvent(input$tabs, {
    updateQueryString(paste0("?tab=", input$tabs), mode = "push")
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
