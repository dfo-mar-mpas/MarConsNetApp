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
  condition <- paste0('input.tabs === "tab_0"')

## FILTERING FOR

  old_pillar_ecol_df <- pillar_ecol_df
  old_all_project_geoms <- all_project_geoms
  pillar_ecol_df <- pillar_ecol_df[-which(pillar_ecol_df$areaID == "Non_Conservation_Area"),]
  #all_project_geoms <- all_project_geoms[-which(all_project_geoms$areaID == "Non_Conservation_Area"),]

  obj <- list.files(file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","data"), full.names = TRUE)[which(grepl("objectives.xlsx",list.files(file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","data"), full.names = TRUE) ))]
  obj_excel <-read_excel(obj)


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
    tags$style(HTML("
    #mytabs > .tabbable > .nav.nav-tabs { display: none; }
  ")),

    # 🔴 START: wrap only the top half in sidebarLayout
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fluidRow(
          shiny::column(2, actionButton(inputId="about", label="User Guide")),
          shiny::column(2, offset = 1, shiny::uiOutput("contextButton")),
          shiny::column(2, shiny::uiOutput("filter_button_ui"))
        ),
        shiny::uiOutput("conditional_ind_Flower"),
        br(), br(),
        shiny::uiOutput("legendUI"),
        br(),
        shiny::uiOutput("region"),
        shiny::uiOutput("mpas"),
        shiny::uiOutput("projects"),
        shiny::conditionalPanel(
          condition = condition,
          shiny::uiOutput("report_button_ui")
        ),
        uiOutput("report_ui")
      ),
      shiny::mainPanel(
        shiny::uiOutput("indicatorText"),
        shiny::uiOutput("DT_ui"),
        shiny::uiOutput("conditionalPlot"),
        shiny::uiOutput("conditionalIndicatorMap"),
        shiny::uiOutput('mytabs'),
        shiny::uiOutput("whaleDisclaimer")
      )
    ),  # 🔴 END: sidebarLayout (top half with gray sidebar)

    # 🔴 NEW: bottom half uses full width (white background)
    fluidRow(
      column(
        width = 12,
        shiny::fluidRow(
          shiny::column(width=3,
                        br(),
                        style = "border-right: 1px solid #ccc;",
                        shiny::uiOutput('gbf_objectives')
                        ),
          shiny::column(width=3,
                        br(),
                        style = "border-left: 1px solid #ccc;",
                        style = "border-right: 1px solid #ccc;",
                        shiny::uiOutput("ebm_objectives")
          ),
          shiny::column(width=3,
                        br(),
                        style = "border-right: 1px solid #ccc;",
                        shiny::uiOutput("networkObjectiveText")
          ),
          shiny::column(width=3,
                        br(),
                        shiny::uiOutput("objectives")
          )
        ),
        fluidRow(
          shiny::column(width=6, alight='right',
          uiOutput('indicator_mode')
          )
        ),
        fluidRow(
          shiny::column(width=5, align='right',
          shiny::uiOutput("conditionalFlower")),
          shiny::column(width=7,
                        shiny:: uiOutput("ecosystem_table"))
      ),
        shiny::uiOutput("threats_home_table")
        # shiny::fluidRow(
        #   shiny::column(width=6, offset=6, align="right", br(), shiny::uiOutput("flowerType"))
        # )
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
    # Build the top-level tabs
    myTabs <- lapply(c(APPTABS$tab, pillar_ecol_df$tab, objective_tabs$tab), function(tabname) {
      if (tabname == "tab_0") {
        # Only tab_0 gets visible subtabs
        tabPanel(
          "tab_0",
          tabsetPanel(
            id = "tab0_subtabs",
            tabPanel("Management Effectiveness", "This tab considers only the effectiveness indicators (i.e. those that directly inform objectives)"),
            tabPanel("Ecosystem Overview", "This tab considers both effectiveness indicators (those that inform objectives) and well as contextual indicators (those that tell us about the ecological status)"),
            tabPanel("Threats")
          )
        )
      } else {
        # Other tabs just have their content (no subtabs)
        tabPanel(tabname)
      }
    })

    do.call(tabsetPanel, c(myTabs, id = "tabs"))
  })


  output$indicator_mode <- renderUI({
    req(state$mpas)
    req(input$tabs)
    if (input$tabs == "tab_0" & input$tab0_subtabs == "Ecosystem Overview") {
    radioButtons(
      inputId = "indicator_mode",
      label = " ",
      choices = c(
        "EBM Framework" = "ebm",
        "Ecological Themes" = "themes"
      ),
      selected = "ebm",
      inline=TRUE
    )
    }
  })

  output$ecosystem_table <- renderUI({
    req(input$indicator_mode)
    if (input$tabs == "tab_0" & input$tab0_subtabs == "Ecosystem Overview") {
      if (input$indicator_mode == "themes") {
        # FIXME
        #table_theme <- c(rep("Oceanography", 3), rep("Biological", 3), rep("Threats", 3))
        showModal(
          modalDialog(
            title = "Grouping in Progress",
            "We are working on grouping the indicators under Oceanographic Conditions, Biological, and Threats.",
            easyClose = TRUE,       # modal closes if user clicks outside or presses Esc
            footer = modalButton("Close")
          )
        )

      } else {
        # Ecological Overview

      if (state$mpas %in% regions$NAME_E) {
        table_ped <- pillar_ecol_df[which(!(pillar_ecol_df$areaID %in% regions$NAME_E)),]

      } else {
        table_ped <- pillar_ecol_df[which(pillar_ecol_df$areaID == state$mpas),]
      }
      if (any(table_ped$indicator == "placeholder") | any(is.na(table_ped$indicator))) {
        table_ped <- table_ped[-which(table_ped$indicator == 'placeholder' | is.na(table_ped$indicator)),]
      }

      table_ped <- table_ped[,c("bin", "indicator", "source", "score", "weight")]


      ddff <- table_ped %>%
        left_join(
          Ecological %>% dplyr::select(labels, grouping),
          by = c("bin" = "labels")   # bin in table_ped matches labels in Ecological
        ) %>%
        # Add placeholders for readiness, quality, cost
        mutate(
          readiness = NA_real_,
          quality   = NA_real_,
          cost      = NA_real_
        ) %>%
        dplyr::select(grouping, bin, indicator, source, score, readiness, quality, cost) %>%
        arrange(grouping, bin) %>%
        setNames(toupper(names(.)))

      if (state$mpas %in% regions$NAME_E) {

        ddff_unique <- ddff %>%
          # calculate weighted score per BIN x INDICATOR
          rowwise() %>%
          mutate(
            SCORE = weighted.mean(
              x = table_ped$score[table_ped$indicator == INDICATOR & table_ped$bin == BIN],
              w = table_ped$weight[table_ped$indicator == INDICATOR & table_ped$bin == BIN],
              na.rm = TRUE
            )
          ) %>%
          ungroup() %>%
          distinct(GROUPING, BIN, INDICATOR, SOURCE, SCORE, READINESS, QUALITY, COST)

      } else {
        ddff_unique <- ddff
      }



      # Render datatable
      datatable(
        ddff_unique,
        rownames = FALSE,
        extensions = "RowGroup",
        options = list(
          rowGroup   = list(dataSrc = 0),                       # group by first column: grouping
          columnDefs = list(list(visible = FALSE, targets = 0)), # hide grouping column
          pageLength = 100
        )
      ) %>%
        formatRound("SCORE", 2)
      }
    }
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

    }
  })


  output$objectives <- shiny::renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0" && !(is.null(state$mpas)) && input$tab0_subtabs == "Management Effectiveness") {
        keepO <- which(names(Objectives_processed) == state$mpas)
      if (!(length(keepO) == 0)) {
        textO <- Objectives_processed[[keepO]]
        textO <- trimws(substr(gsub("\n", "", textO), 2, nchar(gsub("\n", "", textO))), 'both')
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

          #browser()

          KEEP <- pillar_ecol_df[which(grepl(textO[i], pillar_ecol_df$objectives, fixed=TRUE) & pillar_ecol_df$areaID == state$mpas),]

          weight <- KEEP$weight
          ymax <- weighted.mean(KEEP$score, weight, na.rm=TRUE)
          site_grades[i] <- as.character(calc_letter_grade(ymax))
          if (!(site_grades[i] == "NA")) {
            site_color[i] <- unname(flowerPalette[which(names(flowerPalette) == site_grades[i])])
          } else {
            site_color[i] <- "#EDEDED"
           }
        }

        dt_data <- data.frame(
          Link = links,
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
  for (i in 0:(length(unique(APPTABS$tab))+length(pillar_ecol_df$indicator) + length(objective_tabs$tab))) {
    local({
      tab_id <- paste0("tab_", i)
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
        if (input$tabs %in% odf$tab) {
        objective <- gsub("\n", "", odf$objectives[which(odf$tab == tab_id)])
        flower <- odf$flower_plot[which(odf$tab == tab_id)]
        area <- gsub("_", " ", gsub("_CO$", "", odf$area[which(odf$tab == tab_id)]))

        } else if (input$tabs %in% pillar_ecol_df$tab) {
          objective <- " "
          flower <- pillar_ecol_df$bin[which(pillar_ecol_df$tab == tab_id)]
          area <- gsub("_", " ", gsub("_CO$", "", pillar_ecol_df$areaID[which(pillar_ecol_df$tab == tab_id)]))

        }

        if (!(input$tabs %in% objective_tabs$tab)) {
          objective <- " "
          flower <- APPTABS$flower[which(APPTABS$tab == tab_id)]
          area <- gsub("_", " ", gsub("_CO$", "", APPTABS$place[which(APPTABS$tab == tab_id)]))


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
        } else {
          keep_name <- names(objective_indicators)[which(names(objective_indicators) == objective_tabs$objectives[which(objective_tabs$tab == input$tabs)])]
          area <- state$mpas

          if (area %in% MPAs$NAME_E) {
            keep1 <- which(grepl(keep_name, pillar_ecol_df$objectives))
            keep2 <- which(pillar_ecol_df$areaID == area)
            keepind <- intersect(keep1,keep2)

          } else {
            keepind <- which(grepl(keep_name, pillar_ecol_df$objectives))
          }
        }

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
            indicatorScore = pillar_ecol_df$scoring[keepind]
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

    if (input$tabs %in% c(APPTABS$tab, pillar_ecol_df$tab, objective_tabs$tab)) {
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

  output$objective_flower <- shiny::renderPlot({
    req(input$tabs)
    req(input$mpas)
    if (input$tabs %in% objective_tabs$tab) {
      #browser()
      if (state$mpas %in% MPAs$NAME_E) {
      ind_ped <- pillar_ecol_df[which(pillar_ecol_df$areaID == state$mpas),]
      } else {
        ind_ped <- pillar_ecol_df
      }
      OB <- names(objective_indicators)[[which(names(objective_indicators) == objective_tabs$objectives[which(objective_tabs$tab == input$tabs)])]]
      ind_ped$score[which(!grepl(OB, ind_ped$objectives, fixed=TRUE))] <- NA

      if (!(all(is.na(unique(ind_ped$indicator)))) | !(length(ind_ped$indicator) == 0)) {

        MarConsNetAnalysis::plot_flowerplot(ind_ped,
                                            grouping = "objective",
                                            labels = "bin",
                                            score = "score",
                                            max_score=100,
                                            min_score=0,
                                            title=" "
        )

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

  output$threats_home_table <- shiny::renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0" && input$tab0_subtabs == "Threats") {
    dfdt <- data.frame(Pressure=c("Fishing", "Research", "Vessel", "Cables", "Offshore", "Contaminant Debris", "Cumulative Impacts"), Metrics=NA, Score=NA)
    DT::datatable(dfdt, escape = FALSE, options = list(
      pageLength = 100
    ))
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
    if (input$tabs == "tab_0" & input$tab0_subtabs == "Ecosystem Overview") {
      plotOutput("flowerPlot",click="flower_click")
    } else {
      NULL
    }
  })


  output$ebm_objectives <- renderUI({
    req(state$mpas)
    req(input$tabs)
    if (input$tabs == "tab_0" && !(is.null(input$mpas)) && input$tab0_subtabs == "Management Effectiveness") {

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
        data.frame(Target = emb_targets),
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

    if (input$tabs == "tab_0" && !(is.null(state$mpas)) && input$tab0_subtabs == "Management Effectiveness") {
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
      group_by(bin, objective, weight) |>
      reframe(weight = sum(weight,na.rm = TRUE)) |>
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

    if (input$tabs == "tab_0" && !(is.null(state$mpas)) && "Maritimes" %in% state$region && input$tab0_subtabs == "Management Effectiveness") {
      n_objectives <- trimws(substr(gsub("\n", "", N_Objectives), 2, nchar(gsub("\n", "", N_Objectives))), 'both')
      filtered_odf <- objective_tabs[which(objective_tabs$objectives %in% n_objectives),]

      links <- character(length(filtered_odf$objectives))
      grades <- NULL
      grade_colors <- NULL

      for (i in seq_along(filtered_odf$objectives)) {
        message(i)
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
        KEEP <- pillar_ecol_df[which(grepl(filtered_odf$objectives[i], pillar_ecol_df$objectives, fixed=TRUE) & pillar_ecol_df$areaID == state$mpas),]
        } else {
          k1 <- which(grepl(filtered_odf$objectives[i], pillar_ecol_df$objectives, fixed=TRUE))
          k2 <- which(pillar_ecol_df$areaID %in% MPAs$NAME_E)
          keep <- intersect(k1,k2)
          KEEP <- pillar_ecol_df[keep,]
        }

        # if (!(state$mpas == "Maritimes") && i == 4) {
        #   browser()
        # }
        weight <- KEEP$weight
        ymax <- weighted.mean(KEEP$score, weight, na.rm=TRUE)

        grades[i] <- as.character(calc_letter_grade(ymax))
        if (!(all(is.na(KEEP$score)))) {
        grade_colors[i] <- unname(flowerPalette[which(names(flowerPalette) == grades[i])])
        } else {
          grade_colors[i] <- "#EDEDED"
        }
      }



      dt_data <- data.frame(
        Link = links,
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
