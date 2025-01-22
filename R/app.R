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
#' @importFrom DT renderDT dataTableOutput datatable
#' @importFrom leaflet leafletOutput renderLeaflet leaflet addTiles addPolygons
#'  addCircleMarkers  addLegend
#' @importFrom MarConsNetAnalysis plot_flowerplot
#' @importFrom shinyBS bsCollapse bsCollapsePanel
#' @importFrom ggplot2 ggplot geom_bar ylim theme_void coord_flip guides
#' @importFrom viridis viridis
#' @importFrom sf st_as_sf st_within st_coordinates
#' @importFrom dataSPA subsetSPA
#' @importFrom stringr str_extract_all
#' @importFrom magrittr %>%
#'
#' @export
#' @examples
#' \dontrun{
#' app()
#' }

# Define UI

app <- function() {
ui <- shiny::fluidPage(
  shinyjs::useShinyjs(),
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      .shiny-notification {
        background-color: yellow;
      }
    "))),
  shiny::titlePanel("Maritimes Conservation Network App"),
  shiny::fluidRow(
    shiny::column(2, shiny::uiOutput("contextButton")),
    shiny::column(2, shiny::uiOutput("filter_button_ui"))
  ),
  shiny::uiOutput("gohome"),
  #Makes the tabs hide
  shiny::tags$style(shiny::HTML("
    .nav-tabs { display: none; }
  ")),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::actionButton(inputId="about", label="About the App"),
      shiny::uiOutput("mpas"),
      shiny::uiOutput("projects"),
      shiny::uiOutput("projectFinancial"),
      #shiny::uiOutput("fundingSource"),
      #shiny::uiOutput("theme"),
      #shiny::uiOutput("functionalGroup"),
      #shiny::uiOutput("section"),
      #shiny::uiOutput("division"),
      shiny::uiOutput("report")
    ),
    shiny::mainPanel(
      shiny::uiOutput("indicatorText"),
      shiny::uiOutput("DT_ui"),
      shiny::uiOutput('mytabs'),
      shiny::uiOutput("conditionalPlot"),
      shiny::uiOutput("conditionalIndicatorMap"),
      shiny::fluidRow(shiny::column(width=6, align="left",
                             shiny::plotOutput("flowerPlot",click="flower_click")),
                      shiny::column(width=6, align="right",
                                    shiny::uiOutput("networkObjectiveText"),
                                    shiny::uiOutput("siteObjectiveText"),
                                    shiny::uiOutput("objectives"))),

    ) #MAIN
  )
)

# Define server logic
server <- function(input, output, session) {

  state <- shiny::reactiveValues(
    mpas = NULL,
    projects = NULL,
    projectFinancial = NULL,
    fundingSource = NULL,
    theme = NULL,
    functionalGroup = NULL,
    section = NULL,
    division = NULL,
    report = NULL
    )

  rv <- shiny::reactiveValues(button_label = "See All Project Data")

  is_button_visible <- shiny::reactive({
    req(input$mpas)
    req(input$projects)
    length(input$mpas) > 0 && length(state$projects) > 0 && input$tabs == "tab_0" && !(input$mpas == "All")
  })

  input_ids <- c("mpas", "projects", "fundingSource", "theme", "functionalGroup", "section", "division", "report", "projectFinancial") # THE SAME AS STATE

  lapply(input_ids, function(id) {
    shiny::observeEvent(input[[id]], {
      state[[id]] <- input[[id]]
    })
  })
  output$mytabs = shiny::renderUI({
    nTabs = length(APPTABS$flower)+length(binned_indicators$indicators) # FIXME
    myTabs = lapply(paste0('tab_', 0: nTabs), tabPanel)
    do.call(tabsetPanel, c(myTabs, id = "tabs"))
  })


  observeEvent(input$about, {
    req(input$about)
    showModal(modalDialog(
      title = "Things to consider when using the app",
      div(
        p("The score of a area of network is calculated in the following way: When a trend is"),
        p("A) s statistically significant AND matches the desired direction for the
indicator, a score of A is assigned."),
        p("B) is not statistically significant but matches the desired direction for
the indicator, a B is assigned"),
        p("C) has no change a C is assigned"),
        p("D) is not statistically significant and going is the opposite direction
of the desired direction a D is assigned"),
        p("E) is statistically significant and going in the opposite direction,
a F is assigned."),
        p(" "),
        p(" ")
      ),
      p("When projects have a lot of points (e.g. RV, Argo, etc.) the latitude and longitudes are rounded to the nearest decimal when plotting on the map"),
      p(" "),
      p("Whale data has a caveat that it’s difficult to make spatial or temporal comparisons in cetacean species presence based on the sightings data due to variable observer effort, which is not quantified in the database."),
      easyClose = TRUE,  # Allow closing modal by clicking outside or using the 'x'
      footer = modalButton("Close")  # Footer button to close the modal
    ))
  })


  output$mpas <- shiny::renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0") {
      shiny::selectInput("mpas","Select Protected/Conserved Area:",choices = c("All", MPAs$NAME_E), selected=state$mpas)
    }
  })

  output$projects <- shiny::renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0") {
      shiny::selectInput("projects", "Select Project(s):", choices=paste0(dataTable$title, " (", dataTable$id,")"), multiple=TRUE, selected=state$projects)
    }
  })

  output$projectFinancial <- shiny::renderUI({
    req(input$tabs)
    req(input$projects)
    if (!(is.null(input$projects)) && input$tabs == "tab_0") {
      pptp <- unlist(stringr::str_extract_all(state$projects, "\\(([^)]+)\\)"))
      pptp <- gsub("[()]", "", pptp)
      urls <- paste0("http://glf-proxy:8018/mar-spa/reports/", pptp, ".html")
      formatted_urls <- sapply(seq_along(pptp), function(i) {
        paste0('<strong><a href="', urls[i], '" target="_blank">View Investment: ', pptp[i], '</a></strong>')
      })

      HTML(paste(formatted_urls, collapse = "<br>"))

    }
  })


  output$fundingSource <- shiny::renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0") {
      shiny::selectInput("fundingSource", "Select Funding Source(s):", choices=unique(om$funding_source_display), multiple=TRUE, selected=state$fundingSource)
    }
  })

  output$theme <- shiny::renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0") {
      shiny::selectInput("theme", "Select Theme(s):", choices=unique(om$theme), multiple=TRUE, selected=state$theme)
    }
  })

  output$functionalGroup <- shiny::renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0") {
      shiny::selectInput("functionalGroup", "Select Functional Group(s):", choices=unique(om$functional_group), multiple=TRUE, selected=state$functionalGroup)
    }
  })

  output$section <- shiny::renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0") {
      shiny::selectInput("section", "Select Section(s):", choices=dataSPA::subsetSPA(om=om, section="return"), multiple=TRUE, selected=state$section)
    }
  })

  output$division <- shiny::renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0") {
      shiny::selectInput("division", "Select Division(s):", choices=dataSPA::subsetSPA(om=om, division="return"), multiple=TRUE, selected=state$division)
    }
  })

  output$report <- shiny::renderUI({
    req(input$tabs)
    req(input$mpas)
    if (input$tabs == "tab_0") {
      if (!(state$mpas == "All")) { # This will change eventually with project reporting
      shiny::actionButton("report", "Create Report")
      }
    }
  })



  output$siteObjectiveText <- shiny::renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0" && !(is.null(state$mpas))) {
      if (grepl("Marine Protected Area", state$mpas)) {
        string <- gsub("Marine Protected Area", "MPA", state$mpas)
        if (grepl("Estuary", state$mpas)) {
          string <- gsub("Estuary ", "", string)
        }
        string <- gsub("\\.", "", string)
      } else if (state$mpas == "Western/Emerald Banks Conservation Area (Restricted Fisheries Zone)") {
        string <- "WEBCA"
      } else {
        string <- state$mpas
      }

      string <- gsub(" ", "_", string)
      keepO <- which(unlist(lapply(areas, function(x) grepl(x, string, ignore.case=TRUE))))
      if (!(length(keepO) == 0)) {
        shiny::tags$b("Site Level Objectives")
      }
    }
  })

  output$objectives <- shiny::renderUI({
    req(input$tabs)

    if (input$tabs == "tab_0" && !(is.null(state$mpas))) {
      # Process your logic as before
      if (grepl("Marine Protected Area", state$mpas)) {
        string <- gsub("Marine Protected Area", "MPA", state$mpas)
        if (grepl("Estuary", state$mpas)) {
          string <- gsub("Estuary ", "", string)
        }
        string <- gsub("\\.", "", string)
      } else if (grepl("Western", state$mpas)) {
        string <- "WEBCA"
      } else {
        string <- state$mpas
      }
      string <- gsub(" ", "_", string)

      # Find matching objectives
      keepO <- which(unlist(lapply(areas, function(x) grepl(x, string, ignore.case = TRUE))))
      if (!(length(keepO) == 0)) {
        textO <- Objectives[[keepO]]
        links <- lapply(seq_along(textO), function(i) {
          shiny::tags$div(
            shiny::actionLink(inputId = odf$link[which(paste0(gsub("\n", "", odf$objectives), "\n") == textO[[i]])],
                              label = textO[[i]]),
            shiny::tags$br(), # Add a line break after each link
            shiny::tags$br()   # Add second line break

          )
        })
        # Combine all the div elements and return
        return(shiny::tagList(links))
      }
    }

    # Return NULL to avoid rendering anything
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
    if (input$tabs == "tab_0" && !(is.null(state$mpas))) {
      if (grepl("Marine Protected Area", state$mpas)) {
        string <- gsub("Marine Protected Area", "MPA", state$mpas)
        if (grepl("Estuary", state$mpas)) {
          string <- gsub("Estuary ", "", string)
        }
        string <- gsub("\\.", "", string)
      } else if (grepl("Western", state$mpas, ignore.case=TRUE)) {
        string <- "WEBCA"
      } else {
        string <- state$mpas
      }
      string <- gsub(" ", "_", string)

      #string <- gsub("\\.", "", gsub(" ", "", state$mpas))
      keepO <- which(unlist(lapply(areas, function(x) grepl(x, string, ignore.case=TRUE))))
      if (!(length(keepO) == 0)) {
        shiny::actionButton(inputId="contextButton", label="Context")
      }
    } # conditions
  })

  shiny::observeEvent(input$contextButton, {

    if (grepl("Marine Protected Area", state$mpas)) {
      string <- gsub("Marine Protected Area", "MPA", state$mpas)
      if (grepl("Estuary", state$mpas)) {
        string <- gsub("Estuary ", "", string)
      }
      string <- gsub("\\.", "", string)
    } else if (grepl("Western", state$mpas, ignore.case=TRUE)) {
      string <- "WEBCA"
    } else {
      string <- state$mpas
    }
    string <- gsub(" ", "_", string)

    keepC <- which(unlist(lapply(areas, function(x) grepl(x, string, ignore.case=TRUE))))
    textC <- Context[[keepC]]
    textC <- unlist(lapply(textC, function(x) paste(x, "\n\n")))
    shiny::showModal(shiny::modalDialog(
      title = "Marine Protected Area Context",
      shiny::HTML(textC)
    ))
  })

  # Dynmaically coding in which actionLink is selected will update the tab
  for (i in 0:(length(unique(APPTABS$tab))+length(binned_indicators$indicators))) {
    local({
      link_id <- paste0("link_", i)
      shiny::observeEvent(input[[link_id]], {
        selected_tab <- unique(APPTABS$tab[which(APPTABS$link == link_id)])
        if (length(selected_tab) == 0) {
          selected_tab <- unique(binned_indicators$tab[which(binned_indicators$link == link_id)])
        }
        shiny::updateTabsetPanel(session, "tabs", selected = selected_tab)
      })
    })
  }

  # Dynmaically coding in which actionLink is will paste indicators
  calculated_info <- shiny::reactive({
    req(input$tabs)
    link_id <- sub("tab", "link", input$tabs)
    if (input$tabs %in% c(APPTABS$tab, binned_indicators$tab)) {
      if (!(input$tabs == "tab_0")) {
          if (input$tabs %in% odf$tab) {
        objective <- gsub("\n", "", odf$objectives[which(odf$link == link_id)])
        flower <- odf$flower_plot[which(odf$link == link_id)]
        area <- gsub("_", " ", gsub("_CO$", "", odf$area[which(odf$link == link_id)]))

        } else if (input$tabs %in% binned_indicators$tab) {
          objective <- "This flower plot subset is not associated with any network or site level objectives for this location subset."
          flower <- binned_indicators$indicator_bin[which(binned_indicators$link == link_id)]
          area <- gsub("_", " ", gsub("_CO$", "", binned_indicators$applicability[which(binned_indicators$link == link_id)]))

        } else {
          objective <- "This flower plot subset is not associated with any network or site level objectives for this location subset."
          flower <- APPTABS$flower[which(APPTABS$link == link_id)]
          area <- gsub("_", " ", gsub("_CO$", "", APPTABS$place[which(APPTABS$link == link_id)]))
        }

        if (flower %in% c("Productivity", "Habitat", "Biodiversity")) {
          labels <- Ecological$labels[which(Ecological$grouping == flower)]

          flowerBins <- NULL
          for (i in seq_along(labels)) {
            flowerBins[[i]] <- which(grepl(labels[i], gsub("\\(|\\)", "", binned_indicators$indicator_bin), ignore.case = TRUE))
          }

          ki1 <- sort(unique(unlist(flowerBins)))

        } else {
        ki1 <- which(grepl(flower, gsub("\\(|\\)", "", binned_indicators$indicator_bin), ignore.case = TRUE))
        }
        if (!(input$mpas == "All")) {
          #2024/12/31 Issue 7
        #ki2 <- which(tolower(binned_indicators$applicability) %in% tolower(c(gsub(" MPA", "", area), "coastal", "offshore", "all")))
          ki2 <- which(tolower(binned_indicators$area) == tolower(NAME_to_tag(names=input$mpas)))

        } else {
          ki2 <- ki1
        }
        keepind <- intersect(ki1, ki2)
        if (input$tabs %in% binned_indicators$tab) {
          keepind <- which(binned_indicators$tab == input$tabs)
        }
        binned_ind <- gsub("^[0-9]+\\. ", "", gsub("Indicator [0-9]+: ", "", binned_indicators$indicators[keepind]))

        ind_links <- shiny::tagList(lapply(seq_along(binned_indicators$indicators[keepind]), function(i) {
          tab_id <- gsub("^[0-9]+\\. ", "", gsub("Indicator [0-9]+: ", "", binned_indicators$link[keepind][i]))
          shiny::tags$a(
            href = paste0("#", tab_id),
            gsub("^[0-9]+\\. ", "", gsub("Indicator [0-9]+: ", "", binned_indicators$indicators[keepind][i])),
            onclick = sprintf(
              "Shiny.setInputValue('%s', '%s', {priority: 'event'}); $('#yourTabsetId a[data-value=\"%s\"]').tab('show');",
              tab_id,
              gsub("^[0-9]+\\. ", "", gsub("Indicator [0-9]+: ", "", binned_indicators$indicators[keepind][i])),
              paste0('tab_', tab_id)
            )
          )
        }))

        PPTProjects <- sort(unique(om$project_id[which(grepl(area, om$tags, ignore.case = TRUE) & grepl(flower, om$tags, ignore.case = TRUE))]))
        PPTtitles <- unlist(lapply(PPTProjects, function(x) unique(om$project_title[which(om$project_id == x)])))

        indicator_label <- ifelse(flower %in% c("Biodiversity", "Productivity", "Habitat"),
                                  "Ecosystem Based Management Objective:",
                                  "Indicator Bin:")
        CO_label <- ifelse(area %in% c("Scotian Shelf"),
                           "Network Level Conservation Objective:",
                           "Site Level Conservation Objective:")
        indicator_bin_label <- ifelse(grepl("Indicator", flower, ignore.case = TRUE), "\n\n", "Indicators:")
        binned_indicator_label <- ifelse(grepl("Indicator", flower, ignore.case = TRUE), "\n\n",
                                         paste0(binned_ind, collapse = "<br>"))

        if (!(length(PPTProjects) == 0)) {
          urls <- paste0("https://dmapps/en/ppt/projects/", PPTProjects, "/view/")
          formatted_urls <- sapply(seq_along(PPTProjects), function(i) {
            paste0('<strong><a href="', urls[i], '" target="_blank">Project ', PPTProjects[i], '</a></strong>')
          })
          formatted_projects <- paste0(formatted_urls, " - ", PPTtitles)

          activityType <- unlist(lapply(PPTProjects, function(x) unique(om$activity_type[which(om$project_id == x)])))
          activityData <- split(formatted_projects, activityType)

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
            indicator_label = indicator_label,
            flower = flower,
            indicator_bin_label = indicator_bin_label,
            ind_links = ind_links
            #formatted_projects = formatted_projects_grouped
          ))
        } else {
          return(list(
            CO_label = CO_label,
            objective = objective,
            area = area,
            indicator_label = indicator_label,
            flower = flower,
            indicator_bin_label = indicator_bin_label,
            ind_links = ind_links
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
        #"<p><strong>", info$indicator_bin_label, "</strong></p>",
        #"<p>", paste0(info$ind_links, collapse = "<br>"), "</p>",
        #"<p><strong>Projects:</strong></p>",
        "<p>", paste0(info$formatted_projects, collapse = "<br>"), "</p>"
      )
    )
  })

  output$DT <- DT::renderDT({
    #browser()
    req(input$tabs)
    info <- calculated_info()
    req(info)  # Ensure the info is available
    if (!(grepl("Indicator", info$flower, ignore.case=TRUE))) {
    #indj <- trimws(unlist(strsplit(as.character(info$ind_link), "\n")), "both")
    indj <- strsplit(as.character(info$ind_links), "<a href=")[[1]]
    indj <- indj[nzchar(indj)]
    indj <- paste0("<a href=", indj)
    indj <- trimws(gsub("\n", "", indj), "both")
    INDY <- gsub(".*>(.*)<.*", "\\1", indj)
    INDY <- gsub("^(\\d+\\.\\s*-?|^#\\.|^\\s*-)|Indicator \\d+:\\s*|Indicators \\d+:\\s*", "", gsub("Indicator [0-9]+ ", "", trimws(gsub("\n", "", INDY))))
    if (any(grepl("&amp;", INDY))) {
      INDY <- gsub("&amp;", "&", INDY)
    }

    indicator_to_plot$indicators <- gsub("\r\n", "", indicator_to_plot$indicators)
    INDY <- gsub("\r", "", INDY)

    # END TEST

    indicatorStatus <- indicator_to_plot$status[which(indicator_to_plot$indicators %in% INDY)]
    indicatorTrend <- indicator_to_plot$trend[which(indicator_to_plot$indicators %in% INDY)]
    indicatorGrade <- indicator_to_plot$status_grade[which(indicator_to_plot$indicators %in% INDY)]
    indicatorProject <- indicator_to_plot$project[which(indicator_to_plot$indicators %in% INDY)]

    } else {
      indj <- gsub("^(\\d+\\.\\s*-?|^#\\.|^\\s*-)|Indicator \\d+:\\s*|Indicators \\d+:\\s*", "", gsub("Indicator [0-9]+ ", "", trimws(gsub("\n", "", info$objective))))
      ki <- which(gsub("^(\\d+\\.\\s*-?|^#\\.|^\\s*-)|Indicator \\d+:\\s*|Indicators \\d+:\\s*", "", gsub("Indicator [0-9]+ ", "", trimws(gsub("\n", "", indicator_to_plot$indicators)))) == indj)

      indicatorStatus <- indicator_to_plot$status[ki]
      indicatorTrend <- indicator_to_plot$trend[ki]
      indicatorGrade <- as.vector(calc_letter_grade(df_unique$ind_status[ki]))

      indicatorGrade <- indicator_to_plot$status_grade[ki]
      indicatorProject <- indicator_to_plot$project[ki]

    }

    indicatorTitle <- NULL
    for (i in seq_along(indicatorProject)) {
      if (indicatorProject[i] == "project") {
        indicatorTitle[[i]] <- "project"
      } else {
        indicatorTitle[[i]] <- paste0(unique(om$project_title[which(om$project_id == as.numeric(indicatorProject[i]))]), " : ", indicatorProject[i])
      }
    }

    dfdt <- data.frame(
      Indicator = indj,
      Status = indicatorStatus,
      Trend = indicatorTrend,
      Projects = unlist(indicatorTitle),
      Grade=indicatorGrade,
      stringsAsFactors = FALSE
    )
    if (input$tabs %in% c(APPTABS$tab, binned_indicators$tab)) {
      if (!(input$tabs == "tab_0")) {
        # Assuming dfdt is your data frame, and indicatorGrade corresponds to the grade in 'Status' column

        DT::datatable(
          dfdt,
          escape = FALSE,
          options = list(pageLength = 100)
        ) %>%
          formatStyle(
            columns = colnames(dfdt), # Apply styling to all columns in each row
            target = 'row',            # Target the entire row
            backgroundColor = styleEqual(
              names(flowerPalette),    # Map based on Grade values
              flowerPalette[names(flowerPalette)] # Apply corresponding colors from the flowerPalette
            )
          )

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
      if (input$tabs %in% c(APPTABS$tab, binned_indicators$tab)) {
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
    } else if (input$tabs %in% c(APPTABS$tab, binned_indicators$tab)) {
        currentInd <- binned_indicators$indicators[which(binned_indicators$tab == input$tabs)]
        if (!(length(currentInd) == 0)) {
        if (indicator_to_plot$type[which(indicator_to_plot$indicators == currentInd)] == "leaflet") {
          leafletOutput("indicatorLeaflet")
        } else {
          shiny::plotOutput("indicatorPlot")
        }
        }

    } else {
      NULL
    }

  })

  output$conditionalIndicatorMap <- shiny::renderUI({
    req(input$tabs)
    req(state$mpas)
    if (input$tabs %in% c(APPTABS$tab, binned_indicators$tab)) {
      currentInd <- binned_indicators$indicators[which(binned_indicators$tab == input$tabs)]
      if (!(length(currentInd) == 0)) {
          leafletOutput("indicatorMap")
      }

    } else {
      NULL
    }

  })


  output$indicatorPlot <- shiny::renderPlot({
    req(input$tabs)
    currentInd <- binned_indicators$indicators[which(binned_indicators$tab == input$tabs)]
    if (!(length(currentInd) == 0)) {
      indy <- odf$objectives[which(odf$objectives == currentInd)]
      if (length(indy) == 0) {
        indy <- binned_indicators$indicators[which(binned_indicators$tab == input$tabs)]
      }
      plot <- indicator_to_plot$plot[which(indicator_to_plot$indicators == indy)]

      if (indicator_to_plot$type[which(indicator_to_plot$indicators == currentInd)] == "plot") {

        if (grepl("dataframe=TRUE", plot)) {
          plot <- gsub("dataframe=TRUE", "dataframe=FALSE", plot)
        } else if (!(grepl("dataframe", plot))) {
          plot <- paste0(substr(plot, 1, nchar(plot) - 1), ", dataframe=FALSE)")
        }

        eval(parse(text = plot))
      }
    }
  })


  output$indicatorMap <- leaflet::renderLeaflet({ #jaim
    req(input$tabs)
    currentInd <- binned_indicators$indicators[which(binned_indicators$tab == input$tabs)]
    if (!(length(currentInd) == 0)) {
      indy <- odf$objectives[which(odf$objectives == currentInd)]
      if (length(indy) == 0) {
        indy <- binned_indicators$indicators[which(binned_indicators$tab == input$tabs)]
      }
      plot <- indicator_to_plot$plot[which(indicator_to_plot$indicators == indy)]
      mapk <- mapData[[which(names(mapData) == plot)]]

      map <- leaflet() %>%
        addTiles() %>%
        addPolygons(data=mapk$area, color="gray") %>%
        addPolygons(data=mapk$outside, color="red") %>%
        addCircleMarkers(lat=mapk$latitude, lng=mapk$longitude, color="black")

      if ("notIncluded" %in% names(mapk)) {
        map <- map %>%
          addControl(
          html = "<h2 style='text-align: center;'>Disclaimer: No sample station found within protected area of outside boundary</h2>",
          position = "topleft"
        )
      }
      map
    } else {
        NULL
      }
  })


  output$indicatorLeaflet <- leaflet::renderLeaflet({
    req(input$tabs)
    currentInd <- binned_indicators$indicators[which(binned_indicators$tab == input$tabs)]

    if (!(length(currentInd) == 0)) {
      indy <- odf$objectives[which(odf$tab == input$tabs)]
      if (length(indy) == 0) {
        indy <- binned_indicators$indicators[which(binned_indicators$tab == input$tabs)]
      }
      plot <- indicator_to_plot$plot[which(indicator_to_plot$indicators == indy)]
      if (indicator_to_plot$type[which(indicator_to_plot$indicators == currentInd)] == "leaflet") {
        plot2 <- eval(parse(text = plot))
      }
    }

  })



  output$flowerPlot <- shiny::renderPlot({
    req(input$mpas)
    req(input$tabs)
    if (input$tabs == "tab_0") {
      if (state$mpas == "All") {
        NAME <- "Scotian Shelf"
        MarConsNetAnalysis::plot_flowerplot(pillar_ecol_df[which(pillar_ecol_df$area_name == NAME),],
                                            grouping = "objective",
                                            labels = "bin",
                                            score = "ind_status",
                                            max_score=100,
                                            min_score=0,
                                            title=NAME)
      } else {
        if (state$mpas %in% pillar_ecol_df$area_name) {
        NAME <- state$mpas
        MarConsNetAnalysis::plot_flowerplot(pillar_ecol_df[which(pillar_ecol_df$area_name == NAME),],
                                            grouping = "objective",
                                            labels = "bin",
                                            score = "ind_status",
                                            max_score=100,
                                            min_score=0,
                                            title=NAME
                                            )
      }
    }
  }


  })

  shiny::observeEvent(input$flower_click, {
    req(input$mpas)
    req(input$flower_click)
    req(input$tabs)
    xscale <- 0.5
    yscale <- 205/2


    # FURTHER EXPLORE SCALE

    x <- (input$flower_click$x-xscale)/xscale
    y <- (input$flower_click$y+50-yscale)/yscale

    clickangle <- 90-atan2(y,x)*180/pi
    if(clickangle<0) clickangle <- 360+clickangle
    pillar_ecol_df$angle <- (cumsum(pillar_ecol_df$weight)-pillar_ecol_df$weight/2)/sum(pillar_ecol_df$weight)*360

    if(sqrt(x^2+y^2)>0.75){
      wording <- tolower(pillar_ecol_df$objective[which.min(abs(pillar_ecol_df$angle-clickangle))])
    } else {
      wording <-tolower(pillar_ecol_df$bin[which.min(abs(pillar_ecol_df$angle-clickangle))])
    }

    if (wording == "environmental (representativity)") {
      wording <- "environmental representativity"
    }
    if (input$mpas == "All") {
      string <- tolower("Scotian_Shelf")
    } else {
      string <- NAME_to_tag(names=input$mpas)
    }
    k1 <- which(APPTABS$place == string)
    k2 <- which(tolower(APPTABS$flower) == wording)
    updatedTab <- APPTABS$tab[intersect(k1,k2)]
    shiny::updateTabsetPanel(session, "tabs", selected=updatedTab)
  })

  output$networkObjectiveText <- shiny::renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0" && !(is.null(state$mpas))) {
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
              inputId = filtered_odf$link[i],
              label = shiny::HTML(gsub("\n", "<br>", N_Objectives[i]))
            ),
            style = "position: absolute; top: 30px; left: 10px; z-index: 2; font-weight: bold; color: white;"
          )
        )
      })

      # Return the collapse panel with objective divs
      shinyBS::bsCollapse(id="networkObjectivesCollapse", open=NULL,
                          shinyBS::bsCollapsePanel("Click to view Network Objectives",
                                 do.call(tagList, objectiveDivs),
                                 style="primary"))
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
              req(input$mpas %in% c("All", unique(pillar_ecol_df$area_name)))
              # Ensure ymax is properly filtered and has a single value


              if (state$mpas == "All") {
                c1 <- 1:length(pillar_ecol_df$area_name)
              } else {
                c1 <- which(pillar_ecol_df$area_name == state$mpas)
              }

              c2 <- which(tolower(pillar_ecol_df$bin) == tolower(odf$flower_plot[which(odf$objectives == N_Objectives[id])]))
              KEEP <- intersect(c1,c2)
              ymax <- pillar_ecol_df$ind_status[KEEP]

              # Handling empty or multiple ymax cases
              if (length(ymax) == 0) {
                # This means it's a ecological objective (i.e. biodiversity, productivity, habitat)
                c2 <- which(tolower(pillar_ecol_df$objective) == tolower(odf$flower_plot[which(odf$objectives == N_Objectives[id])]))
                KEEP <- intersect(c1,c2)
                ymax <- pillar_ecol_df$ind_status[KEEP]

              }


              ymax <- ymax[-which(ymax == 0)]
              ymax <- weighted.mean(ymax, na.rm=TRUE)


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
        shiny::actionLink(inputId = odf$link[which(odf$objectives == textN[[i]])], label = textN[[i]])
      })
    }
  })

  # Render the map with selected coordinates
  output$map <- leaflet::renderLeaflet({
    req(input$tabs)
    req(state$mpas)
    palette <- viridis::viridis(length(input$projects))
    if (input$tabs == "tab_0") {
      if (!(is.null(state$mpas))) {
        coords <- subarea_coords[[state$mpas]]
        map <- leaflet::leaflet() %>%
          leaflet::addTiles()

        if (!(is.null(state$mpas)) && !(state$mpas == "All")) {
          map <- map %>% leaflet::addPolygons(
            data=MPAs[which(MPAs$NAME_E == state$mpas),]$geoms
          )

        } else if (state$mpas == "All") {
          for (c in seq_along(subarea_coords)) {
            coord <- subarea_coords[[c]]
            map <- map %>%
              leaflet::addPolygons(data=MPAs[c,]$geoms, fillColor = "gray", fillOpacity = 0.5, weight = 2, color="black")
          }
        }


        if (!(is.null(input$projects))) {
          #COMMENT
          projectIds <- dataTable$id[which(dataTable$title %in% sub(" .*", "", input$projects))] # The sub is because input$projects is snowCrabSurvey (1093)

          projectPackages <- dataTable$package[which(dataTable$title %in% sub(" .*", "", input$projects))] # The sub is because input$projects is snowCrabSurvey (1093)

          LAT <- NULL
          LON <- NULL

          for (i in seq_along(projectIds)) {
            pd <- projectData[[which(as.numeric(names(projectData)) %in% projectIds[i])]]
            if (!("argoFloats" %in% class(pd))) {
            if ("list" %in% class(pd)) {
            longitude <- pd[[1]]$lon
            latitude <- pd[[1]]$lat
            } else {
              longitude <- pd$longitude
              latitude <- pd$latitude
              type <- pd$type
              if ("geometry" %in% names(pd)) {
                geometry <- pd$geometry
              }
            }
            } else {
              longitude <- pd[['longitude']]
              latitude <- pd[['latitude']]
              type <- "Argo Floats"
            }

            bad <- unique(c(which(is.na(longitude)), which(is.na(latitude))))
            if (length(bad) > 0) {
              latitude <- latitude[-bad]
              longitude <- longitude[-bad]
              type <- type[-bad]
              if ("geometry" %in% names(pd)) {
                geometry <- geometry[-bad]
              }
            }

            if (length(latitude) > 1000) { # issue 21
              latitude <- round(latitude,1)
              longitude <- round(longitude,1)
              coord <- data.frame(latitude, longitude)

              # Get unique pairs
              unique_coords <- unique(coord)
              latitude <- unique_coords$latitude
              longitude <- unique_coords$longitude
            }


            if (!(rv$button_label == "Filter Project Data") && !(state$mpas %in% "All")) { # We want it filtered
              m <- MPAs$geoms[which(MPAs$NAME_E == state$mpas)]
              coords <- cbind(longitude, latitude)
              points_sf <- sf::st_as_sf(data.frame(coords), coords = c("longitude", "latitude"), crs = st_crs(4326))
              points_within <- sf::st_within(points_sf, m, sparse = FALSE)
              within_points <- points_sf[apply(points_within, 1, any), ]
              longitude <- sf::st_coordinates(within_points)[, 1]
              latitude <- sf::st_coordinates(within_points)[, 2]
            }


            LAT[[i]] <- latitude
            LON[[i]] <- longitude
            if (!(length(latitude) == 0)) {
              if ("geometry" %in% names(pd)) {
                if (!(rv$button_label == "Filter Project Data") && !(state$mpas %in% "All")) {
                  st_crs(geometry) <- 4326
                  geometry <- st_transform(geometry, st_crs(m))
                  geometry <- st_make_valid(geometry)
                  overlaps_poly <- st_intersects(geometry, m, sparse = FALSE)
                  map <- map %>%
                    addPolygons(data=geometry[which(overlaps_poly[,1])], popup=type[which(overlaps_poly[,1])], color="yellow", weight=0.5, opacity=0)
                } else {
                  map <- map %>%
                    addPolygons(data=geometry, popup=type, color="yellow", weight=0.5, opacity=0)

                }

              }
              map <- map %>%
                leaflet::addCircleMarkers(longitude, latitude, radius=3, color=palette[i], popup=ifelse("data.frame" %in% class(pd),type, "Type Unknown"))
            }

            if (i == length(projectIds) && any(unlist(lapply(LAT, length))) == 0) {
              shiny::showNotification("Not all of the selected projects exist in this area. Unfilter the data to see where this project takes place.", duration = 5)
            }
          }
          # END COMMENT

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


  observeEvent(input$report, {
    if (!(state$mpas == "All")) {
    # Define the Rmd file to render
    rmd_file <- system.file("data", "report.Rmd", package = "MarConsNetApp")
    params <- list(mpas = input$mpas,
                   coords = subarea_coords[which(names(subarea_coords) == input$mpas)])
    output_dir <- getwd()  # current directory
    output_file <- file.path(output_dir, "report.html")
    render(rmd_file, output_file = output_file, output_format = "html_document", params = params, envir = new.env())
    output$report_ui <- renderUI({
      tags$iframe(src = "report.html", width = "100%", height = "600px")
    })
    }
  })




}

# Run the application
shiny::shinyApp(ui = ui, server = server)
}
