# library(shiny)
# library(leaflet)
# library(dplyr)
# library(sf)
# library(shinyjs)
# library(viridis)
# library(arcpullr)
# library(devtools)
# source("getLatLon.R")
# source("newLine.R")
# source("NAME_to_tag.R")
# install_github("dfo-mar-mpas/MarConsNetAnalysis", ref="main")
# library(MarConsNetAnalysis)
# load_all("../../MarConsNetData/")
# install_github("https://github.com/dfo-mar-odis/TBSpayRates")
# library(TBSpayRates)
# install_github("https://github.com/j-harbin/dataSPA")
# library(dataSPA)
# library(readxl)
# #source("data_app.R")
# library(ggplot2)
# library(shinyBS)
# install_github("https://github.com/Maritimes/Mar.datawrangling", force=TRUE)
# library(Mar.datawrangling)
# library(DT)


# Define UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .shiny-notification {
        background-color: yellow;
      }
    "))),
  titlePanel("Maritimes Conservation Network App"),
  fluidRow(
    column(2, uiOutput("contextButton")),
    column(2, uiOutput("filter_button_ui"))
  ),
  uiOutput("gohome"),
  #Makes the tabs hide
  tags$style(HTML("
    .nav-tabs { display: none; }
  ")),
  sidebarLayout(
    sidebarPanel(
      uiOutput("mpas"),
      uiOutput("projects"),
      uiOutput("fundingSource"),
      uiOutput("theme"),
      uiOutput("functionalGroup"),
      uiOutput("section"),
      uiOutput("division"),
      uiOutput("report")
    ),
    mainPanel(
      uiOutput("indicatorText"),
      uiOutput("DT_ui"),
      uiOutput('mytabs'),
      uiOutput("conditionalPlot"),

      fluidRow(column(width=6, align="left",
                      plotOutput("flowerPlot",click="flower_click")),
               column(width=6, align="right",
                      uiOutput("networkObjectiveText"),
                      uiOutput("siteObjectiveText"),
                      uiOutput("objectives", container=pre))),

    ) #MAIN
  )
)

# Define server logic
server <- function(input, output, session) {

  state <- reactiveValues(
    mpas = NULL,
    projects = NULL,
    fundingSource = NULL,
    theme = NULL,
    functionalGroup = NULL,
    section = NULL,
    division = NULL,
    report = NULL
    )

  rv <- reactiveValues(button_label = "See All Project Data")

  is_button_visible <- reactive({
    req(input$mpas)
    req(input$projects)
    length(input$mpas) > 0 && length(state$projects) > 0 && input$tabs == "tab_0" && !(input$mpas == "All")
  })

  input_ids <- c("mpas", "projects", "fundingSource", "theme", "functionalGroup", "section", "division", "report") # THE SAME AS STATE

  lapply(input_ids, function(id) {
    observeEvent(input[[id]], {
      state[[id]] <- input[[id]]
    })
  })
  output$mytabs = renderUI({
    nTabs = length(APPTABS$flower)+length(binned_indicators$indicators) # FIXME
    myTabs = lapply(paste0('tab_', 0: nTabs), tabPanel)
    do.call(tabsetPanel, c(myTabs, id = "tabs"))
  })

  output$mpas <- renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0") {
      selectInput("mpas","Select Protected/Conserved Area:",choices = c("All", MPAs$NAME_E), selected=state$mpas)
    }
  })

  output$projects <- renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0") {
      selectInput("projects", "Select Project(s):", choices=paste0(dataTable$title, " (", dataTable$id,")"), multiple=TRUE, selected=state$projects)
    }
  })

  output$fundingSource <- renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0") {
      selectInput("fundingSource", "Select Funding Source(s):", choices=unique(om$funding_source_display), multiple=TRUE, selected=state$fundingSource)
    }
  })

  output$theme <- renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0") {
      selectInput("theme", "Select Theme(s):", choices=unique(om$theme), multiple=TRUE, selected=state$theme)
    }
  })

  output$functionalGroup <- renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0") {
      selectInput("functionalGroup", "Select Functional Group(s):", choices=unique(om$functional_group), multiple=TRUE, selected=state$functionalGroup)
    }
  })

  output$section <- renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0") {
      selectInput("section", "Select Section(s):", choices=subsetSPA(om=om, section="return"), multiple=TRUE, selected=state$section)
    }
  })

  output$division <- renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0") {
      selectInput("division", "Select Division(s):", choices=subsetSPA(om=om, division="return"), multiple=TRUE, selected=state$division)
    }
  })

  output$report <- renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0") {
      actionButton("report", "Create Report")
    }
  })

  output$networkObjectiveText <- renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0" && !(is.null(state$mpas))) {
      bsCollapse(id="networkObjectiveText", open=NULL,
                 bsCollapsePanel("Click to see Network Objectives",
                                 uiOutput("network", container=pre),
                                 style="primary"))
    }
  })


  output$siteObjectiveText <- renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0" && !(is.null(state$mpas))) {
      # if (!(state$mpas == "All")) {
      # browser()
      # }
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
        tags$b("Site Level Objectives")
      }
    }
  })

  output$objectives <- renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0" && !(is.null(state$mpas))) {
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
      keepO <- which(unlist(lapply(areas, function(x) grepl(x, string, ignore.case=TRUE))))
      if (!(length(keepO) == 0)) {
        textO <- Objectives[[keepO]]
        links <- lapply(seq_along(textO), function(i) {
          actionLink(inputId = odf$link[which(odf$objectives == textO[[i]])], label = textO[[i]])
        })
      }
    }
  })


  # Update the button label when clicked
  observeEvent(input$filter_button, {
    rv$button_label <- ifelse(rv$button_label == "See All Project Data", "Filter Project Data", "See All Project Data")
  })

  # Render the action button UI
  output$filter_button_ui <- renderUI({
    if (is_button_visible()) {
      actionButton("filter_button", rv$button_label)
    }
  })

  # Ensure the button is correctly displayed when navigating tabs
  observe({
    output$filter_button_ui <- renderUI({
      if (is_button_visible()) {
        actionButton("filter_button", rv$button_label)
      }
    })
  })



  output$contextButton <- renderUI({
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
        actionButton(inputId="contextButton", label="Context")
      }
    } # conditions
  })

  observeEvent(input$contextButton, {

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
    showModal(modalDialog(
      title = "Marine Protected Area Context",
      HTML(textC)
    ))
  })

  # Dynmaically coding in which actionLink is selected will update the tab
  for (i in 0:(length(unique(APPTABS$tab))+length(binned_indicators$indicators))) {
    local({
      link_id <- paste0("link_", i)
      observeEvent(input[[link_id]], {
        selected_tab <- unique(APPTABS$tab[which(APPTABS$link == link_id)])
        if (length(selected_tab) == 0) {
          selected_tab <- unique(binned_indicators$tab[which(binned_indicators$link == link_id)])
        }
        updateTabsetPanel(session, "tabs", selected = selected_tab)
      })
    })
  }

  # Dynmaically coding in which actionLink is will paste indicators
  calculated_info <- reactive({
    req(input$tabs)

    link_id <- sub("tab", "link", input$tabs)
    if (input$tabs %in% odf$tab) {
      if (!(input$tabs == "tab_0")) {
        objective <- gsub("\n", "", odf$objectives[which(odf$link == link_id)])
        flower <- odf$flower_plot[which(odf$link == link_id)]
        #browser()
        area <- gsub("_", " ", gsub("_CO$", "", odf$area[which(odf$link == link_id)]))
        ki1 <- which(grepl(flower, binned_indicators$indicator_bin, ignore.case = TRUE))
        ki2 <- which(tolower(binned_indicators$applicability) %in% tolower(c(gsub(" MPA", "", area), "coastal", "offshore", "all")))
        keepind <- intersect(ki1, ki2)
        binned_ind <- gsub("^[0-9]+\\. ", "", gsub("Indicator [0-9]+: ", "", binned_indicators$indicators[keepind]))

        ind_links <- tagList(lapply(seq_along(binned_indicators$indicators[keepind]), function(i) {
          tab_id <- gsub("^[0-9]+\\. ", "", gsub("Indicator [0-9]+: ", "", binned_indicators$link[keepind][i]))
          tags$a(
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

          return(list(
            CO_label = CO_label,
            objective = objective,
            area = area,
            indicator_label = indicator_label,
            flower = flower,
            indicator_bin_label = indicator_bin_label,
            ind_links = ind_links,
            formatted_projects = formatted_projects
          ))
        } else {
          return(list(
            CO_label = CO_label,
            objective = objective,
            area = area,
            indicator_label = indicator_label,
            flower = flower,
            indicator_bin_label = indicator_bin_label,
            ind_links = ind_links,
            formatted_projects = "There are no projects for this selection."
          ))
        }
      }
    }
  })

  output$indicatorText <- renderUI({
    info <- calculated_info()
    req(info)  # Ensure the info is available

    HTML(
      paste(
        "<p><strong>", info$CO_label, "</strong></p>",
        "<p>", info$objective, "</p>",
        "<p><strong>Area:</strong></p>",
        "<p>", info$area, "</p>",
        "<p><strong>", info$indicator_label, "</strong></p>",
        "<p>", info$flower, "</p>",
        #"<p><strong>", info$indicator_bin_label, "</strong></p>",
        #"<p>", paste0(info$ind_links, collapse = "<br>"), "</p>",
        "<p><strong>Projects:</strong></p>",
        "<p>", paste0(info$formatted_projects, collapse = "<br>"), "</p>"
      )
    )
  })

  output$DT <- renderDT({
    req(input$tabs)
    info <- calculated_info()
    req(info)  # Ensure the info is available
    if (!(grepl("Indicator", info$flower, ignore.case=TRUE))) {
    indj <- trimws(unlist(strsplit(as.character(info$ind_link), "\n")), "both")
    indicatorStatus <- indicator_to_plot$status[which(gsub("^(\\d+\\.\\s*-?|^#\\.|^\\s*-)|Indicator \\d+:\\s*|Indicators \\d+:\\s*", "", gsub("Indicator [0-9]+ ", "", trimws(gsub("\n", "", indicator_to_plot$indicator)))) %in% gsub(".*>(.*)<.*", "\\1", indj))]
    indicatorTrend <- indicator_to_plot$trend[which(gsub("^(\\d+\\.\\s*-?|^#\\.|^\\s*-)|Indicator \\d+:\\s*|Indicators \\d+:\\s*", "", gsub("Indicator [0-9]+ ", "", trimws(gsub("\n", "", indicator_to_plot$indicator)))) %in% gsub(".*>(.*)<.*", "\\1", indj))]
    } else {
      indj <- gsub("^(\\d+\\.\\s*-?|^#\\.|^\\s*-)|Indicator \\d+:\\s*|Indicators \\d+:\\s*", "", gsub("Indicator [0-9]+ ", "", trimws(gsub("\n", "", info$objective))))
      ki <- which(gsub("^(\\d+\\.\\s*-?|^#\\.|^\\s*-)|Indicator \\d+:\\s*|Indicators \\d+:\\s*", "", gsub("Indicator [0-9]+ ", "", trimws(gsub("\n", "", indicator_to_plot$indicator)))) == indj)
      indicatorStatus <- indicator_to_plot$status[ki]
      indicatorTrend <- indicator_to_plot$trend[ki]


    }
    dfdt <- data.frame(
      Indicator = indj,
      Status = indicatorStatus,
      Trend = indicatorTrend,
      stringsAsFactors = FALSE
    )
    if (input$tabs %in% odf$tab) {
      if (!(input$tabs == "tab_0")) {
      datatable(dfdt, escape = FALSE, options=list(pageLength=100))  # Set escape = FALSE to allow HTML rendering
      } else {
        NULL
      }
    } else {
      NULL
    }
  })

  output$DT_ui <- renderUI({
    req(input$tabs)
    if (!(input$tabs == "tab_0")) {
      if (input$tabs %in% odf$tab) {
      dataTableOutput("DT")
      } else {
        NULL
      }
    } else {
      NULL
    }
  })



  output$conditionalPlot <- renderUI({
    req(input$tabs)
    req(state$mpas)
    if (input$tabs == "tab_0") {
      leafletOutput("map")
    } else if (input$tabs %in% c(APPTABS$tab, binned_indicators$tab)) {
      # FIXME: THIS COULD BE BETTER. BUT ISN'T TOO BAD
        currentInd <- binned_indicators$indicators[which(binned_indicators$tab == input$tabs)]
        if (!(length(currentInd) == 0)) {
        if (indicator_to_plot$type[which(indicator_to_plot$indicator == currentInd)] == "leaflet") {
          leafletOutput("indicatorLeaflet")
        } else {
          plotOutput("indicatorPlot")
        }
        }

    } else {
      NULL
    }

  })


  output$indicatorPlot <- renderPlot({ #JAIM
    req(input$tabs)
    currentInd <- binned_indicators$indicators[which(binned_indicators$tab == input$tabs)]

    if (!(length(currentInd) == 0)) {
      indy <- odf$objectives[which(odf$objectives == currentInd)]
      if (length(indy) == 0) {
        indy <- binned_indicators$indicators[which(binned_indicators$tab == input$tabs)]
      }
      plot <- indicator_to_plot$plot[which(indicator_to_plot$indicator == indy)]
      if (indicator_to_plot$type[which(indicator_to_plot$indicator == currentInd)] == "plot") {
        eval(parse(text = plot))
      }
    }

  })

  output$indicatorLeaflet <- renderLeaflet({
    req(input$tabs)
    currentInd <- binned_indicators$indicators[which(binned_indicators$tab == input$tabs)]

    if (!(length(currentInd) == 0)) {
      indy <- odf$objectives[which(odf$tab == input$tabs)]
      if (length(indy) == 0) {
        indy <- binned_indicators$indicators[which(binned_indicators$tab == input$tabs)]
      }
      plot <- indicator_to_plot$plot[which(indicator_to_plot$indicator == indy)]
      if (indicator_to_plot$type[which(indicator_to_plot$indicator == currentInd)] == "leaflet") {
        plot2 <- eval(parse(text = plot))
      }
    }

  })



  output$flowerPlot <- renderPlot({
    req(input$mpas)
    req(input$tabs)
    if (input$tabs == "tab_0") {
      if (state$mpas == "All") {
        NAME <- "Scotian Shelf"
      } else {
        NAME <- state$mpas
      }

      plot_flowerplot(pillar_ecol_df[which(pillar_ecol_df$area_name == NAME),],
                      grouping = "objective",
                      labels = "bin",
                      score = "ind_status")
    }

  })

  observeEvent(input$flower_click, {
    req(input$mpas)
    req(input$flower_click)
    req(input$tabs)

    xscale <- 0.5
    yscale <- 205/2

    x <- (input$flower_click$x-xscale)/xscale
    y <- (input$flower_click$y+50-yscale)/yscale

    clickangle <- 90-atan2(y,x)*180/pi
    if(clickangle<0) clickangle <- 360+clickangle

    if(sqrt(x^2+y^2)>0.75){
      wording <- pillar_ecol_df$objectives[which.min(abs(pillar_ecol_df$angle-clickangle))]
    } else {
      wording <-pillar_ecol_df$bin[which.min(abs(pillar_ecol_df$angle-clickangle))]
    }
    if (input$mpas == "All") {
      string <- tolower("Scotian_Shelf")
    } else {
      string <- NAME_to_tag(names=input$mpas)
    }
    k1 <- which(APPTABS$place == string)
    k2 <- which(APPTABS$flower == wording)
    updatedTab <- APPTABS$tab[intersect(k1,k2)]
    updateTabsetPanel(session, "tabs", selected=updatedTab)
  })


  output$gohome <- renderUI({
    req(input$tabs)
    req(state$mpas)
    if (!(input$tabs == "tab_0")) {
      actionButton(inputId = "gohome", "Go Home")
    }
  })

  observeEvent(input$gohome, {
    updateTabsetPanel(session, "tabs", selected = "tab_0")
  })


  output$network <- renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0" && !(is.null(state$mpas))) {
      string <- "Scotian_Shelf_CO"
      textN <- N_Objectives
      links <- lapply(seq_along(textN), function(i) {
        actionLink(inputId = odf$link[which(odf$objectives == textN[[i]])], label = textN[[i]])
      })
    }
  })

  # Render the map with selected coordinates
  output$map <- renderLeaflet({
    req(input$tabs)
    req(state$mpas)
    palette <- viridis(length(input$projects))
    if (input$tabs == "tab_0") {
      if (!(is.null(state$mpas))) {
        coords <- subarea_coords[[state$mpas]]
        map <- leaflet() %>%
          addTiles()

        if (!(is.null(state$mpas)) && !(state$mpas == "All")) {
          map <- map %>% addPolygons(
            lng = coords$lng,
            lat = coords$lat,
            fillColor = coords$color,
            fillOpacity = 0.5,
            weight = 2
          )
        } else if (state$mpas == "All") {
          for (c in seq_along(subarea_coords)) {
            coord <- subarea_coords[[c]]
            map <- map %>%
              addPolygons(lat = coord$lat, lng = coord$lng, fillColor = coord$color, fillOpacity = 0.5, weight = 2)
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
            if (!(class(pd) == "argoFloats")) {
            longitude <- pd[[1]]$lon
            latitude <- pd[[1]]$lat
            } else {
              longitude <- pd[['longitude']]
              latitude <- pd[['latitude']]
            }
            bad <- unique(c(which(is.na(longitude)), which(is.na(latitude))))
            if (length(bad) > 0) {
              latitude <- latitude[-bad]
              longitude <- longitude[-bad]
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
              points_sf <- st_as_sf(data.frame(coords), coords = c("longitude", "latitude"), crs = st_crs(4326))
              points_within <- st_within(points_sf, m, sparse = FALSE)
              within_points <- points_sf[apply(points_within, 1, any), ]
              longitude <- st_coordinates(within_points)[, 1]
              latitude <- st_coordinates(within_points)[, 2]
            }

            LAT[[i]] <- latitude
            LON[[i]] <- longitude

            if (!(length(latitude) == 0)) {
              map <- map %>%
                addCircleMarkers(longitude, latitude, radius=3, color=palette[i])
            }

            if (i == length(projectIds) && any(unlist(lapply(LAT, length))) == 0) {
              showNotification("Not all of the selected projects exist in this area. Unfilter the data to see where this project takes place.", duration = 5)
            }
          }
          # END COMMENT

          map <- map %>%
            addLegend(
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

}

# Run the application
shinyApp(ui = ui, server = server)
