library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(shinyjs)
library(viridis)
library(arcpullr)
library(devtools)
source("getLatLon.R")
source("newLine.R")
install_github("dfo-mar-mpas/MarConsNetAnalysis", ref="main")
library(MarConsNetAnalysis)
#install_github("dfo-mar-mpas/MarConsNetData", ref="main")
#library(MarConsNetData)
load_all("../../MarConsNetData/")
install_github("https://github.com/dfo-mar-odis/TBSpayRates")
library(TBSpayRates)
install_github("https://github.com/j-harbin/dataSPA")
library(dataSPA)
library(readxl)

#source("data_app.R")


# Define UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Maritimes Conservation Network App"),
  fluidRow(
  column(2, uiOutput("contextButton")),
  column(2, uiOutput("projectFilter"))
  ),
  uiOutput("gohome"),
  #theme = my_theme,
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
      uiOutput('mytabs'),
              leafletOutput("map"),
              fluidRow(column(6, align="left", uiOutput("networkObjectiveText")),
                       column(6, align="right", uiOutput("siteObjectiveText"))),
              fluidRow(
                column(width=6, align="left", textOutput("network", container=pre)),
                       column(width=6, uiOutput("objectives", container=pre))
                       )
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
    report = NULL,
    projectFilter = NULL,
    filter_selected=FALSE,
  )

  #filter_selected <- reactiveVal(FALSE)


  input_ids <- c("mpas", "projects", "fundingSource", "theme", "functionalGroup", "section", "division", "report", "projectFilter", "filter_selected") # THE SAME AS STATE

lapply(input_ids, function(id) {
  observeEvent(input[[id]], {
    state[[id]] <- input[[id]]
  })
})

  output$mytabs = renderUI({
    nTabs = length(do.call(c, Objectives))
    #nTabs = length(unique(odf$flower_plot))

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
      tags$b("Network Level Objectives")
    }
  })


  output$siteObjectiveText <- renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0" && !(is.null(state$mpas))) {
      if (grepl("Marine Protected Area", state$mpas)) {
        string <- gsub("Marine Protected Area", "MPA", state$mpas)
        if (grepl("Estuary", state$mpas)) {
          string <- gsub("Estuary ", "", string)
        }
        string <- gsub("\\.", "", string)
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

  output$projectFilter <- renderUI({
    req(state$mpas)
    if (input$tabs == "tab_0" && !is.null(state$mpas) && !(state$mpas == "All") && !is.null(state$projects) && length(state$projects) > 0) {
        label <- if (!(state$filter_selected)) "See All Project Data" else "Filter Project Data"
        actionButton(inputId="projectFilter", label=label)
    }
})

  observeEvent(input$projectFilter, {
    if (state$filter_selected) {
      state$filter_selected <- FALSE
      #updateActionButton(session, "projectFilter", label = "See All Project Data")
    } else {
      state$filter_selected <- TRUE
      #updateActionButton(session, "projectFilter", label = "Filter Project Data")
    }
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



    #string <- gsub("\\.", "", gsub(" ", "", input$mpas))
    keepC <- which(unlist(lapply(areas, function(x) grepl(x, string, ignore.case=TRUE))))
    textC <- Context[[keepC]]
    textC <- unlist(lapply(textC, function(x) paste(x, "\n\n")))
    showModal(modalDialog(
      title = "Marine Protected Area Context",
      HTML(textC)
    ))
  })

  # Dynmaically coding in which actionLink is selected will update the tab
  for (i in 0:(length(unique(odf$tab))-1)) {
    local({
      link_id <- paste0("link_", i)
      observeEvent(input[[link_id]], {
        selected_tab <- unique(odf$tab[which(odf$link == link_id)])
        updateTabsetPanel(session, "tabs", selected = selected_tab)
      })
    })
  }

  # Dynmaically coding in which actionLink is will paste indicators

  output$indicatorText <- renderUI({
    req(input$tabs)
    req(state$mpas)
    for (i in 0:(length(odf$objectives)-1)) {
      link_id <- paste0("link_", i)
      if (input$tabs == paste0("tab_", i)) {
        if (!(input$tabs == "tab_0")) {
          objective <- gsub("\n", "",odf$objectives[which(odf$link == link_id)])
          flower <- odf$flower_plot[which(odf$link == link_id)]
          area <- gsub("_", " ", gsub("_CO$", "",odf$area[which(odf$link == link_id)]))
          ki1 <- which(grepl(flower, binned_indicators$indicator_bin, ignore.case=TRUE)) # find matching flower plot
          ki2 <-  which(tolower(binned_indicators$applicability) %in% tolower(c(gsub(" MPA", "", area), "coastal", "offshore", "all"))) # Find matching area
          keepind <-intersect(ki1,ki2)
          binned_ind <-  gsub("^[0-9]+\\. ", "", gsub("Indicator [0-9]+: ", "", binned_indicators$indicators[keepind]))
          # which projects have that flower tag and are in the correct area
          PPTProjects <- sort(unique(om$project_id[which(grepl(area, om$tags, ignore.case=TRUE) & grepl(flower, om$tags, ignore.case=TRUE))]))
          PPTtitles <- unlist(lapply(PPTProjects, function(x) unique(om$project_title[which(om$project_id == x)])))
          formatted_projects <- paste0("<strong>", PPTProjects, "</strong> (", PPTtitles, ")")
          indicator_label <- ifelse(flower %in% c("Biodiversity", "Productivity", "Habitat"), "Ecosystem Based Management Objective:", "Indicator Bin:")

          if (!(length(PPTProjects) == 0)) {
            return(HTML(
              paste(
                "<p><strong>Site Level Objective:</strong></p>",
                "<p>", objective, "</p>",
                "<p><strong>Area:</strong></p>",
                "<p>", area, "</p>",
                "<p><strong>",indicator_label,"</strong></p>",
                "<p>", flower, "</p>",
                "<p><strong>Indicators:</strong></p>",
                "<p>", paste0(binned_ind, collapse="<br>"), "</p>",
                "<p><strong>Projects:</strong></p>",
                "<p>", paste0(formatted_projects, collapse="<br>"), "</p>"
              )
            )
            )
            #HTML(paste("The Objective ", objective, " from ",area," is associated with the ", flower, " indicator bin. The following indicators apply: ", paste0(binned_ind, collapse="\n\n"), ". The following projects provide information: ", paste0(PPTProjects, collapse=",")))
          } else {
            return(HTML(
              paste(
                "<p><strong>Site Level Objective:</strong></p>",
                "<p>", objective, "</p>",
                "<p><strong>Area:</strong></p>",
                "<p>", area, "</p>",
                "<p><strong>",indicator_label,"</strong></p>",
                "<p>", flower, "</p>",
                "<p><strong>Indicators:</strong></p>",
                "<p>", paste0(binned_ind, collapse="<br>"), "</p>",
                "<p><strong>Projects:</strong></p>",
                "<p>", paste0("There are no projects for this area in this indicator bin."), "</p>"
              )
            )
            )

          }
        }
      }
    }
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


  output$network <- renderText({
    req(input$tabs)
    if (input$tabs == "tab_0" && !(is.null(state$mpas))) {
      N_Objectives
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
      for (i in seq_along(projectIds)) {
        pd <- projectData[[which(as.numeric(names(projectData)) %in% projectIds[i])]]
        longitude <- pd[[1]]$lon
        latitude <- pd[[1]]$lat

        # TEST JAIM
        if (!(state$filter_selected) && !(state$mpas %in% "All")) { # We want it filtered
        m <- MPAs$geoms[which(MPAs$NAME_E == state$mpas)]
        coords <- cbind(longitude, latitude)
        points_sf <- st_as_sf(data.frame(coords), coords = c("longitude", "latitude"), crs = st_crs(4326))
        points_within <- st_within(points_sf, m, sparse = FALSE)
        within_points <- points_sf[apply(points_within, 1, any), ]
        longitude <- st_coordinates(within_points)[, 1]
        latitude <- st_coordinates(within_points)[, 2]
        }

        if (!(length(latitude) == 0)) {
        map <- map %>%
          addCircleMarkers(longitude, latitude, radius=3, color=palette[i])
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
