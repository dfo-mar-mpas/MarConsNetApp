library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(shinyjs)
library(viridis)
library(dataSPA)
library(arcpullr)
source("data_app.R")

# Define UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Maritimes Conservation Network App"),
  uiOutput("contextButton"),
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
#
#   state <- reactiveValues(
#     text1 = NULL,
#     text2 = NULL
#   )





  output$mytabs = renderUI({
    nTabs = length(unique(odf$flower_plot))
    myTabs = lapply(paste0('tab_', 0: nTabs), tabPanel)
    do.call(tabsetPanel, c(myTabs, id = "tabs"))
  })

  ## PAGE 1 (HOME)

  output$mpas <- renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0") {
    selectInput("mpas","Select Protected/Conserved Area:",choices = c("All", MPAs$NAME_E))
    }
  })

  output$projects <- renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0") {
    selectInput("projects", "Select Project(s):", choices=paste0(dataTable$title, " (", dataTable$id,")"), multiple=TRUE)
    }
  })

  output$fundingSource <- renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0") {
      selectInput("fundingSource", "Select Funding Source(s):", choices=unique(om$funding_source_display), multiple=TRUE)
    }
  })

output$theme <- renderUI({
  req(input$tabs)
  if (input$tabs == "tab_0") {
    selectInput("theme", "Select Theme(s):", choices=unique(om$theme), multiple=TRUE)
  }
})

output$functionalGroup <- renderUI({
  req(input$tabs)
  if (input$tabs == "tab_0") {
    selectInput("functionalGroup", "Select Functional Group(s):", choices=unique(om$functional_group), multiple=TRUE)
  }
})

output$section <- renderUI({
  req(input$tabs)
  if (input$tabs == "tab_0") {
    selectInput("section", "Select Section(s):", choices=subsetSPA(om=om, section="return"), multiple=TRUE)
  }
})

output$division <- renderUI({
  req(input$tabs)
  if (input$tabs == "tab_0") {
    selectInput("division", "Select Division(s):", choices=subsetSPA(om=om, division="return"), multiple=TRUE)
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
    if (input$tabs == "tab_0" && !(is.null(input$mpas))) {
      tags$b("Network Level Objectives")
    }
  })


  output$siteObjectiveText <- renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0" && !(is.null(input$mpas))) {
      string <- gsub("\\.", "", gsub(" ", "", input$mpas))
      keepO <- which(unlist(lapply(areas, function(x) grepl(x, string, ignore.case=TRUE))))
      if (!(length(keepO) == 0)) {
        tags$b("Site Level Objectives")
      }
    }
  })

  output$objectives <- renderUI({
    req(input$tabs)
    if (input$tabs == "tab_0" && !(is.null(input$mpas))) {
       string <- gsub("\\.", "", gsub(" ", "", input$mpas))
       keepO <- which(unlist(lapply(areas, function(x) grepl(x, string, ignore.case=TRUE))))
       if (!(length(keepO) == 0)) {
       textO <- Objectives[[keepO]]
       links <- lapply(seq_along(textO), function(i) {
         actionLink(inputId = odf$link[which(odf$objectives == textO[[i]])], label = textO[[i]])
       })
       }
    }
  })


  output$contextButton <- renderUI({
    if (input$tabs == "tab_0" && !(is.null(input$mpas))) {
      string <- gsub("\\.", "", gsub(" ", "", input$mpas))
      keepO <- which(unlist(lapply(areas, function(x) grepl(x, string, ignore.case=TRUE))))
      if (!(length(keepO) == 0)) {
        actionButton(inputId="contextButton", label="Context")
      }
    }
  })

  observeEvent(input$contextButton, {
    string <- gsub("\\.", "", gsub(" ", "", input$mpas))
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

  output$indicatorText <- renderText({
    req(input$tabs)
    req(input$mpas)
    for (i in 0:(length(odf$objectives)-1)) {
      link_id <- paste0("link_", i)
      if (input$tabs == paste0("tab_", i)) {
        if (!(input$tabs == "tab_0"))
        return(paste0(unique(odf$flower_plot[which(odf$link == link_id)])))
      }
    }
  })

  output$gohome <- renderUI({
    req(input$tabs)
    req(input$mpas)
    if (!(input$tabs == "tab_0")) {
      actionButton(inputId = "gohome", "Go Home")
    }
  })

  observeEvent(input$gohome, {
    updateTabsetPanel(session, "tabs", selected = "tab_0")
  })


  output$network <- renderText({
    req(input$tabs)
    if (input$tabs == "tab_0" && !(is.null(input$mpas))) {
      N_Objectives
      }
  })


  # Render the map with selected coordinates
  output$map <- renderLeaflet({
    req(input$tabs)
    palette <- viridis(length(input$projects))
    if (input$tabs == "tab_0") {
    if (!(is.null(input$mpas))) {
    coords <- subarea_coords[[input$mpas]]
    map <- leaflet() %>%
      addTiles()

    if (!(is.null(input$mpas)) && !(input$mpas == "All")) {
      map <- map %>% addPolygons(
        lng = coords$lng,
        lat = coords$lat,
        fillColor = coords$color,
        fillOpacity = 0.5,
        weight = 2
      )
    } else if (input$mpas == "All") {
      for (c in seq_along(subarea_coords)) {
        coord <- subarea_coords[[c]]
        map <- map %>%
          addPolygons(lat = coord$lat, lng = coord$lng, fillColor = coord$color, fillOpacity = 0.5, weight = 2)
      }
    }


     if (!(is.null(input$projects))) {
      # projectIds <- dataTable$id[which(dataTable$title %in% sub(" .*", "", input$projects))] # The sub is because input$projects is snowCrabSurvey (1093)
      # for (i in seq_along(projectIds)) {
      #   pd <- projectData[[which(as.numeric(names(projectData)) %in% projectIds[i])]]
      #   map <- map %>%
      #     addCircleMarkers(pd[[1]]$lon, pd[[1]]$lat, radius=3, color=palette[i])
      # }

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
