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
      uiOutput("report"),
      fluidRow(
        column(6, uiOutput("go_page2"))
      ),
      uiOutput("go_home")
      ),
    mainPanel(
      uiOutput('mytabs'),
              leafletOutput("map"),
              fluidRow(column(6, align="left", uiOutput("networkObjectiveText")),
                       column(6, align="right", uiOutput("siteObjectiveText"))),

              #fluidRow(column(width=6, offset=6, uiOutput("siteObjectiveText"))),
              fluidRow(
                column(width=6, align="left", textOutput("network", container=pre)),
                       column(width=6, uiOutput("objectives", container=pre))


                       )#FLUID

              ) #MAIN
  )
)


# Define server logic
server <- function(input, output, session) {
  output$mytabs = renderUI({
    nTabs = length(do.call(c,Objectives))
    myTabs = lapply(paste('tab_', 1: nTabs), tabPanel)
    #browser()
    do.call(tabsetPanel, myTabs)
  })


  current_page <- reactiveVal("home")

  ## PAGE 1 (HOME)

  output$mpas <- renderUI({
    if (current_page() == "home") {
    selectInput("mpas","Select Protected/Conserved Area:",choices = c("All", MPAs$NAME_E))
    }
  })

  output$projects <- renderUI({
    if (current_page() == "home") {
    selectInput("projects", "Select Project(s):", choices=paste0(dataTable$title, " (", dataTable$id,")"), multiple=TRUE)
    }
  })

  output$fundingSource <- renderUI({
    if (current_page() == "home") {
      selectInput("fundingSource", "Select Funding Source(s):", choices=unique(om$funding_source_display), multiple=TRUE)
    }
  })

output$theme <- renderUI({
  if (current_page() == "home") {
    selectInput("theme", "Select Theme(s):", choices=unique(om$theme), multiple=TRUE)
  }
})

output$functionalGroup <- renderUI({
  if (current_page() == "home") {
    selectInput("functionalGroup", "Select Functional Group(s):", choices=unique(om$functional_group), multiple=TRUE)
  }
})

output$section <- renderUI({
  if (current_page() == "home") {
    selectInput("section", "Select Section(s):", choices=subsetSPA(om=om, section="return"), multiple=TRUE)
  }
})

output$division <- renderUI({
  if (current_page() == "home") {
    selectInput("division", "Select Division(s):", choices=subsetSPA(om=om, division="return"), multiple=TRUE)
  }
})

output$report <- renderUI({
  if (current_page() == "home") {
    actionButton("report", "Create Report")
  }
})

  output$go_page2 <- renderUI({
    if (current_page()=="home") {
      actionButton("go_page2", "Biodiversity")
    }
  })

  output$networkObjectiveText <- renderUI({
    if (current_page() == "home" && !(is.null(input$mpas))) {
      tags$b("Network Level Objectives")
    }
  })


  output$siteObjectiveText <- renderUI({
    if (current_page() == "home" && !(is.null(input$mpas))) {
      string <- gsub("\\.", "", gsub(" ", "", input$mpas))
      keepO <- which(unlist(lapply(areas, function(x) grepl(x, string, ignore.case=TRUE))))
      if (!(length(keepO) == 0)) {
        tags$b("Site Level Objectives")
      }
    }
  })

  output$objectives <- renderUI({
    if (current_page() == "home" && !(is.null(input$mpas))) {
       string <- gsub("\\.", "", gsub(" ", "", input$mpas))
       keepO <- which(unlist(lapply(areas, function(x) grepl(x, string, ignore.case=TRUE))))
       if (!(length(keepO) == 0)) {
       textO <- Objectives[[keepO]]
       links <- lapply(seq_along(textO), function(i) {
         actionLink(inputId = odf$link[which(odf$objectives == textO[[1]])], label = textO[[i]])
       })
       }
    }
  })

  # TEST JAIM
  # observeEvent(input$link_13, {
  #   browser()
  #   updateTabsetPanel(session, "mytabs", selected = odf$tab[which(odf$link == "link_13")])
  # })
  #TEST


  output$network <- renderText({
    if (current_page() == "home" && !(is.null(input$mpas))) {
      N_Objectives
      }
  })


  # Render the map with selected coordinates
  output$map <- renderLeaflet({
    palette <- viridis(length(input$projects))
    if (current_page() == "home") {
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




  ## PAGE 2
  observeEvent(input$go_page2, {
    current_page("page2")
  })

  observeEvent(input$go_home, {
    current_page("home")
  })

  output$go_home <- renderUI({
    if (!(current_page()=="home")) {
      actionButton("go_home", "Home")
    }
  })

}

# Run the application
shinyApp(ui = ui, server = server)
