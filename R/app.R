library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(shinyjs)


# Getting the data
# 1. MPAs
MPAs <- data_CPCAD_areas(data_bioregion(),  zones = FALSE)
subarea_coords <- getLatLon(MPAs)

# 2. Project Titles
load(file.path(system.file(package="MarConsNetData"),"data", "dataTable.rda"))

# 3. dataSPA om data
om <- getData(type="om", age=3000, cookie="cookie")

# 4. Objectives
areas <- c("stAnnsBank", "musquash", "laurentianChannel", "gully", "gilbert", "eastport",
           "basinHead", "bancsDesAmericains")

# Step 1: Halle

objectives <- lapply(areas, function(x) data_objectives(type="site", area=x))
Objectives <- vector(mode="list", length(objectives))
for (i in seq_along(objectives)) {
  O <- objectives[[i]]
  for (j in seq_along(O)) {
    Objectives[[i]][[j]] <- newLine(O[j])
  }

}
Objectives <- lapply(Objectives, unlist)
names(Objectives) <- areas


# Theme
my_theme <- bslib::bs_theme(
  bg = "#ecf0f1",
  # Light grey text
  fg = "#2c3e50",
  # Greyish blue background
  primary = "#2980b9",
  # Blue primary color for buttons
  primary_hover = "#3498db" # Lighter blue on hover for buttons
)

# Define UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Maritimes Conservation Network App"),
  theme = my_theme,
  sidebarLayout(
    sidebarPanel(
      uiOutput("mpas"),
      uiOutput("projects"),
      uiOutput("fundingSource"),
      uiOutput("theme"),
      uiOutput("functionalGroup"),
      uiOutput("section"),
      uiOutput("division"),
      uiOutput("international"),
      uiOutput("report"),
      fluidRow(
        column(6, uiOutput("go_page2")),
        column(6, uiOutput("go_page3"))
      ),
      fluidRow(
        column(6, uiOutput("go_page4")),
        column(6, uiOutput("go_page5"))
      ),
      uiOutput("go_home")
      ),
    # Step 3: Halle
    mainPanel(leafletOutput("map"),
              fluidRow(column(6, align="left", uiOutput("networkObjectiveText")),
                       column(6, align="right", uiOutput("siteObjectiveText"))),
              #fluidRow(column(width=6, offset=6, uiOutput("siteObjectiveText"))),
              fluidRow(column(width=6, offset=6, textOutput("objectives", container=pre)))
              ) #MAIN


  )
)


# Define server logic
server <- function(input, output, session) {
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

  output$international <- renderUI({
    if (current_page() == "home") {
      h3("Ecosystem Based Management")
    }
  })


  output$go_page2 <- renderUI({
    if (current_page()=="home") {
      actionButton("go_page2", "Biodiversity")
    }
  })

  output$go_page3 <- renderUI({
    if (current_page()=="home") {
      actionButton("go_page3", "Productivity")
    }
  })

  output$go_page4 <- renderUI({
    if (current_page()=="home") {
      actionButton("go_page4", "Habitat")
    }
  })

  output$go_page5 <- renderUI({
    if (current_page()=="home") {
      actionButton("go_page5", "Threats")
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

  # Step 2: Halle

  output$objectives <- renderText({
    if (current_page() == "home" && !(is.null(input$mpas))) {
       string <- gsub("\\.", "", gsub(" ", "", input$mpas))
       keepO <- which(unlist(lapply(areas, function(x) grepl(x, string, ignore.case=TRUE))))
       if (!(length(keepO) == 0)) {
       textO <- Objectives[[keepO]]
       return(textO)
       }
    }
  })

  # Render the map with selected coordinates
  output$map <- renderLeaflet({
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


    # NOTE THIS ALL WORKS BUT MUST FIX GET_PROJECT_DATA FIRST JAIM
    # if (!(is.null(input$projects))) {
    #   projectIds <- dataTable$id[which(dataTable$title %in% sub(" .*", "", input$projects))] # The sub is because input$projects is snowCrabSurvey (1093)
    #   projectData <- NULL
    #   for (i in seq_along(projectIds)) {
    #     pd <- get_project_data(ids=projectIds[i], taxize=FALSE)
    #     projectData[[i]] <- pd
    #     map <- map %>%
    #       addCircleMarkers(pd[[i]]$lon, pd[[i]]$lat)
    #   }
    # }

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

  ## PAGE 3
  observeEvent(input$go_page3, {
    current_page("page3")
  })

  ## PAGE 4
  observeEvent(input$go_page4, {
    current_page("page4")
  })

  ## PAGE 5
  observeEvent(input$go_page5, {
    current_page("page5")
  })


}

# Run the application
shinyApp(ui = ui, server = server)
