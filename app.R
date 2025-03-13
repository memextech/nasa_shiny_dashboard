# Space Exploration Dashboard
# Main application file

# Load required packages
library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(leaflet)
library(dplyr)
library(config)
library(plotly)

# Source all module files
for (file in list.files("R/modules", pattern = "*.R$", full.names = TRUE)) {
  source(file)
}

# Source utility functions
source("R/utils.R")

# Load configuration
config <- config::get()

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Space Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("APOD", tabName = "apod", icon = icon("star")),
      menuItem("NEO Tracker", tabName = "neo", icon = icon("asteroid")),
      menuItem("Mars Rover", tabName = "mars", icon = icon("robot")),
      menuItem("ISS Tracker", tabName = "iss", icon = icon("satellite")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css")
    ),
    
    tabItems(
      # Dashboard tab
      tabItem(tabName = "dashboard",
              fluidRow(
                # Dashboard content will go here
              )
      ),
      
      # APOD tab
      tabItem(tabName = "apod",
              apodUI("apod")
      ),
      
      # NEO Tracker tab
      tabItem(tabName = "neo",
              neoTrackerUI("neo")
      ),
      
      # Mars Rover tab
      tabItem(tabName = "mars",
              marsRoverUI("mars")
      ),
      
      # ISS Tracker tab
      tabItem(tabName = "iss",
              issTrackerUI("iss")
      ),
      
      # About tab
      tabItem(tabName = "about",
              includeMarkdown("docs/about.md")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Call module servers
  apodServer("apod", config)
  neoTrackerServer("neo", config)
  marsRoverServer("mars", config)
  issTrackerServer("iss", config)
}

# Run the application
shinyApp(ui = ui, server = server)
