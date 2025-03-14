# Space Exploration Dashboard
# Main application file

# Load required packages
library(shiny)
library(httr)
library(jsonlite)
library(leaflet)
library(dplyr)
library(config)
library(plotly)
library(markdown)

# Source all module files
for (file in list.files("R/modules", pattern = "*.R$", full.names = TRUE)) {
  source(file)
}

# Source utility functions
source("R/utils.R")

# Load configuration
config <- config::get()

# UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css")
  ),
  
  # Navigation bar
  navbarPage(
    title = "Space Explorer",
    theme = "css/custom.css",
    id = "nav",
    
    tabPanel(
      "Dashboard",
      icon = icon("dashboard"),
      dashboardUI("dashboard")
    ),
    
    tabPanel(
      "APOD",
      icon = icon("star"),
      apodUI("apod")
    ),
    
    tabPanel(
      "NEO Tracker", 
      icon = icon("meteor"),
      neoTrackerUI("neo")
    ),
    
    tabPanel(
      "Mars Rover",
      icon = icon("robot"),
      marsRoverUI("mars")
    ),
    
    tabPanel(
      "ISS Tracker",
      icon = icon("satellite"),
      issTrackerUI("iss")
    ),
    
    tabPanel(
      "About",
      icon = icon("info-circle"),
      includeMarkdown("docs/about.md")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Call module servers
  dashboardServer("dashboard", config)
  apodServer("apod", config)
  neoTrackerServer("neo", config)
  marsRoverServer("mars", config)
  issTrackerServer("iss", config)
}

# Run the application
shinyApp(ui = ui, server = server)
