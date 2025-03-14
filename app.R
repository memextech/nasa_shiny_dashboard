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
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"),
    
    # Social share meta tags
    tags$meta(property = "og:title", content = "Space Explorer"),
    tags$meta(property = "og:description", content = "Live dashboard tracking space data from NASA APIs and ISS location"),
    tags$meta(property = "og:image", content = "https://memextech.shinyapps.io/space-explorer/assets/social-share.png"),
    tags$meta(property = "og:url", content = "https://memextech.shinyapps.io/space-explorer/"),
    tags$meta(property = "og:type", content = "website"),
    
    # Twitter specific
    tags$meta(name = "twitter:card", content = "summary_large_image"),
    tags$meta(name = "twitter:title", content = "Space Explorer"),
    tags$meta(name = "twitter:description", content = "Live dashboard tracking space data from NASA APIs and ISS location"),
    tags$meta(name = "twitter:image", content = "https://memextech.shinyapps.io/space-explorer/assets/social-share.png")
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
