# ISS Tracker Module

#' ISS Tracker UI Module
#'
#' @param id Module ID
#' @export
issTrackerUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width = 12,
        title = "International Space Station Tracker",
        status = "primary",
        solidHeader = TRUE,
        
        # Auto-update toggle
        checkboxInput(
          ns("auto_update"),
          "Auto-update position (every 5 seconds)",
          TRUE
        ),
        
        # Stats boxes
        fluidRow(
          valueBoxOutput(ns("latitude_box"), width = 4),
          valueBoxOutput(ns("longitude_box"), width = 4),
          valueBoxOutput(ns("velocity_box"), width = 4)
        ),
        
        # Map
        leafletOutput(ns("map"), height = "600px"),
        
        # Additional info
        fluidRow(
          box(
            width = 6,
            title = "Current Location",
            status = "info",
            solidHeader = TRUE,
            textOutput(ns("location_info"))
          ),
          box(
            width = 6,
            title = "Next Pass Predictions",
            status = "info",
            solidHeader = TRUE,
            textOutput(ns("pass_predictions"))
          )
        )
      )
    )
  )
}

#' ISS Tracker Server Module
#'
#' @param id Module ID
#' @param config Configuration object
#' @export
issTrackerServer <- function(id, config) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive timer for auto-updates
    autoInvalidate <- reactiveTimer(5000)  # 5 seconds
    
    # Current ISS position
    iss_position <- reactive({
      if (input$auto_update) {
        autoInvalidate()
      }
      
      # No caching for real-time position
      response <- httr::GET("http://api.open-notify.org/iss-now.json")
      httr::stop_for_status(response)
      data <- httr::content(response)
      
      list(
        timestamp = as.POSIXct(as.numeric(data$timestamp), origin = "1970-01-01"),
        latitude = as.numeric(data$iss_position$latitude),
        longitude = as.numeric(data$iss_position$longitude)
      )
    })
    
    # Value boxes
    output$latitude_box <- renderValueBox({
      pos <- iss_position()
      valueBox(
        round(pos$latitude, 4),
        "Latitude",
        icon = icon("location-arrow"),
        color = "blue"
      )
    })
    
    output$longitude_box <- renderValueBox({
      pos <- iss_position()
      valueBox(
        round(pos$longitude, 4),
        "Longitude",
        icon = icon("location-arrow"),
        color = "green"
      )
    })
    
    output$velocity_box <- renderValueBox({
      valueBox(
        "7.66 km/s",
        "Orbital Velocity",
        icon = icon("tachometer-alt"),
        color = "red"
      )
    })
    
    # Initialize the map
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = 0, lat = 0, zoom = 2)
    })
    
    # Update map with ISS position
    observe({
      pos <- iss_position()
      
      leafletProxy("map") %>%
        clearMarkers() %>%
        addCircleMarkers(
          lng = pos$longitude,
          lat = pos$latitude,
          radius = 10,
          color = "red",
          fillOpacity = 0.7,
          popup = paste(
            "ISS Position<br>",
            "Latitude:", round(pos$latitude, 4), "<br>",
            "Longitude:", round(pos$longitude, 4), "<br>",
            "Time:", format(pos$timestamp, "%Y-%m-%d %H:%M:%S UTC")
          )
        )
    })
    
    # Location info
    output$location_info <- renderText({
      pos <- iss_position()
      paste(
        "The ISS is currently at",
        round(pos$latitude, 4), "°N,",
        round(pos$longitude, 4), "°E",
        "\nLast updated:",
        format(pos$timestamp, "%Y-%m-%d %H:%M:%S UTC")
      )
    })
    
    # Pass predictions (placeholder)
    output$pass_predictions <- renderText({
      "Pass predictions feature coming soon..."
    })
  })
}
