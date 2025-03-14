# ISS Tracker Module

#' ISS Tracker UI Module
#'
#' @param id Module ID
#' @export
issTrackerUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    # Auto-update toggle
    fluidRow(
      column(12,
        div(class = "well",
          checkboxInput(
            ns("auto_update"),
            "Auto-update position (every 5 seconds)",
            TRUE
          )
        )
      )
    ),
    
    # Stats cards
    fluidRow(
      column(4,
        div(class = "well",
          h4("Latitude", class = "text-center"),
          h2(textOutput(ns("latitude")), class = "text-center"),
          div(icon("location-arrow"), class = "text-center")
        )
      ),
      column(4,
        div(class = "well",
          h4("Longitude", class = "text-center"),
          h2(textOutput(ns("longitude")), class = "text-center"),
          div(icon("location-arrow"), class = "text-center")
        )
      ),
      column(4,
        div(class = "well",
          h4("Orbital Velocity", class = "text-center"),
          h2(textOutput(ns("velocity")), class = "text-center"),
          div(icon("tachometer-alt"), class = "text-center")
        )
      )
    ),
    
    # Map
    fluidRow(
      column(12,
        div(class = "well",
          leafletOutput(ns("map"), height = "600px")
        )
      )
    ),
    
    # Additional info
    fluidRow(
      column(6,
        div(class = "well",
          h3("Current Location"),
          textOutput(ns("location_info"))
        )
      ),
      column(6,
        div(class = "well",
          h3("Next Pass Predictions"),
          textOutput(ns("pass_predictions"))
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
    
    # Stats outputs
    output$latitude <- renderText({
      pos <- iss_position()
      round(pos$latitude, 4)
    })
    
    output$longitude <- renderText({
      pos <- iss_position()
      round(pos$longitude, 4)
    })
    
    output$velocity <- renderText({
      "7.66 km/s"
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
