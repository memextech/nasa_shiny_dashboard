# Dashboard Module

#' Dashboard UI Module
#'
#' @param id Module ID
#' @export
dashboardUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    # APOD Section
    fluidRow(
      column(6,
        div(class = "well",
          h3("Astronomy Picture of the Day", 
             tags$a(href = "#", onclick = sprintf("$('a[data-value=\"APOD\"]').tab('show');"), 
                   icon("external-link-alt"), class = "pull-right")),
          uiOutput(ns("apod_preview"))
        )
      ),
      # Mars Rover Section
      column(6,
        div(class = "well",
          h3("Latest from Mars", 
             tags$a(href = "#", onclick = sprintf("$('a[data-value=\"Mars Rover\"]').tab('show');"), 
                   icon("external-link-alt"), class = "pull-right")),
          uiOutput(ns("mars_preview"))
        )
      )
    ),
    
    # NEO Section
    fluidRow(
      column(6,
        div(class = "well",
          h3("Near-Earth Objects", 
             tags$a(href = "#", onclick = sprintf("$('a[data-value=\"NEO Tracker\"]').tab('show');"), 
                   icon("external-link-alt"), class = "pull-right")),
          plotlyOutput(ns("neo_preview"), height = "300px")
        )
      ),
      # ISS Section
      column(6,
        div(class = "well",
          h3("ISS Location", 
             tags$a(href = "#", onclick = sprintf("$('a[data-value=\"ISS Tracker\"]').tab('show');"), 
                   icon("external-link-alt"), class = "pull-right")),
          leafletOutput(ns("iss_preview"), height = "300px")
        )
      )
    )
  )
}

#' Dashboard Server Module
#'
#' @param id Module ID
#' @param config Configuration object
#' @export
dashboardServer <- function(id, config) {
  moduleServer(id, function(input, output, session) {
    
    # APOD Preview
    output$apod_preview <- renderUI({
      # Get today's APOD
      data <- nasa_api_get(
        endpoint = "/planetary/apod",
        params = list(
          date = format_nasa_date(Sys.Date())
        )
      )
      
      if (data$media_type == "image") {
        tags$div(
          tags$img(
            src = data$url,
            class = "img-responsive",
            style = "max-height: 300px; width: auto; margin: 0 auto; display: block;"
          ),
          tags$p(class = "text-center", strong(data$title))
        )
      } else {
        tags$p("Today's astronomy picture is a video. Click the link above to view it.")
      }
    })
    
    # Mars Rover Preview
    output$mars_preview <- renderUI({
      # Get latest Perseverance photo
      data <- nasa_api_get(
        endpoint = "/mars-photos/api/v1/rovers/perseverance/latest_photos"
      )
      
      if (length(data$latest_photos) > 0) {
        latest <- data$latest_photos[[1]]
        tags$div(
          tags$img(
            src = latest$img_src,
            class = "img-responsive",
            style = "max-height: 300px; width: auto; margin: 0 auto; display: block;"
          ),
          tags$p(class = "text-center", 
                strong("Latest from Perseverance"),
                br(),
                "Taken on: ", latest$earth_date)
        )
      }
    })
    
    # NEO Preview
    output$neo_preview <- renderPlotly({
      # Get NEO data for last 7 days
      data <- nasa_api_get(
        endpoint = "/neo/rest/v1/feed",
        params = list(
          start_date = format_nasa_date(Sys.Date() - 7),
          end_date = format_nasa_date(Sys.Date())
        )
      )
      
      # Process data
      neo_list <- list()
      for (date in names(data$near_earth_objects)) {
        neo_list[[length(neo_list) + 1]] <- list(
          date = date,
          count = length(data$near_earth_objects[[date]]),
          hazardous = sum(sapply(data$near_earth_objects[[date]], function(x) x$is_potentially_hazardous_asteroid))
        )
      }
      
      df <- do.call(rbind, lapply(neo_list, as.data.frame))
      df$date <- as.Date(df$date)
      
      # Create plot
      plot_ly(df) %>%
        add_trace(
          x = ~date,
          y = ~count,
          name = "Total NEOs",
          type = "scatter",
          mode = "lines+markers"
        ) %>%
        add_trace(
          x = ~date,
          y = ~hazardous,
          name = "Hazardous",
          type = "scatter",
          mode = "lines+markers"
        ) %>%
        layout(
          showlegend = TRUE,
          xaxis = list(title = "Date"),
          yaxis = list(title = "Number of Objects")
        )
    })
    
    # ISS Preview
    output$iss_preview <- renderLeaflet({
      # Get current ISS position
      response <- httr::GET("http://api.open-notify.org/iss-now.json")
      httr::stop_for_status(response)
      data <- httr::content(response)
      
      lat <- as.numeric(data$iss_position$latitude)
      lng <- as.numeric(data$iss_position$longitude)
      
      # Create map
      leaflet() %>%
        addTiles() %>%
        setView(lng = lng, lat = lat, zoom = 3) %>%
        addCircleMarkers(
          lng = lng,
          lat = lat,
          radius = 10,
          color = "red",
          fillOpacity = 0.7,
          popup = paste(
            "ISS Position<br>",
            "Latitude:", round(lat, 4), "<br>",
            "Longitude:", round(lng, 4)
          )
        )
    })
    
  })
}