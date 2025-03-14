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
        div(class = "dashboard-tile",
          h3(
            "Astronomy Picture of the Day",
            tags$a(href = "#", onclick = sprintf("$('a[data-value=\"APOD\"]').tab('show');"), 
                  icon("external-link-alt"))
          ),
          div(class = "preview-content",
            div(class = "loading-container",
              # Loading spinner
              conditionalPanel(
                condition = "!output.apod_ready",
                div(class = "loading-spinner",
                  icon("spinner", class = "fa-spin"),
                  div(class = "loading-text", "Loading image...")
                ),
                ns = ns
              ),
              # Content
              div(id = ns("apod_content"), class = "content-fade",
                uiOutput(ns("apod_preview"))
              )
            )
          )
        )
      ),
      # Mars Rover Section
      column(6,
        div(class = "dashboard-tile",
          h3(
            "Latest from Mars",
            tags$a(href = "#", onclick = sprintf("$('a[data-value=\"Mars Rover\"]').tab('show');"), 
                  icon("external-link-alt"))
          ),
          div(class = "preview-content",
            div(class = "loading-container",
              conditionalPanel(
                condition = "!output.mars_ready",
                div(class = "loading-spinner",
                  icon("spinner", class = "fa-spin"),
                  div(class = "loading-text", "Loading latest Mars photo...")
                ),
                ns = ns
              ),
              div(id = ns("mars_content"), class = "content-fade",
                uiOutput(ns("mars_preview"))
              )
            )
          )
        )
      )
    ),
    
    # NEO Section
    fluidRow(
      column(6,
        div(class = "dashboard-tile",
          h3(
            "Near-Earth Objects",
            tags$a(href = "#", onclick = sprintf("$('a[data-value=\"NEO Tracker\"]').tab('show');"), 
                  icon("external-link-alt"))
          ),
          div(class = "preview-content",
            div(class = "loading-container",
              conditionalPanel(
                condition = "!output.neo_ready",
                div(class = "loading-spinner",
                  icon("spinner", class = "fa-spin"),
                  div(class = "loading-text", "Loading NEO data...")
                ),
                ns = ns
              ),
              div(id = ns("neo_content"), class = "content-fade",
                plotlyOutput(ns("neo_preview"), height = "100%")
              )
            )
          )
        )
      ),
      # ISS Section
      column(6,
        div(class = "dashboard-tile",
          h3(
            "ISS Location",
            tags$a(href = "#", onclick = sprintf("$('a[data-value=\"ISS Tracker\"]').tab('show');"), 
                  icon("external-link-alt"))
          ),
          div(class = "preview-content",
            div(class = "loading-container",
              conditionalPanel(
                condition = "!output.iss_ready",
                div(class = "loading-spinner",
                  icon("spinner", class = "fa-spin"),
                  div(class = "loading-text", "Loading ISS location...")
                ),
                ns = ns
              ),
              div(id = ns("iss_content"), class = "content-fade",
                leafletOutput(ns("iss_preview"), height = "100%")
              )
            )
          )
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
    
    # Loading state outputs
    output$apod_ready <- reactive({
      !is.null(apod_data())
    })
    outputOptions(output, "apod_ready", suspendWhenHidden = FALSE)
    
    output$mars_ready <- reactive({
      !is.null(mars_data())
    })
    outputOptions(output, "mars_ready", suspendWhenHidden = FALSE)
    
    output$neo_ready <- reactive({
      !is.null(neo_data())
    })
    outputOptions(output, "neo_ready", suspendWhenHidden = FALSE)
    
    output$iss_ready <- reactive({
      !is.null(iss_data())
    })
    outputOptions(output, "iss_ready", suspendWhenHidden = FALSE)
    
    # Reactive data sources
    apod_data <- reactive({
      # Get today's APOD
      nasa_api_get(
        endpoint = "/planetary/apod",
        params = list(
          date = format_nasa_date(Sys.Date())
        )
      )
    })
    
    mars_data <- reactive({
      nasa_api_get(
        endpoint = "/mars-photos/api/v1/rovers/perseverance/latest_photos"
      )
    })
    
    neo_data <- reactive({
      nasa_api_get(
        endpoint = "/neo/rest/v1/feed",
        params = list(
          start_date = format_nasa_date(Sys.Date() - 7),
          end_date = format_nasa_date(Sys.Date())
        )
      )
    })
    
    iss_data <- reactive({
      response <- httr::GET("http://api.open-notify.org/iss-now.json")
      httr::stop_for_status(response)
      httr::content(response)
    })
    
    # APOD Preview
    output$apod_preview <- renderUI({
      data <- apod_data()
      req(data)
      
      if (data$media_type == "image") {
        tags$div(
          div(class = "image-container",
            tags$img(
              src = data$url,
              class = "preview-image"
            )
          ),
          div(class = "preview-text",
            strong(data$title)
          )
        )
      } else {
        tags$p("Today's astronomy picture is a video. Click the link above to view it.")
      }
    })
    
    # Mars Rover Preview
    output$mars_preview <- renderUI({
      data <- mars_data()
      req(data)
      
      if (length(data$latest_photos) > 0) {
        latest <- data$latest_photos[[1]]
        tags$div(
          div(class = "image-container",
            tags$img(
              src = latest$img_src,
              class = "preview-image"
            )
          ),
          div(class = "preview-text",
            strong("Latest from Perseverance"),
            br(),
            "Taken on: ", latest$earth_date
          )
        )
      }
    })
    
    # NEO Preview
    output$neo_preview <- renderPlotly({
      data <- neo_data()
      req(data)
      
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
      
      # Create plot with specific dimensions
      p <- plot_ly(df, height = 300) %>%
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
          margin = list(l = 50, r = 50, t = 30, b = 50),
          xaxis = list(title = "Date"),
          yaxis = list(title = "Number of Objects")
        )
      
      div(class = "chart-container", p)
    })
    
    # ISS Preview
    output$iss_preview <- renderLeaflet({
      data <- iss_data()
      req(data)
      
      lat <- as.numeric(data$iss_position$latitude)
      lng <- as.numeric(data$iss_position$longitude)
      
      # Create map with dark theme
      div(class = "map-container",
        leaflet() %>%
          addProviderTiles("CartoDB.DarkMatter") %>%
          setView(lng = lng, lat = lat, zoom = 2) %>%
          addCircleMarkers(
            lng = lng,
            lat = lat,
            radius = 8,
            color = "#FF4136",
            fillColor = "#FF4136",
            fillOpacity = 0.8,
            weight = 2,
            popup = paste(
              "<strong>ISS Position</strong><br>",
              "Latitude:", round(lat, 4), "<br>",
              "Longitude:", round(lng, 4)
            )
          )
      )
    })
    
  })
}
