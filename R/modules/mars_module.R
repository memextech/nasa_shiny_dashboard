# Mars Rover Photos Module

#' Mars Rover UI Module
#'
#' @param id Module ID
#' @export
marsRoverUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(12,
        div(class = "well",
          h3("Mars Rover Photos"),
          
          # Rover selector
          selectInput(
            ns("rover"),
            "Select Rover:",
            choices = c("Curiosity", "Opportunity", "Spirit", "Perseverance"),
            selected = "Curiosity"
          ),
          
          # Date selector
          dateInput(
            ns("date"),
            "Select Date:",
            value = Sys.Date() - 7,
            max = Sys.Date()
          ),
          
          # Camera selector
          selectInput(
            ns("camera"),
            "Select Camera:",
            choices = c("All", "FHAZ", "RHAZ", "NAVCAM", "MAST", "CHEMCAM", "MAHLI", "MARDI"),
            selected = "All"
          )
        )
      )
    ),
    
    # Stats cards
    fluidRow(
      column(4,
        div(class = "well",
          h4("Total Photos", class = "text-center"),
          h2(textOutput(ns("total_photos")), class = "text-center"),
          div(icon("camera"), class = "text-center")
        )
      ),
      column(4,
        div(class = "well",
          h4("Mars Sol", class = "text-center"),
          h2(textOutput(ns("sol")), class = "text-center"),
          div(icon("sun"), class = "text-center")
        )
      ),
      column(4,
        div(class = "well",
          h4("Cameras Used", class = "text-center"),
          h2(textOutput(ns("cameras")), class = "text-center"),
          div(icon("video"), class = "text-center")
        )
      )
    ),
    
    # Photo gallery
    fluidRow(
      column(12,
        div(class = "well",
          uiOutput(ns("photo_gallery"))
        )
      )
    )
  )
}

#' Mars Rover Server Module
#'
#' @param id Module ID
#' @param config Configuration object
#' @export
marsRoverServer <- function(id, config) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive expression for Mars photos
    mars_photos <- reactive({
      req(input$rover, input$date)
      
      # Get API response
      data <- nasa_api_get(
        endpoint = paste0("/mars-photos/api/v1/rovers/", tolower(input$rover), "/photos"),
        params = list(
          earth_date = format_nasa_date(input$date)
        )
      )
      
      # Filter by camera if selected
      photos <- data$photos
      if (input$camera != "All") {
        photos <- photos[sapply(photos, function(x) x$camera$name == input$camera)]
      }
      
      photos
    })
    
    # Stats outputs
    output$total_photos <- renderText({
      photos <- mars_photos()
      length(photos)
    })
    
    output$sol <- renderText({
      photos <- mars_photos()
      if (length(photos) > 0) {
        photos[[1]]$sol
      } else {
        "N/A"
      }
    })
    
    output$cameras <- renderText({
      photos <- mars_photos()
      if (length(photos) > 0) {
        cameras <- unique(sapply(photos, function(x) x$camera$name))
        length(cameras)
      } else {
        0
      }
    })
    
    # Photo gallery
    output$photo_gallery <- renderUI({
      photos <- mars_photos()
      
      if (length(photos) == 0) {
        return(h4("No photos available for this date/rover combination"))
      }
      
      # Create a grid of photos
      photo_grid <- lapply(photos, function(photo) {
        column(4,
          div(class = "well",
            tags$img(
              src = photo$img_src,
              class = "img-responsive",
              style = "width: 100%; height: auto;"
            ),
            p(
              strong("Camera: "), photo$camera$full_name,
              br(),
              strong("Earth Date: "), photo$earth_date,
              br(),
              strong("Sol: "), photo$sol
            )
          )
        )
      })
      
      do.call(fluidRow, photo_grid)
    })
  })
}
