# Mars Rover Photos Module

#' Mars Rover UI Module
#'
#' @param id Module ID
#' @export
marsRoverUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width = 12,
        title = "Mars Rover Photos",
        status = "primary",
        solidHeader = TRUE,
        
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
        ),
        
        # Stats
        fluidRow(
          valueBoxOutput(ns("total_photos_box"), width = 4),
          valueBoxOutput(ns("sol_box"), width = 4),
          valueBoxOutput(ns("cameras_box"), width = 4)
        ),
        
        # Photo gallery
        uiOutput(ns("photo_gallery"))
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
    
    # Value boxes
    output$total_photos_box <- renderValueBox({
      photos <- mars_photos()
      valueBox(
        length(photos),
        "Total Photos",
        icon = icon("camera"),
        color = "blue"
      )
    })
    
    output$sol_box <- renderValueBox({
      photos <- mars_photos()
      if (length(photos) > 0) {
        sol <- photos[[1]]$sol
      } else {
        sol <- "N/A"
      }
      valueBox(
        sol,
        "Mars Sol",
        icon = icon("sun"),
        color = "yellow"
      )
    })
    
    output$cameras_box <- renderValueBox({
      photos <- mars_photos()
      if (length(photos) > 0) {
        cameras <- unique(sapply(photos, function(x) x$camera$name))
        n_cameras <- length(cameras)
      } else {
        n_cameras <- 0
      }
      valueBox(
        n_cameras,
        "Cameras Used",
        icon = icon("video"),
        color = "green"
      )
    })
    
    # Photo gallery
    output$photo_gallery <- renderUI({
      photos <- mars_photos()
      
      if (length(photos) == 0) {
        return(h4("No photos available for this date/rover combination"))
      }
      
      # Create a grid of photos
      photo_grid <- lapply(photos, function(photo) {
        box(
          width = 4,
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
      })
      
      do.call(fluidRow, photo_grid)
    })
  })
}