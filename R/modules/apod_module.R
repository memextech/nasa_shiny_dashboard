# Astronomy Picture of the Day (APOD) Module

#' APOD UI Module
#'
#' @param id Module ID
#' @export
apodUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(12,
        div(class = "well",
          h3("Astronomy Picture of the Day"),
          
          # Date selector
          dateInput(
            ns("date"),
            "Select Date:",
            value = Sys.Date(),
            max = Sys.Date()
          ),
          
          # Image display
          uiOutput(ns("image_ui")),
          
          # Title and explanation
          htmlOutput(ns("title")),
          htmlOutput(ns("explanation"))
        )
      )
    )
  )
}

#' APOD Server Module
#'
#' @param id Module ID
#' @param config Configuration object
#' @export
apodServer <- function(id, config) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive expression for APOD data
    apod_data <- reactive({
      req(input$date)
      
      # Cache key based on date
      cache_key <- paste0("apod_", format(input$date, "%Y%m%d"))
      
      # Get and cache API response with error handling
      tryCatch({
        response <- nasa_api_get(
          endpoint = "/planetary/apod",
          params = list(
            date = format_nasa_date(input$date)
          )
        )
        
        # Debug output
        print("API Response:")
        print(str(response))
        
        response
      }, error = function(e) {
        print(paste("Error in APOD API call:", e$message))
        NULL
      })
    })
    
    # Render image
    output$image_ui <- renderUI({
      data <- apod_data()
      req(data)
      
      if (data$media_type == "image") {
        tags$img(
          src = data$url,
          class = "img-responsive",
          alt = data$title,
          style = "max-width: 100%; height: auto;"
        )
      } else if (data$media_type == "video") {
        tags$iframe(
          src = data$url,
          frameborder = "0",
          allow = "encrypted-media",
          allowfullscreen = NA,
          width = "100%",
          height = "400px"
        )
      }
    })
    
    # Render title
    output$title <- renderUI({
      data <- apod_data()
      req(data)
      req(data$title)
      
      tags$h3(
        class = "text-primary",
        data$title
      )
    })
    
    # Render explanation
    output$explanation <- renderUI({
      data <- apod_data()
      req(data)
      req(data$explanation)
      
      tags$p(
        class = "text-justify",
        data$explanation
      )
    })
  })
}
