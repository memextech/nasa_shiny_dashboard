# Near-Earth Objects (NEO) Tracker Module

#' NEO Tracker UI Module
#'
#' @param id Module ID
#' @export
neoTrackerUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(12,
        div(class = "well",
          h3("Near-Earth Objects Tracker"),
          
          # Date range selector
          dateRangeInput(
            ns("date_range"),
            "Select Date Range:",
            start = Sys.Date() - 7,
            end = Sys.Date(),
            max = Sys.Date()
          ),
          
          # Hazard filter
          checkboxInput(
            ns("hazardous_only"),
            "Show potentially hazardous asteroids only",
            FALSE
          )
        )
      )
    ),
    
    # Stats cards
    fluidRow(
      column(4,
        div(class = "well",
          h4("Total NEOs", class = "text-center"),
          h2(textOutput(ns("total_neo_count")), class = "text-center"),
          div(icon("meteor"), class = "text-center")
        )
      ),
      column(4,
        div(class = "well",
          h4("Hazardous", class = "text-center"),
          h2(textOutput(ns("hazardous_count")), class = "text-center"),
          div(icon("exclamation-triangle"), class = "text-center")
        )
      ),
      column(4,
        div(class = "well",
          h4("Closest Approach", class = "text-center"),
          h2(textOutput(ns("closest_distance")), class = "text-center"),
          div(icon("ruler"), class = "text-center")
        )
      )
    ),
    
    fluidRow(
      column(12,
        div(class = "well",
          # NEO data table
          DT::dataTableOutput(ns("neo_table"))
        )
      )
    ),
    
    fluidRow(
      column(12,
        div(class = "well",
          # Plot
          plotlyOutput(ns("neo_plot"))
        )
      )
    )
  )
}

#' NEO Tracker Server Module
#'
#' @param id Module ID
#' @param config Configuration object
#' @export
neoTrackerServer <- function(id, config) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive expression for NEO data
    neo_data <- reactive({
      req(input$date_range)
      
      # Get API response
      data <- nasa_api_get(
        endpoint = "/neo/rest/v1/feed",
        params = list(
          start_date = format_nasa_date(input$date_range[1]),
          end_date = format_nasa_date(input$date_range[2])
        )
      )
      
      # Process data
      neo_list <- list()
      for (date in names(data$near_earth_objects)) {
        for (neo in data$near_earth_objects[[date]]) {
          neo_list[[length(neo_list) + 1]] <- list(
            date = date,
            id = neo$id,
            name = neo$name,
            diameter_min = neo$estimated_diameter$kilometers$estimated_diameter_min,
            diameter_max = neo$estimated_diameter$kilometers$estimated_diameter_max,
            hazardous = neo$is_potentially_hazardous_asteroid,
            miss_distance = as.numeric(neo$close_approach_data[[1]]$miss_distance$kilometers),
            velocity = as.numeric(neo$close_approach_data[[1]]$relative_velocity$kilometers_per_hour)
          )
        }
      }
      
      # Convert to data frame
      df <- do.call(rbind, lapply(neo_list, as.data.frame))
      df$date <- as.Date(df$date)
      df
    })
    
    # Filtered data
    filtered_data <- reactive({
      data <- neo_data()
      if (input$hazardous_only) {
        data <- data[data$hazardous, ]
      }
      data
    })
    
    # Stats outputs
    output$total_neo_count <- renderText({
      data <- filtered_data()
      nrow(data)
    })
    
    output$hazardous_count <- renderText({
      data <- filtered_data()
      sum(data$hazardous)
    })
    
    output$closest_distance <- renderText({
      data <- filtered_data()
      paste(round(min(data$miss_distance) / 1000, 2), "km")
    })
    
    # Data table
    output$neo_table <- DT::renderDataTable({
      data <- filtered_data()
      DT::datatable(
        data,
        options = list(pageLength = 10),
        rownames = FALSE
      )
    })
    
    # Plot
    output$neo_plot <- renderPlotly({
      data <- filtered_data()
      
      plot_ly(data) %>%
        add_trace(
          x = ~date,
          y = ~miss_distance,
          size = ~diameter_max,
          color = ~hazardous,
          type = "scatter",
          mode = "markers",
          text = ~paste(
            "Name:", name,
            "<br>Date:", date,
            "<br>Distance:", round(miss_distance, 2), "km",
            "<br>Diameter:", round(diameter_max, 2), "km",
            "<br>Velocity:", round(velocity, 2), "km/h"
          ),
          hoverinfo = "text"
        ) %>%
        layout(
          title = "NEO Close Approaches",
          xaxis = list(title = "Date"),
          yaxis = list(title = "Miss Distance (km)")
        )
    })
  })
}
