# Utility functions for Space Exploration Dashboard

#' Make a GET request to NASA API
#'
#' @param endpoint API endpoint
#' @param params Additional query parameters
#' @return API response
#' @export
nasa_api_get <- function(endpoint, params = list()) {
  base_url <- "https://api.nasa.gov"
  
  # Get API key from config and add to params
  config <- config::get()
  params <- c(params, list(api_key = config$nasa_api_key))
  
  # Make request
  response <- httr::GET(
    url = paste0(base_url, endpoint),
    query = params
  )
  
  # Check for errors
  if (httr::http_error(response)) {
    error_message <- httr::content(response, "text", encoding = "UTF-8")
    stop(paste("NASA API error:", error_message))
  }
  
  # Parse response
  httr::content(response, "parsed")
}

#' Format date for NASA API
#'
#' @param date Date object
#' @return Formatted date string (YYYY-MM-DD)
#' @export
format_nasa_date <- function(date) {
  format(date, "%Y-%m-%d")
}

#' Handle API errors gracefully
#'
#' @param expr Expression to evaluate
#' @return Result or error message
#' @export
handle_api_error <- function(expr) {
  tryCatch(
    expr,
    error = function(e) {
      message("API Error: ", e$message)
      NULL
    },
    warning = function(w) {
      message("Warning: ", w$message)
      NULL
    }
  )
}