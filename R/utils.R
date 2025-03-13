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
  
  # Print debug info
  print(paste("Making request to:", paste0(base_url, endpoint)))
  print("Parameters:")
  print(params)
  
  # Make request
  response <- httr::GET(
    url = paste0(base_url, endpoint),
    query = params
  )
  
  # Print response status
  print(paste("Response status:", httr::status_code(response)))
  
  # Check for errors
  if (httr::http_error(response)) {
    error_message <- httr::content(response, "text", encoding = "UTF-8")
    print(paste("Error response:", error_message))
    stop(paste("NASA API error:", error_message))
  }
  
  # Parse response
  parsed <- httr::content(response, "parsed")
  
  # Print parsed response structure
  print("Parsed response structure:")
  print(str(parsed))
  
  parsed
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

#' Cache API response
#'
#' @param key Cache key
#' @param expr Expression to evaluate
#' @param timeout Cache timeout in seconds
#' @return Cached or fresh result
#' @export
cache_api_response <- function(key, expr, timeout = 3600) {
  # Create cache directory if it doesn't exist
  cache_dir <- "data/cache"
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  
  # Cache file path
  cache_file <- file.path(cache_dir, paste0(key, ".rds"))
  
  # Check if cache exists and is fresh
  if (file.exists(cache_file)) {
    cache_time <- file.info(cache_file)$mtime
    if (difftime(Sys.time(), cache_time, units = "secs") < timeout) {
      print(paste("Using cached data from:", cache_file))
      return(readRDS(cache_file))
    }
  }
  
  # Evaluate expression and cache result
  print(paste("Fetching fresh data for:", key))
  result <- eval(expr)
  
  if (!is.null(result)) {
    print(paste("Caching result to:", cache_file))
    saveRDS(result, cache_file)
  }
  
  result
}
