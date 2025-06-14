# Usage Tracking Utilities
# 
# This file contains functions for tracking KvK API usage and calculating costs.
# Only production API calls are tracked (test environment calls are ignored).

#' Get the path to the usage tracking file
#' 
#' @return Character string with the full path to the usage file
#' @keywords internal
get_usage_file_path <- function() {
  # Create ~/.kvkapiR directory if it doesn't exist
  home_dir <- Sys.getenv("HOME")
  if (home_dir == "") {
    # Fallback for Windows
    home_dir <- Sys.getenv("USERPROFILE")
  }
  
  kvk_dir <- file.path(home_dir, ".kvkapiR")
  if (!dir.exists(kvk_dir)) {
    dir.create(kvk_dir, recursive = TRUE)
  }
  
  file.path(kvk_dir, "usage.rds")
}

#' Load usage data from disk
#' 
#' @return Data frame with usage history, or empty data frame if no data exists
#' @keywords internal
load_usage_data <- function() {
  usage_file <- get_usage_file_path()
  
  if (!file.exists(usage_file)) {
    # Return empty data frame with correct structure
    return(data.frame(
      timestamp = as.POSIXct(character(0)),
      date = as.Date(character(0)),
      year = integer(0),
      month = integer(0),
      call_type = character(0),
      test_environment = logical(0),
      stringsAsFactors = FALSE
    ))
  }
  
  readRDS(usage_file)
}

#' Save usage data to disk
#' 
#' @param usage_data Data frame with usage data
#' @keywords internal
save_usage_data <- function(usage_data) {
  usage_file <- get_usage_file_path()
  saveRDS(usage_data, usage_file)
}

#' Record an API call for usage tracking
#' 
#' This function is called internally by KvK API functions to track usage
#' for cost calculation. Only production API calls are recorded.
#' 
#' @param call_type Character string indicating the type of API call
#'   ("search", "basisprofiel", "vestigingsprofiel", "naamgeving")
#' @param test_environment Logical indicating if this was a test environment call
#' @keywords internal
record_api_call <- function(call_type, test_environment = FALSE) {
  # Skip recording if tracking is disabled
  if (Sys.getenv("KVKAPI_DISABLE_TRACKING", "false") == "true") {
    return(invisible(NULL))
  }
  
  # Skip recording test environment calls
  if (test_environment) {
    return(invisible(NULL))
  }
  
  # Load existing data
  usage_data <- load_usage_data()
  
  # Create new record
  now <- Sys.time()
  new_record <- data.frame(
    timestamp = now,
    date = as.Date(now),
    year = as.integer(format(now, "%Y")),
    month = as.integer(format(now, "%m")),
    call_type = call_type,
    test_environment = test_environment,
    stringsAsFactors = FALSE
  )
  
  # Append to existing data
  usage_data <- rbind(usage_data, new_record)
  
  # Save updated data
  save_usage_data(usage_data)
  
  invisible(NULL)
}

#' Calculate costs for API usage
#' 
#' @param usage_data Data frame with usage data
#' @param monthly_fee Numeric, monthly API key connection fee in euros (default: 6.20)
#' @param cost_per_query Numeric, cost per paid query in euros (default: 0.02)
#' @return List with cost calculations
#' @keywords internal
calculate_costs <- function(usage_data, monthly_fee = 6.20, cost_per_query = 0.02) {
  if (nrow(usage_data) == 0) {
    return(list(
      total_calls = 0,
      free_calls = 0,
      paid_calls = 0,
      query_costs = 0,
      base_costs = 0,
      total_costs = 0,
      months_active = 0
    ))
  }
  
  # Filter out test environment calls (should already be filtered, but be safe)
  usage_data <- usage_data[!usage_data$test_environment, ]
  
  # Count calls by type
  call_counts <- table(usage_data$call_type)
  
  free_calls <- ifelse("search" %in% names(call_counts), call_counts[["search"]], 0)
  paid_calls <- sum(call_counts[names(call_counts) != "search"])
  total_calls <- free_calls + paid_calls
  
  # Calculate months active (unique year-month combinations)
  months_active <- length(unique(paste(usage_data$year, usage_data$month, sep = "-")))
  
  # Calculate costs
  base_costs <- months_active * monthly_fee
  query_costs <- paid_calls * cost_per_query
  total_costs <- base_costs + query_costs
  
  list(
    total_calls = total_calls,
    free_calls = free_calls,
    paid_calls = paid_calls,
    query_costs = query_costs,
    base_costs = base_costs,
    total_costs = total_costs,
    months_active = months_active
  )
}