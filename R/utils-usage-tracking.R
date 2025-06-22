# Usage Tracking Utilities
# 
# This file contains functions for tracking KvK API usage and calculating costs.
# Only production API calls are tracked (test environment calls are ignored).
# Usage data is stored in memory for the current session only.

# Session storage for usage data
.kvkapi_session_usage <- new.env(parent = emptyenv())

#' Initialize session usage data
#' 
#' @return Data frame with correct structure
#' @keywords internal
init_session_usage <- function() {
  if (!exists("usage_data", envir = .kvkapi_session_usage)) {
    .kvkapi_session_usage$usage_data <- data.frame(
      timestamp = as.POSIXct(character(0)),
      date = as.Date(character(0)),
      year = integer(0),
      month = integer(0),
      call_type = character(0),
      test_environment = logical(0),
      stringsAsFactors = FALSE
    )
  }
  .kvkapi_session_usage$usage_data
}

#' Check if usage tracking is enabled
#' 
#' @return Logical indicating if tracking is enabled
#' @keywords internal
usage_tracking_enabled <- function() {
  # Check if explicitly disabled
  if (tolower(Sys.getenv("KVKAPI_DISABLE_TRACKING", "")) == "true") {
    return(FALSE)
  }
  
  # Default to enabled for session-based tracking
  return(TRUE)
}

#' Load usage data from session storage
#' 
#' @return Data frame with usage history from current session
#' @keywords internal
load_usage_data <- function() {
  init_session_usage()
}

#' Save usage data to session storage
#' 
#' @param usage_data Data frame with usage data to save
#' @keywords internal
save_usage_data <- function(usage_data) {
  .kvkapi_session_usage$usage_data <- usage_data
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
  # Skip recording test environment calls
  if (test_environment) {
    return(invisible(NULL))
  }
  
  # Check if tracking is enabled (will ask user if needed)
  if (!usage_tracking_enabled()) {
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
  
  # Check for alerts AFTER recording the new call
  check_usage_alerts_realtime(usage_data, call_type)
  
  invisible(NULL)
}

#' Calculate costs for API usage
#' 
#' @param usage_data Data frame with usage data
#' @param cost_per_query Numeric, cost per paid query in euros (default: 0.02)
#' @return List with cost calculations
#' @keywords internal
calculate_costs <- function(usage_data, cost_per_query = 0.02) {
  if (nrow(usage_data) == 0) {
    return(list(
      total_calls = 0,
      free_calls = 0,
      paid_calls = 0,
      total_costs = 0
    ))
  }
  
  # Filter out test environment calls and count by type
  production_data <- usage_data[!usage_data$test_environment, ]
  
  if (nrow(production_data) == 0) {
    return(list(
      total_calls = 0,
      free_calls = 0,
      paid_calls = 0,
      total_costs = 0
    ))
  }
  
  # Count calls by type
  search_calls <- sum(production_data$call_type == "search")
  paid_calls <- sum(production_data$call_type %in% c("basisprofiel", "vestigingsprofiel", "naamgeving"))
  
  # Calculate costs (only query costs, no monthly fees)
  total_costs <- paid_calls * cost_per_query
  
  list(
    total_calls = nrow(production_data),
    free_calls = search_calls,
    paid_calls = paid_calls,
    total_costs = total_costs
  )
}

#' Display usage summary
#' 
#' @param usage_data Data frame with usage data
#' @keywords internal
display_usage_summary <- function(usage_data) {
  period_text <- "Current Session"
  
  if (nrow(usage_data) == 0) {
    cli::cli_alert_info("No usage data found for {period_text}")
    return(invisible(NULL))
  }
  
  # Calculate costs
  costs <- calculate_costs(usage_data)
  
  # Display summary
  cli::cli_h2("KvK API Usage Report - {period_text}")
  cli::cli_text("")
  
  # Call counts
  cli::cli_h3("API Calls")
  cli::cli_li("Search calls (free): {costs$free_calls}")
  cli::cli_li("Profile calls (EUR 0.02 each): {costs$paid_calls}")
  cli::cli_li("Total calls: {costs$total_calls}")
  cli::cli_text("")
  
  # Cost breakdown
  cli::cli_h3("Session Costs")
  cli::cli_li("Paid API calls: {costs$paid_calls} x EUR 0.02 = EUR {sprintf('%.2f', costs$total_costs)}")
  cli::cli_text("")
}

#' Check usage alerts in real-time
#' 
#' @param usage_data Data frame with current usage data  
#' @param current_call_type Character string of the call type just made
#' @keywords internal
check_usage_alerts_realtime <- function(usage_data, current_call_type) {
  # Get current alerts from options
  max_calls <- getOption("kvkapiR.max_calls", NULL)
  max_cost <- getOption("kvkapiR.max_cost", NULL)
  
  # Only check if alerts are configured
  if (is.null(max_calls) && is.null(max_cost)) {
    return(invisible(NULL))
  }
  
  # Use all session data and filter production calls only
  session_data <- usage_data[!usage_data$test_environment, ]
  
  # Calculate current stats
  costs <- calculate_costs(session_data)
  
  # For call limits, only count paid calls (not search calls)
  paid_calls <- sum(session_data$call_type %in% c("basisprofiel", "vestigingsprofiel", "naamgeving"))
  
  # Check call limit (only for paid calls)
  if (!is.null(max_calls) && current_call_type %in% c("basisprofiel", "vestigingsprofiel", "naamgeving")) {
    if (paid_calls >= max_calls) {
      # Check if this is the first time hitting the limit
      previous_paid_calls <- paid_calls - 1
      
      if (previous_paid_calls < max_calls) {
        # First time hitting limit
        cli::cli_alert_danger("USAGE LIMIT ALERT: This \"{current_call_type}\" call brings your paid calls to {paid_calls}, reaching the limit of {max_calls} for this session!")
        cli::cli_text("Consider using `kvk_usage_report()` to review your usage.")
      } else {
        # Continued usage over limit
        cli::cli_alert_warning("Usage limit reminder: This \"{current_call_type}\" call brings your paid calls to {paid_calls}, which exceeds your limit of {max_calls} for this session.")
      }
    }
  }
  
  # Check cost limit
  if (!is.null(max_cost)) {
    # For paid calls, add their cost to the message
    call_cost <- if (current_call_type %in% c("basisprofiel", "vestigingsprofiel", "naamgeving")) " (EUR 0.02)" else ""
    
    if (costs$total_costs >= max_cost) {
      previous_costs <- calculate_costs(session_data[-nrow(session_data), ])$total_costs
      
      if (previous_costs < max_cost) {
        # First time hitting cost limit
        cli::cli_alert_danger("COST LIMIT ALERT: This \"{current_call_type}{call_cost}\" call brings your total to EUR {sprintf('%.2f', costs$total_costs)}, reaching the limit of EUR {sprintf('%.2f', max_cost)} for this session!")
      } else {
        # Continued usage over limit
        cli::cli_alert_warning("Cost limit reminder: This \"{current_call_type}{call_cost}\" call brings your total to EUR {sprintf('%.2f', costs$total_costs)}, which exceeds your limit of EUR {sprintf('%.2f', max_cost)} for this session.")
      }
    }
  }
}

#' Generate API usage report for current session
#' 
#' Display or return API usage statistics and costs for the current R session.
#' This function helps you monitor your KvK API usage and associated costs.
#' 
#' @param format Character string specifying output format:
#'   \itemize{
#'     \item \code{"summary"} (default): Human-readable console output with 
#'           costs and call counts for this session
#'     \item \code{"tibble"}: Structured summary data as a tibble
#'     \item \code{"detailed"}: Detailed data with one row per API call, suitable 
#'           for analysis and visualization
#'   }
#'   
#' @return 
#' \itemize{
#'   \item \code{format = "summary"}: Invisible NULL (output printed to console)
#'   \item \code{format = "tibble"}: A tibble with session usage summary
#'   \item \code{format = "detailed"}: A tibble with detailed call-by-call data
#' }
#' 
#' @details
#' The summary format displays API call counts and costs for the current R session only.
#' All usage data is session-based and will be reset when you restart R.
#' Search calls are free, while profile calls cost EUR 0.02 each.
#' 
#' Only production API calls are included in reports. Test environment calls are not tracked.
#' 
#' @examplesIf interactive()
#' # Display human-readable summary
#' kvk_usage_report()
#' 
#' # Get structured summary as tibble
#' summary_data <- kvk_usage_report(format = "tibble")
#' print(summary_data)
#' 
#' # Get detailed call data for custom analysis
#' call_data <- kvk_usage_report(format = "detailed")
#' head(call_data)
#' 
#' @seealso
#' \code{\link{kvk_usage_alert}} for setting usage limits,
#' \code{\link{kvk_export_usage}} for exporting data to CSV,
#' \code{\link{kvk_reset_usage}} for clearing usage history
#' 
#' @export
kvk_usage_report <- function(format = "summary") {
  # Load usage data
  usage_data <- load_usage_data()
  
  if (nrow(usage_data) == 0) {
    if (format == "summary") {
      cli::cli_alert_info("No usage data found. Make some API calls first!")
    }
    return(if (format == "summary") invisible(NULL) else data.frame())
  }
  
  if (format == "summary") {
    display_usage_summary(usage_data)
    return(invisible(NULL))
  } else if (format == "tibble") {
    return(prepare_session_usage(usage_data))
  } else if (format == "detailed") {
    # Return production calls only in detailed format
    production_data <- usage_data[!usage_data$test_environment, ]
    return(tibble::as_tibble(production_data))
  } else {
    cli::cli_abort("Invalid format. Choose 'summary', 'tibble', or 'detailed'.")
  }
}

#' Prepare session usage summary
#' 
#' @param usage_data Data frame with usage data
#' @return Tibble with session summary
#' @keywords internal
prepare_session_usage <- function(usage_data) {
  if (nrow(usage_data) == 0) {
    return(tibble::tibble(
      Search = integer(0),
      Basisprofiel = integer(0),
      Vestiging = integer(0),
      Naamgeving = integer(0),
      `Total Calls` = integer(0),
      `Costs (EUR)` = numeric(0)
    ))
  }
  
  # Filter production calls only
  production_data <- usage_data[!usage_data$test_environment, ]
  
  if (nrow(production_data) == 0) {
    return(tibble::tibble(
      Search = integer(0),
      Basisprofiel = integer(0),
      Vestiging = integer(0),
      Naamgeving = integer(0),
      `Total Calls` = integer(0),
      `Costs (EUR)` = numeric(0)
    ))
  }
  
  # Summarize session data
  search_calls <- sum(production_data$call_type == "search")
  basisprofiel_calls <- sum(production_data$call_type == "basisprofiel")
  vestiging_calls <- sum(production_data$call_type == "vestigingsprofiel")
  naamgeving_calls <- sum(production_data$call_type == "naamgeving")
  total_calls <- nrow(production_data)
  
  # Calculate costs (only paid calls)
  paid_calls <- basisprofiel_calls + vestiging_calls + naamgeving_calls
  total_cost <- paid_calls * 0.02
  
  tibble::tibble(
    Search = search_calls,
    Basisprofiel = basisprofiel_calls,
    Vestiging = vestiging_calls,
    Naamgeving = naamgeving_calls,
    `Total Calls` = total_calls,
    `Costs (EUR)` = total_cost
  )
}

#' Set usage and cost alerts for current session
#' 
#' Configure automatic alerts to monitor your KvK API usage within the current
#' R session. Alerts are triggered in real-time when you exceed the specified 
#' limits during API calls.
#' 
#' @param max_calls Integer maximum number of paid API calls before triggering alerts,
#'   or NULL to disable call alerts. Only basisprofiel, vestigingsprofiel, and 
#'   naamgeving calls count towards this limit. Search calls are free and do not count. 
#'   When reached, you'll see a danger alert. Continued usage beyond this limit will 
#'   show reminder warnings.
#' @param max_cost Numeric maximum cost in euros before triggering alerts, or NULL
#'   to disable cost alerts. For example, 5.45 represents EUR 5.45. Costs are 
#'   calculated as EUR 0.02 per paid API call (search calls are free).
#'   
#' @details
#' Usage alerts help you control API consumption within your current R session.
#' When limits are exceeded, the package displays informative messages:
#' 
#' \itemize{
#'   \item \strong{First alert}: Danger alert when the limit is first reached
#'   \item \strong{Reminder alerts}: Warning messages for continued usage over limits
#' }
#' 
#' Alert settings are stored in R options and apply only to the current session.
#' All alert configurations are automatically reset when you restart R.
#' To disable all alerts within the current session, call the function with both
#' parameters set to NULL: \code{kvk_usage_alert()}.
#' 
#' Only production API calls count towards limits. Test environment calls are ignored.
#' 
#' @return No return value, called for side effects. Sets usage alert options
#'   for the current session.
#' 
#' @examplesIf interactive()
#' # Set session limit for paid calls only (search calls don't count)
#' kvk_usage_alert(max_calls = 100)
#' 
#' # Set session cost limit to EUR 5.00
#' kvk_usage_alert(max_cost = 5.00)
#' 
#' # Set both paid call and cost limits (EUR 10.50)
#' kvk_usage_alert(max_calls = 500, max_cost = 10.50)
#' 
#' # Disable all alerts for this session
#' kvk_usage_alert()
#' 
#' @seealso
#' \code{\link{kvk_usage_report}} for viewing current usage,
#' \code{\link{kvk_export_usage}} for exporting usage data,
#' \code{\link{kvk_reset_usage}} for clearing usage history
#' 
#' @export
kvk_usage_alert <- function(max_calls = NULL, max_cost = NULL) {
  # Validate max_cost is numeric if provided
  if (!is.null(max_cost) && !is.numeric(max_cost)) {
    cli::cli_abort("max_cost must be a numeric value (e.g., 5.45 for EUR 5.45)")
  }
  
  # Store settings in options
  options(
    kvkapiR.max_calls = max_calls,
    kvkapiR.max_cost = max_cost
  )
  
  # Display confirmation
  if (is.null(max_calls) && is.null(max_cost)) {
    cli::cli_alert_success("All usage alerts have been disabled for this session")
  } else {
    if (!is.null(max_calls)) {
      cli::cli_alert_success("Max paid calls: {max_calls} for this session")
    }
    if (!is.null(max_cost)) {
      cli::cli_alert_success("Max cost: EUR {sprintf('%.2f', max_cost)} for this session")
    }
  }
}

#' Export session usage data to CSV file
#' 
#' Export your KvK API usage data from the current session to a CSV file 
#' for external analysis, reporting, or record keeping. Choose between 
#' summary data or detailed call-by-call data.
#' 
#' @param file_path Character string specifying the path where the CSV file
#'   should be saved. Can include relative or absolute paths. The file will
#'   be created if it doesn't exist, or overwritten if it does.
#' @param format Character string specifying the export format:
#'   \itemize{
#'     \item \code{"summary"} (default): Session summary with call counts by type
#'           and total costs. Ideal for quick reporting and budget tracking.
#'     \item \code{"detailed"}: One row per API call with detailed information
#'           including timestamp, date, call type, and other metadata. Perfect for
#'           creating custom visualizations and detailed analysis.
#'   }
#'   
#' @details
#' The exported data only includes production API calls from the current R session. 
#' Test environment calls are not included in the export. All data is session-based
#' and will be lost when you restart R.
#' 
#' \strong{Summary format} includes these columns:
#' \itemize{
#'   \item \code{Search}, \code{Basisprofiel}, \code{Vestiging}, \code{Naamgeving}: Call counts by type
#'   \item \code{Total Calls}: Total number of API calls in this session
#'   \item \code{Costs (EUR)}: Session costs (EUR 0.02 per paid call)
#' }
#' 
#' \strong{Detailed format} includes these columns:
#' \itemize{
#'   \item \code{timestamp}: Exact time of the API call
#'   \item \code{date}: Date of the API call  
#'   \item \code{year}, \code{month}: Extracted date components
#'   \item \code{call_type}: Type of API call (search, basisprofiel, etc.)
#'   \item \code{test_environment}: Always FALSE for production calls
#' }
#' 
#' @return Invisibly returns NULL. The function is called for its side effect
#'   of creating a CSV file.
#'   
#' @examplesIf interactive()
#' # Export session summary (default)
#' kvk_export_usage("session_summary.csv")
#' 
#' # Export detailed call data for analysis
#' kvk_export_usage("detailed_calls.csv", format = "detailed")
#' 
#' # Export to specific directory
#' kvk_export_usage("reports/session_data.csv", format = "summary")
#' 
#' @seealso
#' \code{\link{kvk_usage_report}} for viewing usage in R,
#' \code{\link{kvk_usage_alert}} for setting usage limits,
#' \code{\link{kvk_reset_usage}} for clearing usage history
#' 
#' @export
kvk_export_usage <- function(file_path, format = "summary") {
  usage_data <- load_usage_data()
  
  if (nrow(usage_data) == 0) {
    cli::cli_alert_info("No usage data to export from this session.")
    return(invisible(NULL))
  }
  
  if (format == "summary") {
    # Export session summary
    summary_data <- prepare_session_usage(usage_data)
    utils::write.csv(summary_data, file_path, row.names = FALSE)
    cli::cli_alert_success("Session summary exported to: '{file_path}'")
    cli::cli_text("Format: \"summary\"")
    cli::cli_text("Records: {nrow(summary_data)}")
  } else if (format == "detailed") {
    # Export all production calls
    production_data <- usage_data[!usage_data$test_environment, ]
    utils::write.csv(production_data, file_path, row.names = FALSE)
    cli::cli_alert_success("Session usage data exported to: '{file_path}'")
    cli::cli_text("Format: \"detailed\"")
    cli::cli_text("Records: {nrow(production_data)}")
  } else {
    cli::cli_abort("Invalid format. Choose 'summary' or 'detailed'.")
  }
}

#' Reset session usage tracking data
#' 
#' Clear all API usage data from the current R session. This action cannot be undone
#' and will reset your session usage history including call counts, timestamps,
#' and cost calculations.
#' 
#' @details
#' This function clears the session-based usage tracking data stored in memory.
#' After reset:
#' \itemize{
#'   \item All session usage history is cleared
#'   \item \code{\link{kvk_usage_report}} will show no data
#'   \item Session cost calculations restart from zero
#'   \item Usage alerts continue with the new baseline
#' }
#' 
#' The function only affects session usage data. It does not change:
#' \itemize{
#'   \item Usage alert settings (these are stored in R options)
#'   \item API key configuration
#'   \item Package preferences
#' }
#' 
#' This can be useful for:
#' \itemize{
#'   \item Starting fresh tracking within a long R session
#'   \item Clearing test data before production use
#'   \item Resetting session counters for specific analyses
#'   \item Troubleshooting usage tracking issues
#' }
#' 
#' @return Invisibly returns NULL. The function is called for its side effect
#'   of clearing session usage data.
#'   
#' @examplesIf interactive()
#' # Clear session usage history
#' kvk_reset_usage()
#' 
#' # Verify reset worked
#' kvk_usage_report()  # Should show "No usage data found"
#' 
#' # Usage tracking will restart with new API calls
#' results <- kvk_search(plaats = "Amsterdam")
#' kvk_usage_report()  # Will show the new call
#' 
#' @seealso
#' \code{\link{kvk_usage_report}} for viewing current usage,
#' \code{\link{kvk_export_usage}} for exporting session data,
#' \code{\link{kvk_usage_alert}} for setting usage limits
#' 
#' @export
kvk_reset_usage <- function() {
  # Reset session storage
  if (exists("usage_data", envir = .kvkapi_session_usage)) {
    .kvkapi_session_usage$usage_data <- data.frame(
      timestamp = as.POSIXct(character(0)),
      date = as.Date(character(0)),
      year = integer(0),
      month = integer(0),
      call_type = character(0),
      test_environment = logical(0),
      stringsAsFactors = FALSE
    )
    cli::cli_alert_success("Session usage data has been reset.")
  } else {
    cli::cli_alert_info("No usage data found to reset.")
  }
}