# Usage Tracking Utilities
# 
# This file contains functions for tracking KvK API usage and calculating costs.
# Only production API calls are tracked (test environment calls are ignored).
# This system is CRAN-compliant with opt-in consent and proper user directory usage.

#' Get the path to the usage tracking file
#' 
#' @return Character string with the full path to the usage file
#' @keywords internal
get_usage_file_path <- function() {
  # Use tools::R_user_dir for R >= 4.0.0, fallback to old method for older versions
  if (getRversion() >= "4.0.0") {
    kvk_dir <- tools::R_user_dir("kvkapiR", "data")
  } else {
    # Fallback for R < 4.0.0
    home_dir <- Sys.getenv("HOME")
    if (home_dir == "") {
      home_dir <- Sys.getenv("USERPROFILE")
    }
    kvk_dir <- file.path(home_dir, ".kvkapiR")
  }
  
  file.path(kvk_dir, "usage.rds")
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
  
  # Check if user has opted in (stored in package environment)
  opt_in <- getOption("kvkapiR.usage_tracking_opted_in", NULL)
  if (!is.null(opt_in)) {
    return(opt_in)
  }
  
  # If no preference stored and in interactive session, ask user
  if (interactive()) {
    return(ask_usage_tracking_permission())
  }
  
  # Default to disabled in non-interactive sessions
  return(FALSE)
}

#' Ask user for permission to enable usage tracking
#' 
#' @return Logical indicating user's choice
#' @keywords internal
ask_usage_tracking_permission <- function() {
  cat("\n")
  cli::cli_h3("kvkapiR Usage Tracking")
  cli::cli_text("This package can track API usage to help you monitor costs.")
  cli::cli_text("Data is stored locally in your user directory.")
  cli::cli_text("No data is sent to external servers.")
  cat("\n")
  
  response <- readline("Enable usage tracking? (yes/no): ")
  response <- tolower(trimws(response))
  
  enabled <- response %in% c("yes", "y", "ja", "j")
  
  # Store the preference
  options(kvkapiR.usage_tracking_opted_in = enabled)
  
  if (enabled) {
    cli::cli_alert_success("Usage tracking enabled. You can disable it anytime with:")
    cli::cli_code("Sys.setenv(KVKAPI_DISABLE_TRACKING = 'true')")
    
    # Create directory now that user has consented
    kvk_dir <- dirname(get_usage_file_path())
    if (!dir.exists(kvk_dir)) {
      dir.create(kvk_dir, recursive = TRUE, showWarnings = FALSE)
    }
  } else {
    cli::cli_alert_info("Usage tracking disabled. You can enable it anytime with:")
    cli::cli_code("options(kvkapiR.usage_tracking_opted_in = TRUE)")
  }
  
  return(enabled)
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
#' @param usage_data Data frame with usage data to save
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
  
  # Filter out test environment calls and count by type
  production_data <- usage_data[!usage_data$test_environment, ]
  
  if (nrow(production_data) == 0) {
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
  
  # Count calls by type
  search_calls <- sum(production_data$call_type == "search")
  paid_calls <- sum(production_data$call_type %in% c("basisprofiel", "vestigingsprofiel", "naamgeving"))
  
  # Calculate unique months of activity
  unique_months <- length(unique(paste(production_data$year, production_data$month, sep = "-")))
  
  # Calculate costs
  query_costs <- paid_calls * cost_per_query
  base_costs <- unique_months * monthly_fee
  total_costs <- query_costs + base_costs
  
  list(
    total_calls = nrow(production_data),
    free_calls = search_calls,
    paid_calls = paid_calls,
    query_costs = query_costs,
    base_costs = base_costs,
    total_costs = total_costs,
    months_active = unique_months
  )
}

#' Display usage summary
#' 
#' @param usage_data Data frame with usage data
#' @param year Optional year to filter by
#' @param month Optional month to filter by
#' @keywords internal
display_usage_summary <- function(usage_data, year = NULL, month = NULL) {
  if (!is.null(year) && !is.null(month)) {
    usage_data <- usage_data[usage_data$year == year & usage_data$month == month, ]
    period_text <- paste(month.name[month], year)
  } else if (!is.null(year)) {
    usage_data <- usage_data[usage_data$year == year, ]
    period_text <- as.character(year)
  } else {
    period_text <- "All time"
  }
  
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
  cli::cli_h3("Costs")
  cli::cli_li("Monthly base fee: EUR {sprintf('%.2f', costs$base_costs)}")
  cli::cli_li("Query costs: EUR {sprintf('%.2f', costs$query_costs)}")
  cli::cli_li("Total costs: EUR {sprintf('%.2f', costs$total_costs)}")
  cli::cli_text("")
  
  if (costs$months_active > 0) {
    cli::cli_text("Active months: {costs$months_active}")
  }
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
  alert_period <- getOption("kvkapiR.alert_period", "month")
  
  # Only check if alerts are configured
  if (is.null(max_calls) && is.null(max_cost)) {
    return(invisible(NULL))
  }
  
  # Filter data based on alert period
  now <- Sys.time()
  current_year <- as.integer(format(now, "%Y"))
  current_month <- as.integer(format(now, "%m"))
  
  if (alert_period == "month") {
    period_data <- usage_data[usage_data$year == current_year & usage_data$month == current_month, ]
    period_text <- paste(month.name[current_month], current_year)
  } else if (alert_period == "year") {
    period_data <- usage_data[usage_data$year == current_year, ]
    period_text <- as.character(current_year)
  } else {
    period_data <- usage_data
    period_text <- "total"
  }
  
  # Filter production calls only
  period_data <- period_data[!period_data$test_environment, ]
  
  # Calculate current stats
  total_calls <- nrow(period_data)
  costs <- calculate_costs(period_data)
  
  # Check call limit
  if (!is.null(max_calls) && total_calls >= max_calls) {
    # Check if this is the first time hitting the limit
    previous_calls <- total_calls - 1
    
    if (previous_calls < max_calls) {
      # First time hitting limit
      cli::cli_alert_danger("USAGE LIMIT ALERT: This \"{current_call_type} (EUR 0.02)\" call brings your total to {total_calls} calls, reaching the limit of {max_calls} for {period_text}!")
      cli::cli_text("Consider using `kvk_usage_report()` to review your usage.")
    } else {
      # Continued usage over limit
      cli::cli_alert_warning("Usage limit reminder: This \"{current_call_type} (EUR 0.02)\" call brings your total to {total_calls} calls, which exceeds your limit of {max_calls} for {period_text}.")
    }
  }
  
  # Check cost limit
  if (!is.null(max_cost)) {
    cost_limit_num <- as.numeric(gsub("[^0-9.]", "", max_cost))
    
    if (costs$total_costs >= cost_limit_num) {
      previous_costs <- calculate_costs(period_data[-nrow(period_data), ])$total_costs
      
      if (previous_costs < cost_limit_num) {
        # First time hitting cost limit
        cli::cli_alert_danger("Cost alert: \"EUR {sprintf('%.2f', costs$total_costs)}\" exceeds limit of \"{max_cost}\" for {period_text}")
      }
    }
  }
}

#' Generate API usage report
#' 
#' Display or return API usage statistics and costs in various formats.
#' This function helps you monitor your KvK API usage and associated costs.
#' 
#' @param format Character string specifying output format:
#'   \itemize{
#'     \item \code{"summary"} (default): Human-readable console output with 
#'           costs and call counts
#'     \item \code{"tibble"}: Monthly aggregated data as a tibble, one row per month
#'     \item \code{"tidy"}: Detailed data with one row per API call, suitable 
#'           for analysis and visualization
#'   }
#' @param year Integer year to filter results (e.g., 2025). If NULL, shows all years.
#' @param month Integer month (1-12) to filter results. Only used when year is also specified.
#'   
#' @return 
#' \itemize{
#'   \item \code{format = "summary"}: Invisible NULL (output printed to console)
#'   \item \code{format = "tibble"}: A tibble with monthly usage summary
#'   \item \code{format = "tidy"}: A tibble with detailed call-by-call data
#' }
#' 
#' @details
#' The summary format displays:
#' \itemize{
#'   \item API call counts by type (search calls are free)
#'   \item Cost breakdown including monthly base fee (EUR 6.20) and query costs (EUR 0.02 per profile call)
#'   \item Total costs and number of active months
#' }
#' 
#' The tibble format provides monthly aggregated data suitable for reporting and analysis.
#' The tidy format provides raw call data perfect for creating custom visualizations.
#' 
#' Only production API calls are included in reports. Test environment calls are not tracked or reported.
#' 
#' @examplesIf interactive()
#' # Display human-readable summary
#' kvk_usage_report()
#' 
#' # Get monthly summary as tibble
#' monthly_data <- kvk_usage_report(format = "tibble")
#' print(monthly_data)
#' 
#' # Get detailed call data for visualization
#' library(ggplot2)
#' call_data <- kvk_usage_report(format = "tidy")
#' ggplot(call_data, aes(x = date, fill = call_type)) +
#'   geom_bar() +
#'   labs(title = "API Usage Over Time")
#'   
#' # View specific month
#' kvk_usage_report(year = 2025, month = 6)
#' 
#' # View entire year
#' kvk_usage_report(year = 2025)
#' 
#' @seealso
#' \code{\link{kvk_usage_alert}} for setting usage limits,
#' \code{\link{kvk_export_usage}} for exporting data to CSV,
#' \code{\link{kvk_reset_usage}} for clearing usage history
#' 
#' @export
kvk_usage_report <- function(format = "summary", year = NULL, month = NULL) {
  # Load usage data
  usage_data <- load_usage_data()
  
  if (nrow(usage_data) == 0) {
    if (format == "summary") {
      cli::cli_alert_info("No usage data found. Make some API calls first!")
    }
    return(if (format == "summary") invisible(NULL) else data.frame())
  }
  
  # Filter by year/month if specified
  filtered_data <- usage_data
  if (!is.null(year)) {
    filtered_data <- filtered_data[filtered_data$year == year, ]
  }
  if (!is.null(month)) {
    filtered_data <- filtered_data[filtered_data$month == month, ]
  }
  
  if (format == "summary") {
    display_usage_summary(filtered_data, year, month)
    return(invisible(NULL))
  } else if (format == "tibble") {
    return(prepare_monthly_usage(filtered_data))
  } else if (format == "tidy") {
    # Return production calls only in tidy format
    production_data <- filtered_data[!filtered_data$test_environment, ]
    return(tibble::as_tibble(production_data))
  } else {
    cli::cli_abort("Invalid format. Choose 'summary', 'tibble', or 'tidy'.")
  }
}

#' Prepare monthly usage summary
#' 
#' @param usage_data Data frame with usage data
#' @return Tibble with monthly summary
#' @keywords internal
prepare_monthly_usage <- function(usage_data) {
  if (nrow(usage_data) == 0) {
    return(tibble::tibble(
      Year = integer(0),
      Month = character(0),
      Search = integer(0),
      Basisprofiel = integer(0),
      Vestiging = integer(0),
      Naamgeving = integer(0),
      `Costs (EUR)` = numeric(0)
    ))
  }
  
  # Filter production calls only
  production_data <- usage_data[!usage_data$test_environment, ]
  
  if (nrow(production_data) == 0) {
    return(tibble::tibble(
      Year = integer(0),
      Month = character(0),
      Search = integer(0),
      Basisprofiel = integer(0),
      Vestiging = integer(0),
      Naamgeving = integer(0),
      `Costs (EUR)` = numeric(0)
    ))
  }
  
  # Group by year and month
  monthly_summary <- production_data |>
    dplyr::mutate(year_month = paste(year, sprintf("%02d", month), sep = "-")) |>
    dplyr::group_by(year, month, year_month) |>
    dplyr::summarise(
      search = sum(call_type == "search"),
      basisprofiel = sum(call_type == "basisprofiel"),
      vestigingsprofiel = sum(call_type == "vestigingsprofiel"),
      naamgeving = sum(call_type == "naamgeving"),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      month_name = month.name[month],
      paid_calls = basisprofiel + vestigingsprofiel + naamgeving,
      query_costs = paid_calls * 0.02,
      base_cost = 6.20,
      total_cost = query_costs + base_cost
    ) |>
    dplyr::select(
      Year = year,
      Month = month_name,
      Search = search,
      Basisprofiel = basisprofiel,
      Vestiging = vestigingsprofiel,
      Naamgeving = naamgeving,
      `Costs (EUR)` = total_cost
    )
  
  tibble::as_tibble(monthly_summary)
}

#' Set usage and cost alerts
#' 
#' Configure automatic alerts to monitor your KvK API usage and prevent
#' unexpected costs. Alerts are triggered in real-time when you exceed
#' the specified limits during API calls.
#' 
#' @param max_calls Integer maximum number of API calls before triggering alerts.
#'   When reached, you'll see a danger alert. Continued usage beyond this limit
#'   will show reminder warnings.
#' @param max_cost Numeric or character maximum cost before triggering alerts.
#'   Can be specified as a number (e.g., 25) or string (e.g., "EUR 25").
#'   Costs include both the monthly base fee (EUR 6.20) and per-query charges (EUR 0.02).
#' @param period Character string specifying the alert period:
#'   \itemize{
#'     \item \code{"month"} (default): Monitor usage within each calendar month
#'     \item \code{"year"}: Monitor usage within each calendar year  
#'     \item \code{"total"}: Monitor cumulative usage across all time
#'   }
#'   
#' @details
#' Usage alerts help you stay within budget and monitor API consumption patterns.
#' When limits are exceeded, the package displays informative messages:
#' 
#' \itemize{
#'   \item \strong{First alert}: Danger alert when the limit is first reached
#'   \item \strong{Reminder alerts}: Warning messages for continued usage over limits
#' }
#' 
#' Alert settings are stored in R options for the current session and will apply
#' to all subsequent API calls. You must specify at least one of \code{max_calls}
#' or \code{max_cost}.
#' 
#' Only production API calls count towards limits. Test environment calls are ignored.
#' 
#' @examplesIf interactive()
#' # Set monthly call limit
#' kvk_usage_alert(max_calls = 100, period = "month")
#' 
#' # Set monthly cost limit  
#' kvk_usage_alert(max_cost = 25, period = "month")
#' 
#' # Set both call and cost limits
#' kvk_usage_alert(max_calls = 500, max_cost = "EUR 50", period = "month")
#' 
#' # Set yearly limits for annual budget planning
#' kvk_usage_alert(max_cost = 300, period = "year")
#' 
#' # Monitor total cumulative usage
#' kvk_usage_alert(max_calls = 1000, period = "total")
#' 
#' @seealso
#' \code{\link{kvk_usage_report}} for viewing current usage,
#' \code{\link{kvk_export_usage}} for exporting usage data,
#' \code{\link{kvk_reset_usage}} for clearing usage history
#' 
#' @export
kvk_usage_alert <- function(max_calls = NULL, max_cost = NULL, period = "month") {
  if (is.null(max_calls) && is.null(max_cost)) {
    cli::cli_abort("Must specify either max_calls or max_cost (or both)")
  }
  
  if (!period %in% c("month", "year", "total")) {
    cli::cli_abort("period must be 'month', 'year', or 'total'")
  }
  
  # Store settings in options
  options(
    kvkapiR.max_calls = max_calls,
    kvkapiR.max_cost = max_cost,
    kvkapiR.alert_period = period
  )
  
  # Display confirmation
  if (!is.null(max_calls)) {
    cli::cli_alert_success("Max calls: {max_calls} per {period}")
  }
  if (!is.null(max_cost)) {
    cli::cli_alert_success("Max cost: \"{max_cost}\" per {period}")
  }
}

#' Export usage data to CSV file
#' 
#' Export your KvK API usage data to a CSV file for external analysis,
#' reporting, or record keeping. Choose between detailed call-by-call data
#' or monthly aggregated summaries.
#' 
#' @param file_path Character string specifying the path where the CSV file
#'   should be saved. Can include relative or absolute paths. The file will
#'   be created if it doesn't exist, or overwritten if it does.
#' @param format Character string specifying the export format:
#'   \itemize{
#'     \item \code{"tidy"} (default): One row per API call with detailed information
#'           including timestamp, date, call type, and other metadata. Perfect for
#'           creating custom visualizations and detailed analysis.
#'     \item \code{"monthly"}: Aggregated monthly summary with call counts by type
#'           and cost calculations. Ideal for reporting and budget tracking.
#'   }
#'   
#' @details
#' The exported data only includes production API calls. Test environment calls
#' are not included in the export.
#' 
#' \strong{Tidy format} includes these columns:
#' \itemize{
#'   \item \code{timestamp}: Exact time of the API call
#'   \item \code{date}: Date of the API call  
#'   \item \code{year}, \code{month}: Extracted date components
#'   \item \code{call_type}: Type of API call (search, basisprofiel, etc.)
#'   \item \code{test_environment}: Always FALSE for production calls
#' }
#' 
#' \strong{Monthly format} includes these columns:
#' \itemize{
#'   \item \code{Year}, \code{Month}: Time period
#'   \item \code{Search}, \code{Basisprofiel}, \code{Vestiging}, \code{Naamgeving}: Call counts by type
#'   \item \code{Costs (EUR)}: Total costs including base fee and query charges
#' }
#' 
#' @return Invisibly returns NULL. The function is called for its side effect
#'   of creating a CSV file.
#'   
#' @examplesIf interactive()
#' # Export detailed call data for analysis
#' kvk_export_usage("my_api_usage.csv", format = "tidy")
#' 
#' # Export monthly summary for reporting
#' kvk_export_usage("monthly_report.csv", format = "monthly")
#' 
#' # Export to specific directory
#' kvk_export_usage("reports/2025_usage.csv", format = "tidy")
#' 
#' # Use in data analysis workflow
#' kvk_export_usage("temp_usage.csv", format = "tidy")
#' usage_data <- read.csv("temp_usage.csv")
#' # Perform custom analysis...
#' 
#' @seealso
#' \code{\link{kvk_usage_report}} for viewing usage in R,
#' \code{\link{kvk_usage_alert}} for setting usage limits,
#' \code{\link{kvk_reset_usage}} for clearing usage history
#' 
#' @export
kvk_export_usage <- function(file_path, format = "tidy") {
  usage_data <- load_usage_data()
  
  if (nrow(usage_data) == 0) {
    cli::cli_alert_info("No usage data to export.")
    return(invisible(NULL))
  }
  
  if (format == "tidy") {
    # Export all production calls
    production_data <- usage_data[!usage_data$test_environment, ]
    utils::write.csv(production_data, file_path, row.names = FALSE)
    cli::cli_alert_success("Usage data exported to: '{file_path}'")
    cli::cli_text("Format: \"tidy\"")
    cli::cli_text("Records: {nrow(production_data)}")
  } else if (format == "monthly") {
    # Export monthly summary
    monthly_data <- prepare_monthly_usage(usage_data)
    utils::write.csv(monthly_data, file_path, row.names = FALSE)
    cli::cli_alert_success("Usage data exported to: '{file_path}'")
    cli::cli_text("Format: \"monthly\"")
    cli::cli_text("Records: {nrow(monthly_data)}")
  } else {
    cli::cli_abort("Invalid format. Choose 'tidy' or 'monthly'.")
  }
}

#' Reset usage tracking data
#' 
#' Permanently delete all stored API usage data. This action cannot be undone
#' and will clear your entire usage history including call counts, timestamps,
#' and cost calculations.
#' 
#' @details
#' This function removes the usage tracking file from your user data directory.
#' After reset:
#' \itemize{
#'   \item All usage history is permanently deleted
#'   \item \code{\link{kvk_usage_report}} will show no data
#'   \item Cost calculations will restart from zero
#'   \item Usage alerts will use the new baseline
#' }
#' 
#' The function only affects stored usage data. It does not change:
#' \itemize{
#'   \item Usage alert settings (these are stored in R options)
#'   \item API key configuration
#'   \item Package preferences
#' }
#' 
#' This can be useful for:
#' \itemize{
#'   \item Starting fresh tracking after a billing period
#'   \item Clearing test data before production use
#'   \item Privacy reasons when sharing a computer
#'   \item Troubleshooting usage tracking issues
#' }
#' 
#' @return Invisibly returns NULL. The function is called for its side effect
#'   of deleting the usage data file.
#'   
#' @examplesIf interactive()
#' # Clear all usage history
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
#' \code{\link{kvk_export_usage}} for backing up data before reset,
#' \code{\link{kvk_usage_alert}} for setting usage limits
#' 
#' @export
kvk_reset_usage <- function() {
  usage_file <- get_usage_file_path()
  
  if (file.exists(usage_file)) {
    unlink(usage_file)
    cli::cli_alert_success("Usage data has been reset.")
  } else {
    cli::cli_alert_info("No usage data found to reset.")
  }
}