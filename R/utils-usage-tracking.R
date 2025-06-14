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

#' Generate KvK API usage report
#' 
#' @description
#' Display a summary of your KvK API usage and associated costs.
#' This function provides insights into your API consumption patterns
#' and helps track expenses.
#' 
#' @param format Character string specifying the output format:
#'   - "summary" (default): Display a formatted summary in the console
#'   - "tibble": Return usage data as a tibble with monthly aggregation
#'   - "tidy": Return usage data in tidy format (one row per API call)
#' @param year Integer, specific year to filter (default: NULL shows all years).
#' @param monthly_fee Numeric, monthly API key connection fee in euros (default: 6.20)
#' @param cost_per_query Numeric, cost per paid query in euros (default: 0.02)
#' 
#' @return Depends on format parameter:
#'   - format="summary": NULL (invisible), displays summary in console
#'   - format="tibble": A tibble with monthly usage statistics
#'   - format="tidy": A tibble with all individual API calls
#' 
#' @export
#' @examples
#' \dontrun{
#' # Display usage summary for current year
#' kvk_usage_report()
#' 
#' # Get usage data as tibble
#' usage_tibble <- kvk_usage_report(format = "tibble")
#' 
#' # Get raw usage data in tidy format
#' usage_tidy <- kvk_usage_report(format = "tidy")
#' 
#' # Show all historical data
#' kvk_usage_report(year = NULL)
#' }
kvk_usage_report <- function(format = c("summary", "tibble", "tidy"),
                            year = NULL,
                            monthly_fee = 6.20,
                            cost_per_query = 0.02) {
  
  format <- match.arg(format)
  
  # Load usage data
  usage_data <- load_usage_data()
  
  if (nrow(usage_data) == 0) {
    if (format == "summary") {
      cli::cli_alert_info("No API usage recorded yet.")
      cli::cli_text("Usage tracking automatically records production API calls.")
      cli::cli_text("Test environment calls are not tracked.")
      return(invisible(NULL))
    } else if (format == "tibble") {
      # Return empty tibble with correct structure
      return(tibble::tibble(
        year = integer(),
        month = integer(),
        month_name = character(),
        search = integer(),
        basisprofiel = integer(),
        vestigingsprofiel = integer(),
        naamgeving = integer(),
        total_calls = integer(),
        paid_calls = integer(),
        query_costs = numeric(),
        base_cost = numeric(),
        total_cost = numeric()
      ))
    } else { # tidy
      # Return empty tibble with usage data structure
      return(tibble::as_tibble(usage_data))
    }
  }
  
  # Filter by year if specified
  if (!is.null(year)) {
    if (is.numeric(year)) {
      year <- as.integer(year)
    }
    usage_data <- usage_data[usage_data$year == year, ]
    
    if (nrow(usage_data) == 0) {
      if (format == "summary") {
        cli::cli_alert_info("No API usage recorded for year {year}.")
        return(invisible(NULL))
      } else {
        # Return empty structure
        return(kvk_usage_report(format = format, year = as.integer(format(Sys.Date(), "%Y"))))
      }
    }
  }
  
  # Return tidy format if requested
  if (format == "tidy") {
    return(tibble::as_tibble(usage_data))
  }
  
  # Prepare monthly aggregation
  monthly_data <- prepare_monthly_usage(usage_data, monthly_fee, cost_per_query)
  
  # Return tibble format if requested
  if (format == "tibble") {
    return(monthly_data)
  }
  
  # Otherwise display summary
  display_usage_summary(usage_data, monthly_data, year, monthly_fee, cost_per_query)
  
  # Check for alerts
  check_usage_alerts(usage_data, year)
  
  return(invisible(NULL))
}

#' Prepare monthly usage data
#' 
#' @param usage_data Data frame with usage records
#' @param monthly_fee Monthly base fee
#' @param cost_per_query Cost per paid query
#' @return Tibble with monthly aggregated data
#' @keywords internal
prepare_monthly_usage <- function(usage_data, monthly_fee, cost_per_query) {
  # Create year-month grouping
  usage_data$year_month <- paste(usage_data$year, 
                                sprintf("%02d", usage_data$month), 
                                sep = "-")
  
  # Count calls by type per month
  monthly_counts <- usage_data |>
    dplyr::group_by(year, month, year_month) |>
    dplyr::summarise(
      search = sum(call_type == "search"),
      basisprofiel = sum(call_type == "basisprofiel"),
      vestigingsprofiel = sum(call_type == "vestigingsprofiel"),
      naamgeving = sum(call_type == "naamgeving"),
      .groups = "drop"
    )
  
  # Calculate costs
  monthly_counts$total_calls <- monthly_counts$search + monthly_counts$basisprofiel + 
                               monthly_counts$vestigingsprofiel + monthly_counts$naamgeving
  monthly_counts$paid_calls <- monthly_counts$basisprofiel + 
                              monthly_counts$vestigingsprofiel + monthly_counts$naamgeving
  monthly_counts$query_costs <- monthly_counts$paid_calls * cost_per_query
  monthly_counts$base_cost <- monthly_fee
  monthly_counts$total_cost <- monthly_counts$base_cost + monthly_counts$query_costs
  
  # Add month name for display
  monthly_counts$month_name <- format(as.Date(paste0(monthly_counts$year_month, "-01")), "%B")
  
  # Select and order columns
  monthly_counts <- monthly_counts |>
    dplyr::select(year, month, month_name, search, basisprofiel, vestigingsprofiel, 
                  naamgeving, total_calls, paid_calls, query_costs, base_cost, total_cost) |>
    dplyr::arrange(year, month)
  
  return(monthly_counts)
}

#' Display usage summary in console
#' 
#' @param usage_data Raw usage data
#' @param monthly_data Monthly aggregated data
#' @param year Year filter (NULL for all)
#' @param monthly_fee Monthly base fee
#' @param cost_per_query Cost per paid query
#' @keywords internal
display_usage_summary <- function(usage_data, monthly_data, year, monthly_fee, cost_per_query) {
  # Title
  if (is.null(year)) {
    cli::cli_h1("KvK API Usage Report (All Time)")
  } else {
    cli::cli_h1("KvK API Usage Report ({year})")
  }
  
  # Monthly overview table
  cli::cli_h2("Monthly Overview")
  
  # Create display table
  display_data <- monthly_data |>
    dplyr::mutate(
      Month = paste(month_name, year),
      Search = search,
      Basisprofiel = basisprofiel,
      Vestiging = vestigingsprofiel,
      Naamgeving = naamgeving,
      `Costs (€)` = sprintf("%.2f", total_cost)
    ) |>
    dplyr::select(Month, Search, Basisprofiel, Vestiging, Naamgeving, `Costs (€)`)
  
  # Print table
  print(knitr::kable(display_data, format = "simple"))
  
  # Summary statistics
  cli::cli_h2("Summary")
  
  # Calculate totals
  total_costs_calc <- calculate_costs(usage_data, monthly_fee, cost_per_query)
  
  cli::cli_text("Total API calls: {.val {total_costs_calc$total_calls}} ",
                "({.val {total_costs_calc$free_calls}} free, ",
                "{.val {total_costs_calc$paid_calls}} paid)")
  
  if (!is.null(year)) {
    cli::cli_text("Total costs {year}: {.val €{sprintf('%.2f', total_costs_calc$total_costs)}} ",
                  "({.val €{sprintf('%.2f', total_costs_calc$base_costs)}} base + ",
                  "{.val €{sprintf('%.2f', total_costs_calc$query_costs)}} queries)")
  } else {
    cli::cli_text("Total costs: {.val €{sprintf('%.2f', total_costs_calc$total_costs)}} ",
                  "({.val €{sprintf('%.2f', total_costs_calc$base_costs)}} base + ",
                  "{.val €{sprintf('%.2f', total_costs_calc$query_costs)}} queries)")
  }
  
  # Show tracking info
  cli::cli_h3("Tracking Information")
  cli::cli_text("Usage data stored in: {.path {get_usage_file_path()}}")
  cli::cli_text("Disable tracking: Set environment variable {.envvar KVKAPI_DISABLE_TRACKING=true}")
  cli::cli_text("  In R: {.code Sys.setenv(KVKAPI_DISABLE_TRACKING = 'true')}")
}

#' Reset KvK API usage statistics
#' 
#' @description
#' Remove all stored API usage data. This action cannot be undone.
#' 
#' @param confirm Logical, require confirmation before deleting (default: TRUE).
#'   Set to FALSE to skip confirmation prompt.
#' 
#' @return Logical, TRUE if data was successfully reset, FALSE otherwise.
#' 
#' @export
#' @examples
#' \dontrun{
#' # Reset with confirmation prompt
#' kvk_reset_usage()
#' 
#' # Reset without confirmation (use with caution)
#' kvk_reset_usage(confirm = FALSE)
#' }
kvk_reset_usage <- function(confirm = TRUE) {
  usage_file <- get_usage_file_path()
  
  if (!file.exists(usage_file)) {
    cli::cli_alert_info("No usage data found to reset.")
    return(invisible(FALSE))
  }
  
  # Get current stats before deletion
  usage_data <- load_usage_data()
  stats <- calculate_costs(usage_data)
  
  if (confirm) {
    cli::cli_alert_warning("This will delete all API usage history:")
    cli::cli_text("  • {.val {stats$total_calls}} API calls recorded")
    cli::cli_text("  • {.val €{sprintf('%.2f', stats$total_costs)}} in tracked costs")
    cli::cli_text("  • Data from {.val {stats$months_active}} month(s)")
    
    if (!interactive()) {
      cli::cli_abort("Cannot reset usage data in non-interactive mode. Use {.code confirm = FALSE} to force reset.")
    }
    
    # Use menu for cleaner interaction
    response <- utils::menu(
      choices = c("Yes, delete all usage data", "No, keep the data"),
      title = "\nAre you sure you want to continue?"
    )
    
    if (response != 1) {
      cli::cli_alert_info("Reset cancelled.")
      return(invisible(FALSE))
    }
  }
  
  # Delete the file
  file.remove(usage_file)
  cli::cli_alert_success("Usage data has been reset.")
  
  return(invisible(TRUE))
}

#' Export KvK API usage data
#' 
#' @description
#' Export usage data to a CSV file for external analysis or record keeping.
#' 
#' @param file Character string, path to export file. If NULL (default),
#'   exports to "kvk_usage_YYYY-MM-DD.csv" in current directory.
#' @param format Character string, data format to export:
#'   - "tidy": Raw usage data (one row per API call)
#'   - "monthly": Monthly aggregated data
#' 
#' @return Character string with the path to the exported file (invisible).
#' 
#' @export
#' @examples
#' \dontrun{
#' # Export to default filename
#' kvk_export_usage()
#' 
#' # Export to specific file
#' kvk_export_usage("my_usage_data.csv")
#' 
#' # Export monthly summary
#' kvk_export_usage(format = "monthly")
#' }
kvk_export_usage <- function(file = NULL, format = c("tidy", "monthly")) {
  format <- match.arg(format)
  
  # Load usage data
  usage_data <- load_usage_data()
  
  if (nrow(usage_data) == 0) {
    cli::cli_alert_info("No usage data to export.")
    return(invisible(NULL))
  }
  
  # Prepare data based on format
  if (format == "tidy") {
    export_data <- usage_data
  } else {
    export_data <- prepare_monthly_usage(usage_data, 6.20, 0.02)
  }
  
  # Determine filename
  if (is.null(file)) {
    file <- sprintf("kvk_usage_%s.csv", format(Sys.Date(), "%Y-%m-%d"))
  }
  
  # Export to CSV
  utils::write.csv(export_data, file, row.names = FALSE)
  
  cli::cli_alert_success("Usage data exported to: {.path {file}}")
  cli::cli_text("  Format: {.val {format}}")
  cli::cli_text("  Records: {.val {nrow(export_data)}}")
  
  return(invisible(file))
}

#' Set usage alert thresholds
#' 
#' @description
#' Configure alerts for when API usage exceeds specified thresholds.
#' Alerts are shown when calling kvk_usage_report().
#' 
#' @param max_calls Integer, maximum number of API calls before alert.
#'   Set to NULL to disable call limit alert.
#' @param max_cost Numeric, maximum cost in euros before alert.
#'   Set to NULL to disable cost limit alert.
#' @param period Character string, period for limits:
#'   - "month": Limits apply per calendar month
#'   - "year": Limits apply per calendar year
#'   - "total": Limits apply to all-time usage
#' 
#' @return NULL (invisible). Alerts are stored as options.
#' 
#' @export
#' @examples
#' \dontrun{
#' # Set monthly limit of 100 calls
#' kvk_usage_alert(max_calls = 100, period = "month")
#' 
#' # Set yearly cost limit of €50
#' kvk_usage_alert(max_cost = 50, period = "year")
#' 
#' # Set both limits
#' kvk_usage_alert(max_calls = 500, max_cost = 25, period = "month")
#' 
#' # Disable alerts
#' kvk_usage_alert(max_calls = NULL, max_cost = NULL)
#' }
kvk_usage_alert <- function(max_calls = NULL, max_cost = NULL, 
                           period = c("month", "year", "total")) {
  period <- match.arg(period)
  
  # Store in options
  options(
    kvkapiR.alert_max_calls = max_calls,
    kvkapiR.alert_max_cost = max_cost,
    kvkapiR.alert_period = period
  )
  
  # Show confirmation
  if (is.null(max_calls) && is.null(max_cost)) {
    cli::cli_alert_success("Usage alerts disabled.")
  } else {
    cli::cli_alert_success("Usage alerts configured:")
    if (!is.null(max_calls)) {
      cli::cli_text("  • Max calls: {.val {max_calls}} per {period}")
    }
    if (!is.null(max_cost)) {
      cli::cli_text("  • Max cost: {.val €{max_cost}} per {period}")
    }
  }
  
  return(invisible(NULL))
}

#' Check usage against alert thresholds
#' 
#' @param usage_data Data frame with usage data
#' @param year Optional year filter
#' @keywords internal
check_usage_alerts <- function(usage_data, year = NULL) {
  # Get alert settings
  max_calls <- getOption("kvkapiR.alert_max_calls")
  max_cost <- getOption("kvkapiR.alert_max_cost")
  period <- getOption("kvkapiR.alert_period", "month")
  
  if (is.null(max_calls) && is.null(max_cost)) {
    return(invisible(NULL))
  }
  
  # Filter data based on period and year
  current_date <- Sys.Date()
  
  if (period == "month") {
    # Current month only
    usage_subset <- usage_data[
      usage_data$year == as.integer(format(current_date, "%Y")) &
      usage_data$month == as.integer(format(current_date, "%m")),
    ]
    period_desc <- format(current_date, "%B %Y")
  } else if (period == "year") {
    # Current year or specified year
    target_year <- ifelse(is.null(year), 
                         as.integer(format(current_date, "%Y")), 
                         year)
    usage_subset <- usage_data[usage_data$year == target_year, ]
    period_desc <- as.character(target_year)
  } else {
    # Total (all data)
    usage_subset <- usage_data
    period_desc <- "all time"
  }
  
  if (nrow(usage_subset) == 0) {
    return(invisible(NULL))
  }
  
  # Calculate current usage
  costs <- calculate_costs(usage_subset)
  
  # Check thresholds
  alerts_triggered <- FALSE
  
  if (!is.null(max_calls) && costs$total_calls > max_calls) {
    cli::cli_alert_warning("Usage alert: {.val {costs$total_calls}} calls exceeds limit of {.val {max_calls}} for {period_desc}")
    alerts_triggered <- TRUE
  }
  
  if (!is.null(max_cost) && costs$total_costs > max_cost) {
    cli::cli_alert_warning("Cost alert: {.val €{sprintf('%.2f', costs$total_costs)}} exceeds limit of {.val €{max_cost}} for {period_desc}")
    alerts_triggered <- TRUE
  }
  
  if (alerts_triggered) {
    cli::cli_text("")  # Empty line for spacing
  }
  
  return(invisible(NULL))
}

#' Check usage alerts in real-time when recording a call
#' 
#' @param usage_data Updated usage data including the new call
#' @param call_type Type of the call just made
#' @keywords internal
check_usage_alerts_realtime <- function(usage_data, call_type) {
  # Get alert settings
  max_calls <- getOption("kvkapiR.alert_max_calls")
  max_cost <- getOption("kvkapiR.alert_max_cost")
  period <- getOption("kvkapiR.alert_period", "month")
  
  if (is.null(max_calls) && is.null(max_cost)) {
    return(invisible(NULL))
  }
  
  # Filter data based on period
  current_date <- Sys.Date()
  
  if (period == "month") {
    # Current month only
    usage_subset <- usage_data[
      usage_data$year == as.integer(format(current_date, "%Y")) &
      usage_data$month == as.integer(format(current_date, "%m")),
    ]
    period_desc <- format(current_date, "%B %Y")
  } else if (period == "year") {
    # Current year
    usage_subset <- usage_data[
      usage_data$year == as.integer(format(current_date, "%Y")),
    ]
    period_desc <- as.character(format(current_date, "%Y"))
  } else {
    # Total (all data)
    usage_subset <- usage_data
    period_desc <- "all time"
  }
  
  # Calculate current usage
  costs <- calculate_costs(usage_subset)
  
  # Calculate usage BEFORE this call (for "just exceeded" detection)
  usage_before <- usage_subset[seq_len(nrow(usage_subset) - 1), ]
  costs_before <- if (nrow(usage_before) > 0) {
    calculate_costs(usage_before)
  } else {
    list(total_calls = 0, total_costs = 0)
  }
  
  # Check if we JUST exceeded the limits with this call
  call_cost <- ifelse(call_type == "search", 0, 0.02)
  call_desc <- ifelse(call_type == "search", "search (free)", 
                     paste0(call_type, " (€0.02)"))
  
  # Check call limit
  if (!is.null(max_calls)) {
    if (costs_before$total_calls < max_calls && costs$total_calls >= max_calls) {
      cli::cli_div(theme = list(span.alert = list(color = "red", "font-weight" = "bold")))
      cli::cli_alert_warning(paste0(
        "{.alert USAGE LIMIT ALERT}: This {.val {call_desc}} call brings your total to ",
        "{.val {costs$total_calls}} calls, exceeding the limit of {.val {max_calls}} for {period_desc}!"
      ))
      cli::cli_end()
    }
  }
  
  # Check cost limit
  if (!is.null(max_cost)) {
    if (costs_before$total_costs < max_cost && costs$total_costs >= max_cost) {
      cli::cli_div(theme = list(span.alert = list(color = "red", "font-weight" = "bold")))
      cli::cli_alert_warning(paste0(
        "{.alert COST LIMIT ALERT}: This {.val {call_desc}} call brings your total cost to ",
        "{.val €{sprintf('%.2f', costs$total_costs)}}, exceeding the limit of {.val €{max_cost}} for {period_desc}!"
      ))
      cli::cli_text("  Consider using {.code kvk_usage_report()} to review your usage.")
      cli::cli_end()
    }
  }
  
  return(invisible(NULL))
}