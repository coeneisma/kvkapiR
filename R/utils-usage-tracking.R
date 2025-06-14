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