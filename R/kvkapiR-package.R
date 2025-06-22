#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
#' @importFrom tibble tibble as_tibble
## usethis namespace: end

# Global variable bindings to avoid R CMD check NOTEs
utils::globalVariables(c(
  "content",
  # Usage tracking variables
  "year", "month", "year_month", "call_type", "month_name",
  "basisprofiel", "vestigingsprofiel", "naamgeving", "search",
  "total_calls", "paid_calls", "query_costs", "base_cost", "total_cost",
  # Display variables
  "Month", "Search", "Basisprofiel", "Vestiging", "Naamgeving", "Costs (EUR)"
))

.onAttach <- function(libname, pkgname) {
  # Show usage tracking info on first load
  if (interactive() && is.null(getOption("kvkapiR.usage_tracking_opted_in"))) {
    packageStartupMessage(
      "Welcome to kvkapiR! This package automatically tracks your API usage to help monitor costs.\n",
      "Usage data is stored in memory for your current session only.\n",
      "To disable tracking, use: Sys.setenv(KVKAPI_DISABLE_TRACKING = 'true')"
    )
  }
}

NULL
