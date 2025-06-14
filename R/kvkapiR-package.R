#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
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

NULL
