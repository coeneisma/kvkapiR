#' Retrieve all results from the KvK Search API (up to 1,000 results)
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' This function automatically paginates through the KvK API to collect
#' available results. Due to API limitations, it retrieves a maximum of 1,000
#' records. When this happens, a warning will be displayed.
#'
#' If `test_environment = TRUE`, it switches to the KvK's test environment,
#' using a mock API key for testing purposes.
#'
#' @param ... Named arguments passed to the API query (e.g., naam = "Koudum").
#'   Available arguments can be found at
#'   [https://developers.kvk.nl/documentation/zoeken-api#input]().
#' @param test_environment A logical value. If TRUE, uses the test environment
#'   instead of the live API. Defaults to FALSE.
#'
#' @return A tibble containing the retrieved results. Possible parameters can be
#'   found under the `Results` section of
#'   [https://developers.kvk.nl/documentation/zoeken-api#output]().
#'
#' @export
#' @examples
#' # Examples using the production API (requires API key)
#' if (nzchar(Sys.getenv("KVK_API_KEY"))) {
#'   koudum <- kvk_search(plaats = "Koudum")
#'   print(koudum)
#'   
#'   rotterdam <- kvk_search(plaats = "Rotterdam")
#'   print(rotterdam)
#' }
#' 
#' # Examples using test environment (no API key required)
#' test_data <- kvk_search(plaats = "Utrecht", test_environment = TRUE)
#' print(test_data)
kvk_search <- function(..., test_environment = FALSE) {

  # Determine API URL and API key based on test environment flag
  if (test_environment) {
    API_URL <- paste0(KVK_TEST_BASE_URL, "/v2/zoeken")
    KVK_API_KEY <- KVK_TEST_API_KEY
    cli::cli_alert_info("You are using the KvK test environment. Test data will be returned.")
  } else {
    # Retrieve API key from environment variables for production use
    KVK_API_KEY <- Sys.getenv("KVK_API_KEY")
    if (KVK_API_KEY == "") cli::cli_abort("API key is missing. Set it using `kvk_set_api_key('your_key')`")
    API_URL <- paste0(KVK_PROD_BASE_URL, "/v2/zoeken")
  }

  # First request to determine total number of results, with error handling
  first_request <- tryCatch(
    {
      httr2::request(API_URL) |>
        httr2::req_headers(apikey = KVK_API_KEY, Accept = "application/json") |>
        httr2::req_url_query(resultatenPerPagina = 1, pagina = 1, ...) |>
        httr2::req_perform() |>
        httr2::resp_body_json()
    },
    error = function(e) {
      if (grepl("HTTP 404", e$message)) {
        cli::cli_alert_info("No results found for the given search query.")
        return(NULL)  # Return NULL instead of an error
      } else {
        cli::cli_abort(e$message)  # Other errors are raised normally
      }
    }
  )

  # If NULL was returned, stop execution
  if (is.null(first_request)) return(NULL)

  if (!"totaal" %in% names(first_request)) cli::cli_abort("Unexpected API response: 'totaal' not found")

  total_results <- first_request$totaal
  if (total_results == 0) {
    cli::cli_alert_info("No results found.")
    return(NULL)
  }

  # If more than maximum results, limit and show a warning
  if (total_results > KVK_MAX_RESULTS) {
    total_results <- KVK_MAX_RESULTS
    cli::cli_alert_warning("API response contains more than {KVK_MAX_RESULTS} results. Only the first {KVK_MAX_RESULTS} will be retrieved.")
  }

  # Determine total number of pages (maximum based on results per page)
  total_pages <- min(ceiling(total_results / KVK_RESULTS_PER_PAGE), KVK_RESULTS_PER_PAGE)

  all_results <- list()

  # Loop through pages (up to max 100)
  for (page in 1:total_pages) {
    request <- tryCatch(
      {
        httr2::request(API_URL) |>
          httr2::req_headers(apikey = KVK_API_KEY, Accept = "application/json") |>
          httr2::req_url_query(resultatenPerPagina = KVK_RESULTS_PER_PAGE, pagina = page, ...) |>
          httr2::req_perform() |>
          httr2::resp_body_json()
      },
      error = function(e) {
        if (grepl("HTTP 404", e$message)) {
          cli::cli_alert_info("No results found on page {page}.")
          return(NULL)
        } else {
          cli::cli_abort(e$message)
        }
      }
    )

    # If NULL, skip to next page
    if (is.null(request)) next

    if ("resultaten" %in% names(request)) {
      all_results <- append(all_results, request$resultaten)
    }
  }

  # If no results, return NULL
  if (length(all_results) == 0) {
    cli::cli_alert_info("No results found.")
    return(NULL)
  }

  # Convert to tibble
  data <- dplyr::tibble(content = all_results) |>
    tidyr::unnest_wider(col = content)

  return(data)
}
