#' Set KvK API Key for the current session
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' This function sets the `KVK_API_KEY` environment variable for the **current R session**
#' using `Sys.setenv()`. The key will be available until the session ends.
#'
#' @param KVK_API_KEY A string containing the KvK API key.
#'
#' @details The function sets `KVK_API_KEY` using `Sys.setenv()`, making it available
#' for the current session. However, it does **not** persist across sessions.
#'
#' If you want to store the API key permanently, use `kvk_store_api_key()`.
#'
#' @return Invisibly returns `TRUE` if the key was set.
#'
#' @examples
#' \dontrun{
#' # Set the API key for the current session
#' kvk_set_api_key("abcd1234")
#' }
#'
#' @export
kvk_set_api_key <- function(KVK_API_KEY) {
  Sys.setenv(KVK_API_KEY = KVK_API_KEY)
  message("KVK_API_KEY has been set for this session.")
  invisible(TRUE)
}


#' Store KvK API Key in .Renviron for persistent use
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' This function saves the `KVK_API_KEY` to the `.Renviron` file, making it
#' persist across R sessions. If an API key is already stored, you must set
#' `overwrite = TRUE` to replace it.
#'
#' @param KVK_API_KEY A string containing the KvK API key.
#' @param overwrite A logical value indicating whether to overwrite an existing
#'   `KVK_API_KEY` entry in `.Renviron`. Defaults to `FALSE`.
#'
#' @details The function modifies `.Renviron` to store the API key permanently.
#' - If the key exists and `overwrite = FALSE`, it returns an error.
#' - If the key exists and `overwrite = TRUE`, it replaces the existing key.
#' - If the key does not exist, it appends a new `KVK_API_KEY` entry to `.Renviron`.
#'
#' **Important:** You must restart R for the changes to take effect.
#'
#' @return Invisibly returns `TRUE` if the key was added or updated.
#'
#' @examples
#' \dontrun{
#' # Store the API key persistently (only if not already set)
#' kvk_store_api_key("abcd1234")
#'
#' # Overwrite an existing API key
#' kvk_store_api_key("newkey5678", overwrite = TRUE)
#' }
#'
#' @export
kvk_store_api_key <- function(KVK_API_KEY, overwrite = FALSE) {
  renviron_path <- file.path(Sys.getenv("HOME"), ".Renviron")

  # Maak een backup van .Renviron voor veiligheid
  if (file.exists(renviron_path)) {
    file.copy(renviron_path, paste0(renviron_path, "_backup"), overwrite = TRUE)
    renviron_content <- readLines(renviron_path)
  } else {
    renviron_content <- character(0)
  }

  # Zoek naar bestaande API-key
  key_exists <- any(grepl("^KVK_API_KEY=", renviron_content))

  if (key_exists && !overwrite) {
    stop("KVK_API_KEY already exists in .Renviron. Use overwrite = TRUE to update it.")
  }

  # Filter oude versies van de sleutel eruit
  renviron_content <- renviron_content[!grepl("^KVK_API_KEY=", renviron_content)]

  # Voeg de nieuwe sleutel toe
  renviron_content <- c(renviron_content, paste0("KVK_API_KEY=", KVK_API_KEY))

  # Schrijf de gewijzigde inhoud terug
  writeLines(renviron_content, renviron_path)

  message("KVK_API_KEY has been stored in .Renviron. Restart R for changes to take effect.")

  invisible(TRUE)
}



#' Retrieve all results from the KvK Search API (up to 1.000 results)
#'
#' @description `r lifecycle::badge('experimental')`
#'
#'   This function automatically paginates through the KvK API to collect
#'   available results. Due to API limitations, it retrieves a maximum of 1.000
#'   records. When this happens, a warning will be displayed.
#'
#' @param ... Named arguments passed to the API query (e.g., naam = "Koudum").
#'   Available arguments can be found at
#'   [https://developers.kvk.nl/documentation/zoeken-api#input]().
#'
#'
#' @return A tibble containing the retrieved results. Possible parameters can be
#'   found under the `Results` section of
#'   [https://developers.kvk.nl/documentation/zoeken-api#output]().
#'
#' @export
#' @examples
#'
#' koudum <- kvk_search(plaats = "Koudum")
#' koudum
#'
#' rotterdam <- kvk_search(plaats = "Rotterdam")
#' rotterdam
kvk_search <- function(...) {

  # Retrieve API key from environment variables
  KVK_API_KEY <- Sys.getenv("KVK_API_KEY")
  if (KVK_API_KEY == "") stop("API key is missing. Set it using `Sys.setenv(KVK_API_KEY='your_key')`")

  # API URL
  API_URL <- "https://api.kvk.nl/api/v2/zoeken"

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
        message("No results found for the given search query.")
        return(NULL)  # Return NULL instead of an error
      } else {
        stop(e)  # Other errors are raised normally
      }
    }
  )

  # If NULL was returned, stop execution
  if (is.null(first_request)) return(NULL)

  if (!"totaal" %in% names(first_request)) stop("Unexpected API response: 'totaal' not found")

  total_results <- first_request$totaal
  if (total_results == 0) {
    message("No results found.")
    return(NULL)
  }

  # If more than 1,000 results, limit and show a warning
  if (total_results > 1000) {
    total_results <- 1000
    warning("API response contains more than 1,000 results. Only the first 1,000 will be retrieved.")
  }

  # Determine total number of pages (maximum 100)
  total_pages <- min(ceiling(total_results / 100), 100)

  all_results <- list()

  # Loop through pages (up to max 100)
  for (pagina in 1:total_pages) {
    request <- tryCatch(
      {
        httr2::request(API_URL) |>
          httr2::req_headers(apikey = KVK_API_KEY, Accept = "application/json") |>
          httr2::req_url_query(resultatenPerPagina = 100, pagina = pagina, ...) |>
          httr2::req_perform() |>
          httr2::resp_body_json()
      },
      error = function(e) {
        if (grepl("HTTP 404", e$message)) {
          message("No results found on page ", pagina, ".")
          return(NULL)
        } else {
          stop(e)
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
    message("No results found.")
    return(NULL)
  }

  # Convert to tibble
  data <- dplyr::tibble(inhoud = all_results) |>
    tidyr::unnest_wider(col = inhoud)

  return(data)
}
