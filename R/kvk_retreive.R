#' Retrieve the Basisprofiel for a given KvK number
#'
#' @description `r lifecycle::badge('experimental')`
#'
#'   This function retrieves the basis profile for a given KvK number
#'   (`kvkNummer`) using the KvK Basisprofiel API. Users can also request
#'   additional data, such as owner information, main establishment, and other
#'   locations by specifying the `include` parameter.
#'
#'   The function also supports the `test_environment` argument. When set to
#'   `TRUE`, the function will query the KvK test API environment, providing a
#'   set of fictitious test data.
#'
#' @param kvkNummer A string representing the KvK number for which the basis
#'   profile is requested.
#' @param geoData A logical value indicating whether geo-data should be included
#'   in the response. Defaults to `FALSE`. If `TRUE`, the response will include
#'   geo-data.
#' @param include A character specifying additional data to include. Possible
#'   values: `"eigenaar"` (owner), `"hoofdvestiging"` (main establishment),
#'   `"vestigingen"` (locations). Defaults to `NULL` (basic profile only).
#' @param test_environment A logical value indicating whether to use the test
#'   environment (`TRUE`) or the production environment (`FALSE`). Defaults to
#'   `FALSE`.
#'
#' @details The function retrieves data from the KvK Basisprofiel API. If
#'   `geoData = TRUE`, geo-data (e.g., location data) will be included in the
#'   returned profile. The `include` parameter allows users to request
#'   additional details, such as ownership information or business locations.
#'
#' **Important:** If `test_environment = TRUE`, no actual API key is required. You will be using
#'   test data from the KvK test environment.
#'
#' @return A tibble containing the retrieved basis profile. If `geoData = TRUE`,
#'   the returned tibble will also include geographical information.
#'
#' @examples
#' \dontrun{
#' # Retrieve basis profile for a given KvK number without geo-data
#' basis_profile <- kvk_get_basisprofiel(kvkNummer = "12345678")
#'
#' # Retrieve basis profile with geo-data
#' basis_profile_geo <- kvk_get_basisprofiel(kvkNummer = "12345678", geoData = TRUE)
#'
#' # Retrieve basis profile including owner and main establishment
#' basis_profile_extended <- kvk_get_basisprofiel(kvkNummer = "12345678", include = "eigenaar"))
#'
#' # Retrieve basis profile from the test environment
#' basis_profile_test <- kvk_get_basisprofiel(kvkNummer = "12345678", test_environment = TRUE)
#' }
#'
#' @export
kvk_get_basisprofiel <- function(kvkNummer, geoData = FALSE, include = NULL, test_environment = FALSE) {

  # Determine API URL and API key based on test_environment flag
  if (test_environment) {
    base_url <- "https://api.kvk.nl/test/api/v1/basisprofielen/"
    KVK_API_KEY <- "l7xx1f2691f2520d487b902f4e0b57a0b197"
    cli::cli_alert_info("You are using the KvK test environment. Test data will be returned.")
  } else {
    base_url <- "https://api.kvk.nl/api/v1/basisprofielen/"
    KVK_API_KEY <- Sys.getenv("KVK_API_KEY")
    if (KVK_API_KEY == "") {
      cli::cli_abort("API key is missing. Set it using `kvk_set_api_key('your_key')`")
    }
  }

  # Validate 'include' parameter to ensure only allowed values are passed
  valid_options <- c("eigenaar", "hoofdvestiging", "vestigingen")
  if (!is.null(include)) {
    invalid_values <- setdiff(include, valid_options)
    if (length(invalid_values) > 0) {
      cli::cli_abort("Invalid values in 'include': {.val {invalid_values}}. Choose from: {.val {valid_options}}")
    }
  }

  # Construct the dynamic URL based on parameters
  url_path <- paste0(base_url, kvkNummer)
  if (!is.null(include)) {
    url_path <- paste0(url_path, "/", paste(include, collapse = "/"))
  }

  # Perform the API request with error handling
  response <- tryCatch(
    {
      httr2::request(url_path) |>
        httr2::req_headers(apikey = KVK_API_KEY, Accept = "application/json") |>
        httr2::req_url_query(geoData = tolower(as.character(geoData))) |>
        httr2::req_perform() |>
        httr2::resp_body_json() |>
        list_to_tibble()
    },
    error = function(e) {
      if (grepl("HTTP 404", e$message)) {
        cli::cli_alert_info("No results found for KvK number: {.val {kvkNummer}}")
        return(NULL)
      } else {
        cli::cli_abort("Error retrieving basis profile: {e$message}")
      }
    }
  )

  return(response)
}

#' Retrieve the Vestigingsprofiel for a given establishment number
#'
#' @description `r lifecycle::badge('experimental')`
#'
#'   This function retrieves the establishment profile (`vestigingsprofiel`) for
#'   a given establishment number (`vestigingsnummer`) using the KvK
#'   Vestigingsprofiel API.
#'
#'   The function also supports the `test_environment` argument. When set to
#'   `TRUE`, the function will query the KvK test API environment, providing a
#'   set of fictitious test data.
#'
#' @param vestigingsnummer A string representing the establishment number for
#'   which the profile is requested.
#' @param geoData A logical value indicating whether geo-data should be included
#'   in the response. Defaults to `FALSE`. If `TRUE`, the response will include
#'   geo-data.
#' @param test_environment A logical value indicating whether to use the test
#'   environment (`TRUE`) or the production environment (`FALSE`). Defaults to
#'   `FALSE`.
#'
#' @details The function retrieves data from the KvK Vestigingsprofiel API. If
#'   `geoData = TRUE`, geo-data (e.g., location data) will be included in the
#'   returned profile.
#'
#' **Important:** If `test_environment = TRUE`, no actual API key is required. You will be using
#'   test data from the KvK test environment.
#'
#' @return A tibble containing the retrieved establishment profile. If `geoData
#'   = TRUE`, the returned tibble will also include geographical information.
#'
#' @examples
#' \dontrun{
#' # Retrieve vestigingsprofiel for a given establishment number without geo-data
#' vestigingsprofiel <- kvk_get_vestigingsprofiel(vestigingsnummer = "000038509504")
#'
#' # Retrieve vestigingsprofiel with geo-data
#' vestigingsprofiel_geo <- kvk_get_vestigingsprofiel(vestigingsnummer = "000038509504", geoData = TRUE)
#'
#' # Retrieve vestigingsprofiel from the test environment
#' vestigingsprofiel_test <- kvk_get_vestigingsprofiel(vestigingsnummer = "000038509504", test_environment = TRUE)
#' }
#'
#' @export
kvk_get_vestigingsprofiel <- function(vestigingsnummer, geoData = FALSE, test_environment = FALSE) {

  # Determine API URL and API key based on test_environment flag
  if (test_environment) {
    base_url <- "https://api.kvk.nl/test/api/v1/vestigingsprofielen/"
    KVK_API_KEY <- "l7xx1f2691f2520d487b902f4e0b57a0b197"
    cli::cli_alert_info("You are using the KvK test environment. Test data will be returned.")
  } else {
    base_url <- "https://api.kvk.nl/api/v1/vestigingsprofielen/"
    KVK_API_KEY <- Sys.getenv("KVK_API_KEY")
    if (KVK_API_KEY == "") {
      cli::cli_abort("API key is missing. Set it using `kvk_set_api_key('your_key')`")
    }
  }

  # Construct the API request with error handling
  response <- tryCatch(
    {
      httr2::request(paste0(base_url, vestigingsnummer)) |>
        httr2::req_headers(apikey = KVK_API_KEY, Accept = "application/json") |>
        httr2::req_url_query(geoData = tolower(as.character(geoData))) |>
        httr2::req_perform() |>
        httr2::resp_body_json() |>
        list_to_tibble()
    },
    error = function(e) {
      if (grepl("HTTP 404", e$message)) {
        cli::cli_alert_info("No results found for vestigingsnummer: {.val {vestigingsnummer}}")
        return(NULL)
      } else {
        cli::cli_abort("Error retrieving vestigingsprofiel: {e$message}")
      }
    }
  )

  return(response)
}

#' Retrieve the Naamgeving for a given KvK number
#'
#' @description `r lifecycle::badge('experimental')`
#'
#'   This function retrieves the name information (`naamgeving`) for a given KvK
#'   number (`kvkNummer`) using the KvK Naamgevingen API.
#'
#'   The function also supports the `test_environment` argument. When set to
#'   `TRUE`, the function will query the KvK test API environment, providing a
#'   set of fictitious test data.
#'
#' @param kvkNummer A string representing the KvK number for which the name
#'   information is requested.
#' @param test_environment A logical value indicating whether to use the test
#'   environment (`TRUE`) or the production environment (`FALSE`). Defaults to
#'   `FALSE`.
#'
#' @details The function retrieves data from the KvK Naamgevingen API.
#'
#' **Important:** If `test_environment = TRUE`, no actual API key is required. You will be using
#'   test data from the KvK test environment.
#'
#' @return A tibble containing the retrieved name information.
#'
#' @examples
#' \dontrun{
#' # Retrieve naamgeving for a given KvK number
#' naamgeving <- kvk_get_naamgeving(kvkNummer = "68750110")
#'
#' # Retrieve naamgeving from the test environment
#' naamgeving_test <- kvk_get_naamgeving(kvkNummer = "68750110", test_environment = TRUE)
#' }
#'
#' @export
kvk_get_naamgeving <- function(kvkNummer, test_environment = FALSE) {

  # Determine API URL and API key based on test_environment flag
  if (test_environment) {
    base_url <- "https://api.kvk.nl/test/api/v1/naamgevingen/kvknummer/"
    KVK_API_KEY <- "l7xx1f2691f2520d487b902f4e0b57a0b197"
    cli::cli_alert_info("You are using the KvK test environment. Test data will be returned.")
  } else {
    base_url <- "https://api.kvk.nl/api/v1/naamgevingen/kvknummer/"
    KVK_API_KEY <- Sys.getenv("KVK_API_KEY")
    if (KVK_API_KEY == "") {
      cli::cli_abort("API key is missing. Set it using `kvk_set_api_key('your_key')`")
    }
  }

  # Construct the dynamic URL based on parameters
  url_path <- paste0(base_url, kvkNummer)

  # Perform the API request with error handling
  response <- tryCatch(
    {
      httr2::request(url_path) |>
        httr2::req_headers(apikey = KVK_API_KEY, Accept = "application/json") |>
        httr2::req_perform() |>
        httr2::resp_body_json() |>
        list_to_tibble()
    },
    error = function(e) {
      if (grepl("HTTP 404", e$message)) {
        cli::cli_alert_info("No results found for KvK number: {.val {kvkNummer}}")
        return(NULL)
      } else {
        cli::cli_abort("Error retrieving name information: {e$message}")
      }
    }
  )

  return(response)
}

#' Convert a nested list to a tibble
#'
#' This function takes a nested list and converts it into a tibble. Each element
#' of the list becomes a separate column in the tibble. If a nested list
#' contains atomic elements of equal length, it is transformed into a tibble.
#' Otherwise, it is stored as a list-column.
#'
#' @param lst A named list to be converted into a tibble.
#'
#' @return A tibble where each element of the list is represented as a column.
#'   Nested lists with uniform atomic elements are converted into tibbles, while
#'   other lists are stored as list-columns.
#'
#' @examples
#' sample_list <- list(
#'   id = "12345",
#'   name = "Test Company",
#'   employees = 10,
#'   addresses = list(
#'     list(street = "Main St", number = 1),
#'     list(street = "Second St", number = 2)
#'   )
#' )
#' list_to_tibble(sample_list)
list_to_tibble <- function(lst) {
  # Ensure input is a list
  if (!base::is.list(lst)) stop("Input must be a list.")

  # Function to process each element correctly
  process_element <- function(x) {
    if (base::is.atomic(x)) {
      return(x)  # Directly include atomic values
    } else if (base::is.list(x)) {
      # If all elements are atomic and have the same length, convert to a tibble
      if (base::all(base::sapply(x, base::is.atomic)) &&
          base::length(base::unique(base::sapply(x, base::length))) == 1) {
        return(tibble::as_tibble(x))
      } else {
        return(base::list(x))  # Otherwise, store as a list-column
      }
    } else {
      return(base::as.character(x))  # Convert other types to character
    }
  }

  # Apply conversion and ensure each list element becomes a separate tibble column
  df <- tibble::tibble(!!!purrr::map(lst, process_element))

  return(df)
}
