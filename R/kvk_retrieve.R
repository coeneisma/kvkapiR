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
#' @param include A character vector specifying additional data to include. Possible
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
#' # Examples using the production API (requires API key)
#' if (nzchar(Sys.getenv("KVK_API_KEY"))) {
#'   # Retrieve basis profile for a given KvK number without geo-data
#'   basis_profile <- kvk_get_basisprofiel(kvkNummer = "69599084")
#'   print(basis_profile)
#'   
#'   # Retrieve basis profile with geo-data
#'   basis_profile_geo <- kvk_get_basisprofiel(kvkNummer = "69599084", geoData = TRUE)
#'   
#'   # Retrieve basis profile including owner information
#'   basis_profile_owner <- kvk_get_basisprofiel(kvkNummer = "69599084", include = "eigenaar")
#' }
#' 
#' # Examples using test environment (no API key required)
#' # These use the KvK test dataset with Donald Duck themed businesses
#' basis_profile_test <- kvk_get_basisprofiel(kvkNummer = "68727720", test_environment = TRUE)
#' print(basis_profile_test)
#'
#' @export
kvk_get_basisprofiel <- function(kvkNummer, geoData = FALSE, include = NULL, test_environment = FALSE) {

  # Validate input
  validate_kvk_nummer(kvkNummer)

  # Determine API URL and API key based on test_environment flag
  if (test_environment) {
    base_url <- paste0(KVK_TEST_BASE_URL, "/v1/basisprofielen/")
    KVK_API_KEY <- KVK_TEST_API_KEY
    cli::cli_alert_info("You are using the KvK test environment. Test data will be returned.")
  } else {
    base_url <- paste0(KVK_PROD_BASE_URL, "/v1/basisprofielen/")
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
  req <- httr2::request(url_path) |>
    httr2::req_headers(apikey = KVK_API_KEY, Accept = "application/json") |>
    httr2::req_url_query(geoData = tolower(as.character(geoData))) |>
    httr2::req_error(is_error = function(resp) FALSE)  # Don't throw on HTTP errors
  
  # Perform the request
  resp <- tryCatch(
    {
      httr2::req_perform(req)
    },
    error = function(e) {
      # For connection errors (not HTTP errors)
      cli::cli_abort("Connection error accessing {api_name}: {e$message}")
      return(NULL)
    }
  )
  
  # If we got no response, return NULL
  if (is.null(resp)) {
    return(NULL)
  }
  
  # Check if the response is an error
  if (httr2::resp_is_error(resp)) {
    handle_kvk_http_error(
      e = NULL,
      api_name = "Basisprofiel API",
      resource_id = kvkNummer,
      resp = resp,
      status_code = httr2::resp_status(resp)
    )
    return(NULL)
  }
  
  # Parse successful response
  response <- httr2::resp_body_json(resp) |> list_to_tibble()
  
  # Record successful API call for usage tracking
  record_api_call("basisprofiel", test_environment)
  
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
#' # Examples using the production API (requires API key)
#' if (nzchar(Sys.getenv("KVK_API_KEY"))) {
#'   # Retrieve vestigingsprofiel for a given establishment number
#'   vestigingsprofiel <- kvk_get_vestigingsprofiel(vestigingsnummer = "000038509504")
#'   print(vestigingsprofiel)
#'   
#'   # Retrieve vestigingsprofiel with geo-data
#'   vestigingsprofiel_geo <- kvk_get_vestigingsprofiel(
#'     vestigingsnummer = "000038509504", 
#'     geoData = TRUE
#'   )
#' }
#' 
#' # Examples using test environment (no API key required)
#' vestigingsprofiel_test <- kvk_get_vestigingsprofiel(
#'   vestigingsnummer = "000019716893", 
#'   test_environment = TRUE
#' )
#' print(vestigingsprofiel_test)
#'
#' @export
kvk_get_vestigingsprofiel <- function(vestigingsnummer, geoData = FALSE, test_environment = FALSE) {

  # Validate input
  validate_vestigingsnummer(vestigingsnummer)

  # Determine API URL and API key based on test_environment flag
  if (test_environment) {
    base_url <- paste0(KVK_TEST_BASE_URL, "/v1/vestigingsprofielen/")
    KVK_API_KEY <- KVK_TEST_API_KEY
    cli::cli_alert_info("You are using the KvK test environment. Test data will be returned.")
  } else {
    base_url <- paste0(KVK_PROD_BASE_URL, "/v1/vestigingsprofielen/")
    KVK_API_KEY <- Sys.getenv("KVK_API_KEY")
    if (KVK_API_KEY == "") {
      cli::cli_abort("API key is missing. Set it using `kvk_set_api_key('your_key')`")
    }
  }

  # Construct the API request with error handling
  req <- httr2::request(paste0(base_url, vestigingsnummer)) |>
    httr2::req_headers(apikey = KVK_API_KEY, Accept = "application/json") |>
    httr2::req_url_query(geoData = tolower(as.character(geoData))) |>
    httr2::req_error(is_error = function(resp) FALSE)  # Don't throw on HTTP errors
  
  # Perform the request
  resp <- tryCatch(
    {
      httr2::req_perform(req)
    },
    error = function(e) {
      # For connection errors (not HTTP errors)
      cli::cli_abort("Connection error accessing {api_name}: {e$message}")
      return(NULL)
    }
  )
  
  # If we got no response, return NULL
  if (is.null(resp)) {
    return(NULL)
  }
  
  # Check if the response is an error
  if (httr2::resp_is_error(resp)) {
    handle_kvk_http_error(
      e = NULL,
      api_name = "Vestigingsprofiel API",
      resource_id = vestigingsnummer,
      resp = resp,
      status_code = httr2::resp_status(resp)
    )
    return(NULL)
  }
  
  # Parse successful response
  response <- httr2::resp_body_json(resp) |> list_to_tibble()
  
  # Record successful API call for usage tracking
  record_api_call("vestigingsprofiel", test_environment)
  
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
#' # Examples using the production API (requires API key)
#' if (nzchar(Sys.getenv("KVK_API_KEY"))) {
#'   # Retrieve naamgeving for a given KvK number
#'   naamgeving <- kvk_get_naamgeving(kvkNummer = "68750110")
#'   print(naamgeving)
#' }
#' 
#' # Examples using test environment (no API key required)
#' naamgeving_test <- kvk_get_naamgeving(kvkNummer = "68727720", test_environment = TRUE)
#' print(naamgeving_test)
#'
#' @export
kvk_get_naamgeving <- function(kvkNummer, test_environment = FALSE) {

  # Validate input
  validate_kvk_nummer(kvkNummer)

  # Determine API URL and API key based on test_environment flag
  if (test_environment) {
    base_url <- paste0(KVK_TEST_BASE_URL, "/v1/naamgevingen/kvknummer/")
    KVK_API_KEY <- KVK_TEST_API_KEY
    cli::cli_alert_info("You are using the KvK test environment. Test data will be returned.")
  } else {
    base_url <- paste0(KVK_PROD_BASE_URL, "/v1/naamgevingen/kvknummer/")
    KVK_API_KEY <- Sys.getenv("KVK_API_KEY")
    if (KVK_API_KEY == "") {
      cli::cli_abort("API key is missing. Set it using `kvk_set_api_key('your_key')`")
    }
  }

  # Construct the dynamic URL based on parameters
  url_path <- paste0(base_url, kvkNummer)

  # Perform the API request with error handling
  req <- httr2::request(url_path) |>
    httr2::req_headers(apikey = KVK_API_KEY, Accept = "application/json") |>
    httr2::req_error(is_error = function(resp) FALSE)  # Don't throw on HTTP errors
  
  # Perform the request
  resp <- tryCatch(
    {
      httr2::req_perform(req)
    },
    error = function(e) {
      # For connection errors (not HTTP errors)
      cli::cli_abort("Connection error accessing {api_name}: {e$message}")
      return(NULL)
    }
  )
  
  # If we got no response, return NULL
  if (is.null(resp)) {
    return(NULL)
  }
  
  # Check if the response is an error
  if (httr2::resp_is_error(resp)) {
    handle_kvk_http_error(
      e = NULL,
      api_name = "Naamgevingen API",
      resource_id = kvkNummer,
      resp = resp,
      status_code = httr2::resp_status(resp)
    )
    return(NULL)
  }
  
  # Parse successful response
  response <- httr2::resp_body_json(resp) |> list_to_tibble()
  
  # Record successful API call for usage tracking
  record_api_call("naamgeving", test_environment)
  
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
#' @keywords internal
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
