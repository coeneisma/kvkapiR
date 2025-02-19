#' Set KvK API Key for the current session
#'
#' @description `r lifecycle::badge('experimental')`
#'
#'   This function sets the specified KvK API key for the **current R session**
#'   using `Sys.setenv()`. The key will be available until the session ends.
#'
#' @param api_name A string containing the name of the API. One of: "search",
#'   "basisprofiel", "vestigingsprofiel", "naamgeving".
#' @param api_key A string containing the API key for the specified API.
#'
#' @details The function sets the specified API key using `Sys.setenv()`, making
#'   it available for the current session. However, it does **not** persist
#'   across sessions.
#'
#'   If you want to store the key permanently, use `kvk_store_api_key()`.
#'
#' @return Invisibly returns `TRUE` if the key was set.
#'
#' @examples
#' \dontrun{
#' # Set the API key for the current session
#' kvk_set_api_key("search", "abcd1234")
#' }
#'
#' @export
kvk_set_api_key <- function(api_name, api_key) {
  valid_api_names <- c("search", "basisprofiel", "vestigingsprofiel", "naamgeving")

  # Check if the api_name is valid
  if (!(api_name %in% valid_api_names)) {
    stop("Invalid api_name. Must be one of: 'search', 'basisprofiel', 'vestigingsprofiel', 'naamgeving'.")
  }

  api_key_var <- paste0("KVK_", toupper(api_name), "_API_KEY")
  Sys.setenv(api_key_var = api_key)
  message(paste(api_key_var, "has been set for this session."))
  invisible(TRUE)
}

#' Store KvK API Key in .Renviron for persistent use
#'
#' @description `r lifecycle::badge('experimental')`
#'
#'   This function saves the specified KvK API key to the `.Renviron` file,
#'   making it persist across R sessions. If an API key is already stored, you
#'   must set `overwrite = TRUE` to replace it.
#'
#' @param api_name A string containing the name of the API. One of: "search",
#'   "basisprofiel", "vestigingsprofiel", "naamgeving".
#' @param api_key A string containing the API key for the specified API.
#' @param overwrite A logical value indicating whether to overwrite an existing
#'   API key entry in `.Renviron`. Defaults to `FALSE`.
#'
#' @details The function modifies `.Renviron` to store the API key permanently.
#' - If the key exists and `overwrite = FALSE`, it returns an error.
#' - If the key exists and `overwrite = TRUE`, it replaces the existing key.
#' - If the key does not exist, it appends a new API key entry for the specified API to `.Renviron`.
#'
#' **Important:** You must restart R for the changes to take effect.
#'
#' @return Invisibly returns `TRUE` if the key was added or updated.
#'
#' @examples
#' \dontrun{
#' # Store the API key persistently for Search API
#' kvk_store_api_key("search", "abcd1234")
#'
#' # Overwrite an existing API key for Basisprofiel API
#' kvk_store_api_key("basisprofiel", "newkey5678", overwrite = TRUE)
#' }
#'
#' @export
kvk_store_api_key <- function(api_name, api_key, overwrite = FALSE) {
  valid_api_names <- c("search", "basisprofiel", "vestigingsprofiel", "naamgeving")

  # Check if the api_name is valid
  if (!(api_name %in% valid_api_names)) {
    stop("Invalid api_name. Must be one of: 'search', 'basisprofiel', 'vestigingsprofiel', 'naamgeving'.")
  }

  renviron_path <- file.path(Sys.getenv("HOME"), ".Renviron")

  # Make a backup of .Renviron for safety
  if (file.exists(renviron_path)) {
    file.copy(renviron_path, paste0(renviron_path, "_backup"), overwrite = TRUE)
    renviron_content <- readLines(renviron_path)
  } else {
    renviron_content <- character(0)
  }

  # Look for existing API key entry
  api_key_var <- paste0("KVK_", toupper(api_name), "_API_KEY")
  key_exists <- any(grepl(paste0("^", api_key_var, "="), renviron_content))

  if (key_exists && !overwrite) {
    stop(paste(api_key_var, "already exists in .Renviron. Use overwrite = TRUE to update it."))
  }

  # Filter out old versions of the key
  renviron_content <- renviron_content[!grepl(paste0("^", api_key_var, "="), renviron_content)]

  # Add the new key
  renviron_content <- c(renviron_content, paste0(api_key_var, "=", api_key))

  # Write the modified content back
  writeLines(renviron_content, renviron_path)

  message(paste(api_key_var, "has been stored in .Renviron. Restart R for changes to take effect."))

  invisible(TRUE)
}


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
#'
#' koudum <- kvk_search(plaats = "Koudum")
#' koudum
#'
#' rotterdam <- kvk_search(plaats = "Rotterdam")
#' rotterdam
#'
#' # Use test environment
#' test_data <- kvk_search(plaats = "Utrecht", test_environment = TRUE)
#' test_data
kvk_search <- function(..., test_environment = FALSE) {

  # Determine API URL and API key based on test environment flag
  if (test_environment) {
    API_URL <- "https://api.kvk.nl/test/api/v2/zoeken"
    KVK_SEARCH_API_KEY <- "l7xx1f2691f2520d487b902f4e0b57a0b197"
    message("You are using the KvK test environment. Test data will be returned.")
  } else {
    # Retrieve API key from environment variables for production use
    KVK_SEARCH_API_KEY <- Sys.getenv("KVK_SEARCH_API_KEY")
    if (KVK_SEARCH_API_KEY == "") stop("API key is missing. Set it using `Sys.setenv(KVK_SEARCH_API_KEY='your_key')`")
    API_URL <- "https://api.kvk.nl/api/v2/zoeken"
  }

  # First request to determine total number of results, with error handling
  first_request <- tryCatch(
    {
      httr2::request(API_URL) |>
        httr2::req_headers(apikey = KVK_SEARCH_API_KEY, Accept = "application/json") |>
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
          httr2::req_headers(apikey = KVK_SEARCH_API_KEY, Accept = "application/json") |>
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
    KVK_BASISPROFIEL_API_KEY <- "l7xx1f2691f2520d487b902f4e0b57a0b197"
    message("You are using the KvK test environment. Test data will be returned.")
  } else {
    base_url <- "https://api.kvk.nl/api/v1/basisprofielen/"
    KVK_BASISPROFIEL_API_KEY <- Sys.getenv("KVK_BASISPROFIEL_API_KEY")
    if (KVK_BASISPROFIEL_API_KEY == "") {
      stop("API key is missing. Set it using `kvk_set_api_key(basisprofiel='your_key')`")
    }
  }

  # Validate 'include' parameter to ensure only allowed values are passed
  valid_options <- c("eigenaar", "hoofdvestiging", "vestigingen")
  if (!is.null(include)) {
    invalid_values <- setdiff(include, valid_options)
    if (length(invalid_values) > 0) {
      stop("Invalid values in 'include': ", paste(invalid_values, collapse = ", "),
           ". Choose from: ", paste(valid_options, collapse = ", "))
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
        httr2::req_headers(apikey = KVK_BASISPROFIEL_API_KEY, Accept = "application/json") |>
        httr2::req_url_query(geoData = tolower(as.character(geoData))) |>
        httr2::req_perform() |>
        httr2::resp_body_json() |>
        list_to_tibble()
    },
    error = function(e) {
      if (grepl("HTTP 404", e$message)) {
        message("No results found for KvK number: ", kvkNummer)
        return(NULL)
      } else {
        stop("Error retrieving basis profile: ", e$message)
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
    KVK_VESTIGINGSPROFIEL_API_KEY <- "l7xx1f2691f2520d487b902f4e0b57a0b197"
    message("You are using the KvK test environment. Test data will be returned.")
  } else {
    base_url <- "https://api.kvk.nl/api/v1/vestigingsprofielen/"
    KVK_VESTIGINGSPROFIEL_API_KEY <- Sys.getenv("KVK_VESTIGINGSPROFIEL_API_KEY")
    if (KVK_VESTIGINGSPROFIEL_API_KEY == "") {
      stop("API key is missing. Set it using `kvk_set_api_key(vestigingsprofiel='your_key')`")
    }
  }

  # Construct the dynamic URL based on parameters
  url_path <- paste0(base_url, vestigingsnummer)

  # Perform the API request with error handling
  response <- tryCatch(
    {
      httr2::request(url_path) |>
        httr2::req_headers(apikey = KVK_VESTIGINGSPROFIEL_API_KEY, Accept = "application/json") |>
        httr2::req_url_query(geoData = tolower(as.character(geoData))) |>
        httr2::req_perform() |>
        httr2::resp_body_json() |>
        list_to_tibble()
    },
    error = function(e) {
      if (grepl("HTTP 404", e$message)) {
        message("No results found for establishment number: ", vestigingsnummer)
        return(NULL)
      } else {
        stop("Error retrieving establishment profile: ", e$message)
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
    KVK_NAAMGEVING_API_KEY <- "l7xx1f2691f2520d487b902f4e0b57a0b197"
    message("You are using the KvK test environment. Test data will be returned.")
  } else {
    base_url <- "https://api.kvk.nl/api/v1/naamgevingen/kvknummer/"
    KVK_NAAMGEVING_API_KEY <- Sys.getenv("KVK_NAAMGEVING_API_KEY")
    if (KVK_NAAMGEVING_API_KEY == "") {
      stop("API key is missing. Set it using `kvk_set_api_key(naamgeving='your_key')`")
    }
  }

  # Construct the dynamic URL based on parameters
  url_path <- paste0(base_url, kvkNummer)

  # Perform the API request with error handling
  response <- tryCatch(
    {
      httr2::request(url_path) |>
        httr2::req_headers(apikey = KVK_NAAMGEVING_API_KEY, Accept = "application/json") |>
        httr2::req_perform() |>
        httr2::resp_body_json() |>
        list_to_tibble()
    },
    error = function(e) {
      if (grepl("HTTP 404", e$message)) {
        message("No results found for KvK number: ", kvkNummer)
        return(NULL)
      } else {
        stop("Error retrieving name information: ", e$message)
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
