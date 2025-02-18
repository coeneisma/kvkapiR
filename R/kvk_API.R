
#' Set KvK API Key in .Renviron
#' @description `r lifecycle::badge('experimental')`
#'
#' This function sets or updates the `KVK_API_KEY` environment variable in the
#' `.Renviron` file. If the key already exists and `overwrite = FALSE`, no
#' changes are made.
#'
#' @param KVK_API_KEY A string containing the KvK API key to be stored in
#'   `.Renviron`.
#' @param overwrite A logical value indicating whether to overwrite an existing
#'   `KVK_API_KEY`. Defaults to `FALSE`.
#'
#' @details The function reads the `.Renviron` file, checks if a `KVK_API_KEY`
#'   entry exists, and:
#' - If the key exists and `overwrite = FALSE`, it returns a message and does nothing.
#' - If the key exists and `overwrite = TRUE`, it replaces the existing key.
#' - If the key does not exist, it appends a new `KVK_API_KEY` entry to `.Renviron`.
#'
#'   After setting the key, you may need to restart your R session for changes
#'   to take effect.
#'
#' @return Invisibly returns `TRUE` if the key was added/updated, or `FALSE` if
#'   no change was made.
#'
#' @examples
#' \dontrun{
#' # Set the API key if it is not already set
#' set_KVK_API_KEY("abcd1234")
#'
#' # Overwrite an existing API key
#' set_KVK_API_KEY("newkey5678", overwrite = TRUE)
#' }
#'
#' @export
kvk_set_api_key <- function(KVK_API_KEY, overwrite = FALSE) {
  renviron_path <- file.path(Sys.getenv("HOME"), ".Renviron")

  # Lees de inhoud van .Renviron als het bestand bestaat
  if (file.exists(renviron_path)) {
    renviron_content <- readLines(renviron_path)
  } else {
    renviron_content <- character(0)
  }

  # Kijk of er al een KVK_API_KEY regel bestaat
  key_exists <- any(grepl("^KVK_API_KEY=", renviron_content))

  if (key_exists && !overwrite) {
    message("KVK_API_KEY is already set. Use overwrite = TRUE to update it.")
    return(invisible(FALSE))
  }

  # Maak de nieuwe regel
  new_line <- paste0("KVK_API_KEY=", KVK_API_KEY)

  if (key_exists && overwrite) {
    # Overschrijf de bestaande KVK_API_KEY regel
    renviron_content <- sub("^KVK_API_KEY=.*", new_line, renviron_content)
  } else {
    # Voeg de nieuwe KVK_API_KEY regel toe
    renviron_content <- c(renviron_content, new_line)
  }

  # Schrijf de nieuwe inhoud terug naar .Renviron
  writeLines(renviron_content, renviron_path)

  message("KVK_API_KEY has been successfully set in .Renviron.")
  return(invisible(TRUE))
}


#' Retrieve all results from the KvK Search API (up to 1.000 results)
#'
#' @description `r lifecycle::badge('experimental')`
#'
#'   This function automatically paginates through the KvK API to collect
#'   available results. Due to API limitations, it retrieves a maximum of 1.000
#'   records.
#'
#' @param ... Named arguments passed to the API query (e.g., naam = "Koudum"). Available arguments are:
#'
#' * `kvkNummer`: Dutch KVK number. Consists of 8 digits.
#' * `rsin`: Legal Entities and Partnerships Identification Number.
#' * `vestigingsnummer`: Branch number. Unique number consisting of 12 digits.
#' * `naam`: The name under which an establishment or legal entity trades. This can be: active or inactive statutory name, trade name, or name.
#' * `straatnaam`
#' * `huisnummer`: Only in combination with postal code.
#' * `huisletter`: Only in combination with house number. Consists of 1 (alphabetic) character.
#' * `postcode`: Only in combination with house number.
#' * `plaats`
#' * `postbusnummer`: Only allowed in combination with postcode
#' * `type`: Filter by type: main branch, branch, and/or legal entity.  Combine multiple by using ‘&’. E.g. :   type=nevenvestiging&type=hoofdvestiging&type=rechtspersoon.
#' * `inclusiefInactieveRegistraties`: Possible values are "true" or "false". Default value is "false".
#'
#' @return A tibble containing the retrieved results with the following
#'   parameters:
#'
#' * `kvkNummer`: Dutch KVK number. Consists of 8 digits.
#' * `rsin`: If RSIN is also input.
#' * `vestigingsnummer`: Branch number. Unique number consisting of 12 digits.
#' * `naam`: The name under which a branch or legal entity trades. This field contains the (statutory) name or the first trade name. If you searched for a name that is not the statutory or first trade name, we will show the trade name you used for your search query.
#' * `adres`:  List which content indicates whether it's a correspondence or a visiting address, containing the following information:
#'   * `straatnaam`: Street name
#'   * `huisnummer`: If postal code and house number are also input.
#'   * `huisletter`: If postal code and house number are also input.
#'   * `postcode`: If postal code and house number are also input.
#'   * `plaats`: Place
#' * `type`: Main branch, branch office, and/or legal entity.
#' * `actief`: Indicates whether a company/establishment/legal entity is registered (Yes) or deregistered (No).
#' * `vervallenNaam`: Contains the expired trade name or statutory name with which this search result was found.
#' * `links`: No links will be shown if the deregistration occurred before 22 May 2010 and if "active" is "false". Otherwise a List containing:
#'
#'   1. Link to the first search result. 2. Link to Basisprofiel (based on KVK
#'   number). 3. Link to Vestigingsprofiel (based on branch number).
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
