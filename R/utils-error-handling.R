#' Get IPD error message in English
#'
#' @description
#' Internal function to translate IPD error codes to user-friendly English messages.
#'
#' @param ipd_code Character string with the IPD error code (e.g., "IPD0004")
#' @param resource_id Optional resource ID to include in the message
#'
#' @return Character string with the error message
#'
#' @keywords internal
#' @noRd
get_ipd_error_message <- function(ipd_code, resource_id = NULL) {
  # IPD code translations based on KvK documentation
  messages <- list(
    "IPD0001" = "The requested product does not exist",
    "IPD0004" = if (!is.null(resource_id)) {
      paste0("The KvK number '", resource_id, "' is not valid")
    } else {
      "The KvK number is not valid"
    },
    "IPD0005" = if (!is.null(resource_id)) {
      paste0("The product cannot be delivered for KvK number '", resource_id, "'")
    } else {
      "The product cannot be delivered for this KvK number"
    },
    "IPD0006" = if (!is.null(resource_id)) {
      paste0("The branch number '", resource_id, "' is not valid")
    } else {
      "The branch number (vestigingsnummer) is not valid"
    },
    "IPD0007" = if (!is.null(resource_id)) {
      paste0("The product cannot be delivered for branch number '", resource_id, "'")
    } else {
      "The product cannot be delivered for this branch number"
    },
    "IPD0010" = if (!is.null(resource_id)) {
      paste0("The RSIN number '", resource_id, "' is not valid")
    } else {
      "The RSIN number is not valid"
    },
    "IPD1002" = "The data is temporarily unavailable because it is being processed. Please try again later",
    "IPD1998" = "General input parameter error",
    "IPD1999" = "The specified parameter(s) are invalid",
    "IPD5200" = "No data was found that matches the specified search parameters",
    "IPD5203" = "The specified type is not valid",
    "IPD9999" = "A general technical error occurred"
  )
  
  # Return specific message or generic one
  msg <- messages[[ipd_code]]
  if (is.null(msg)) {
    return(paste0("Error ", ipd_code, " occurred"))
  }
  return(msg)
}

#' Parse error response from KvK API
#'
#' @description
#' Internal function to parse error responses from the KvK API and extract
#' IPD error codes and messages.
#'
#' @param resp httr2 response object
#'
#' @return List with error_code and error_message, or NULL if no error found
#'
#' @keywords internal
#' @noRd
parse_error_response <- function(resp) {
  # Try to get the response body as JSON
  body <- tryCatch({
    httr2::resp_body_json(resp)
  }, error = function(e) {
    return(NULL)
  })
  
  # Check if we have an error structure
  if (!is.null(body) && !is.null(body$fout) && is.list(body$fout)) {
    # Get the first error (usually there's only one)
    error_info <- body$fout[[1]]
    
    if (!is.null(error_info$code)) {
      return(list(
        error_code = error_info$code,
        error_message = error_info$omschrijving %||% NA_character_
      ))
    }
  }
  
  return(NULL)
}

#' Handle HTTP errors from KvK API requests with IPD support
#'
#' @description
#' Internal function to handle HTTP errors from KvK API requests in a consistent manner.
#' Extracts the HTTP status code and IPD error codes to provide specific error messages.
#'
#' @param e The error object caught by tryCatch (can be NULL if called with response only)
#' @param api_name Character string identifying which API was called
#' @param resource_id Optional character string with the resource ID
#' @param resp Optional httr2 response object for extracting IPD codes
#' @param status_code Optional HTTP status code if already known
#'
#' @return Always returns NULL. This function is called for its side effects.
#'
#' @keywords internal
#' @noRd
handle_kvk_http_error <- function(e = NULL, api_name, resource_id = NULL, resp = NULL, status_code = NULL) {
  # Get status code from error or parameter
  if (is.null(status_code) && !is.null(e)) {
    status_code <- extract_http_status(e$message)
  }
  
  # Try to get IPD error info from response
  ipd_info <- NULL
  if (!is.null(resp)) {
    ipd_info <- parse_error_response(resp)
  }
  
  # If we have IPD error info, use that for the message
  if (!is.null(ipd_info)) {
    msg <- get_ipd_error_message(ipd_info$error_code, resource_id)
    
    # Add HTTP status for context
    msg <- paste0(msg, " (HTTP ", status_code, ")")
    
    # Show appropriate message type based on status code
    if (status_code == 404) {
      cli::cli_alert_info(msg)
    } else {
      cli::cli_warn(msg)
    }
    
    return(NULL)
  }
  
  # Fall back to generic HTTP status messages
  if (is.na(status_code)) {
    if (!is.null(e)) {
      cli::cli_abort("Error accessing {api_name}: {e$message}")
    } else {
      cli::cli_abort("Error accessing {api_name}")
    }
    return(NULL)
  }
  
  # Generic messages based on HTTP status
  if (status_code == 400) {
    msg <- "Bad request (HTTP 400). "
    if (!is.null(resource_id)) {
      msg <- paste0(msg, "The provided ID '", resource_id, "' or parameters are invalid.")
    } else {
      msg <- paste0(msg, "The parameters provided are invalid.")
    }
    cli::cli_warn(msg)
    
  } else if (status_code == 401) {
    cli::cli_warn("Unauthorized (HTTP 401). Your API key is missing or invalid, or you don't have access to the {api_name}.")
    
  } else if (status_code == 403) {
    cli::cli_warn("Forbidden (HTTP 403). You don't have permission to access this resource.")
    
  } else if (status_code == 404) {
    if (!is.null(resource_id)) {
      cli::cli_alert_info("No results found (HTTP 404). No data available for ID: {.val {resource_id}}")
    } else {
      cli::cli_alert_info("No results found (HTTP 404).")
    }
    
  } else if (status_code == 429) {
    cli::cli_warn("Too many requests (HTTP 429). You have exceeded the rate limit.")
    
  } else if (status_code == 500) {
    cli::cli_warn("Internal server error (HTTP 500). The KVK API is experiencing issues.")
    
  } else if (status_code == 503) {
    cli::cli_warn("Service unavailable (HTTP 503). The KVK API is currently unavailable or undergoing maintenance.")
    
  } else {
    # For any other error, show the original error message
    if (!is.null(e)) {
      cli::cli_abort("Error accessing {api_name}: {e$message}")
    } else {
      cli::cli_abort("Error accessing {api_name} (HTTP {status_code})")
    }
  }
  
  return(NULL)
}

#' Extract HTTP status code from error message
#'
#' @description
#' Internal function to extract HTTP status codes from error messages.
#' Looks for patterns like "HTTP 404" in the error text.
#'
#' @param error_message Character string containing the error message
#'
#' @return Numeric HTTP status code, or NA if no status code is found
#'
#' @keywords internal
#' @noRd
extract_http_status <- function(error_message) {
  status_match <- regexpr("HTTP ([0-9]{3})", error_message)
  
  if (status_match > 0) {
    status_code <- as.numeric(substr(error_message, 
                                     status_match + 5, 
                                     status_match + 7))
    return(status_code)
  }
  
  return(NA)
}