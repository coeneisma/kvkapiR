# Constants for KvK API
KVK_TEST_API_KEY <- "l7xx1f2691f2520d487b902f4e0b57a0b197"
KVK_TEST_BASE_URL <- "https://api.kvk.nl/test/api"
KVK_PROD_BASE_URL <- "https://api.kvk.nl/api"
KVK_MAX_RESULTS <- 1000L
KVK_RESULTS_PER_PAGE <- 100L

#' Set KvK API Key for the current session
#'
#'
#'   This function sets the specified KvK API key for the **current R session**
#'   using `Sys.setenv()`. The key will be available until the session ends.
#'
#' @param api_key A string containing the API key.
#'
#' @details The function sets the API key using `Sys.setenv()`, making
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
#' kvk_set_api_key("abcd1234")
#' }
#'
#' @export
kvk_set_api_key <- function(api_key) {
  Sys.setenv(KVK_API_KEY = api_key)
  cli::cli_alert_success("KVK_API_KEY has been set for this session.")
  invisible(TRUE)
}

#' Store KvK API Key in .Renviron for persistent use
#'
#'
#'   This function saves the specified KvK API key to the `.Renviron` file,
#'   making it persist across R sessions. If an API key is already stored, you
#'   must set `overwrite = TRUE` to replace it.
#'
#' @param api_key A string containing the API key.
#' @param overwrite A logical value indicating whether to overwrite an existing
#'   API key entry in `.Renviron`. Defaults to `FALSE`.
#'
#' @details The function modifies `.Renviron` to store the API key permanently.
#' - If the key exists and `overwrite = FALSE`, it returns an error.
#' - If the key exists and `overwrite = TRUE`, it replaces the existing key.
#' - If the key does not exist, it appends a new API key entry to `.Renviron`.
#'
#' **Important:** You must restart R for the changes to take effect.
#'
#' @return Invisibly returns `TRUE` if the key was added or updated.
#'
#' @examples
#' \dontrun{
#' # Store the API key persistently
#' kvk_store_api_key("abcd1234")
#'
#' # Overwrite an existing API key
#' kvk_store_api_key("newkey5678", overwrite = TRUE)
#' }
#'
#' @export
kvk_store_api_key <- function(api_key, overwrite = FALSE) {
  renviron_path <- file.path(Sys.getenv("HOME"), ".Renviron")

  # Make a backup of .Renviron for safety
  if (file.exists(renviron_path)) {
    file.copy(renviron_path, paste0(renviron_path, "_backup"), overwrite = TRUE)
    renviron_content <- readLines(renviron_path)
  } else {
    renviron_content <- character(0)
  }

  # Look for existing API key entry
  api_key_var <- "KVK_API_KEY"
  key_exists <- any(grepl(paste0("^", api_key_var, "="), renviron_content))

  if (key_exists && !overwrite) {
    cli::cli_abort("KVK_API_KEY already exists in .Renviron. Use overwrite = TRUE to update it.")
  }

  # Filter out old versions of the key
  renviron_content <- renviron_content[!grepl(paste0("^", api_key_var, "="), renviron_content)]

  # Add the new key
  renviron_content <- c(renviron_content, paste0(api_key_var, "=", api_key))

  # Write the modified content back
  writeLines(renviron_content, renviron_path)

  cli::cli_alert_success("KVK_API_KEY has been stored in .Renviron. Restart R for changes to take effect.")

  invisible(TRUE)
}
