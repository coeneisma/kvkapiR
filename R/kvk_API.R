# Constants for KvK API
KVK_TEST_API_KEY <- "l7xx1f2691f2520d487b902f4e0b57a0b197"
KVK_TEST_BASE_URL <- "https://api.kvk.nl/test/api"
KVK_PROD_BASE_URL <- "https://api.kvk.nl/api"
KVK_MAX_RESULTS <- 1000L
KVK_RESULTS_PER_PAGE <- 100L

#' Set KvK API Key for the current session
#'
#' This function sets the specified KvK API key for the **current R session**
#' using `Sys.setenv()`. The key will be available until the session ends.
#'
#' @param api_key A string containing the API key.
#'
#' @details The function sets the API key using `Sys.setenv()`, making
#'   it available for the current session only.
#'   
#'   To store your API key permanently across R sessions, add the following 
#'   line to your .Renviron file:
#'   
#'   \code{KVK_API_KEY=l7xxYourActualApiKeyHere1234567890abcdef}
#'   
#'   You can edit your .Renviron file by running:
#'   \code{usethis::edit_r_environ()}
#'   
#'   After adding the key, restart R for the changes to take effect.
#'
#' @return Invisibly returns `TRUE` if the key was set.
#'
#' @examples
#' \dontrun{
#' # Set the API key for the current session
#' kvk_set_api_key("abcd1234")
#' 
#' # To store permanently, add this line to .Renviron:
#' # KVK_API_KEY=your_actual_api_key_here
#' }
#'
#' @export
kvk_set_api_key <- function(api_key) {
  Sys.setenv(KVK_API_KEY = api_key)
  cli::cli_alert_success("KVK_API_KEY has been set for this session.")
  invisible(TRUE)
}

