# Save original API key to restore after tests
original_api_key <- Sys.getenv("KVK_API_KEY")

# Check for GitHub Actions secret
if (Sys.getenv("KVK_SEARCH_API_KEY") != "" && Sys.getenv("KVK_API_KEY") == "") {
  Sys.setenv(KVK_API_KEY = Sys.getenv("KVK_SEARCH_API_KEY"))
}

# Ensure cleanup after all tests
withr::defer({
  if (original_api_key != "") {
    Sys.setenv(KVK_API_KEY = original_api_key)
  } else {
    Sys.unsetenv("KVK_API_KEY")
  }
}, envir = testthat::teardown_env())