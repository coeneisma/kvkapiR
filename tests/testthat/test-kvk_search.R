test_that("kvk_search works with test environment", {
  # Test environment should work without API key
  Sys.unsetenv("KVK_API_KEY")
  
  expect_message(
    result <- kvk_search(plaats = "Utrecht", test_environment = TRUE),
    "You are using the KvK test environment"
  )
  
  # Check result structure
  expect_s3_class(result, "tbl_df")
  expect_true("kvkNummer" %in% names(result))
  expect_true("naam" %in% names(result))
})

test_that("kvk_search requires API key for production", {
  # Ensure no API key is set
  Sys.unsetenv("KVK_API_KEY")
  
  expect_error(
    kvk_search(plaats = "Amsterdam"),
    "API key is missing"
  )
})

test_that("kvk_search handles no results gracefully", {
  # First check if we have an API key from GitHub secrets
  if (Sys.getenv("KVK_SEARCH_API_KEY") != "") {
    Sys.setenv(KVK_API_KEY = Sys.getenv("KVK_SEARCH_API_KEY"))
  }
  
  skip_if(Sys.getenv("KVK_API_KEY") == "", "No API key available for testing")
  
  # Search for non-existent place
  expect_message(
    result <- kvk_search(plaats = "NonExistentPlaceXYZ123"),
    "No results found"
  )
  
  expect_null(result)
})

test_that("kvk_search handles various parameters", {
  # Test with test environment - use a valid search that returns results
  expect_message(
    result <- kvk_search(
      plaats = "Utrecht",
      test_environment = TRUE
    ),
    "You are using the KvK test environment"
  )
  
  if (!is.null(result)) {
    expect_s3_class(result, "tbl_df")
  }
})

test_that("kvk_search warns about >1000 results", {
  # First check if we have an API key from GitHub secrets
  if (Sys.getenv("KVK_SEARCH_API_KEY") != "") {
    Sys.setenv(KVK_API_KEY = Sys.getenv("KVK_SEARCH_API_KEY"))
  }
  
  skip_if(Sys.getenv("KVK_API_KEY") == "", "No API key available for testing")
  
  # Search for Rotterdam which should have >1000 results
  # cli::cli_alert_warning doesn't throw R warnings, so capture the message
  expect_message(
    result <- kvk_search(plaats = "Rotterdam"),
    "API response contains more than 1,000 results"
  )
  
  # Should still return results (max 1000)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1000)
})