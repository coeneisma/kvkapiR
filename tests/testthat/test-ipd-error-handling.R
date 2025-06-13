test_that("get_ipd_error_message returns correct messages", {
  # Test IPD0004 with and without resource_id
  expect_equal(
    get_ipd_error_message("IPD0004", "12345"),
    "The KvK number '12345' is not valid"
  )
  
  expect_equal(
    get_ipd_error_message("IPD0004", NULL),
    "The KvK number is not valid"
  )
  
  # Test IPD0006 for branch numbers
  expect_equal(
    get_ipd_error_message("IPD0006", "123456789012"),
    "The branch number '123456789012' is not valid"
  )
  
  # Test general errors
  expect_equal(
    get_ipd_error_message("IPD1002"),
    "The data is temporarily unavailable because it is being processed. Please try again later"
  )
  
  expect_equal(
    get_ipd_error_message("IPD5200"),
    "No data was found that matches the specified search parameters"
  )
  
  expect_equal(
    get_ipd_error_message("IPD9999"),
    "A general technical error occurred"
  )
  
  # Test unknown code
  expect_equal(
    get_ipd_error_message("IPD0000"),
    "Error IPD0000 occurred"
  )
})

test_that("parse_error_response works with actual response structure", {
  # Skip these tests as they require complex mocking
  skip("Complex mocking required for httr2 responses")
})

test_that("handle_kvk_http_error falls back to generic messages without response", {
  # Test without response object (old behavior)
  expect_warning(
    handle_kvk_http_error(
      e = list(message = "HTTP 400 Bad Request"),
      api_name = "Test API",
      resource_id = "12345"
    ),
    "Bad request.*12345.*invalid"
  )
})

test_that("Integration: API functions show IPD error messages", {
  skip_if(Sys.getenv("KVK_API_KEY") == "", "No API key available")
  
  # Test with invalid KvK number (too short)
  expect_warning(
    result <- kvk_get_basisprofiel("12345", test_environment = TRUE),
    "The KvK number '12345' is not valid"
  )
  expect_null(result)
  
  # Test with invalid vestigingsnummer (too short)
  expect_warning(
    result <- kvk_get_vestigingsprofiel("12345", test_environment = TRUE),
    "The branch number '12345' is not valid"
  )
  expect_null(result)
})