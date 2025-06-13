test_that("extract_http_status extracts status codes correctly", {
  # Test various error message formats
  expect_equal(extract_http_status("HTTP 404 Not Found"), 404)
  expect_equal(extract_http_status("Error: HTTP 401 Unauthorized"), 401)
  expect_equal(extract_http_status("Something went wrong HTTP 500"), 500)
  expect_equal(extract_http_status("HTTP 200 OK"), 200)
  
  # Test when no status code is present
  expect_true(is.na(extract_http_status("No status code here")))
  expect_true(is.na(extract_http_status("HTTP without number")))
  expect_true(is.na(extract_http_status("")))
})

test_that("handle_kvk_http_error shows correct messages for different status codes", {
  # Note: Updated tests to work with new function signature that accepts resp parameter
  # Test 400 Bad Request - expect warning
  expect_warning(
    handle_kvk_http_error(
      list(message = "HTTP 400 Bad Request"),
      api_name = "Test API",
      resource_id = "12345"
    ),
    "Bad request.*12345.*invalid"
  )
  
  # Test 401 Unauthorized - expect warning
  expect_warning(
    handle_kvk_http_error(
      list(message = "HTTP 401 Unauthorized"),
      api_name = "Test API"
    ),
    "Unauthorized.*Test API"
  )
  
  # Test 404 Not Found (should be info message, not warning)
  expect_message(
    handle_kvk_http_error(
      list(message = "HTTP 404 Not Found"),
      api_name = "Test API",
      resource_id = "67890"
    ),
    "No results found.*67890"
  )
  
  # Test 429 Rate Limit - expect warning
  expect_warning(
    handle_kvk_http_error(
      list(message = "HTTP 429 Too Many Requests"),
      api_name = "Test API"
    ),
    "Too many requests.*rate limit"
  )
  
  # Test 500 Internal Server Error - expect warning
  expect_warning(
    handle_kvk_http_error(
      list(message = "HTTP 500 Internal Server Error"),
      api_name = "Test API"
    ),
    "Internal server error"
  )
  
  # Test 503 Service Unavailable - expect warning
  expect_warning(
    handle_kvk_http_error(
      list(message = "HTTP 503 Service Unavailable"),
      api_name = "Test API"
    ),
    "Service unavailable"
  )
})

test_that("handle_kvk_http_error returns NULL", {
  # All error handling should return NULL
  result <- handle_kvk_http_error(
    list(message = "HTTP 404 Not Found"),
    api_name = "Test API"
  )
  
  expect_null(result)
})

test_that("handle_kvk_http_error handles unknown status codes", {
  # Test unknown HTTP status code
  expect_error(
    handle_kvk_http_error(
      list(message = "HTTP 418 I'm a teapot"),
      api_name = "Test API"
    ),
    "Error accessing Test API"
  )
  
  # Test no status code
  expect_error(
    handle_kvk_http_error(
      list(message = "Something went wrong"),
      api_name = "Test API"
    ),
    "Error accessing Test API"
  )
})

test_that("handle_kvk_http_error works without resource_id", {
  # Test that messages work when resource_id is NULL
  expect_warning(
    handle_kvk_http_error(
      list(message = "HTTP 400 Bad Request"),
      api_name = "Test API",
      resource_id = NULL
    ),
    "parameters.*invalid"
  )
  
  expect_message(
    handle_kvk_http_error(
      list(message = "HTTP 404 Not Found"),
      api_name = "Test API",
      resource_id = NULL
    ),
    "No results found"
  )
})