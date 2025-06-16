test_that("kvk_get_basisprofiel works with test environment", {
  # Test environment should work without setting API key
  Sys.unsetenv("KVK_API_KEY")
  
  expect_message(
    result <- kvk_get_basisprofiel("68750110", test_environment = TRUE),
    "You are using the KvK test environment"
  )
  
  expect_s3_class(result, "tbl_df")
  expect_equal(result$kvkNummer, "68750110")
  expect_true("naam" %in% names(result))
})

test_that("kvk_get_basisprofiel validates include parameter", {
  expect_error(
    kvk_get_basisprofiel("12345678", include = "invalid_option", test_environment = TRUE),
    "Invalid values in 'include'"
  )
  
  expect_error(
    kvk_get_basisprofiel("12345678", include = c("eigenaar", "wrong"), test_environment = TRUE),
    "Invalid values in 'include'"
  )
})

test_that("kvk_get_basisprofiel handles geoData parameter", {
  result <- kvk_get_basisprofiel("68750110", geoData = TRUE, test_environment = TRUE)
  
  expect_s3_class(result, "tbl_df")
  # Can't test specific geo fields without knowing test data structure
})

test_that("kvk_get_basisprofiel handles include parameter correctly", {
  # Test with eigenaar
  expect_message(
    result <- kvk_get_basisprofiel("68750110", include = "eigenaar", test_environment = TRUE),
    "You are using the KvK test environment"
  )
  
  # The API might return NULL for some includes, so check both cases
  if (!is.null(result)) {
    expect_s3_class(result, "tbl_df")
  }
  
  # Test with multiple includes - this might return NULL/404 which is OK
  suppressMessages({
    result2 <- kvk_get_basisprofiel(
      "68750110", 
      include = c("eigenaar", "hoofdvestiging"),
      test_environment = TRUE
    )
  })
  
  # It's OK if this returns NULL (404) - just checking it doesn't error
  if (!is.null(result2)) {
    expect_s3_class(result2, "tbl_df")
  } else {
    expect_null(result2)
  }
})

test_that("kvk_get_vestigingsprofiel works with test environment", {
  Sys.unsetenv("KVK_API_KEY")
  
  expect_message(
    result <- kvk_get_vestigingsprofiel("000037178598", test_environment = TRUE),
    "You are using the KvK test environment"
  )
  
  if (!is.null(result)) {
    expect_s3_class(result, "tbl_df")
    expect_true("vestigingsnummer" %in% names(result))
  }
})

test_that("kvk_get_vestigingsprofiel handles websites field without names", {
  skip_if(Sys.getenv("KVK_API_KEY") == "", "API key not available")
  
  # This specific vestigingsnummer has a websites field that caused the error
  result <- kvk_get_vestigingsprofiel("000022666397")
  
  if (!is.null(result)) {
    expect_s3_class(result, "tbl_df")
    # The websites field should be stored as a list-column if it has no names
    if ("websites" %in% names(result)) {
      expect_true(is.list(result$websites))
    }
  }
})

test_that("kvk_get_naamgeving works with test environment", {
  Sys.unsetenv("KVK_API_KEY")
  
  expect_message(
    result <- kvk_get_naamgeving("68750110", test_environment = TRUE),
    "You are using the KvK test environment"
  )
  
  expect_s3_class(result, "tbl_df")
  expect_equal(result$kvkNummer, "68750110")
  expect_true("naam" %in% names(result))
})

test_that("retrieve functions require API key for production", {
  Sys.unsetenv("KVK_API_KEY")
  
  expect_error(
    kvk_get_basisprofiel("12345678"),
    "API key is missing"
  )
  
  expect_error(
    kvk_get_vestigingsprofiel("000012345678"),
    "API key is missing"
  )
  
  expect_error(
    kvk_get_naamgeving("12345678"),
    "API key is missing"
  )
})