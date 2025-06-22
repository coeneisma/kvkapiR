test_that("kvk_set_api_key sets environment variable correctly", {
  # Arrange
  test_key <- "test_api_key_12345"
  
  # Act
  expect_message(
    result <- kvk_set_api_key(test_key),
    "KVK_API_KEY has been set for this session"
  )
  
  # Assert
  expect_true(result)
  expect_invisible(kvk_set_api_key(test_key))  # Test invisibility separately
  expect_equal(Sys.getenv("KVK_API_KEY"), test_key)
})

