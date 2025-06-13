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

test_that("kvk_store_api_key creates backup and writes to .Renviron", {
  # Skip on CRAN and CI
  skip_on_cran()
  skip_on_ci()
  
  # Create temporary .Renviron
  temp_home <- tempdir()
  original_home <- Sys.getenv("HOME")
  Sys.setenv(HOME = temp_home)
  
  withr::defer({
    Sys.setenv(HOME = original_home)
  })
  
  temp_renviron <- file.path(temp_home, ".Renviron")
  
  # Test 1: Write new key to empty .Renviron
  test_key <- "test_key_123"
  
  expect_message(
    kvk_store_api_key(test_key),
    "KVK_API_KEY has been stored in .Renviron"
  )
  
  expect_true(file.exists(temp_renviron))
  content <- readLines(temp_renviron)
  expect_true(any(grepl("^KVK_API_KEY=test_key_123$", content)))
  
  # Test 2: Attempt to overwrite without overwrite = TRUE
  expect_error(
    kvk_store_api_key("new_key_456"),
    "KVK_API_KEY already exists in .Renviron"
  )
  
  # Test 3: Overwrite with overwrite = TRUE
  expect_message(
    kvk_store_api_key("new_key_456", overwrite = TRUE),
    "KVK_API_KEY has been stored in .Renviron"
  )
  
  content <- readLines(temp_renviron)
  expect_true(any(grepl("^KVK_API_KEY=new_key_456$", content)))
  expect_false(any(grepl("^KVK_API_KEY=test_key_123$", content)))
  
  # Test 4: Check backup was created
  backup_file <- paste0(temp_renviron, "_backup")
  expect_true(file.exists(backup_file))
  
  # Cleanup
  unlink(temp_renviron)
  unlink(backup_file)
})