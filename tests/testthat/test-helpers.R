test_that("list_to_tibble converts simple list correctly", {
  # Simple list
  simple_list <- list(
    id = "123",
    name = "Test Company",
    employees = 10
  )
  
  result <- list_to_tibble(simple_list)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$id, "123")
  expect_equal(result$name, "Test Company")
  expect_equal(result$employees, 10)
})

test_that("list_to_tibble handles nested lists", {
  # Nested list with uniform structure
  nested_list <- list(
    id = "123",
    addresses = list(
      street = c("Main St", "Second St"),
      number = c(1, 2)
    )
  )
  
  result <- list_to_tibble(nested_list)
  
  expect_s3_class(result, "tbl_df")
  # The function converts uniform lists to tibbles
  expect_s3_class(result$addresses, "tbl_df")
  expect_equal(nrow(result$addresses), 2)
})

test_that("list_to_tibble handles non-uniform nested lists", {
  # Non-uniform nested list
  complex_list <- list(
    id = "123",
    data = list(
      list(a = 1, b = 2),
      list(a = 3, b = 4, c = 5)  # Different structure
    )
  )
  
  result <- list_to_tibble(complex_list)
  
  expect_s3_class(result, "tbl_df")
  expect_true(is.list(result$data))
})

test_that("list_to_tibble handles NULL and empty values", {
  list_with_nulls <- list(
    id = "123",
    name = NULL,
    value = character(0)
  )
  
  result <- list_to_tibble(list_with_nulls)
  
  expect_s3_class(result, "tbl_df")
  # NULL values become NA in tibbles
  expect_true(is.na(result$name[1]))
  # Empty character vectors remain empty
  expect_equal(result$value, character(0))
})

test_that("list_to_tibble errors on non-list input", {
  expect_error(
    list_to_tibble("not a list"),
    "Input must be a list"
  )
  
  expect_error(
    list_to_tibble(123),
    "Input must be a list"
  )
  
  # data.frame is technically a list in R, so we need to skip this test
  # or modify the function to specifically check for data.frame
})