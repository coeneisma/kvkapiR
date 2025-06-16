# Test list_to_tibble function robustness

test_that("unnamed lists are handled correctly", {
    input <- list(
      websites = list("https://example.com", "https://test.nl"),
      emails = list("test@example.com")
    )
    
    result <- kvkapiR:::list_to_tibble(input)
    expect_s3_class(result, "tbl_df")
    expect_true(is.list(result$websites))
    expect_true(is.list(result$emails))
})

test_that("mixed named/unnamed lists are handled", {
    input <- list(
      regular_field = "value",
      mixed_list = list(a = 1, b = 2, 3, 4),  # partially named
      unnamed_list = list(1, 2, 3)
    )
    
    result <- kvkapiR:::list_to_tibble(input)
    expect_s3_class(result, "tbl_df")
    expect_equal(result$regular_field, "value")
    expect_true(is.list(result$mixed_list))
    expect_true(is.list(result$unnamed_list))
})

test_that("lists with empty names are handled", {
    input <- list(
      field1 = "value1",
      field2 = structure(list("value", "value2"), names = c("", "name"))  # one empty name
    )
    
    result <- kvkapiR:::list_to_tibble(input)
    expect_s3_class(result, "tbl_df")
    expect_true(is.list(result$field2))
})

test_that("deeply nested structures are handled", {
    input <- list(
      level1 = list(
        level2 = list(
          level3 = list("deep", "values")
        )
      )
    )
    
    result <- kvkapiR:::list_to_tibble(input)
    expect_s3_class(result, "tbl_df")
    expect_true(is.list(result$level1))
})

test_that("NULL values are handled", {
    input <- list(
      field1 = "value",
      field2 = NULL,
      field3 = list(NULL, "value")
    )
    
    result <- kvkapiR:::list_to_tibble(input)
    expect_s3_class(result, "tbl_df")
    # NULL fields are included but have NA value
    expect_true("field2" %in% names(result))
    expect_true(is.na(result$field2))
    expect_true(is.list(result$field3))
})

# Integration test with real API data
test_that("kvk_get_vestigingsprofiel handles all field types correctly", {
  skip_if(Sys.getenv("KVK_API_KEY") == "", "API key not available")
  
  # Test multiple vestigingsnummers known to have different data structures
  test_vestigingen <- c(
    "000022666397",  # Known to have unnamed websites field
    "000022201726",  # Regular vestiging
    "000023151153"   # Another regular vestiging
  )
  
  for (vnr in test_vestigingen) {
    result <- kvk_get_vestigingsprofiel(vnr)
    
    if (!is.null(result)) {
      expect_s3_class(result, "tbl_df")
      expect_true("vestigingsnummer" %in% names(result))
      
      # Check that all columns are properly named
      expect_false(any(names(result) == ""))
      expect_false(any(is.na(names(result))))
      
      # If websites field exists, it should be a list
      if ("websites" %in% names(result)) {
        expect_true(is.list(result$websites))
      }
    }
  }
})

# Test all retrieve functions for robustness
test_that("all retrieve functions handle edge cases", {
  skip_if(Sys.getenv("KVK_API_KEY") == "", "API key not available")
  
  # Search for a variety of companies
  search_results <- kvk_search(plaats = "Amsterdam")
  
  if (nrow(search_results) > 0) {
    # Test basisprofiel
    for (i in 1:min(3, nrow(search_results))) {
      kvk <- search_results$kvkNummer[i]
      result <- kvk_get_basisprofiel(kvk)
      
      if (!is.null(result)) {
        expect_s3_class(result, "tbl_df")
        expect_false(any(names(result) == ""))
      }
    }
    
    # Test naamgeving
    for (i in 1:min(3, nrow(search_results))) {
      kvk <- search_results$kvkNummer[i]
      result <- kvk_get_naamgeving(kvk)
      
      if (!is.null(result)) {
        expect_s3_class(result, "tbl_df")
        expect_false(any(names(result) == ""))
      }
    }
  }
})