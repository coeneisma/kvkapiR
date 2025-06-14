test_that("get_usage_file_path returns correct path", {
  path <- get_usage_file_path()
  expect_true(is.character(path))
  expect_true(grepl("\\.kvkapiR/usage\\.rds$", path))
})

test_that("load_usage_data returns empty data frame when no file exists", {
  # Use temporary file that doesn't exist
  temp_file <- tempfile(fileext = ".rds")
  
  # Mock get_usage_file_path to return temp file
  with_mocked_bindings(
    get_usage_file_path = function() temp_file,
    {
      data <- load_usage_data()
      expect_s3_class(data, "data.frame")
      expect_equal(nrow(data), 0)
      expect_equal(ncol(data), 6)
      expect_named(data, c("timestamp", "date", "year", "month", 
                          "call_type", "test_environment"))
    }
  )
})

test_that("save and load usage data works correctly", {
  # Create test data
  test_data <- data.frame(
    timestamp = Sys.time(),
    date = Sys.Date(),
    year = 2025L,
    month = 6L,
    call_type = "search",
    test_environment = FALSE,
    stringsAsFactors = FALSE
  )
  
  # Use temporary file
  temp_file <- tempfile(fileext = ".rds")
  
  # Mock get_usage_file_path
  with_mocked_bindings(
    get_usage_file_path = function() temp_file,
    {
      # Save data
      save_usage_data(test_data)
      expect_true(file.exists(temp_file))
      
      # Load data back
      loaded_data <- load_usage_data()
      expect_equal(nrow(loaded_data), 1)
      expect_equal(loaded_data$call_type, "search")
      expect_equal(loaded_data$test_environment, FALSE)
    }
  )
  
  # Clean up
  unlink(temp_file)
})

test_that("record_api_call skips test environment calls", {
  temp_file <- tempfile(fileext = ".rds")
  
  with_mocked_bindings(
    get_usage_file_path = function() temp_file,
    {
      # Record test environment call (should be skipped)
      record_api_call("search", test_environment = TRUE)
      
      data <- load_usage_data()
      expect_equal(nrow(data), 0)
      
      # Record production call (should be saved)
      record_api_call("search", test_environment = FALSE)
      
      data <- load_usage_data()
      expect_equal(nrow(data), 1)
      expect_equal(data$call_type[1], "search")
      expect_false(data$test_environment[1])
    }
  )
  
  unlink(temp_file)
})

test_that("record_api_call respects KVKAPI_DISABLE_TRACKING", {
  temp_file <- tempfile(fileext = ".rds")
  
  # Save current env var
  old_env <- Sys.getenv("KVKAPI_DISABLE_TRACKING")
  
  with_mocked_bindings(
    get_usage_file_path = function() temp_file,
    {
      # Enable tracking disable
      Sys.setenv(KVKAPI_DISABLE_TRACKING = "true")
      
      # Try to record call (should be skipped)
      record_api_call("search", test_environment = FALSE)
      
      data <- load_usage_data()
      expect_equal(nrow(data), 0)
      
      # Re-enable tracking
      Sys.setenv(KVKAPI_DISABLE_TRACKING = "false")
      
      # Record call (should work now)
      record_api_call("basisprofiel", test_environment = FALSE)
      
      data <- load_usage_data()
      expect_equal(nrow(data), 1)
      expect_equal(data$call_type[1], "basisprofiel")
    }
  )
  
  # Restore env var
  if (old_env == "") {
    Sys.unsetenv("KVKAPI_DISABLE_TRACKING")
  } else {
    Sys.setenv(KVKAPI_DISABLE_TRACKING = old_env)
  }
  
  unlink(temp_file)
})

test_that("calculate_costs works correctly", {
  # Test with empty data
  costs <- calculate_costs(data.frame())
  expect_equal(costs$total_calls, 0)
  expect_equal(costs$free_calls, 0)
  expect_equal(costs$paid_calls, 0)
  expect_equal(costs$total_costs, 0)
  
  # Test with mixed data
  test_data <- data.frame(
    timestamp = rep(Sys.time(), 5),
    date = rep(Sys.Date(), 5),
    year = rep(2025L, 5),
    month = c(1L, 1L, 2L, 2L, 2L),
    call_type = c("search", "search", "basisprofiel", 
                  "vestigingsprofiel", "naamgeving"),
    test_environment = rep(FALSE, 5)
  )
  
  costs <- calculate_costs(test_data)
  expect_equal(costs$total_calls, 5)
  expect_equal(costs$free_calls, 2)  # 2 search calls
  expect_equal(costs$paid_calls, 3)   # 3 paid calls
  expect_equal(costs$query_costs, 0.06)  # 3 * 0.02
  expect_equal(costs$months_active, 2)   # January and February
  expect_equal(costs$base_costs, 12.40)  # 2 * 6.20
  expect_equal(costs$total_costs, 12.46) # 12.40 + 0.06
})

test_that("kvk_usage_report returns correct format", {
  temp_file <- tempfile(fileext = ".rds")
  
  with_mocked_bindings(
    get_usage_file_path = function() temp_file,
    {
      # Test with no data - expect message about no data
      expect_message(result <- kvk_usage_report(format = "summary"), 
                     "No API usage recorded yet")
      expect_null(result)
      
      tibble_result <- kvk_usage_report(format = "tibble")
      expect_s3_class(tibble_result, "tbl_df")
      expect_equal(nrow(tibble_result), 0)
      
      tidy_result <- kvk_usage_report(format = "tidy")
      expect_s3_class(tidy_result, "tbl_df")
      expect_equal(nrow(tidy_result), 0)
      
      # Add some test data
      test_data <- data.frame(
        timestamp = Sys.time(),
        date = Sys.Date(),
        year = 2025L,
        month = 6L,
        call_type = "search",
        test_environment = FALSE
      )
      save_usage_data(test_data)
      
      # Test tibble format
      tibble_result <- kvk_usage_report(format = "tibble")
      expect_s3_class(tibble_result, "tbl_df")
      expect_equal(nrow(tibble_result), 1)
      expect_true("total_cost" %in% names(tibble_result))
      
      # Test tidy format
      tidy_result <- kvk_usage_report(format = "tidy")
      expect_s3_class(tidy_result, "tbl_df")
      expect_equal(nrow(tidy_result), 1)
      expect_equal(tidy_result$call_type[1], "search")
    }
  )
  
  unlink(temp_file)
})

test_that("kvk_reset_usage works correctly", {
  temp_file <- tempfile(fileext = ".rds")
  
  with_mocked_bindings(
    get_usage_file_path = function() temp_file,
    {
      # Test with no file
      result <- kvk_reset_usage(confirm = FALSE)
      expect_false(result)
      
      # Create some data
      test_data <- data.frame(
        timestamp = Sys.time(),
        date = Sys.Date(),
        year = 2025L,
        month = 6L,
        call_type = "search",
        test_environment = FALSE
      )
      save_usage_data(test_data)
      expect_true(file.exists(temp_file))
      
      # Reset without confirm
      result <- kvk_reset_usage(confirm = FALSE)
      expect_true(result)
      expect_false(file.exists(temp_file))
      
      # Verify data is gone
      data <- load_usage_data()
      expect_equal(nrow(data), 0)
    }
  )
})

test_that("kvk_export_usage works correctly", {
  temp_file <- tempfile(fileext = ".rds")
  temp_csv <- tempfile(fileext = ".csv")
  
  with_mocked_bindings(
    get_usage_file_path = function() temp_file,
    {
      # Test with no data
      result <- kvk_export_usage(file = temp_csv)
      expect_null(result)
      
      # Add test data
      test_data <- data.frame(
        timestamp = Sys.time(),
        date = Sys.Date(),
        year = 2025L,
        month = 6L,
        call_type = c("search", "basisprofiel"),
        test_environment = FALSE
      )
      save_usage_data(test_data)
      
      # Export tidy format
      result <- kvk_export_usage(file = temp_csv, format = "tidy")
      expect_equal(result, temp_csv)
      expect_true(file.exists(temp_csv))
      
      # Read exported data
      exported <- read.csv(temp_csv)
      expect_equal(nrow(exported), 2)
      expect_true("call_type" %in% names(exported))
      
      # Clean up
      unlink(temp_csv)
      
      # Export monthly format
      result <- kvk_export_usage(file = temp_csv, format = "monthly")
      expect_equal(result, temp_csv)
      expect_true(file.exists(temp_csv))
      
      # Read monthly data
      monthly <- read.csv(temp_csv)
      expect_equal(nrow(monthly), 1)  # One month
      expect_true("total_cost" %in% names(monthly))
      
      unlink(temp_csv)
    }
  )
  
  unlink(temp_file)
})

test_that("kvk_usage_alert sets options correctly", {
  # Save current options
  old_calls <- getOption("kvkapiR.alert_max_calls")
  old_cost <- getOption("kvkapiR.alert_max_cost")
  old_period <- getOption("kvkapiR.alert_period")
  
  # Test setting alerts - expect success message
  expect_message(kvk_usage_alert(max_calls = 100, max_cost = 50, period = "month"),
                 "Usage alerts configured")
  expect_equal(getOption("kvkapiR.alert_max_calls"), 100)
  expect_equal(getOption("kvkapiR.alert_max_cost"), 50)
  expect_equal(getOption("kvkapiR.alert_period"), "month")
  
  # Test disabling alerts - expect success message
  expect_message(kvk_usage_alert(max_calls = NULL, max_cost = NULL),
                 "Usage alerts disabled")
  expect_null(getOption("kvkapiR.alert_max_calls"))
  expect_null(getOption("kvkapiR.alert_max_cost"))
  
  # Restore options
  options(
    kvkapiR.alert_max_calls = old_calls,
    kvkapiR.alert_max_cost = old_cost,
    kvkapiR.alert_period = old_period
  )
})

test_that("check_usage_alerts triggers correctly", {
  # Create test data that exceeds limits
  test_data <- data.frame(
    timestamp = rep(Sys.time(), 10),
    date = rep(Sys.Date(), 10),
    year = rep(as.integer(format(Sys.Date(), "%Y")), 10),
    month = rep(as.integer(format(Sys.Date(), "%m")), 10),
    call_type = c(rep("search", 5), rep("basisprofiel", 5)),
    test_environment = rep(FALSE, 10)
  )
  
  # Set low limits
  options(
    kvkapiR.alert_max_calls = 5,
    kvkapiR.alert_max_cost = 5,
    kvkapiR.alert_period = "month"
  )
  
  # Should trigger alerts (capture output)
  expect_message(
    check_usage_alerts(test_data),
    "Usage alert"
  )
  
  # Clean up
  options(
    kvkapiR.alert_max_calls = NULL,
    kvkapiR.alert_max_cost = NULL,
    kvkapiR.alert_period = NULL
  )
})