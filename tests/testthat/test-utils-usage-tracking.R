# Tests for session-based usage tracking functionality

# Skip usage tracking tests that simulate production calls if no API key available
# Even though these tests don't make real API calls, they simulate production behavior
skip_if_no_api_key <- function() {
  skip_if_not(nzchar(Sys.getenv("KVK_SEARCH_API_KEY")), "API key not available for usage tracking tests")
}

test_that("init_session_usage creates correct structure", {
  # Reset session data first
  if (exists("usage_data", envir = .kvkapi_session_usage)) {
    rm("usage_data", envir = .kvkapi_session_usage)
  }
  
  data <- init_session_usage()
  
  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 0)
  expect_equal(ncol(data), 6)
  expect_named(data, c("timestamp", "date", "year", "month", "call_type", "test_environment"))
  expect_true(inherits(data$timestamp, "POSIXct"))
  expect_true(inherits(data$date, "Date"))
  expect_true(is.integer(data$year))
  expect_true(is.integer(data$month))
  expect_true(is.character(data$call_type))
  expect_true(is.logical(data$test_environment))
})

test_that("load_usage_data and save_usage_data work correctly", {
  # Reset session data
  if (exists("usage_data", envir = .kvkapi_session_usage)) {
    rm("usage_data", envir = .kvkapi_session_usage)
  }
  
  # Load empty data
  data <- load_usage_data()
  expect_equal(nrow(data), 0)
  
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
  
  # Save and reload
  save_usage_data(test_data)
  loaded_data <- load_usage_data()
  
  expect_equal(nrow(loaded_data), 1)
  expect_equal(loaded_data$call_type, "search")
  expect_equal(loaded_data$test_environment, FALSE)
})

test_that("usage_tracking_enabled respects environment variable", {
  # Test default (enabled)
  expect_true(usage_tracking_enabled())
  
  # Test disabled
  withr::with_envvar(
    c(KVKAPI_DISABLE_TRACKING = "true"),
    {
      expect_false(usage_tracking_enabled())
    }
  )
  
  # Test case insensitive
  withr::with_envvar(
    c(KVKAPI_DISABLE_TRACKING = "TRUE"),
    {
      expect_false(usage_tracking_enabled())
    }
  )
  
  # Test other values don't disable
  withr::with_envvar(
    c(KVKAPI_DISABLE_TRACKING = "false"),
    {
      expect_true(usage_tracking_enabled())
    }
  )
})

test_that("record_api_call works correctly", {
  skip_if_no_api_key()
  
  # Reset session data
  if (exists("usage_data", envir = .kvkapi_session_usage)) {
    rm("usage_data", envir = .kvkapi_session_usage)
  }
  
  # Test environment calls should be skipped
  record_api_call("search", test_environment = TRUE)
  data <- load_usage_data()
  expect_equal(nrow(data), 0)
  
  # Production calls should be recorded
  record_api_call("search", test_environment = FALSE)
  data <- load_usage_data()
  expect_equal(nrow(data), 1)
  expect_equal(data$call_type[1], "search")
  expect_false(data$test_environment[1])
  
  # Multiple calls should accumulate
  record_api_call("basisprofiel", test_environment = FALSE)
  data <- load_usage_data()
  expect_equal(nrow(data), 2)
  expect_equal(data$call_type[2], "basisprofiel")
})

test_that("record_api_call respects tracking preferences", {
  skip_if_no_api_key()
  
  # Reset session data
  if (exists("usage_data", envir = .kvkapi_session_usage)) {
    rm("usage_data", envir = .kvkapi_session_usage)
  }
  
  # Test with tracking disabled
  withr::with_envvar(
    c(KVKAPI_DISABLE_TRACKING = "true"),
    {
      record_api_call("search", test_environment = FALSE)
      data <- load_usage_data()
      expect_equal(nrow(data), 0)
    }
  )
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
    month = rep(6L, 5),
    call_type = c("search", "search", "basisprofiel", "vestigingsprofiel", "naamgeving"),
    test_environment = rep(FALSE, 5),
    stringsAsFactors = FALSE
  )
  
  costs <- calculate_costs(test_data)
  expect_equal(costs$total_calls, 5)
  expect_equal(costs$free_calls, 2)  # 2 search calls
  expect_equal(costs$paid_calls, 3)  # 3 paid calls
  expect_equal(costs$total_costs, 0.06)  # 3 * 0.02
  
  # Test with test environment calls (should be filtered out)
  test_data_with_test <- test_data
  test_data_with_test$test_environment[1:2] <- TRUE
  
  costs_filtered <- calculate_costs(test_data_with_test)
  expect_equal(costs_filtered$total_calls, 3)  # Only production calls
  expect_equal(costs_filtered$free_calls, 0)   # No production search calls
  expect_equal(costs_filtered$paid_calls, 3)   # 3 paid calls
  expect_equal(costs_filtered$total_costs, 0.06)
})

test_that("kvk_usage_report returns correct formats", {
  # Reset session data
  if (exists("usage_data", envir = .kvkapi_session_usage)) {
    rm("usage_data", envir = .kvkapi_session_usage)
  }
  
  # Test with no data
  expect_message(result <- kvk_usage_report(format = "summary"), "No usage data found")
  expect_null(result)
  
  tibble_result <- kvk_usage_report(format = "tibble")
  expect_s3_class(tibble_result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(tibble_result), 0)
  
  detailed_result <- kvk_usage_report(format = "detailed")
  expect_s3_class(detailed_result, "data.frame")
  expect_equal(nrow(detailed_result), 0)
  
  # Add test data
  test_data <- data.frame(
    timestamp = c(Sys.time(), Sys.time() + 1),
    date = rep(Sys.Date(), 2),
    year = rep(2025L, 2),
    month = rep(6L, 2),
    call_type = c("search", "basisprofiel"),
    test_environment = rep(FALSE, 2),
    stringsAsFactors = FALSE
  )
  save_usage_data(test_data)
  
  # Test tibble format
  tibble_result <- kvk_usage_report(format = "tibble")
  expect_s3_class(tibble_result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(tibble_result), 1)  # Session summary
  expect_true("Search" %in% names(tibble_result))
  expect_true("Basisprofiel" %in% names(tibble_result))
  expect_true("Costs (EUR)" %in% names(tibble_result))
  
  # Test detailed format
  detailed_result <- kvk_usage_report(format = "detailed")
  expect_s3_class(detailed_result, "tbl_df")
  expect_equal(nrow(detailed_result), 2)  # Individual calls
  expect_true("call_type" %in% names(detailed_result))
  expect_true("timestamp" %in% names(detailed_result))
  
  # Test invalid format
  expect_error(kvk_usage_report(format = "invalid"), "Invalid format")
})

test_that("kvk_usage_alert sets and resets options correctly", {
  # Save current options
  old_calls <- getOption("kvkapiR.max_calls")
  old_cost <- getOption("kvkapiR.max_cost")
  
  # Test setting alerts
  expect_message(kvk_usage_alert(max_calls = 100, max_cost = 5.00), "Max paid calls: 100")
  expect_equal(getOption("kvkapiR.max_calls"), 100)
  expect_equal(getOption("kvkapiR.max_cost"), 5.00)
  
  # Test setting only calls
  expect_message(kvk_usage_alert(max_calls = 50), "Max paid calls: 50")
  expect_equal(getOption("kvkapiR.max_calls"), 50)
  
  # Test setting only cost
  expect_message(kvk_usage_alert(max_cost = 10.50), "Max cost: EUR 10.50")
  expect_equal(getOption("kvkapiR.max_cost"), 10.50)
  
  # Test resetting all alerts
  expect_message(kvk_usage_alert(), "All usage alerts have been disabled")
  expect_null(getOption("kvkapiR.max_calls"))
  expect_null(getOption("kvkapiR.max_cost"))
  
  # Test validation
  expect_error(kvk_usage_alert(max_cost = "invalid"), "max_cost must be a numeric value")
  
  # Restore options
  options(kvkapiR.max_calls = old_calls, kvkapiR.max_cost = old_cost)
})

test_that("kvk_reset_usage clears session data", {
  # Add some test data
  test_data <- data.frame(
    timestamp = Sys.time(),
    date = Sys.Date(),
    year = 2025L,
    month = 6L,
    call_type = "search",
    test_environment = FALSE,
    stringsAsFactors = FALSE
  )
  save_usage_data(test_data)
  
  # Verify data exists
  data <- load_usage_data()
  expect_equal(nrow(data), 1)
  
  # Reset data
  expect_message(kvk_reset_usage(), "Session usage data has been reset")
  
  # Verify data is cleared
  data <- load_usage_data()
  expect_equal(nrow(data), 0)
  
  # Test reset when no data exists  
  if (exists("usage_data", envir = .kvkapi_session_usage)) {
    rm("usage_data", envir = .kvkapi_session_usage)
  }
  expect_message(kvk_reset_usage(), "No usage data found to reset")
})

test_that("kvk_export_usage works correctly", {
  # Reset session data
  if (exists("usage_data", envir = .kvkapi_session_usage)) {
    rm("usage_data", envir = .kvkapi_session_usage)
  }
  
  temp_csv <- tempfile(fileext = ".csv")
  
  # Test with no data
  expect_message(kvk_export_usage(temp_csv), "No usage data to export")
  expect_false(file.exists(temp_csv))
  
  # Add test data
  test_data <- data.frame(
    timestamp = c(Sys.time(), Sys.time() + 1),
    date = rep(Sys.Date(), 2),
    year = rep(2025L, 2),
    month = rep(6L, 2),
    call_type = c("search", "basisprofiel"),
    test_environment = rep(FALSE, 2),
    stringsAsFactors = FALSE
  )
  save_usage_data(test_data)
  
  # Test summary export
  expect_message(kvk_export_usage(temp_csv, format = "summary"), "Session summary exported")
  expect_true(file.exists(temp_csv))
  
  exported <- read.csv(temp_csv)
  expect_equal(nrow(exported), 1)  # Session summary
  expect_true("Search" %in% names(exported))
  expect_true("Basisprofiel" %in% names(exported))
  
  unlink(temp_csv)
  
  # Test detailed export
  expect_message(kvk_export_usage(temp_csv, format = "detailed"), "Session usage data exported")
  expect_true(file.exists(temp_csv))
  
  exported <- read.csv(temp_csv)
  expect_equal(nrow(exported), 2)  # Individual calls
  expect_true("call_type" %in% names(exported))
  
  unlink(temp_csv)
  
  # Test invalid format
  expect_error(kvk_export_usage(temp_csv, format = "invalid"), "Invalid format")
})

test_that("check_usage_alerts_realtime triggers correctly for paid calls only", {
  skip_if_no_api_key()
  
  # Reset session data
  if (exists("usage_data", envir = .kvkapi_session_usage)) {
    rm("usage_data", envir = .kvkapi_session_usage)
  }
  
  # Save current options
  old_calls <- getOption("kvkapiR.max_calls")
  old_cost <- getOption("kvkapiR.max_cost")
  
  # Set test limits
  options(kvkapiR.max_calls = 2, kvkapiR.max_cost = 0.03)
  
  # Add search calls (should not trigger call alerts)
  test_data <- data.frame(
    timestamp = rep(Sys.time(), 3),
    date = rep(Sys.Date(), 3),
    year = rep(2025L, 3),
    month = rep(6L, 3),
    call_type = rep("search", 3),
    test_environment = rep(FALSE, 3),
    stringsAsFactors = FALSE
  )
  save_usage_data(test_data)
  
  # Search call should not trigger call limit alert
  expect_silent(check_usage_alerts_realtime(test_data, "search"))
  
  # Add first paid call - should not trigger yet (limit is 2)
  test_data_paid <- rbind(test_data, data.frame(
    timestamp = Sys.time(),
    date = Sys.Date(),
    year = 2025L,
    month = 6L,
    call_type = "basisprofiel",
    test_environment = FALSE,
    stringsAsFactors = FALSE
  ))
  save_usage_data(test_data_paid)
  
  expect_silent(check_usage_alerts_realtime(test_data_paid, "basisprofiel"))
  
  # Add second paid call - should trigger call limit alert
  test_data_paid2 <- rbind(test_data_paid, data.frame(
    timestamp = Sys.time(),
    date = Sys.Date(),
    year = 2025L,
    month = 6L,
    call_type = "vestigingsprofiel",
    test_environment = FALSE,
    stringsAsFactors = FALSE
  ))
  save_usage_data(test_data_paid2)
  
  expect_message(
    check_usage_alerts_realtime(test_data_paid2, "vestigingsprofiel"),
    "USAGE LIMIT ALERT.*paid calls to 2.*reaching the limit of 2"
  )
  
  # Add third paid call - should trigger reminder
  test_data_paid3 <- rbind(test_data_paid2, data.frame(
    timestamp = Sys.time(),
    date = Sys.Date(),
    year = 2025L,
    month = 6L,
    call_type = "naamgeving",
    test_environment = FALSE,
    stringsAsFactors = FALSE
  ))
  save_usage_data(test_data_paid3)
  
  expect_message(
    check_usage_alerts_realtime(test_data_paid3, "naamgeving"),
    "Usage limit reminder.*paid calls to 3.*exceeds your limit of 2"
  )
  
  # Test cost alerts - first paid call costs EUR 0.02 (under limit)
  # Second paid call brings total to EUR 0.04 (over limit of EUR 0.03)
  expect_message(
    check_usage_alerts_realtime(test_data_paid2, "vestigingsprofiel"),
    "COST LIMIT ALERT.*EUR 0.04.*reaching the limit of EUR 0.03"
  )
  
  # Restore options
  options(kvkapiR.max_calls = old_calls, kvkapiR.max_cost = old_cost)
})

test_that("real-time alerts work during record_api_call", {
  skip_if_no_api_key()
  
  # Reset session data
  if (exists("usage_data", envir = .kvkapi_session_usage)) {
    rm("usage_data", envir = .kvkapi_session_usage)
  }
  
  # Save current options
  old_calls <- getOption("kvkapiR.max_calls")
  old_cost <- getOption("kvkapiR.max_cost")
  
  # Set low limits for testing
  options(kvkapiR.max_calls = 1, kvkapiR.max_cost = 0.01)
  
  # Search calls should not trigger alerts
  expect_silent(record_api_call("search", FALSE))
  expect_silent(record_api_call("search", FALSE))
  expect_silent(record_api_call("search", FALSE))
  
  # First paid call should trigger both call and cost alerts
  expect_message(
    record_api_call("basisprofiel", FALSE),
    "USAGE LIMIT ALERT.*paid calls to 1.*reaching the limit of 1"
  )
  
  # Second paid call should trigger reminder messages
  expect_message(
    record_api_call("vestigingsprofiel", FALSE),
    "Usage limit reminder.*paid calls to 2.*exceeds your limit of 1"
  )
  
  # Restore options
  options(kvkapiR.max_calls = old_calls, kvkapiR.max_cost = old_cost)
})