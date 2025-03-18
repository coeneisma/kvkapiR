# test-kvk_search.R
library(testthat)
library(kvkapiR)

# Mock httr2 response voor kvk_search
mock_search_response <- function(status = 200, body = NULL) {
  if (status == 200) {
    if (is.null(body)) {
      body <- list(
        totaal = 2,
        resultaten = list(
          list(kvkNummer = "12345678", naam = "Test Bedrijf 1"),
          list(kvkNummer = "87654321", naam = "Test Bedrijf 2")
        )
      )
    }
  }

  mock_resp <- structure(
    list(
      status_code = status,
      headers = list(`Content-Type` = "application/json"),
      body = jsonlite::toJSON(body, auto_unbox = TRUE)
    ),
    class = "httr2_response"
  )

  return(mock_resp)
}

# Test dat de search functie correct werkt met de test environment
test_that("kvk_search works with test environment", {
  # We gebruiken with_mocked_bindings om httr2::req_perform te mocken
  with_mocked_bindings({
    result <- kvk_search(plaats = "Utrecht", test_environment = TRUE)
    expect_true(is.data.frame(result) || is.null(result))
  }, httr2::req_perform = function(req) { mock_search_response() })
})

# Test dat de search functie correct omgaat met HTTP 404 (geen resultaten)
test_that("kvk_search handles no results properly", {
  with_mocked_bindings({
    result <- kvk_search(plaats = "NonExistentPlace")
    expect_null(result)
  }, httr2::req_perform = function(req) {
    stop(structure(list(message = "HTTP 404"), class = c("error", "condition")))
  })
})

# Test dat de search functie correct omgaat met paginering
test_that("kvk_search handles pagination correctly", {
  # Eerste request (alleen totaal bepalen)
  request_count <- 0

  with_mocked_bindings({
    result <- kvk_search(plaats = "Amsterdam")

    # Controleer dat er meerdere requests zijn gedaan (paginering)
    expect_true(request_count > 1)
    expect_true(is.data.frame(result))
  }, httr2::req_perform = function(req) {
    request_count <<- request_count + 1

    if (request_count == 1) {
      # Eerste request voor totaal aantal
      return(mock_search_response(body = list(totaal = 150, resultaten = list())))
    } else {
      # Volgende requests voor daadwerkelijke data
      return(mock_search_response())
    }
  })
})
