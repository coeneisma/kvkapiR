test_that("validate_kvk_nummer works correctly", {
  # Valid KvK numbers (8 digits)
  expect_invisible(validate_kvk_nummer("12345678"))
  expect_invisible(validate_kvk_nummer("00000000"))
  expect_invisible(validate_kvk_nummer("99999999"))
  
  # Invalid KvK numbers
  expect_error(validate_kvk_nummer("1234567"), "kvkNummer must be 8 digits")
  expect_error(validate_kvk_nummer("123456789"), "kvkNummer must be 8 digits")
  expect_error(validate_kvk_nummer("12a45678"), "kvkNummer must be 8 digits")
  expect_error(validate_kvk_nummer(""), "kvkNummer must be 8 digits")
  expect_error(validate_kvk_nummer("abcdefgh"), "kvkNummer must be 8 digits")
})

test_that("validate_vestigingsnummer works correctly", {
  # Valid vestigingsnummers (12 digits)
  expect_invisible(validate_vestigingsnummer("123456789012"))
  expect_invisible(validate_vestigingsnummer("000000000000"))
  expect_invisible(validate_vestigingsnummer("999999999999"))
  
  # Invalid vestigingsnummers
  expect_error(validate_vestigingsnummer("12345678901"), "vestigingsnummer must be 12 digits")
  expect_error(validate_vestigingsnummer("1234567890123"), "vestigingsnummer must be 12 digits")
  expect_error(validate_vestigingsnummer("12a456789012"), "vestigingsnummer must be 12 digits")
  expect_error(validate_vestigingsnummer(""), "vestigingsnummer must be 12 digits")
  expect_error(validate_vestigingsnummer("abcdefghijkl"), "vestigingsnummer must be 12 digits")
})