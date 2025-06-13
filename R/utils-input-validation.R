#' Validate KvK number format
#' 
#' @param kvk_nummer Character string representing a KvK number
#' @return Invisible TRUE if valid, otherwise throws error
#' @keywords internal
validate_kvk_nummer <- function(kvk_nummer) {
  if (!grepl("^[0-9]{8}$", kvk_nummer)) {
    cli::cli_abort("kvkNummer must be 8 digits. Received: {kvk_nummer}")
  }
  invisible(TRUE)
}

#' Validate vestigingsnummer format
#' 
#' @param vestigingsnummer Character string representing a vestigingsnummer
#' @return Invisible TRUE if valid, otherwise throws error
#' @keywords internal
validate_vestigingsnummer <- function(vestigingsnummer) {
  if (!grepl("^[0-9]{12}$", vestigingsnummer)) {
    cli::cli_abort("vestigingsnummer must be 12 digits. Received: {vestigingsnummer}")
  }
  invisible(TRUE)
}