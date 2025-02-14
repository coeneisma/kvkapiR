



#' Read KVK-API
#'
#' Test to make sure the environmental variables on github work to build the
#' pkgdown website.
#'
#' @return Tibble
#' @export
#'
#' @examples
#'
#' x <- inlezen_kvk()
#' print(x)
inlezen_kvk <- function(){
  API_URL <- "https://api.kvk.nl/api/v2/zoeken"
  API_KEY <- Sys.getenv("KVK_API_KEY")

  request <- httr2::request(API_URL) |>
    httr2::req_headers(
      apikey = API_KEY,
      Accept = "application/json"
    ) |>
    httr2::req_url_query(naam = "Koudum",
                  resultatenPerPagina = "100",
                  pagina = "1")

  response <- request |>
    httr2::req_perform()

  resultaten <- response |>
    httr2::resp_body_json()

  n_pagina <- resultaten$totaal %% 10
  rest <- resultaten$totaal - n_pagina * 10
  if(rest > 0){
    n_pagina <- n_pagina + 1
  }


  data <- dplyr::tibble(pagina = 1, inhoud = resultaten$resultaten) |>
    # as_tibble()# |>
    tidyr::unnest_wider(col = inhoud) |>
    tidyr::unnest_wider(col = adres) |>
    dplyr::select(-type) |>
    tidyr::unnest_wider(col = binnenlandsAdres)


}
