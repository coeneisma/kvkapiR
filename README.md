
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kvkapiR

<!-- badges: start -->
<!-- badges: end -->

The goal of kvkapiR is to provides a convenient interface to the Dutch
Chamber of Commerce (KvK) API. This package is built using the `httr2`
package and follows best practices for wrapping APIs in R, as outlined
in the httr2 documentation
(<https://httr2.r-lib.org/articles/wrapping-apis.html>). It simplifies
authentication, request handling, and response parsing when interacting
with the KvK API.

## Installation

You can install the development version of kvkapiR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("coeneisma/kvkapiR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(kvkapiR)

koudum <- kvk_search(plaats = "Koudum")
koudum
#> # A tibble: 515 × 6
#>    kvkNummer vestigingsnummer naam                     adres        type  links 
#>    <chr>     <chr>            <chr>                    <list>       <chr> <list>
#>  1 01036576  000007810083     Stichting Gemeenschapsc… <named list> neve… <list>
#>  2 60112891  000045511098     Corriente 't Kofjehúske  <named list> neve… <list>
#>  3 92692966  000032358083     M. de Boer               <named list> neve… <list>
#>  4 40005051  000021251479     Bogerman                 <named list> neve… <list>
#>  5 30168016  000057781184     It Fûgelnêst             <named list> neve… <list>
#>  6 58496505  000027833429     HFK Verhuur              <named list> hoof… <list>
#>  7 62724843  000031708129     De Klink Exploitatie B.… <named list> hoof… <list>
#>  8 01123666  000040288196     Winterberging IJzendoorn <named list> hoof… <list>
#>  9 40001473  000000303062     De Sândobbe              <named list> hoof… <list>
#> 10 01091668  000000678279     Multiservice Beheer en … <named list> hoof… <list>
#> # ℹ 505 more rows
```

``` r
rotterdam <- kvk_search(plaats = "Rotterdam", type = "hoofdvestiging")
#> Warning in kvk_search(plaats = "Rotterdam", type = "hoofdvestiging"): API
#> response contains more than 1.000 results. Only the first 1.000 will be
#> retrieved.
```
