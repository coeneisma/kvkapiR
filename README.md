
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kvkapiR

<!-- badges: start -->

[![R-CMD-check](https://github.com/coeneisma/kvkapiR/workflows/R-CMD-check/badge.svg)](https://github.com/coeneisma/kvkapiR/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of kvkapiR is to provide a convenient R programming language
interface to the Dutch Chamber of Commerce (KvK) APIs. This package is
built using the httr2 package and follows best practices for wrapping
APIs in R, as outlined in the [httr2
documentation](https://httr2.r-lib.org/articles/wrapping-apis.html). It
simplifies authentication, request handling, and response parsing when
interacting with the KvK API.

Currently, the package provides access to the [following KvK
APIs](https://developers.kvk.nl/documentation#available-apis):

- KvK Search API
- KvK Basisprofiel API
- KvK Vestigingsprofiel API
- KvK Naamgeving API

More details can be found [on the developers website of the
KvK](https://developers.kvk.nl/apis).

## Installation

You can install the development version of `kvkapiR` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("coeneisma/kvkapiR")
```

To install the latest development version:

``` r
# install.packages("devtools")
devtools::install_github("coeneisma/kvkapiR", ref = "development")
```

## Example

Here are basic examples demonstrating how to use the `kvk_search()`
function to retrieve KvK registrations:

``` r
library(kvkapiR)

koudum <- kvk_search(plaats = "Koudum")
koudum
#> # A tibble: 509 × 6
#>    kvkNummer vestigingsnummer naam                     adres        type  links 
#>    <chr>     <chr>            <chr>                    <list>       <chr> <list>
#>  1 01036576  000007810083     Stichting Gemeenschapsc… <named list> neve… <list>
#>  2 92692966  000032358083     M. de Boer               <named list> neve… <list>
#>  3 60112891  000045511098     Corriente 't Kofjehúske  <named list> neve… <list>
#>  4 40005051  000021251479     Bogerman                 <named list> neve… <list>
#>  5 30168016  000057781184     It Fûgelnêst             <named list> neve… <list>
#>  6 01123666  000040288196     Winterberging IJzendoorn <named list> hoof… <list>
#>  7 58496505  000027833429     HFK Verhuur              <named list> hoof… <list>
#>  8 62724843  000031708129     De Klink Exploitatie B.… <named list> hoof… <list>
#>  9 01091668  000000678279     Multiservice Beheer en … <named list> hoof… <list>
#> 10 41005555  000021707499     Stichting Jongerenwerk … <named list> hoof… <list>
#> # ℹ 499 more rows
```

You can also combine multiple search parameters:

``` r
library(kvkapiR)

rotterdam <- kvk_search(naam = "huisartsen", plaats = "Rotterdam")
rotterdam
#> # A tibble: 200 × 7
#>    kvkNummer vestigingsnummer naam       adres        type  links  vervallenNaam
#>    <chr>     <chr>            <chr>      <list>       <chr> <list> <chr>        
#>  1 24475068  000009219072     Wijkprakt… <named list> hoof… <list> <NA>         
#>  2 81943938  000048204625     Jans Huis… <named list> hoof… <list> <NA>         
#>  3 24477358  000010787356     Van Schai… <named list> hoof… <list> <NA>         
#>  4 24484189  000016351312     Sanitas H… <named list> neve… <list> <NA>         
#>  5 97110795  000062375466     Dante Hui… <named list> hoof… <list> <NA>         
#>  6 66346339  000034979034     Huisartse… <named list> hoof… <list> <NA>         
#>  7 62380281  000031391419     Huisartse… <named list> hoof… <list> <NA>         
#>  8 54446538  000024210382     Huisartse… <named list> hoof… <list> <NA>         
#>  9 24484189  000036373648     Sanitas H… <named list> hoof… <list> <NA>         
#> 10 85464007  000051508060     itj huisa… <named list> hoof… <list> <NA>         
#> # ℹ 190 more rows
```

See `vignette("kvkapiR")` for more examples and information about the
usage of this package.
