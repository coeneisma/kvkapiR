
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kvkapiR

<!-- badges: start -->
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The kvkapiR package provides a user-friendly interface to the Dutch
Chamber of Commerce (KvK) Search API within the R programming
environment. Built using the `httr2` package, it adheres to best
practices for API wrappers in R, as outlined in the [httr2
documentation](https://httr2.r-lib.org/articles/wrapping-apis.html).

This package simplifies authentication, request handling, and response
parsing, making it easier to interact with the KvK API.

Currently, kvkapiR only supports the [KvK search
API](https://developers.kvk.nl/apis/zoeken). Additional KvK APIs are
available, and more details can be found [on the developers website of
the KvK](https://developers.kvk.nl/apis).

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
#> # A tibble: 511 × 6
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
#> # ℹ 501 more rows
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
#>  3 24484189  000016351312     Sanitas H… <named list> neve… <list> <NA>         
#>  4 24477358  000010787356     Van Schai… <named list> hoof… <list> <NA>         
#>  5 66346339  000034979034     Huisartse… <named list> hoof… <list> <NA>         
#>  6 54446538  000024210382     Huisartse… <named list> hoof… <list> <NA>         
#>  7 62380281  000031391419     Huisartse… <named list> hoof… <list> <NA>         
#>  8 24484189  000036373648     Sanitas H… <named list> hoof… <list> <NA>         
#>  9 85464007  000051508060     itj huisa… <named list> hoof… <list> <NA>         
#> 10 24476795  000010613765     Baggerman… <named list> hoof… <list> <NA>         
#> # ℹ 190 more rows
```

See `vignette("kvkapiR")` for more examples and information about the
usage of this package.
