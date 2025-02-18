
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kvkapiR

<!-- badges: start -->
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
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

This is a basic example which shows you how use the `kvk_search()`
function:

``` r
library(kvkapiR)

koudum <- kvk_search(plaats = "Koudum")
koudum
#> # A tibble: 514 × 6
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
#> # ℹ 504 more rows
```

``` r
rotterdam <- kvk_search(naam = "huisartsen", plaats = "Rotterdam")
rotterdam
#> # A tibble: 199 × 7
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
#> # ℹ 189 more rows
```
