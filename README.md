
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kvkapiR

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/kvkapiR)](https://CRAN.R-project.org/package=kvkapiR)
[![R-CMD-check](https://github.com/coeneisma/kvkapiR/workflows/R-CMD-check/badge.svg)](https://github.com/coeneisma/kvkapiR/actions)
<!-- badges: end -->

The goal of kvkapiR is to provide a convenient R programming language
interface to the Dutch Chamber of Commerce (KvK) APIs. This package is
built using the httr2 package and follows best practices for wrapping
APIs in R, as outlined in the [httr2
documentation](https://httr2.r-lib.org/articles/wrapping-apis.html). It
simplifies authentication, request handling, and response parsing when
interacting with the KvK API.

The package provides access to [all the KvK
APIs](https://developers.kvk.nl/documentation#available-apis):

- KvK Search API
- KvK Basisprofiel API
- KvK Vestigingsprofiel API
- KvK Naamgeving API

More details can be found [on the developers website of the
KvK](https://developers.kvk.nl/apis).

## Installation

The package has been submitted to CRAN for review. Once it’s accepted
and the CRAN status badge shows “CRAN: OK”, you can install the stable
version from CRAN with:

``` r
install.packages("kvkapiR")
```

Until then, you can install the current stable version from
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

## Setting up API Access

### API Key

To use the KvK API, you need an API key. You can apply for one at the
[KvK Developer
Portal](https://developers.kvk.nl/apply-for-apis?step=api-overview).

``` r
# Set API key for current session
kvk_set_api_key("your_api_key_here")

# Or add to .Renviron file manually:
# KVK_API_KEY=your_api_key_here
```

### Test Environment

The package includes support for the KvK test environment with fictional
Donald Duck data:

``` r
# No API key needed for test environment
test_results <- kvk_search(naam = "Donald Duck", test_environment = TRUE)
```

## Basic Usage

### Search for Businesses

``` r
library(kvkapiR)

# Search by location
koudum <- kvk_search(plaats = "Koudum")
koudum
#> # A tibble: 512 × 6
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
#> # ℹ 502 more rows

# Combine search criteria
snackbar <- kvk_search(naam = "snackbar", plaats = "Utrecht")
snackbar
#> # A tibble: 17 × 7
#>    kvkNummer vestigingsnummer naam       adres        type  links  vervallenNaam
#>    <chr>     <chr>            <chr>      <list>       <chr> <list> <chr>        
#>  1 30055828  000001534106     "Snackbar… <named list> hoof… <list>  <NA>        
#>  2 30149968  000001373366     "Snackbar… <named list> hoof… <list>  <NA>        
#>  3 30061168  000007852940     "Snackbar… <named list> hoof… <list>  <NA>        
#>  4 91479894  000057155518     "De Snack… <named list> hoof… <list>  <NA>        
#>  5 77162838  000044881207     "Snackbar… <named list> hoof… <list>  <NA>        
#>  6 30048901  000015639428     "Snackbar… <named list> neve… <list>  <NA>        
#>  7 63545659  000032452004     "Snackbar… <named list> hoof… <list>  <NA>        
#>  8 60975784  000028183169     "Snackbar… <named list> hoof… <list>  <NA>        
#>  9 91242894  000002767961     "Snackbar… <named list> hoof… <list>  <NA>        
#> 10 30083297  000009193693     "Cafetari… <named list> hoof… <list> "SNACKBAR HO…
#> 11 30050751  000007808208     "Petit-Re… <named list> hoof… <list> "Snackbar \"…
#> 12 30192953  000002554313     "Cafetari… <named list> hoof… <list> "Snackbar de…
#> 13 30156975  000013513508     "Bestaria… <named list> hoof… <list> "Snackbar \"…
#> 14 86470833  000005957397     "Giros"    <named list> hoof… <list> "Snackbar Gi…
#> 15 29036082  000020552270     "Restaura… <named list> hoof… <list> "Snackbar de…
#> 16 76537293  000044313047     "Biltstra… <named list> hoof… <list> "Snackbar de…
#> 17 76449548  000044233884     "Snackbar… <named list> hoof… <list>  <NA>

# Search by address
address_search <- kvk_search(postcode = "2594BD", huisnummer = "10")

# Filter by business type (multiple types allowed)
mixed_types <- kvk_search(
  plaats = "Amsterdam",
  type = "hoofdvestiging",
  type = "rechtspersoon"
)
#> ! API response contains more than 1000 results. Only the first 1000 will be retrieved.
```

### Retrieve Detailed Profiles

Each profile retrieval costs EUR 0.02 (free for government
organizations):

``` r
# Get basic company profile (using a social organization)
profile <- kvk_get_basisprofiel("01036576")

# Get establishment details
establishment <- kvk_get_vestigingsprofiel("000007810083")

# Get name history
names <- kvk_get_naamgeving("01036576")
```

## Usage Tracking and Cost Management

The package includes automatic session-based usage tracking:

``` r
# View current session usage
kvk_usage_report()

# Set session alerts
kvk_usage_alert(max_cost = 5.00)

# Export session data
kvk_export_usage("session_usage.csv")
```

### Pricing

- **Monthly base fee**: EUR 6.20 (when you have API access)
- **Search API**: Free (after base fee)
- **Profile APIs**: EUR 0.02 per call
- **Government organizations**: All API calls are free
- **Access requirement**: Only authorized business representatives can
  apply

## More Information

For detailed examples and advanced usage, see `vignette("kvkapiR")` or
visit the [package website](https://coeneisma.github.io/kvkapiR/).
