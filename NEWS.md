# kvkapiR 0.0.0.9005 (development version)

## Major improvements

* Package is now production-ready with all experimental badges removed
* Comprehensive usage tracking and cost monitoring system added:
  - `kvk_usage_report()`: View usage statistics and costs
  - `kvk_usage_alert()`: Set usage/cost limits with real-time alerts
  - `kvk_export_usage()`: Export usage data to CSV
  - `kvk_reset_usage()`: Clear usage history
  - Automatic tracking of all production API calls
  - Privacy-respecting local storage with opt-out option

## Error handling and validation

* Centralized error handling with Dutch IPD code translations
* Input validation for kvkNummer (8 digits) and vestigingsnummer (12 digits)
* User-friendly error messages instead of generic HTTP errors

## Code quality

* Complete test suite with 170+ tests covering all functionality
* GitHub Actions CI/CD workflow for R CMD check on multiple platforms
* All R CMD check warnings resolved
* CRAN-compliant code (ASCII-only, proper global variable declarations)
* Constants defined for API URLs, test keys, and pagination limits

## Documentation

* Comprehensive package documentation
* `pkgdown` site with custom KvK color scheme
* Updated vignette with conditional chunk execution
* Examples use conditional execution for CRAN compatibility

## Bug fixes

* Fixed non-ASCII characters (replaced € with EUR)
* Added proper spacing in currency displays (EUR 0.02 instead of EUR0.02)
* Real-time usage alerts now show reminders when operating over limits
* Improved alert behavior for both first-time and continued limit exceedances

# kvkapiR 0.0.0.9000

* Initial setup of package.
* `pkgdown` site generated with custom KvK color scheme.
* Added multiple examples to `vignette("kvkapiR")`