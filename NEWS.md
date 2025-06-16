# kvkapiR 0.1.2

First CRAN release.

## New features

* `kvk_search()` searches the Dutch Chamber of Commerce (KvK) business registry 
  with automatic pagination up to 1,000 results.
* `kvk_get_basisprofiel()`, `kvk_get_vestigingsprofiel()`, and `kvk_get_naamgeving()` 
  retrieve detailed business profiles, establishment information, and company name histories.
* `kvk_usage_report()` displays API usage statistics and costs with multiple output formats.
* `kvk_usage_alert()` sets usage and cost limits with real-time alerts.
* `kvk_export_usage()` exports usage data to CSV in tidy or monthly formats.
* `kvk_reset_usage()` clears usage tracking history.
* All functions support test environment via `test_environment = TRUE` for cost-free testing.
* Search functions support multiple business types in single query and street-only search.
* Automatic error handling with user-friendly messages and Dutch IPD code translations.

## Documentation

* Added comprehensive vignette with practical examples and cost information.
* Added pkgdown website with custom KvK color scheme.

