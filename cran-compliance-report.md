# CRAN Compliance Report for kvkapiR

Date: 2025-06-15
Package Version: 0.1.0

## Summary

The kvkapiR package has been reviewed for compliance with CRAN Repository Policy. Below are the findings organized by policy area.

## 1. Copyright and Licensing ✓

**Status: COMPLIANT**

- **License**: MIT + file LICENSE (properly formatted)
- **Copyright**: Properly attributed to "kvkapiR authors" (2025)
- **Author**: Coen Eisma listed as author, creator, and copyright holder
- **Third-party code**: No third-party code found
- **ORCID**: Author has proper ORCID identifier

## 2. Package Size ✓

**Status: COMPLIANT**

- **Documentation size**: 208K (well below 5MB limit)
- **Data directory**: Not present (no data included)
- **Total package size**: 14M (includes development files, built package will be smaller)
- **Vignettes**: Present but reasonably sized

## 3. Internet Usage ✓

**Status: COMPLIANT**

- **Error handling**: Comprehensive error handling implemented in `utils-error-handling.R`
- **API failures**: Proper HTTP status code handling (400, 401, 403, 404, 429, 500, 503)
- **User messages**: Clear, informative error messages for all failure scenarios
- **Network timeouts**: Using httr2 which has proper timeout handling
- **Graceful degradation**: Functions return NULL on failure with appropriate warnings

Example error handling:
- "The KvK number '12345' is not valid (HTTP 400)"
- "Service unavailable (HTTP 503). The KVK API is currently unavailable or undergoing maintenance."

## 4. File System Usage ⚠️

**Status: NEEDS ATTENTION**

The package writes to the user's home directory for usage tracking:

- **Location**: `~/.kvkapiR/usage.rds`
- **Purpose**: Track API usage and costs
- **Issue**: CRAN policy requires explicit user permission before writing to user directories

**Recommendations**:
1. Add an explicit opt-in mechanism for usage tracking
2. Or use `tools::R_user_dir()` for proper cross-platform directory handling
3. Document clearly in the package description that files are written
4. Provide environment variable to disable: `KVKAPI_DISABLE_TRACKING=true`

## 5. Examples and Tests ✓

**Status: COMPLIANT**

- **Examples**: Use `@examplesIf nzchar(Sys.getenv("KVK_API_KEY"))` for conditional execution
- **Test environment examples**: Provided for all functions (no API key required)
- **Runtime**: Examples are quick (API calls only)
- **Test skipping**: Only one test uses `skip_on_cran()` for API key testing
- **Test coverage**: 171 passing tests

## 6. Dependencies ✓

**Status: COMPLIANT**

All dependencies are from CRAN:
- **Imports**: cli, dplyr, httr2, knitr, lifecycle, purrr, tibble, tidyr
- **Suggests**: testthat (>= 3.0.0), withr
- **Non-CRAN**: None found
- **System dependencies**: None required

## 7. Security ✓

**Status: COMPLIANT**

- **HTTPS only**: All API calls use HTTPS (verified in kvk_API.R)
- **API key handling**: Stored in environment variables, not in code
- **No hardcoded credentials**: Test environment uses safe test key
- **Secure storage**: API keys stored in .Renviron with backup mechanism

## Recommendations for CRAN Submission

### Must Fix:
1. **File system usage**: Implement proper user consent for usage tracking or use `tools::R_user_dir()`

### Should Consider:
1. **Package description**: Mention that the package tracks usage in `~/.kvkapiR/`
2. **Documentation**: Add a privacy/data storage section to the README
3. **Examples**: Ensure all examples complete in < 5 seconds
4. **Vignettes**: Add `eval = FALSE` for long-running code chunks if any exist

### Nice to Have:
1. Add `\value` sections to all internal functions (even though not exported)
2. Consider adding more `skip_on_cran()` for tests that make actual API calls
3. Add explicit statement about data collection in package startup message

## Overall Assessment

The package is well-structured and mostly CRAN-compliant. The main issue is the automatic writing to the user's home directory without explicit permission. Once this is addressed, the package should be ready for CRAN submission.