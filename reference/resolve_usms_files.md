# Resolve the deprecated `usms_files` argument into `usms`.

Must be called directly from the exported function `fn_name`, so that
lifecycle reports the deprecation against the user's call.

## Usage

``` r
resolve_usms_files(usms, usms_files, fn_name)
```
