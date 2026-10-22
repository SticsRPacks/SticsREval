# Keep the wanted USMs that are available.

Logs a warning listing the wanted USMs that aren't available, and stops
if none of them are.

## Usage

``` r
select_available_usms(available_usms, wanted_usms, location)
```

## Arguments

- available_usms:

  character vector of available USM names

- wanted_usms:

  character vector of requested USM names

- location:

  description of where the USMs are looked for, used in messages (e.g. a
  workspace path)

## Value

the unique wanted USMs that are available
