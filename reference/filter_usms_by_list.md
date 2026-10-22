# Restrict a USM data frame to the given USMs.

Restrict a USM data frame to the given USMs.

## Usage

``` r
filter_usms_by_list(usm_df, wanted_usms)
```

## Arguments

- usm_df:

  data frame containing USM information, as returned by
  `get_sms_usms_list`

- wanted_usms:

  character vector of USM names to keep

## Value

the filtered data frame
