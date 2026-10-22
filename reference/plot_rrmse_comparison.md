# Plot the rRMSE comparison

Plots the rRMSE of the new version against the reference version, one
point per variable, colored by status. Each point is labeled with its
variable name and number of observations (`ggrepel` keeps labels apart
and away from their point).

## Usage

``` r
plot_rrmse_comparison(rrmse_comparison, percentage = 5)
```

## Arguments

- rrmse_comparison:

  Data frame of the per-variable rRMSE comparison (as exported to
  `global_rrmse_comparison.csv` or `species_rrmse_comparison.csv`), with
  columns `variable`, `n_obs`, `rrmse_ref`, `rrmse_eval` and `status`.

- percentage:

  Threshold (%) above which a variable is flagged as critical, drawn as
  a dashed line above the identity line.

## Value

A `ggplot` object.
