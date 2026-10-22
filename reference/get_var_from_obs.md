# Derive variable names from observation data

Returns the variable names found in `obs` (e.g. as returned by
[`SticsRFiles::get_obs()`](https://rdrr.io/pkg/SticsRFiles/man/get_obs.html)),
excluding metadata columns (`Date`, `situation`, `species`, `version`,
`Plant`). This is how
[`run_simulations`](https://sticsrpacks.github.io/SticsREval/reference/run_simulations.md)
derives the variables to simulate when its `vars` argument isn't
provided. Useful to inspect that default list, or to extend it — e.g. to
combine the observed variables with extra ones (such as the balance
closure variables) into a single
[`run_simulations`](https://sticsrpacks.github.io/SticsREval/reference/run_simulations.md)
call:

## Usage

``` r
get_var_from_obs(obs)
```

## Arguments

- obs:

  a list of observation data frames, one per USM, as returned by
  [`SticsRFiles::get_obs()`](https://rdrr.io/pkg/SticsRFiles/man/get_obs.html)

## Value

a character vector of variable names

## Details


    obs <- SticsRFiles::get_obs(usms_workspace, usm = usms)
    vars <- c(get_var_from_obs(obs), balance_vars)
    run_simulations(..., vars = vars)
