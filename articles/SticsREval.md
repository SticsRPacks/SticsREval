# Get started with SticsREval

`SticsREval` evaluates a candidate version of the STICS crop model and
compares it to a reference version. This vignette walks through the full
workflow, from a raw SMS repository to a browsable HTML dashboard.

The code below is not run when the vignette is built: it requires a
STICS executable, an SMS repository and the [Quarto
CLI](https://quarto.org).

``` r

library(SticsREval)
```

## Overview

The workflow has three stages, each handled by plain functions taking
explicit arguments:

1.  **Prepare the data**: build a STICS text workspace from the SMS
    repository with
    [`gen_workspace_from_sms()`](https://sticsrpacks.github.io/SticsREval/reference/gen_workspace_from_sms.md),
    then run each STICS version on it with
    [`run_simulations()`](https://sticsrpacks.github.io/SticsREval/reference/run_simulations.md).
    This produces RDS files of simulations and observations.
2.  **Evaluate**: compare the candidate version against the observations
    and the reference version with
    [`evaluate()`](https://sticsrpacks.github.io/SticsREval/reference/evaluate.md),
    and check water and nitrogen balances with
    [`balance_closure_test()`](https://sticsrpacks.github.io/SticsREval/reference/balance_closure_test.md).
3.  **Report**: render the exported results as an HTML dashboard with
    [`render_report()`](https://sticsrpacks.github.io/SticsREval/reference/render_report.md).

[`evaluate()`](https://sticsrpacks.github.io/SticsREval/reference/evaluate.md)
and
[`balance_closure_test()`](https://sticsrpacks.github.io/SticsREval/reference/balance_closure_test.md)
never run STICS themselves: they only read the RDS files produced by
[`run_simulations()`](https://sticsrpacks.github.io/SticsREval/reference/run_simulations.md).
Simulations can thus be computed once and evaluated many times.

## 1. Prepare the data

### Build the workspace

[`gen_workspace_from_sms()`](https://sticsrpacks.github.io/SticsREval/reference/gen_workspace_from_sms.md)
converts the SMS repository (XML input files) into a STICS text
workspace, with one folder per USM:

``` r

gen_workspace_from_sms(
  sms_path   = "/path/to/sms_repository",
  stics_path = "/path/to/stics_distribution",
  output_dir = "workspace/"
)
```

To restrict the workspace to some USMs, pass their names to `usms`. They
can be read from text files listing one USM per line with
[`read_usms_files()`](https://sticsrpacks.github.io/SticsREval/reference/read_usms_files.md):

``` r

usms <- read_usms_files(c("usms_wheat.txt", "usms_maize.txt"))

gen_workspace_from_sms(
  sms_path   = "/path/to/sms_repository",
  stics_path = "/path/to/stics_distribution",
  output_dir = "workspace/",
  usms       = usms
)
```

### Run the simulations

Run the reference version once.
[`run_simulations()`](https://sticsrpacks.github.io/SticsREval/reference/run_simulations.md)
writes `simulations.rds` and `observations.rds` to `output_dir`:

``` r

run_simulations(
  stics_exe      = "/path/to/stics_reference",
  usms_workspace = "workspace/",
  metadata_file  = "metadata.csv",
  output_dir     = "reference/"
)
```

By default, the simulated variables are the observed ones. The balance
closure test needs additional, non-observed variables: use
[`get_var_from_obs()`](https://sticsrpacks.github.io/SticsREval/reference/get_var_from_obs.md)
to combine both sets into a single run of the candidate version.

``` r

balance_vars <- c(
  "init_H2O_balance", "final_H2O_balance",
  "init_plant_N_balance", "final_plant_N_balance",
  "init_soil_mineral_N_balance", "final_soil_mineral_N_balance",
  "init_soil_organic_N_balance", "final_soil_organic_N_balance",
  "init_soil_organic_C_balance", "final_soil_organic_C_balance"
)

obs <- SticsRFiles::get_obs("workspace/")
vars <- c(get_var_from_obs(obs), balance_vars)

run_simulations(
  stics_exe      = "/path/to/stics_candidate",
  usms_workspace = "workspace/",
  metadata_file  = "metadata.csv",
  output_dir     = "outputs/",
  vars           = vars
)
```

## 2. Evaluate

### Statistical evaluation

[`evaluate()`](https://sticsrpacks.github.io/SticsREval/reference/evaluate.md)
runs three evaluations of the candidate version:

- **global**: statistics over all species and USMs combined;
- **per species**: variables whose rRMSE deteriorates by more than
  `percentage` % compared to the reference are flagged;
- **per USM**: USMs where one or more variables are locally degraded
  compared to the reference are flagged.

Variables with fewer than 10 observations are not tested.

``` r

evaluate(
  usms_workspace = "workspace/",
  sim_rds        = "outputs/simulations.rds",
  obs_rds        = "outputs/observations.rds",
  ref_sim_rds    = "reference/simulations.rds",
  output_dir     = "outputs/",
  percentage     = 5
)
```

Results (CSV files, and RDS files with the data of the degraded
variables) are written to `output_dir`, and a summary is printed to the
console. Without `ref_sim_rds`, the candidate version is evaluated
against observations only. Set `stop_on_failure = TRUE` to make
[`evaluate()`](https://sticsrpacks.github.io/SticsREval/reference/evaluate.md)
throw an error when an evaluation fails, e.g. in a CI pipeline.

### Balance closure test

[`balance_closure_test()`](https://sticsrpacks.github.io/SticsREval/reference/balance_closure_test.md)
checks that the initial and final water and nitrogen balances of each
USM are equal, and stops with an error listing the USMs where they are
not:

``` r

balance_closure_test(
  sim_rds    = "outputs/simulations.rds",
  output_dir = "outputs/"
)
```

## 3. Report

[`render_report()`](https://sticsrpacks.github.io/SticsREval/reference/render_report.md)
turns the files written by
[`evaluate()`](https://sticsrpacks.github.io/SticsREval/reference/evaluate.md)
into an HTML site, with a summary homepage, a global page and one page
per species:

``` r

render_report("outputs/")
```

It can also be triggered directly with `evaluate(..., report = TRUE)`.
The site links to its shared assets through relative paths, so keep it
intact when moving or sharing it.

To use the rRMSE comparison plot outside the report, build it from the
exported CSV files with
[`plot_rrmse_comparison()`](https://sticsrpacks.github.io/SticsREval/reference/plot_rrmse_comparison.md):

``` r

comparison <- read.csv("outputs/csv/global_rrmse_comparison.csv")
plot_rrmse_comparison(comparison, percentage = 5)
```

## Parallel execution

All workflow functions accept `parallel` and `cores` arguments:

``` r

evaluate(
  usms_workspace = "workspace/",
  sim_rds        = "outputs/simulations.rds",
  obs_rds        = "outputs/observations.rds",
  ref_sim_rds    = "reference/simulations.rds",
  output_dir     = "outputs/",
  parallel       = TRUE,
  cores          = 4
)
```
