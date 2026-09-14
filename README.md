# SticsREval <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/SticsRPacks/SticsREval/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/SticsRPacks/SticsREval/actions/workflows/R-CMD-check.yml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: LGPL-3.0](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
[![Docker Image](https://img.shields.io/badge/docker-ghcr.io-blue?logo=docker)](https://github.com/SticsRPacks/SticsREval/pkgs/container/stics-r-eval)
<!-- badges: end -->

## Overview

`SticsREval` is an R package for **evaluating and comparing versions of the STICS crop model**. It supports multiple evaluation workflows, each exposed as a plain function taking explicit arguments — there is no shared configuration object to build first.

The package currently provides the following evaluation workflows:

- **Statistical Evaluation** — assesses whether a new version of STICS performs better, equally, or worse than a reference version, both against field observations and against reference simulation outputs. It includes:
  - global evaluation across all species and USMs
  - species-level evaluation
  - USM-level evaluation
- **Balance Closure Test** — checks the internal consistency of water and nitrogen balances in the simulations

It can also render evaluation results as a browsable HTML dashboard via `render_report()`.

---

## Installation

You can install the development version of `SticsREval` from GitHub:

```r
# install.packages("devtools")
devtools::install_github("SticsRPacks/SticsREval")
```

Or using the `pak` package:

```r
# install.packages("pak")
pak::pak("SticsRPacks/SticsREval")
```

### Reproducible environment with `renv`

`SticsREval` uses [`renv`](https://rstudio.github.io/renv/) to ensure reproducible package dependencies. The `renv.lock` file records the exact versions of all dependencies.

To restore the project environment locally:

```r
# install.packages("renv")
renv::restore()
```

This will install all required packages at the versions specified in `renv.lock`. It is recommended to run this after cloning the repository and before running any code.

---

### Dependencies

`SticsREval` relies on the following SticsRPacks packages:

| Package | Role |
|---|---|
| [`SticsRFiles`](https://github.com/SticsRPacks/SticsRFiles) | Reading simulated and observed data |
| [`SticsOnR`](https://github.com/SticsRPacks/SticsOnR) | Running STICS simulations (optional) |
| [`CroPlotR`](https://github.com/SticsRPacks/CroPlotR) | Computing statistical criteria and generating plots |

> **Note:** `render_report()` additionally requires the [Quarto CLI](https://quarto.org) to be installed on your system (it's not an R package, so `renv::restore()` / `install.packages()` won't install it). It's already included in the [Docker image](#docker).

---

## Workflow

`evaluate()` takes explicit arguments (paths, options, filters) and orchestrates the entire statistical evaluation pipeline — global, per-species and per-USM:

```
  evaluate(usms_workspace, sim_rds, obs_rds, ref_sim_rds, ...)
           │
           ├──► loads/prepares sim, obs & reference data into a temporary workspace
           ├──► statistical evaluation vs obs & reference (all species)
           ├──► statistical evaluation vs obs & reference (per species)
           ├──► statistical evaluation vs obs & reference (per USM)
           ├──► exports evaluation results to output_dir
           ├──► prints a summary of results
           └──► stops with an error if any evaluation failed
```

`balance_closure_test()` runs independently of `evaluate()`. Like `evaluate()`, it always reads pre-computed simulation data from `sim_rds` — it never runs simulations itself:

```
  balance_closure_test(sim_rds, ...)                          ← water & nitrogen balance closure check
```

Upstream, two standalone functions prepare the data these workflows consume, starting from a raw SMS repository:

```
  gen_workspace_from_sms(sms_path, stics_path, output_dir)
           │  builds a Stics text workspace (one folder per USM) from raw SMS data
           ▼
     output_dir/                                     ← usable as `usms_workspace` from here on
           │
           └──► run_simulations(stics_exe, usms_workspace = output_dir, ...)
                     │  runs Stics and derives the variables to simulate from the observations
                     ├──► output_dir/simulations.rds     ← reusable as `sim_rds` / `ref_sim_rds`
                     └──► output_dir/observations.rds    ← reusable as `obs_rds`
```

`evaluate()` never runs STICS simulations itself — it always reads pre-computed `sim_rds` / `obs_rds` (and `ref_sim_rds`). `run_simulations()` is how those files are produced, both for the version under evaluation and for the reference version — see [Data Preparation](#data-preparation) below.

---

## Data Preparation

### `gen_workspace_from_sms()`

Converts a raw SMS repository (XML input files) into a Stics text workspace — one folder per USM — ready to be used as `usms_workspace` by the rest of the package.

```r
gen_workspace_from_sms(
  sms_path   = "/path/to/sms_repository",
  stics_path = "/path/to/stics_distribution",
  output_dir = "workspace/",
  usms_files = NULL,
  parallel   = FALSE,
  cores      = NA
)
```

| Argument | Description |
|---|---|
| `sms_path` | Path to the SMS repository |
| `stics_path` | Path to the Stics distribution (used to copy model input files) |
| `output_dir` | Path to the Stics text workspace to generate |
| `usms_files` | Character vector of one or more paths to text files, each listing USM names (one per line), to restrict which USMs are generated. `NULL` (default) generates all evaluation and calibration USMs |
| `parallel` / `cores` | Parallel execution options |

### `run_simulations()`

Runs Stics simulations for the USMs found in a text workspace (as generated by `gen_workspace_from_sms()`), and saves both the simulation and observation data as RDS files in `output_dir`.

```r
run_simulations(
  stics_exe      = "/path/to/stics",
  usms_workspace = "workspace/",
  metadata_file  = "metadata.csv",
  output_dir     = "outputs/",
  usms_files     = NULL,
  vars           = NULL,
  parallel       = FALSE,
  cores          = NA
)
```

| Argument | Description |
|---|---|
| `stics_exe` | Path to the Stics executable to run |
| `usms_workspace` | Path to the Stics text workspace containing the USMs to simulate |
| `metadata_file` | Path to the metadata CSV file describing USM rotations |
| `output_dir` | Directory where `simulations.rds` and `observations.rds` are written |
| `usms_files` | Character vector of one or more paths to text files listing the USMs to simulate. `NULL` (default) simulates all USMs found in `usms_workspace` |
| `vars` | Character vector of variable names to simulate. `NULL` (default) derives them automatically from the observation files found in `usms_workspace`. Pass this explicitly to simulate variables that aren't observed, e.g. the balance closure variables consumed by `balance_closure_test()` |
| `parallel` / `cores` | Parallel execution options |

By default, the variables to simulate are derived automatically from the observation files found in `usms_workspace`, using `get_var_from_obs()`. The resulting `simulations.rds` / `observations.rds` files can be passed as `sim_rds` / `obs_rds` (or `ref_sim_rds` for a reference version) to `evaluate()` — see [From raw SMS data to evaluation](#from-raw-sms-data-to-evaluation) below.

#### `get_var_from_obs()`

Returns the variable names found in a list of observation data frames (e.g. as returned by `SticsRFiles::get_obs()`), excluding metadata columns (`Date`, `situation`, `species`, `version`, `Plant`). This is exactly how `run_simulations()` derives its default `vars` — exposed so you can inspect that list, or extend it with extra variables (such as the balance closure ones) into a single `run_simulations()` call instead of running simulations twice:

```r
obs <- SticsRFiles::get_obs("workspace/", usm = usms)
vars <- c(get_var_from_obs(obs), balance_vars)

run_simulations(
  stics_exe      = "/path/to/stics",
  usms_workspace = "workspace/",
  metadata_file  = "metadata.csv",
  output_dir     = "outputs/",
  vars           = vars
)
```

---

## Common Arguments

There is no shared configuration object: every workflow function (`evaluate()`, `balance_closure_test()`, `run_simulations()`, `gen_workspace_from_sms()`) takes its own explicit arguments and validates them independently as soon as it's called — all errors are collected and reported together, before anything is read from or written to disk. A few argument names and conventions are shared across these functions:

| Argument | Description |
|---|---|
| `usms_workspace` | Path to the Stics text workspace (one folder per USM), as generated by `gen_workspace_from_sms()` |
| `sim_rds` / `obs_rds` / `ref_sim_rds` | Paths to RDS files containing pre-computed simulation/observation data, as produced by `run_simulations()` |
| `output_dir` | Directory where results (CSV exports, plots) are written. Created if it doesn't exist |
| `parallel` / `cores` | Enable parallel execution (`FALSE` by default) and the number of cores to use (`NA` = auto; required when `parallel = TRUE`) |
| `verbose` | Logging verbosity level: `0` = silent, `1` = info, `2` = debug (default: `1`) |

---

## Tests and Evaluations

### Statistical Evaluation

`evaluate()` runs the full statistical evaluation:

```r
evaluate(
  usms_workspace     = "workspace/",
  sim_rds            = "outputs/simulations.rds",
  obs_rds            = "outputs/observations.rds",
  ref_sim_rds        = "reference/simulations.rds",
  output_dir         = "outputs/",
  report             = FALSE,
  percentage         = 5,
  species            = NULL,
  usms               = NULL,
  var2exclude        = NULL,
  ratio_threshold    = 50,
  degraded_threshold = 20,
  max_degraded_vars  = 3,
  parallel           = FALSE,
  cores              = NA,
  verbose            = 1L
)
```

| Argument | Description |
|---|---|
| `usms_workspace` | Path to the USMs input data directory (required) — used to determine the species associated with each USM |
| `sim_rds` | Path to an RDS file containing pre-computed simulation outputs for the evaluated version (required). Produced by `run_simulations()` |
| `obs_rds` | Path to an RDS file containing pre-computed observation data (required). Produced by `run_simulations()` |
| `ref_sim_rds` | Path to an RDS file containing the reference version's simulation outputs, used for regression detection. `NULL` = evaluate against observations only, without regression comparison |
| `output_dir` | Output directory for CSV exports and plots. `NULL` = results are not exported |
| `report` | If `TRUE` and `output_dir` is set, renders an HTML [dashboard](#dashboard) summarizing the exported results once evaluation is done (default: `FALSE`) |
| `percentage` | Threshold (%) above which a variable is flagged as deteriorated vs. the reference (default: `5`) |
| `species` | Optional character vector of species to evaluate. `NULL` = all available |
| `usms` | Optional character vector of USMs to evaluate. `NULL` = all available |
| `var2exclude` | Optional character vector of variables to exclude from evaluation |
| `ratio_threshold` | Threshold (%) above which a single variable makes a USM fail the USM-level evaluation (default: `50`) |
| `degraded_threshold` | Threshold (%) above which a variable is considered degraded for the USM-level evaluation (default: `20`) |
| `max_degraded_vars` | Maximum number of degraded variables tolerated per USM before it's considered failed (default: `3`) |

`evaluate()` loads simulation, observation and reference data into a temporary evaluation workspace (cleaned up automatically), runs the global, per-species and per-USM evaluations described below, exports results to `output_dir` (if defined), prints a summary, and stops with an error if any evaluation failed. It never runs STICS simulations itself — use `run_simulations()` to produce `sim_rds` / `obs_rds` beforehand.

#### Global evaluation

Computes statistics (RMSE, nRMSE, bias, R², etc.) across **all species and USMs combined**, against field observations and, when `ref_sim_rds` is provided, against the reference version's simulation outputs. Writes `global_stats.csv` to `output_dir`. Considered successful if no variable shows a critical deterioration.

#### Species evaluation

Computes statistics per species (RMSE, nRMSE, bias, R², etc.) against field observations and, when `ref_sim_rds` is provided, against the reference version. Flags variables and USMs where performance has deteriorated beyond the `percentage` threshold, for the species/USMs selected via `species` and `usms`. Considered successful if no species shows a critical deterioration. Writes, to `output_dir`:
- `species_stats.csv` — statistical metrics per species
- `plots/<species>_species_comparison.png` — rRMSE comparison scatter plot (see below)
- `plots/<species>_scatter_plots.html` — interactive scatter plots for deteriorated variables

##### rRMSE comparison plot

For each species, a scatter plot compares the **rRMSE of the new version vs. the reference version**, one point per variable, colour-coded by regression status:

| Colour | Status | Condition |
|--------|--------|-----------|
| 🔴 Red | Critical | ratio ≥ `percentage` % |
| 🟠 Orange | Warning | 0 % < ratio < `percentage` % |
| 🟢 Green | Improved | ratio ≤ 0 % |

A diagonal line (slope = 1) marks perfect parity; a dashed line (slope = 1 + `percentage`/100) marks the deterioration threshold. Variable names are displayed as repelled labels. The plot is only generated when a reference version (`ref_sim_rds`) is available.

#### USM evaluation

Evaluates model performance at the **USM (situation) level** to identify situations where the evaluated STICS version is locally degraded compared to the reference version. Unlike the species evaluation, which detects degradation at the species level, this identifies individual USMs where one or more variables show an abnormal increase in RMSE compared to the reference.

For each variable and each USM, the following ratio is computed:

\[
RMSE\_ratio = 100 \times
\frac{RMSE_{eval} - RMSE_{ref}}{RMSE_{species}}
\]

where:

- `RMSE_eval` is the RMSE of the evaluated version for this variable and USM.
- `RMSE_ref` is the RMSE of the reference version for this variable and USM.
- `RMSE_species` is the RMSE of the evaluated version computed over the whole species.

A USM fails the evaluation if at least one of the following conditions is met:

- one variable has `RMSE_ratio > ratio_threshold` (default: 50%)
- more than `max_degraded_vars` variables have `RMSE_ratio > degraded_threshold` (default: 20%)

USMs and variables with fewer than 10 observations (`n_obs`) are ignored when determining failed USMs. Writes `Deteriorated_USM.csv` to `output_dir`. Considered successful if no USM fails the evaluation.

#### Dashboard

`render_report(output_dir)` turns the CSV/plot files written by `evaluate(..., output_dir = output_dir)` into a browsable HTML site: a homepage with the evaluation summary (pass/fail status, per-species status/USM counts), a global page with the degraded variables and global stats, and one page per species with that species' statistics, degraded variables, deteriorated/failed USMs, rRMSE comparison plot and scatter plots. Nothing is recomputed — only the files `evaluate()` already wrote are read.

It can be called on its own once `evaluate(..., output_dir = output_dir)` has run:

```r
evaluate(
  usms_workspace = "workspace/",
  output_dir     = "outputs/",
  sim_rds        = "outputs/simulations.rds",
  obs_rds        = "outputs/observations.rds",
  ref_sim_rds    = "reference/simulations.rds"
)

render_report("outputs/")
```

or triggered directly from `evaluate()` by passing `report = TRUE` (requires `output_dir` to be set):

```r
evaluate(
  usms_workspace = "workspace/",
  output_dir     = "outputs/",
  sim_rds        = "outputs/simulations.rds",
  obs_rds        = "outputs/observations.rds",
  ref_sim_rds    = "reference/simulations.rds",
  report         = TRUE
)
```

Requires the Quarto CLI and the `quarto`/`DT` R packages (see [Dependencies](#dependencies)). The rendered site (`outputs/index.html`) links to plots via relative paths, so keep `output_dir` intact when sharing or moving results.

The homepage and global page are always rendered first and sequentially, but the per-species pages can be rendered in parallel with `parallel = TRUE` / `cores` (same convention as the other workflow functions — `render_report(output_dir, parallel = TRUE, cores = 4)`). When triggered via `evaluate(..., report = TRUE)`, the species pages inherit `evaluate()`'s own `parallel`/`cores` arguments.

---

### `balance_closure_test()`

Checks the **water and nitrogen balance closure** for each simulated USM. For each USM, it compares the initial and final values of the following five balances:

| Balance | Checked fields |
|---|---|
| Water | `init_H2O_balance` / `final_H2O_balance` |
| Plant nitrogen | `init_plant_N_balance` / `final_plant_N_balance` |
| Soil mineral nitrogen | `init_soil_mineral_N_balance` / `final_soil_mineral_N_balance` |
| Soil organic nitrogen | `init_soil_organic_N_balance` / `final_soil_organic_N_balance` |
| Soil organic carbon | `init_soil_organic_C_balance` / `final_soil_organic_C_balance` |

A USM is flagged if the absolute rounded difference between its initial and final values is greater than 1. USMs with missing fields or only NA values are silently skipped.

Like `evaluate()`, `balance_closure_test()` never runs STICS simulations itself — it reads simulation data from `sim_rds`. Since the balance fields above aren't observed variables, pass them explicitly via the `vars` argument of `run_simulations()` so they end up in `sim_rds`:

```r
balance_vars <- c(
  "init_H2O_balance", "final_H2O_balance",
  "init_plant_N_balance", "final_plant_N_balance",
  "init_soil_mineral_N_balance", "final_soil_mineral_N_balance",
  "init_soil_organic_N_balance", "final_soil_organic_N_balance",
  "init_soil_organic_C_balance", "final_soil_organic_C_balance"
)

run_simulations(
  stics_exe      = "/path/to/stics",
  usms_workspace = "path/to/usms_workspace",
  metadata_file  = "metadata.csv",
  output_dir     = "/path/to/output_dir",
  vars           = balance_vars
)

balance_closure_test(
  sim_rds    = "/path/to/output_dir/simulations.rds",
  output_dir = "/path/to/output_dir"
)
```

| Argument | Description |
|---|---|
| `sim_rds` | Path to an RDS file containing pre-computed simulation data, including the balance variables above. Produced by `run_simulations()`. The USMs to test are the names of this list, optionally restricted by `usms` |
| `output_dir` | Directory where balance error details (`balance_errors_details.csv`) are written, if any. `NULL` (default) skips the export |
| `usms` | Optional character vector of USMs to restrict the test to. `NULL` = all available |
| `parallel` / `cores` | Parallel execution options |
| `verbose` | Logging verbosity level: `0` = silent, `1` = info, `2` = debug (default: `1`) |

`balance_closure_test()` validates these arguments, logs a summary of the test, and stops with an error listing any USMs with balance closure issues.

If `output_dir` is defined, the balance closure details will be written to a CSV file in the specified directory.

---

## Complete Example

### From raw SMS data to evaluation

```r
library(SticsREval)

balance_vars <- c(
  "init_H2O_balance", "final_H2O_balance",
  "init_plant_N_balance", "final_plant_N_balance",
  "init_soil_mineral_N_balance", "final_soil_mineral_N_balance",
  "init_soil_organic_N_balance", "final_soil_organic_N_balance",
  "init_soil_organic_C_balance", "final_soil_organic_C_balance"
)

# 1. Build a Stics text workspace from the raw SMS repository
gen_workspace_from_sms(
  sms_path   = "/path/to/sms_repository",
  stics_path = "/path/to/stics_distribution",
  output_dir = "workspace/"
)

# 2. Pre-compute the reference version's simulations and observations once
run_simulations(
  stics_exe      = "/path/to/stics_reference",
  usms_workspace = "workspace/",
  metadata_file  = "metadata.csv",
  output_dir     = "reference/"
)

# 3. Run the candidate version's simulations, combining the observed
#    variables with the balance closure ones into a single call
obs  <- SticsRFiles::get_obs("workspace/")
vars <- c(get_var_from_obs(obs), balance_vars)

run_simulations(
  stics_exe      = "/path/to/stics_candidate",
  usms_workspace = "workspace/",
  metadata_file  = "metadata.csv",
  output_dir     = "outputs/",
  vars           = vars
)

# 4. Evaluate the candidate version against the reference
evaluate(
  usms_workspace = "workspace/",
  output_dir     = "outputs/",
  sim_rds        = "outputs/simulations.rds",
  obs_rds        = "outputs/observations.rds",
  ref_sim_rds    = "reference/simulations.rds",
  percentage     = 5
)

# 5. Check water and nitrogen balance closure
balance_closure_test(
  sim_rds    = "outputs/simulations.rds",
  output_dir = "outputs/"
)

# 6. Render the results as a browsable HTML dashboard
render_report("outputs/")
```

### Simple usage

Assuming `run_simulations()` has already produced the RDS files (see above):

```r
library(SticsREval)

# 1. Run the full statistical evaluation (global, per species,
#    and per USM), export results, and print summaries
evaluate(
  usms_workspace = "workspace/",
  output_dir     = "outputs/",
  sim_rds        = "outputs/simulations.rds",
  obs_rds        = "outputs/observations.rds",
  ref_sim_rds    = "reference_simulations.rds",
  percentage     = 5
)

# 2. Check water and nitrogen balance closure
balance_closure_test(
  sim_rds    = "outputs/simulations.rds",
  output_dir = "outputs/"
)

# 3. Render the results as a browsable HTML dashboard
render_report("outputs/")
```

---

## Docker

A pre-built Docker image is available on the GitHub Container Registry, so you can run `SticsREval` without installing R or any dependencies locally. It also bundles the [Quarto CLI](https://quarto.org), so `render_report()` works out of the box.

### Pull the image

```bash
docker pull ghcr.io/sticsrpacks/stics-r-eval:latest
```

### Run an interactive R session

```bash
docker run --rm -it \
  -v /path/to/your/workspace:/workspace \
  ghcr.io/sticsrpacks/stics-r-eval:latest \
  R
```

Then inside R:

```r
library(SticsREval)

run_simulations(
  stics_exe      = "/path/to/stics",
  usms_workspace = "/workspace/",
  metadata_file  = "/workspace/metadata.csv",
  output_dir     = "/workspace/outputs/"
)

evaluate(
  usms_workspace = "/workspace/",
  output_dir     = "/workspace/outputs/",
  sim_rds        = "/workspace/outputs/simulations.rds",
  obs_rds        = "/workspace/outputs/observations.rds"
)

balance_closure_test(
  sim_rds    = "/workspace/outputs/simulations.rds",
  output_dir = "/workspace/outputs/"
)
```

### Run a script non-interactively

```bash
docker run --rm \
  -v /path/to/your/workspace:/workspace \
  -v /path/to/your/script.R:/script.R \
  ghcr.io/sticsrpacks/stics-r-eval:latest \
  Rscript /script.R
```

### Build the image locally

```bash
# Without a GitHub token
docker build -t stics-r-eval .

# With a GitHub token (needed to install private SticsRPacks dependencies)
docker build \
  --secret id=GITHUB_TOKEN,src=<(echo $GITHUB_PAT) \
  -t stics-r-eval .
```

> **Note:** The image is based on [`rocker/r-ver:4`](https://rocker-project.org/) and uses [`renv`](https://rstudio.github.io/renv/) to ensure reproducible package versions. Dependencies are restored from `renv.lock` at build time.

---

## Related packages

- [SticsRFiles](https://github.com/SticsRPacks/SticsRFiles) — Read/write STICS input and output files
- [SticsOnR](https://github.com/SticsRPacks/SticsOnR) — Run STICS simulations from R
- [CroPlotR](https://github.com/SticsRPacks/CroPlotR) — Crop model evaluation statistics and plots

---

## Citation

If you use `SticsREval` in your work, please cite it as follows:

```r
citation("SticsREval")
```

---

## Contributing

Contributions are welcome! Please open an [issue](https://github.com/SticsRPacks/SticsREval/issues) or submit a pull request on GitHub.

---

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.