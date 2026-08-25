# SticsREval <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/SticsRPacks/SticsREval/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/SticsRPacks/SticsREval/actions/workflows/R-CMD-check.yml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: LGPL-3.0](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
[![Docker Image](https://img.shields.io/badge/docker-ghcr.io-blue?logo=docker)](https://github.com/SticsRPacks/SticsREval/pkgs/container/stics-r-eval)
<!-- badges: end -->

## Overview

`SticsREval` is an R package for **evaluating and comparing versions of the STICS crop model**. It supports multiple evaluation workflows implemented as independent classes sharing the same `Configuration` object. Tests can be run individually or combined into a custom pipeline.

The package currently provides the following evaluation workflows:

- **Statistical Evaluation** — assesses whether a new version of STICS performs better, equally, or worse than a reference version, both against field observations and against reference simulation outputs. It includes:
  - global evaluation across all species and USMs
  - species-level evaluation
  - USM-level evaluation
- **Balance Closure Test** — checks the internal consistency of water and nitrogen balances in the simulations

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

---

## Workflow

`Configuration` is the single entry point for all parameters. The simplest way to run a full evaluation is via the `evaluate()` function, which orchestrates the entire pipeline:

```
  Configuration$new(...)                         ← defines all parameters (paths, options, filters)
           │
           └──► evaluate(config)
                     │
                     ├──► USMSWorkspace$new(config)$load()      ← loads/prepares sim, obs & reference data
                     ├──► GlobalEvaluation$new(config)$run()    ← statistical evaluation vs obs & reference (all species)
                     ├──► SpeciesEvaluation$new(config)$run()   ← statistical evaluation vs obs & reference (per species)
                     ├──► USMEvaluation$new(config)$run()       ← statistical evaluation vs obs & reference (per USM)
                     ├──► $export(...)                          ← exports evaluation results to output_dir
                     ├──► $summary()                            ← prints a summary of results
                     └──► stops with an error if any evaluation failed
```

`BalanceClosureTest` runs independently of `evaluate()` and loads its own simulation data directly from `usms_workspace`:

```
  Configuration$new(...)
           │
           └──► BalanceClosureTest$new(config)$run()          ← water & nitrogen balance closure check
```

For advanced use cases, `GlobalEvaluation`, `SpeciesEvaluation` and `USMEvaluation` can also be instantiated and run individually instead of using `evaluate()`:

```
  Configuration$new(...)
           │
           ├──► GlobalEvaluation$new(config)$run()            ← statistical evaluation vs obs & reference (all species)
           │         │
           │         └──► $export(...)                        ← export statistics to CSV
           │
           ├──► SpeciesEvaluation$new(config)$run()           ← statistical evaluation vs obs & reference (per species)
           │         │
           │         └──► $export(...)                        ← export statistics & plots
           │
           └──► USMEvaluation$new(config)$run()               ← USM-level regression detection based on RMSE ratio
                     │
                     └──► $export(...)                        ← export deteriorated USMs
```

`GlobalEvaluation`, `SpeciesEvaluation` and `USMEvaluation` are independent classes and store their evaluation results as an internal attribute of the object. When used individually (without `evaluate()`), the data must first be loaded into the evaluation workspace via `USMSWorkspace$new(config)$load()`.

Upstream of `Configuration`, two standalone functions prepare the data these workflows consume, starting from a raw SMS repository:

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
| `parallel` / `cores` | Parallel execution options |

The variables to simulate are derived automatically from the observation files found in `usms_workspace`. The resulting `simulations.rds` / `observations.rds` files can be passed as `sim_rds` / `obs_rds` (or `ref_sim_rds` for a reference version) in a `Configuration` — see [From raw SMS data to evaluation](#from-raw-sms-data-to-evaluation) below.

---

## Configuration

### `Configuration`

Encapsulates and validates all configuration parameters for the package. The same object is passed to all workflow classes and functions. Fields are validated against a declarative schema at construction time — all errors are collected and reported together.

```r
library(SticsREval)

config <- Configuration$new(
  stics_exe          = "/path/to/stics",
  usms_workspace     = "workspace/",
  metadata_file      = "metadata.csv",
  eval_workspace     = "eval_workspace/",
  output_dir         = "outputs/",
  verbose            = 1L,
  parallel           = FALSE,
  cores              = NA,
  ref_sim_rds        = "reference/simulations.rds",
  sim_rds            = "outputs/simulations.rds",
  obs_rds            = "outputs/observations.rds",
  percentage         = 5,
  species            = NULL,
  usms               = NULL,
  var2exclude        = NULL
)
```

| Field | Description |
|---|---|
| `stics_exe` | Path to the STICS executable (required for the balance closure workflow) |
| `usms_workspace` | Path to the USMs input data directory (required) |
| `metadata_file` | Path to the metadata CSV file describing simulations (required for the balance closure workflow) |
| `eval_workspace` | Optional path to the evaluation workspace, used internally to stage simulation and observation data as Parquet datasets before evaluation. Created automatically by `evaluate()` if it doesn't exist. If not provided, a temporary directory will be used |
| `output_dir` | Output directory for CSV exports and plots (required for export and plots workflows) |
| `verbose` | Logging verbosity level: `0` = silent, `1` = info, `2` = debug (default: `1`) |
| `parallel` | Enable parallel execution (default: `FALSE`) |
| `cores` | Number of cores for parallel execution (`NA` = auto; required when `parallel = TRUE`) |
| `ref_sim_rds` | Path to an RDS file containing the reference version's simulation outputs, used for regression detection (required for evaluation) |
| `sim_rds` | Path to an RDS file containing pre-computed simulation outputs for the new version (required for evaluation). Produced by `run_simulations()` |
| `obs_rds` | Path to an RDS file containing pre-computed observation data (required for evaluation). Produced by `run_simulations()` |
| `percentage` | Threshold (%) above which a variable is flagged as deteriorated vs. the reference (default: `5`) |
| `species` | Optional character vector of species to evaluate. Used by `SpeciesEvaluation` and `USMEvaluation`; ignored by `GlobalEvaluation`. `NULL` = all available |
| `usms` | Optional character vector of USMs to evaluate. `NULL` = all available. |
| `var2exclude` | Optional character vector of variables to exclude from evaluation. |

`Configuration` also exposes workflow-specific validation methods called internally by each function:

- `config$validate_eval()` — checks requirements for the statistical evaluation workflow (used by `evaluate()`, `GlobalEvaluation`, `SpeciesEvaluation`, and `USMEvaluation`)
- `config$validate_balance_closure()` — checks requirements for the balance closure test workflow

---

## Tests and Evaluations

### Statistical Evaluation

The simplest way to run the full statistical evaluation is via the `evaluate()` function:

```r
evaluate(config)
```

This loads simulation, observation and reference data from `sim_rds`, `obs_rds` and `ref_sim_rds` into `eval_workspace`, runs `GlobalEvaluation`, `SpeciesEvaluation`, and `USMEvaluation`, exports results to `output_dir` (if defined), prints a summary, and stops with an error if any evaluation failed. `evaluate()` never runs STICS simulations itself — use `run_simulations()` to produce `sim_rds` / `obs_rds` beforehand.

#### `GlobalEvaluation`

Computes statistics (RMSE, nRMSE, bias, R², etc.) across **all species and USMs combined**, against field observations and, when `ref_sim_rds` is provided, against the reference version's simulation outputs.

```r
global_eval <- GlobalEvaluation$new(config)
global_eval$run()
global_eval$summary()
global_eval$export()
```

- `run()` computes the global statistics and, if a reference is available, the rRMSE comparison against it.
- `summary()` prints a report of the comparison to the console.
- `export()` writes `global_stats.csv` to `output_dir`.
- `global_eval$success` is `TRUE` if no variable shows a critical deterioration.

#### `SpeciesEvaluation`

Computes statistics per species (RMSE, nRMSE, bias, R², etc.) against field observations and, when `ref_sim_rds` is provided, against the reference version. Flags variables and USMs where performance has deteriorated beyond the `percentage` threshold.

```r
species_eval <- SpeciesEvaluation$new(config)
species_eval$run()
species_eval$summary()
species_eval$export()
```

- `run()` computes per-species statistics and rRMSE comparisons for the species selected via `config$species` and `config$usms`.
- `summary()` prints a report per species, grouped by degradation level (major, minor, none).
- `export()` writes, to `output_dir`:
  - `species_stats.csv` — statistical metrics per species
  - `plots/<species>_species_comparison.png` — rRMSE comparison scatter plot (see below)
  - `plots/<species>_scatter_plots.html` — interactive scatter plots for deteriorated variables
- `species_eval$success` is `TRUE` if no species shows a critical deterioration.

Both classes accept optional `workspace` and `logger` (and, for `SpeciesEvaluation`, `backend`) arguments for dependency injection in tests or advanced use:

```r
SpeciesEvaluation$new(
  config,
  workspace = EvalWorkspace$new(config$eval_workspace),
  backend   = ParallelBackend$new(config$parallel, config$cores),
  logger    = default_logger
)$run()
```

##### rRMSE comparison plot

`SpeciesEvaluation$export()` generates, for each species, a scatter plot comparing the **rRMSE of the new version vs. the reference version**, one point per variable, colour-coded by regression status:

| Colour | Status | Condition |
|--------|--------|-----------|
| 🔴 Red | Critical | ratio ≥ `percentage` % |
| 🟠 Orange | Warning | 0 % < ratio < `percentage` % |
| 🟢 Green | Improved | ratio ≤ 0 % |

A diagonal line (slope = 1) marks perfect parity; a dashed line (slope = 1 + `percentage`/100) marks the deterioration threshold. Variable names are displayed as repelled labels. The plot is only generated when a reference version (`ref_sim_rds`) is available.

#### `USMEvaluation`

Evaluates model performance at the **USM (situation) level** to identify situations where the evaluated STICS version is locally degraded compared to the reference version.

Unlike `SpeciesEvaluation`, which detects degradation at the species level, `USMEvaluation` identifies individual USMs where one or more variables show an abnormal increase in RMSE compared to the reference.

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

USMs and variables with fewer than 10 observations (`n_obs`) are ignored when determining failed USMs.

```r
usm_eval <- USMEvaluation$new(config)
usm_eval$run()
usm_eval$summary()
usm_eval$export()
```

- `run()` computes RMSE ratios for each variable and USM.
- `summary()` prints the list of failed and passed USMs by species.
- `export()` writes `Deteriorated_USM.csv` to `output_dir`.
- `usm_eval$success` is `TRUE` if no USM fails the evaluation.

The evaluation thresholds can be customized:

```r
usm_eval <- USMEvaluation$new(
  config,
  ratio_threshold = 50,
  degraded_threshold = 20,
  max_degraded_vars = 3
)
```

`USMEvaluation` can also be used manually by providing pre-computed statistics, which is useful for unit testing:

```r
usm_eval <- USMEvaluation$new(
  species = "Species1",
  stats_usm = stats_usm,
  stats_species = stats_species
)

usm_eval$get_data()
usm_eval$failed_usms
```

The returned data contains one row per variable/USM/species combination:

| Column | Description |
|---|---|
| `species` | Species name |
| `situation` | USM identifier |
| `variable` | Evaluated variable |
| `rmse_eval` | RMSE of the evaluated version |
| `rmse_ref` | RMSE of the reference version |
| `rmse_species` | Species-level RMSE of the evaluated version |
| `rmse_ratio` | Relative RMSE deterioration ratio (%) |
| `n_obs` | Number of observations used |

`USMEvaluation` is independent from `evaluate()` and can be run after loading the workspace:

```r
USMSWorkspace$new(config)$load()

usm_eval <- USMEvaluation$new(config)
usm_eval$run()
usm_eval$summary()
```

---

### `BalanceClosureTest`

Checks the **water and nitrogen balance closure** for each simulated USM. For each USM, the class compares the initial and final values of the following five balances:

| Balance | Checked fields |
|---|---|
| Water | `init_H2O_balance` / `final_H2O_balance` |
| Plant nitrogen | `init_plant_N_balance` / `final_plant_N_balance` |
| Soil mineral nitrogen | `init_soil_mineral_N_balance` / `final_soil_mineral_N_balance` |
| Soil organic nitrogen | `init_soil_organic_N_balance` / `final_soil_organic_N_balance` |
| Soil organic carbon | `init_soil_organic_C_balance` / `final_soil_organic_C_balance` |

A USM is flagged if the absolute rounded difference between its initial and final values is greater than 1. USMs with missing fields or only NA values are silently skipped.

```r
config <- Configuration$new(
  stics_exe      = "/path/to/stics",
  metadata_file  = "metadata.csv",
  usms_workspace = "path/to/usms_workspace",
  output_dir     = "/path/to/output_dir"
)

BalanceClosureTest$new(config)$run()
```

The `run()` method logs a summary of the test and stops with an error listing any USMs with balance closure issues. It respects the `usms`, `parallel`, and `cores` filters defined in the `Configuration`.

If `output_dir` is defined, the balance closure details will be written to a CSV file in the specified directory.

---

## Complete Example

### From raw SMS data to evaluation

```r
library(SticsREval)

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

# 3. Run the candidate version's simulations
run_simulations(
  stics_exe      = "/path/to/stics_candidate",
  usms_workspace = "workspace/",
  metadata_file  = "metadata.csv",
  output_dir     = "outputs/"
)

# 4. Evaluate the candidate version against the reference
config <- Configuration$new(
  stics_exe      = "/path/to/stics_candidate",
  usms_workspace = "workspace/",
  metadata_file  = "metadata.csv",
  eval_workspace = "eval_workspace/",
  output_dir     = "outputs/",
  sim_rds        = "outputs/simulations.rds",
  obs_rds        = "outputs/observations.rds",
  ref_sim_rds    = "reference/simulations.rds",
  percentage     = 5
)

evaluate(config)
BalanceClosureTest$new(config)$run()
```

### Simple usage

```r
library(SticsREval)

# 1. Configure the evaluation
config <- Configuration$new(
  stics_exe      = "/path/to/stics_candidate",
  usms_workspace = "workspace/",
  metadata_file  = "metadata.csv",
  eval_workspace = "eval_workspace/",
  output_dir     = "outputs/",
  sim_rds        = "outputs/simulations.rds",
  obs_rds        = "outputs/observations.rds",
  ref_sim_rds    = "reference_simulations.rds",
  percentage     = 5
)

# 2. Run the full statistical evaluation (global, per species,
#    and per USM), export results, and print summaries
evaluate(config)

# 3. Check water and nitrogen balance closure
BalanceClosureTest$new(config)$run()
```

### Advanced usage

```r
library(SticsREval)

# 1. Configure the evaluation
config <- Configuration$new(
  stics_exe      = "/path/to/stics_candidate",
  usms_workspace = "workspace/",
  metadata_file  = "metadata.csv",
  eval_workspace = "eval_workspace/",
  output_dir     = "outputs/",
  sim_rds        = "outputs/simulations.rds",
  obs_rds        = "outputs/observations.rds",
  ref_sim_rds    = "reference_simulations.rds",
  percentage     = 5
)

# 2. Load simulation, observation and reference data into the eval workspace
USMSWorkspace$new(config)$load()

# 3. Run the global evaluation (all species combined)
global_eval <- GlobalEvaluation$new(config)
global_eval$run()
global_eval$summary()
global_eval$export()

# 4. Run the per-species evaluation
species_eval <- SpeciesEvaluation$new(config)
species_eval$run()
species_eval$summary()
species_eval$export()

# 5. Run the USM-level evaluation
usm_eval <- USMEvaluation$new(config)
usm_eval$run()
usm_eval$summary()
usm_eval$export()

# 6. Check water and nitrogen balance closure
BalanceClosureTest$new(config)$run()
```

---

## Docker

A pre-built Docker image is available on the GitHub Container Registry, so you can run `SticsREval` without installing R or any dependencies locally.

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

config <- Configuration$new(
  stics_exe      = "/path/to/stics",
  usms_workspace = "/workspace/",
  metadata_file  = "/workspace/metadata.csv",
  eval_workspace = "/workspace/eval_workspace/",
  output_dir     = "/workspace/outputs/",
  sim_rds        = "/workspace/outputs/simulations.rds",
  obs_rds        = "/workspace/outputs/observations.rds"
)

evaluate(config)
BalanceClosureTest$new(config)$run()
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