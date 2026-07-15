# SticsREval <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/SticsRPacks/SticsREval/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/SticsRPacks/SticsREval/actions/workflows/R-CMD-check.yml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: LGPL-3.0](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
[![Docker Image](https://img.shields.io/badge/docker-ghcr.io-blue?logo=docker)](https://github.com/SticsRPacks/SticsREval/pkgs/container/stics-r-eval)
<!-- badges: end -->

## Overview

`SticsREval` is an R package for **evaluating and comparing versions of the STICS crop model**. It supports multiple types of evaluation, each implemented as an independent class sharing the same `Configuration` object. Tests can be run individually or combined into a custom pipeline.

The package currently provides two types of evaluation:

- **Statistical Evaluation** — assesses whether a new version of STICS performs better, equally, or worse than a reference version, both against field observations and against reference simulation outputs
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
                     ├──► $export(...)                          ← export statistics, deteriorated USMs & plots to output_dir
                     ├──► $summary()                             ← prints a summary of results
                     └──► stops with an error if any evaluation failed
```

`BalanceClosureTest` runs independently of `evaluate()` and loads its own simulation data directly from `usms_workspace`:

```
  Configuration$new(...)
           │
           └──► BalanceClosureTest$new(config)$run()          ← water & nitrogen balance closure check
```

For advanced use cases, `GlobalEvaluation` and `SpeciesEvaluation` can also be instantiated and run individually instead of using `evaluate()`:

```
  Configuration$new(...)
           │
           ├──► GlobalEvaluation$new(config)$run()            ← statistical evaluation vs obs & reference (all species)
           │         │
           │         └──► $export(...)                        ← export statistics to CSV
           │
           └──► SpeciesEvaluation$new(config)$run()           ← statistical evaluation vs obs & reference (per species)
                     │
                     └──► $export(...)                        ← export statistics, deteriorated USMs & plots
```

`GlobalEvaluation` and `SpeciesEvaluation` are independent classes and store their evaluation results as an internal attribute of the object. When used individually (without `evaluate()`), the data must first be loaded into the evaluation workspace via `USMSWorkspace$new(config)$load()`.

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
  run_simulations    = TRUE,
  verbose            = 1L,
  parallel           = FALSE,
  cores              = NA,
  ref_sim_rds        = NULL,
  sim_rds            = NULL,
  obs_rds            = NULL,
  percentage         = 5,
  species            = NULL,
  usms               = NULL,
  var2exclude        = NULL
)
```

| Field | Description |
|---|---|
| `stics_exe` | Path to the STICS executable (required) |
| `usms_workspace` | Path to the USMs input data directory (required) |
| `metadata_file` | Path to the metadata CSV file describing simulations (required when `run_simulations = TRUE`) |
| `eval_workspace` | Path to the evaluation workspace, used internally to stage simulation and observation data as Parquet datasets before evaluation. Created automatically by `evaluate()` if it doesn't exist |
| `output_dir` | Output directory for CSV exports and plots (required for export and plots workflows) |
| `run_simulations` | Whether to run STICS simulations (default: `FALSE`). Alternative to providing pre-computed data via `sim_rds` / `obs_rds` |
| `verbose` | Logging verbosity level: `0` = silent, `1` = info, `2` = debug (default: `1`) |
| `parallel` | Enable parallel execution (default: `FALSE`) |
| `cores` | Number of cores for parallel execution (`NA` = auto; required when `parallel = TRUE`) |
| `ref_sim_rds` | Optional path to an RDS file containing the reference version's simulation outputs, used for regression detection. If not provided, evaluation runs against observations only, without regression comparison |
| `sim_rds` | Path to an RDS file containing pre-computed simulation outputs for the new version (alternative to `run_simulations` + `usms_workspace`) |
| `obs_rds` | Path to an RDS file containing pre-computed observation data (alternative to `run_simulations` + `usms_workspace`) |
| `percentage` | Threshold (%) above which a variable is flagged as deteriorated vs. the reference (default: `5`) |
| `species` | Optional character vector of species to evaluate. Used by `SpeciesEvaluation` to determine which species to evaluate in the provided data; ignored by `GlobalEvaluation`. `NULL` = all available |
| `usms` | Optional character vector of USMs to evaluate. `NULL` = all available. |
| `var2exclude` | Optional character vector of variables to exclude from evaluation. |

`Configuration` also exposes workflow-specific validation methods called internally by each function:

- `config$validate_eval()` — checks requirements for the statistical evaluation workflow (used by `evaluate()`, `GlobalEvaluation`, and `SpeciesEvaluation`)
- `config$validate_balance_closure()` — checks requirements for the balance closure test workflow

---

## Tests and Evaluations

### Statistical Evaluation

The simplest way to run the full statistical evaluation is via the `evaluate()` function:

```r
evaluate(config)
```

This loads simulation, observation and reference data into `eval_workspace`, runs both `GlobalEvaluation` and `SpeciesEvaluation`, exports results to `output_dir` (if defined), prints a summary, and stops with an error if any evaluation failed.

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
  - `rRMSE_per_usm.csv` — rRMSE broken down per USM
  - `Deteriorated_USM.csv` — USMs with deteriorated performance vs. the reference version
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

A USM is flagged if its rounded initial and final values differ. USMs with missing fields or fully `NA` values are silently skipped.

```r
config <- Configuration$new(
  stics_exe      = "/path/to/stics",
  metadata_file  = "metadata.csv",
  usms_workspace = "path/to/usms_workspace"
)

BalanceClosureTest$new(config)$run()
```

The `run()` method logs a summary of the test and stops with an error listing any USMs with balance closure issues. It respects the `usms`, `parallel`, and `cores` filters defined in the `Configuration`.

---

## Complete Example

### Simple usage

```r
library(SticsREval)

# 1. Configure the evaluation
config <- Configuration$new(
  stics_exe       = "/path/to/stics_candidate",
  usms_workspace  = "workspace/",
  metadata_file   = "metadata.csv",
  eval_workspace  = "eval_workspace/",
  output_dir      = "outputs/",
  run_simulations = TRUE,
  ref_sim_rds     = "reference_simulations.rds",
  percentage      = 5
)

# 2. Run the full statistical evaluation (global + per species),
#    export results, and print a summary
evaluate(config)

# 3. Check water and nitrogen balance closure
BalanceClosureTest$new(config)$run()
```

### Advanced usage

```r
library(SticsREval)

# 1. Configure the evaluation
config <- Configuration$new(
  stics_exe       = "/path/to/stics_candidate",
  usms_workspace  = "workspace/",
  metadata_file   = "metadata.csv",
  eval_workspace  = "eval_workspace/",
  output_dir      = "outputs/",
  run_simulations = TRUE,
  ref_sim_rds     = "reference_simulations.rds",
  percentage      = 5
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

# 5. Check water and nitrogen balance closure
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

config <- Configuration$new(
  stics_exe       = "/path/to/stics",
  usms_workspace  = "/workspace/",
  metadata_file   = "/workspace/metadata.csv",
  eval_workspace  = "/workspace/eval_workspace/",
  output_dir      = "/workspace/outputs/",
  run_simulations = TRUE
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