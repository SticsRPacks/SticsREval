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

`Configuration` is the single entry point for all parameters. Once defined, it is passed to any combination of evaluation classes and export functions:

```
  Configuration$new(...)                         ← defines all parameters (paths, options, filters)
           │
           ├──► Evaluation$new(config)$run()                  ← statistical evaluation vs obs & reference
           │         │
           │         └──► export_stats_to_csv(config)         ← export statistics + deteriorated USMs to CSV
           │
           └──► BalanceClosureTest$new(config)$run()          ← water & nitrogen balance closure check
```

All evaluation results are stored in an **`EvalWorkspace`**, which manages Parquet datasets on disk.

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
  reference_version  = NULL,
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
| `eval_workspace` | Path to the evaluation workspace — (required for evaluation) |
| `output_dir` | Output directory for CSV exports and plots (required for export and plots workflows) |
| `run_simulations` | Whether to run STICS simulations (default: `FALSE`) |
| `verbose` | Logging verbosity level: `0` = silent, `1` = info, `2` = debug (default: `1`) |
| `parallel` | Enable parallel execution (default: `FALSE`) |
| `cores` | Number of cores for parallel execution (`NA` = auto; required when `parallel = TRUE`) |
| `reference_version` | Version string present in `eval_workspace` to use as the reference for regression detection |
| `percentage` | Threshold (%) above which a variable is flagged as deteriorated vs. the reference (default: `5`) |
| `species` | Optional character vector of species to evaluate. `NULL` = all available. |
| `usms` | Optional character vector of USMs to evaluate. `NULL` = all available. |
| `var2exclude` | Optional character vector of variables to exclude from evaluation. |

`Configuration` also exposes workflow-specific validation methods called internally by each function:

- `config$validate_eval()` — checks requirements for the statistical evaluation workflow
- `config$validate_export()` — checks that `output_dir` is set and writable

---

## Tests and Evaluations

### Statistical Evaluation

#### `Evaluation`

The core statistical evaluation class. It orchestrates the full evaluation workflow:

- Optionally initializes the `EvalWorkspace` (loading simulations and observations from the USMs workspace)
- Computes statistics per species (RMSE, nRMSE, bias, R², etc.) against **field observations**
- When `reference_version` is set, compares statistics against the **reference version** to detect regressions
- Flags variables and USMs where performance has deteriorated beyond the `percentage` threshold
- Saves all results as **Arrow/Parquet files** in the `eval_workspace` directory
- Displays a summary of comparison results

```r
Evaluation$new(config)$run()
```

You can also inject custom dependencies for testing or advanced use:

```r
Evaluation$new(
  config,
  workspace = EvalWorkspace$new("eval_workspace/"),
  backend   = ParallelBackend$new(FALSE, NA),
  logger    = default_logger
)$run()
```

---

#### `export_stats_to_csv()`

Exports the evaluation statistics to CSV files in `output_dir`:

| File | Content |
|---|---|
| `species_stats.csv` | Statistical metrics per species |
| `global_stats.csv` | Global statistical criteria (RMSE, nRMSE, bias, R², etc.) |
| `RMSE_per_usm.csv` | RMSE broken down per USM |
| `Deteriorated_USM.csv` | USMs with deteriorated performance vs. the reference version |

```r
export_stats_to_csv(config)
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

A USM is flagged if its rounded initial and final values differ. USMs with missing fields or fully `NA` values are silently skipped.

```r
config <- Configuration$new(
  stics_exe      = "/path/to/stics",
  metadata_file  = "metadata.csv",
  usms_workspace = "path/to/usms_workspace"
)

BalanceClosureTest$new(config)$run()
```

The `run()` method logs a summary of the test and lists any USMs with balance closure issues. It respects the `usms`, `parallel`, and `cores` filters defined in the `Configuration`.

---

## `EvalWorkspace`

Manages reading and writing all evaluation data (simulations, observations, statistics, comparisons) stored as Parquet datasets on disk. Used internally by `Evaluation` and `export_stats_to_csv` but can also be used directly to inspect results.

```r
ws <- EvalWorkspace$new("eval_workspace/")

# Read simulations and observations
sim <- ws$get_sim(species = "wheat")
obs <- ws$get_obs(species = "wheat")

# Read statistics
stats <- ws$get_stats(species = "wheat", collect = TRUE)

# List all evaluated versions
ws$get_all_versions()

# Access data for a specific version
ws_ref <- ws$with_version("v10.0")
ref_stats <- ws_ref$get_stats(species = "wheat")
```

| Method | Description |
|---|---|
| `get_sim(species, usms, var2exclude)` | Return simulated data |
| `get_obs(species, usms, var2exclude)` | Return observed data |
| `get_stats(species)` | Return global evaluation statistics |
| `get_rmse_per_usm(species)` | Return RMSE broken down per USM |
| `get_deteriorated_usm(species, percentage)` | Return deteriorated USM comparison |
| `get_species_comparison(species, percentage)` | Return RMSE comparison vs reference |
| `get_all_versions()` | List all evaluated STICS versions in the workspace |
| `with_version(version)` | Return a new `EvalWorkspace` scoped to a specific version |

---

## Complete Example

```r
library(SticsREval)

# 1. Configure the evaluation
config <- Configuration$new(
  stics_exe         = "/path/to/stics_candidate",
  usms_workspace    = "workspace/",
  metadata_file     = "metadata.csv",
  eval_workspace    = "eval_workspace/",
  output_dir        = "outputs/",
  run_simulations   = TRUE,
  reference_version = "v10.0",
  percentage        = 5
)

# 2. Run statistical evaluation vs observations and reference version
Evaluation$new(config)$run()

# 3. Export statistics and deteriorated USMs to CSV
export_stats_to_csv(config)

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

Evaluation$new(config)$run()
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