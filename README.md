# SticsREval <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/SticsRPacks/SticsREval/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/SticsRPacks/SticsREval/actions/workflows/R-CMD-check.yml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: LGPL-3.0](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
[![Docker Image](https://img.shields.io/badge/docker-ghcr.io-blue?logo=docker)](https://github.com/SticsRPacks/SticsREval/pkgs/container/stics-r-eval)
<!-- badges: end -->

## Overview

`SticsREval` is an R package for **evaluating and comparing versions of the STICS crop model**. The core idea is to assess whether a new version of STICS performs better, equally, or worse than a **reference version**, both against field observations and against the reference simulation outputs.

The workflow:

1. Runs STICS simulations for the version under evaluation via [`SticsOnR`](https://github.com/SticsRPacks/SticsOnR)
2. Compares the resulting outputs against **field observations** and against **reference simulation statistics** from a previous model version
3. Computes statistical evaluation criteria (RMSE, nRMSE, bias, R², etc.) using [`CroPlotR`](https://github.com/SticsRPacks/CroPlotR)
4. Flags variables and USMs where performance has **deteriorated** relative to the reference, using a configurable threshold
5. Stores all results in [Apache Arrow / Parquet](https://arrow.apache.org/) format and exports them as CSV files and diagnostic plots

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

`SticsREval` evaluates a **candidate version** of STICS by running it and comparing its outputs against two sources:

- **Field observations** — to assess absolute model performance
- **Reference version statistics** — to detect regressions relative to a known-good version

```
  Configuration$new(...)               ← defines all parameters (paths, options, filters)
           │
           ▼
  Evaluation$new(config)$run()         ← runs simulations + compares vs obs & reference → saves .parquet files
           │
           ├──► export_stats_to_csv(config)     ← export statistics + deteriorated USMs to CSV
           └──► gen_plots(config)               ← comparison plots + scatter plots of regressions
```

All functions read from and write to an **`EvalWorkspace`**, which manages the Parquet datasets on disk.

---

## Main Classes and Functions

### `Configuration`

Encapsulates and validates all configuration parameters for the package. The same object is passed to all workflow classes and functions (`Evaluation`, `export_stats_to_csv`, `gen_plots`). Fields are validated against a declarative schema at construction time — all errors are collected and reported together.

```r
library(SticsREval)

config <- Configuration$new(
  stics_exe          = "/path/to/stics",
  usms_workspace     = "workspace/",
  metadata_file      = "metadata.csv",
  eval_workspace     = "eval_workspace/",   # required
  output_dir         = "outputs/",
  run_simulations    = TRUE,                # set to FALSE to skip simulations
  init_workspace     = TRUE,
  verbose            = 1L,
  parallel           = FALSE,
  cores              = NA,
  reference_version  = NULL,               # version string in eval_workspace to use as reference
  percentage         = 5,                  # threshold (%) to flag deteriorated variables
  species            = NULL,
  usms               = NULL,
  var2exclude        = NULL
)
```

| Field | Description |
|---|---|
| `stics_exe` | Path to the STICS executable (required when `init_workspace = TRUE` and `run_simulations = TRUE`) |
| `usms_workspace` | Path to the USMs input data directory (required when `init_workspace = TRUE`) |
| `metadata_file` | Path to the metadata CSV file describing simulations (required when `init_workspace = TRUE`) |
| `eval_workspace` | Path to the evaluation workspace — **required** |
| `output_dir` | Output directory for CSV exports and plots (required for export and plots workflows) |
| `run_simulations` | Whether to run STICS simulations (default: `FALSE`) |
| `init_workspace` | Whether to initialize the evaluation workspace before running (default: `FALSE`) |
| `verbose` | Logging verbosity level: `0` = silent, `1` = info, `2` = debug (default: `1`) |
| `parallel` | Enable parallel execution (default: `FALSE`) |
| `cores` | Number of cores for parallel execution (`NA` = auto; required when `parallel = TRUE`) |
| `reference_version` | Version string present in `eval_workspace` to use as the reference for regression detection |
| `percentage` | Threshold (%) above which a variable is flagged as deteriorated vs. the reference (default: `5`) |
| `species` | Optional character vector of species to evaluate. `NULL` = all available. |
| `usms` | Optional character vector of USMs to evaluate. `NULL` = all available. |
| `var2exclude` | Optional character vector of variables to exclude from evaluation. |

`Configuration` also exposes workflow-specific validation methods called internally by each function:

- `config$validate_eval()` — checks requirements for the evaluation workflow
- `config$validate_export()` — checks that `output_dir` is set and writable
- `config$validate_plots()` — checks requirements for the plots workflow

---

### `Evaluation`

The **core class** of the package. It orchestrates the full evaluation workflow:

- Optionally initializes the `EvalWorkspace` (loading simulations and observations from the USMs workspace)
- Computes statistics per species (RMSE, nRMSE, bias, R², etc.) against **field observations**
- When `reference_version` is set, compares statistics against the **reference version** to detect regressions
- Flags variables and USMs where performance has deteriorated beyond the `percentage` threshold
- Saves all results as **Arrow/Parquet files** in the `eval_workspace` directory
- Displays a summary of comparison results

Errors during evaluation are caught and logged without stopping the full process.

```r
Evaluation$new(config)$run()
```

You can also inject custom dependencies for testing or advanced use:

```r
Evaluation$new(
  config,
  workspace = EvalWorkspace$new("eval_workspace/"),  # default
  backend   = ParallelBackend$new(FALSE, NA),         # default
  logger    = default_logger                          # default
)$run()
```

After running, the `eval_workspace` directory will contain Parquet files with simulated data, observed data, and evaluation statistics per species.

---

### `export_stats_to_csv()`

Exports the evaluation statistics per species to CSV files. For each species, a subdirectory is created under `output_dir` and three files are written when the corresponding data are available:

| File | Content |
|---|---|
| `Criteres_stats.csv` | Global statistical criteria (RMSE, nRMSE, bias, R², etc.) |
| `RMSE_per_usm.csv` | RMSE broken down per USM (simulation unit) |
| `Deteriorated_USM.csv` | List of USMs with deteriorated performance vs. the reference version |

```r
export_stats_to_csv(config)
```

---

### `gen_plots()`

Generates diagnostic plots for each species. For each species, a subdirectory is created under `output_dir` and the following are produced:

- **Comparison plots** — observed vs. simulated for all variables
- **Scatter plots** (`scatter_plots.html`) — comparing the candidate version against the reference version, highlighting deteriorated variables (only generated when `reference_version` is set and regressions are detected)

```r
gen_plots(config)
```

For advanced use, injectable parameters allow substituting the workspace, parallel backend, or plot generation functions:

```r
gen_plots(
  config,
  workspace     = EvalWorkspace$new(config$eval_workspace),  # default
  backend       = ParallelBackend$new(config$parallel, config$cores),  # default
  scatter_fn    = gen_scatter_plot,  # default
  comparison_fn = function(x, dir) x$plot_comparison(dir)   # default
)
```

---

### `EvalWorkspace`

Manages reading and writing all evaluation data (simulations, observations, statistics, comparisons) stored as Parquet datasets on disk. It is used internally by `Evaluation`, `export_stats_to_csv`, and `gen_plots`, but can also be used directly to inspect results.

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

Key methods:

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
  init_workspace    = TRUE,
  reference_version = "v10.0",   # version already in eval_workspace to compare against
  percentage        = 5          # flag variables with >5% deterioration
)

# 2. Run simulations + evaluate vs observations and reference version
Evaluation$new(config)$run()

# 3. Export statistics and deteriorated USMs to CSV
export_stats_to_csv(config)

# 4. Generate comparison and regression scatter plots
gen_plots(config)
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
  stics_exe      = "/path/to/stics",
  usms_workspace = "/workspace/",
  metadata_file  = "/workspace/metadata.csv",
  eval_workspace = "/workspace/eval_workspace/",
  output_dir     = "/workspace/outputs/",
  init_workspace = TRUE,
  run_simulations = TRUE
)

Evaluation$new(config)$run()
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