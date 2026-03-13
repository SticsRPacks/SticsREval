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
  STICS candidate version          Reference version stats
  (stics_exe + workspace)          (reference_data_dir)
           │                                │
           ▼                                │
     make_config()   ◄──────────────────────┘
           │
           ▼
      evaluate()     ← runs simulations + compares vs obs & reference → saves .parquet files
           │
           ├──► export_species_sim()    ← export simulations per species (.parquet)
           ├──► export_stats_to_csv()   ← export statistics + deteriorated USMs to CSV
           └──► gen_plots()             ← comparison plots + scatter plots of regressions
```

---

## Main Functions

### `make_config()`

Creates and validates the configuration object that is passed to all other functions. All parameters for simulation, evaluation, and export are defined here.

```r
library(SticsREval)

config <- make_config(
  stics_exe          = "/path/to/stics",
  workspace          = "workspace/",
  metadata_file      = "metadata.csv",
  run_simulations    = TRUE,      # set to FALSE to skip simulations
  verbose            = 1,
  parallel           = FALSE,
  cores              = NA,
  reference_data_dir = NULL,      # path to reference version stats for regression detection
  percentage         = 5,         # threshold (%) to flag deteriorated variables
  eval_workspace     = "eval_workspace/",
  init_workspace     = TRUE,
  output_dir         = "outputs/",
  force              = TRUE,
  species            = NULL
  usms               = NULL
)
```

| Argument | Description |
|---|---|
| `stics_exe` | Path to the STICS executable |
| `workspace` | Path to the simulation input directory |
| `metadata_file` | Path to the metadata CSV file describing simulations |
| `run_simulations` | Whether to run STICS simulations (default: `TRUE`) |
| `verbose` | Logging verbosity level (default: `1`) |
| `parallel` | Enable parallel execution (default: `FALSE`) |
| `cores` | Number of cores for parallel execution (`NA` = auto) |
| `reference_data_dir` | Path to the **reference version** simulation statistics, used to detect regressions |
| `percentage` | Threshold (%) above which a variable is flagged as deteriorated vs. the reference (default: `5`) |
| `eval_workspace` | Path to the evaluation workspace |
| `init_workspace` | Whether to initialize the workspace (default: `TRUE`) |
| `output_dir` | Output directory for exports |
| `force` | Whether to overwrite an existing non-empty evaluation workspace without prompting. (default: `FALSE`) |
| `species` | Optional list of species to evaluate. If NULL, all available species are evaluated. (default: `NULL`)  |
| `usms` | Optional list of USMs to evaluate. If NULL, all available USMs are evaluated. (default: `NULL`) |

---

### `evaluate()`

The **core function** of the package. It orchestrates the full evaluation workflow:

- Runs STICS simulations for the candidate version (via `SticsOnR`)
- Compares simulation outputs against **field observations** (absolute performance)
- Compares simulation statistics against **reference version statistics** to detect regressions (when `reference_data_dir` is provided)
- Flags variables and USMs where performance has deteriorated beyond the `percentage` threshold
- Saves all results as **Arrow/Parquet files** in the `eval_workspace` directory
- Displays a summary of comparison results

Errors during evaluation are caught and logged gracefully without stopping the full process.

```r
evaluate(config)
```

After running, the `eval_workspace` directory will contain Parquet files with simulated data, observed data, and evaluation statistics per species.

---

### `export_species_sim()`

Exports the simulated data **per species** from the evaluation workspace to individual Parquet files (one `Simulations.parquet` per species directory), enabling flexible downstream analysis.

```r
export_species_sim(config)
```

---

### `export_stats_to_csv()`

Exports the evaluation statistics per species to CSV files. For each species, three files are produced when available:

| File | Content |
|---|---|
| `Criteres_stats.csv` | Global statistical criteria (RMSE, nRMSE, bias, R², etc.) |
| `RMSE_per_usm.csv` | RMSE broken down per USM (simulation unit) |
| `Deteriorated_USM.csv` | List of USMs with deteriorated performance |

```r
export_stats_to_csv(config)
```

---

### `gen_plots()`

Generates diagnostic plots for each species:

- **Comparison plots** — observed vs. simulated for all variables, for the candidate version
- **Scatter plots** — comparing the candidate version against the reference version statistics, highlighting variables that have deteriorated beyond the `percentage` threshold (only generated when `reference_data_dir` is set in `make_config()` and regressions are detected)

```r
gen_plots(config)
```

---

## Complete Example

```r
library(SticsREval)

# 1. Configure the evaluation
#    - stics_exe points to the candidate version of STICS to evaluate
#    - reference_data_dir points to the statistics of the reference version
config <- make_config(
  stics_exe          = "/path/to/stics_candidate",
  workspace          = "workspace/",
  metadata_file      = "metadata.csv",
  run_simulations    = TRUE,
  reference_data_dir = "reference_version_stats/",  # stats from the reference version
  percentage         = 5,                            # flag variables with >5% deterioration
  eval_workspace     = "eval_workspace/",
  output_dir         = "outputs/"
)

# 2. Run simulations + evaluate vs observations and reference version stats
evaluate(config)

# 3. Export simulations per species to Parquet
export_species_sim(config)

# 4. Export statistics and deteriorated USMs to CSV
export_stats_to_csv(config)

# 5. Generate comparison and regression scatter plots
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

config <- make_config(
  stics_exe     = "/path/to/stics",
  workspace     = "/workspace/",
  metadata_file = "/workspace/metadata.csv",
  output_dir    = "/workspace/outputs/"
)

evaluate(config)
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

---
