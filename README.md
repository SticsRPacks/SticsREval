# SticsREval <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![GitHub release](https://img.shields.io/github/v/release/SticsRPacks/SticsREval)](https://github.com/SticsRPacks/SticsREval/releases/latest)
[![R-CMD-check](https://github.com/SticsRPacks/SticsREval/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/SticsRPacks/SticsREval/actions/workflows/R-CMD-check.yml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: LGPL-3.0](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
[![Docker Image](https://img.shields.io/badge/docker-ghcr.io-blue?logo=docker)](https://github.com/SticsRPacks/SticsREval/pkgs/container/stics-r-eval)
<!-- badges: end -->

`SticsREval` evaluates a candidate version of the STICS crop model against
field observations and a reference version:

- **statistical evaluation** (`evaluate()`): global, per-species and per-USM
  comparison of the candidate version with the reference;
- **balance closure test** (`balance_closure_test()`): consistency of the water
  and nitrogen balances;
- **HTML dashboard** (`render_report()`) to browse the results.

See the [Get started](https://sticsrpacks.github.io/SticsREval/articles/SticsREval.html)
vignette for the full workflow and the
[reference](https://sticsrpacks.github.io/SticsREval/reference/) for each
function's arguments.

## Installation

```r
# install.packages("pak")
pak::pak("SticsRPacks/SticsREval")
```

To restore the exact dependency versions recorded in `renv.lock` after cloning
the repository, run `renv::restore()`.

`render_report()` also requires the [Quarto CLI](https://quarto.org), which is
not an R package and must be installed separately (it is included in the
[Docker image](#docker)).

## Example

```r
library(SticsREval)

# Build a STICS workspace from the SMS repository
gen_workspace_from_sms(
  sms_path   = "/path/to/sms_repository",
  stics_path = "/path/to/stics_distribution",
  output_dir = "workspace/"
)

# Run the reference and candidate versions
run_simulations(
  stics_exe      = "/path/to/stics_reference",
  usms_workspace = "workspace/",
  metadata_file  = "metadata.csv",
  output_dir     = "reference/"
)
run_simulations(
  stics_exe      = "/path/to/stics_candidate",
  usms_workspace = "workspace/",
  metadata_file  = "metadata.csv",
  output_dir     = "outputs/"
)

# Evaluate the candidate version and render the dashboard
evaluate(
  usms_workspace = "workspace/",
  sim_rds        = "outputs/simulations.rds",
  obs_rds        = "outputs/observations.rds",
  ref_sim_rds    = "reference/simulations.rds",
  output_dir     = "outputs/",
  report         = TRUE
)
```

## Docker

A pre-built image, including the Quarto CLI, is available on the GitHub
Container Registry:

```bash
docker pull ghcr.io/sticsrpacks/stics-r-eval:v0.1.1

# Interactive R session
docker run --rm -it -v /path/to/your/workspace:/workspace \
  ghcr.io/sticsrpacks/stics-r-eval:v0.1.1 R

# Non-interactive script
docker run --rm -v /path/to/your/workspace:/workspace \
  -v /path/to/your/script.R:/script.R \
  ghcr.io/sticsrpacks/stics-r-eval:v0.1.1 Rscript /script.R
```

To build it locally (a GitHub token is needed to install private SticsRPacks
dependencies):

```bash
docker build --secret id=GITHUB_TOKEN,src=<(echo $GITHUB_PAT) -t stics-r-eval .
```

## Related packages

- [SticsRFiles](https://github.com/SticsRPacks/SticsRFiles): read/write STICS
  input and output files
- [SticsOnR](https://github.com/SticsRPacks/SticsOnR): run STICS simulations
  from R
- [CroPlotR](https://github.com/SticsRPacks/CroPlotR): crop model evaluation
  statistics and plots

## Citation and contributing

To cite `SticsREval`, run `citation("SticsREval")`. Contributions are welcome
through [issues](https://github.com/SticsRPacks/SticsREval/issues) and pull
requests. Please note that this project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md); by contributing, you agree
to abide by its terms.
