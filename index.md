# SticsREval ![](reference/figures/logo.png)

`SticsREval` evaluates a candidate version of the STICS crop model
against field observations and a reference version:

- **statistical evaluation**
  ([`evaluate()`](https://sticsrpacks.github.io/SticsREval/reference/evaluate.md)):
  global, per-species and per-USM comparison of the candidate version
  with the reference;
- **balance closure test**
  ([`balance_closure_test()`](https://sticsrpacks.github.io/SticsREval/reference/balance_closure_test.md)):
  consistency of the water and nitrogen balances;
- **HTML dashboard**
  ([`render_report()`](https://sticsrpacks.github.io/SticsREval/reference/render_report.md))
  to browse the results.

See the [Get
started](https://sticsrpacks.github.io/SticsREval/articles/SticsREval.html)
vignette for the full workflow and the
[reference](https://sticsrpacks.github.io/SticsREval/reference/) for
each function’s arguments.

## Installation

``` r

# install.packages("pak")
pak::pak("SticsRPacks/SticsREval")
```

To restore the exact dependency versions recorded in `renv.lock` after
cloning the repository, run
[`renv::restore()`](https://rstudio.github.io/renv/reference/restore.html).

[`render_report()`](https://sticsrpacks.github.io/SticsREval/reference/render_report.md)
also requires the [Quarto CLI](https://quarto.org), which is not an R
package and must be installed separately (it is included in the [Docker
image](#docker)).

## Example

``` r

library(SticsREval)

# Build a STICS workspace from the SMS repository
gen_workspace_from_sms(
  sms_path   = "/path/to/sms_repository",
  stics_path = "/path/to/stics_distribution",
  output_dir = "workspace/"
)

# ... or from the IDEStics API, for the given USMs. The rotations are written
# to workspace/metadata.csv, which can then be passed as `metadata_file` to
# run_simulations().
gen_workspace_from_idestics(
  idestics_url  = "https://idestics.example.org",
  stics_path    = "/path/to/stics_distribution",
  output_dir    = "workspace/",
  stics_version = "11.0.0",
  usms          = read_usms_files("usms.txt")
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

[`gen_workspace_from_idestics()`](https://sticsrpacks.github.io/SticsREval/reference/gen_workspace_from_idestics.md)
reads the IDEStics credentials from the `IDESTICS_USERNAME` and
`IDESTICS_PASSWORD` environment variables, for instance set in your
user-level `~/.Renviron`. Otherwise, they are prompted for in an
interactive session, the password being masked.

## Docker

A pre-built image, including the Quarto CLI, is available on the GitHub
Container Registry:

``` bash
docker pull ghcr.io/sticsrpacks/stics-r-eval:v0.2.0

# Interactive R session
docker run --rm -it -v /path/to/your/workspace:/workspace \
  ghcr.io/sticsrpacks/stics-r-eval:v0.2.0 R

# Non-interactive script
docker run --rm -v /path/to/your/workspace:/workspace \
  -v /path/to/your/script.R:/script.R \
  ghcr.io/sticsrpacks/stics-r-eval:v0.2.0 Rscript /script.R
```

A non-interactive script can’t prompt for the IDEStics credentials: pass
them from your environment with
`-e IDESTICS_USERNAME -e IDESTICS_PASSWORD`.

To build it locally (a GitHub token is needed to install private
SticsRPacks dependencies):

``` bash
docker build --secret id=GITHUB_TOKEN,src=<(echo $GITHUB_PAT) -t stics-r-eval .
```

## Related packages

- [SticsRFiles](https://github.com/SticsRPacks/SticsRFiles): read/write
  STICS input and output files
- [SticsOnR](https://github.com/SticsRPacks/SticsOnR): run STICS
  simulations from R
- [CroPlotR](https://github.com/SticsRPacks/CroPlotR): crop model
  evaluation statistics and plots

## Citation and contributing

To cite `SticsREval`, run `citation("SticsREval")`. Contributions are
welcome through
[issues](https://github.com/SticsRPacks/SticsREval/issues) and pull
requests.
