# Run All Evaluations

Loads simulation, observation and reference data from `sim_rds`,
`obs_rds` and `ref_sim_rds` into a temporary evaluation workspace, runs
the global, per-species and per-USM evaluations, exports results to
`output_dir` (if defined), prints a summary, and stops with an error if
any evaluation failed (if `stop_on_failure`). An evaluation that
couldn't be done (e.g. no reference data) is reported as not evaluated,
not as failed. `evaluate()` never runs STICS simulations itself — use
[`run_simulations`](https://sticsrpacks.github.io/SticsREval/reference/run_simulations.md)
to produce `sim_rds` / `obs_rds` beforehand.

## Usage

``` r
evaluate(
  usms_workspace,
  sim_rds,
  obs_rds,
  ref_sim_rds = NULL,
  output_dir = NULL,
  report = FALSE,
  percentage = 5,
  species = NULL,
  usms = NULL,
  var2exclude = NULL,
  ratio_threshold = 50,
  degraded_threshold = 20,
  max_degraded_vars = 3,
  parallel = FALSE,
  cores = NA,
  verbose = 1L,
  stop_on_failure = FALSE
)
```

## Arguments

- usms_workspace:

  path to the Stics text workspace containing the USMs to evaluate. Used
  to determine the species associated with each USM

- sim_rds:

  path to an `.rds` file containing pre-computed simulation results for
  the evaluated version — see
  [`run_simulations`](https://sticsrpacks.github.io/SticsREval/reference/run_simulations.md)

- obs_rds:

  path to an `.rds` file containing pre-computed observation data — see
  [`run_simulations`](https://sticsrpacks.github.io/SticsREval/reference/run_simulations.md)

- ref_sim_rds:

  path to an `.rds` file containing pre-computed simulation results for
  the reference version. If NULL (default), evaluation runs against
  observations only, without regression comparison against a reference

- output_dir:

  directory where CSV exports and plots are written. If NULL (default),
  results are not exported

- report:

  Boolean. If `TRUE` and `output_dir` is set, renders an HTML dashboard
  (see
  [`render_report`](https://sticsrpacks.github.io/SticsREval/reference/render_report.md))
  summarizing the exported results once evaluation is done. Default
  `FALSE`. The per-species pages are rendered according to
  `parallel`/`cores`.

- percentage:

  threshold (%) above which a variable is flagged as deteriorated vs.
  the reference (default 5)

- species:

  optional character vector of species to evaluate. NULL (default)
  evaluates all available species

- usms:

  optional character vector of USMs to evaluate. NULL (default)
  evaluates all available USMs

- var2exclude:

  optional character vector of variables to exclude from evaluation

- ratio_threshold:

  numeric threshold (%) above which a single variable makes a USM fail
  the USM-level evaluation (default 50)

- degraded_threshold:

  numeric threshold (%) above which a variable is considered degraded
  for the USM-level evaluation (default 20)

- max_degraded_vars:

  maximum number of degraded variables (ratio \> `degraded_threshold`)
  tolerated per USM before it is considered failed (default 3, i.e.
  fails starting at 4 degraded variables)

- parallel:

  Boolean. Is the computation to be done in parallel ?

- cores:

  Number of cores to use for parallel computation

- verbose:

  Integer. Logging verbosity level: 0 = warnings and errors only, 1 =
  info, 2 = debug

- stop_on_failure:

  Boolean. If TRUE, stops with an error if at least one evaluation
  failed (evaluations that couldn't be done, e.g. for lack of reference
  data, don't count as failed). If FALSE (default), returns normally

## Value

Invisibly `NULL`. Called for its side effects: workspace creation,
evaluation runs, and console reporting.

## Details

The function proceeds in the following steps:

1.  Creates a temporary evaluation workspace, cleaned up on exit.

2.  Loads simulation/observation/reference data into it.

3.  Runs the global, per-species and per-USM evaluations.

4.  Exports results to `output_dir` if defined.

5.  Prints their respective summaries.

6.  Displays a CLI report listing each evaluation as **success** (green
    ✔), **failed** (red ✗) or **not evaluated** (yellow, with the
    reason, e.g. no reference data).

7.  Stops with an error if at least one evaluation failed (if
    `stop_on_failure`).
