# USMEvaluation class

Internal class. Detects USMs (situations) where the model performance on
a given variable is very degraded compared to a reference version,
relative to the overall performance of the species on that variable.
Instantiated internally by
[`evaluate`](https://sticsrpacks.github.io/SticsREval/reference/evaluate.md).

## Details

For each variable and each USM, the following ratio is computed:

\$\$RMSE\\ratio = 100 \times \frac{RMSE\_{eval} -
RMSE\_{ref}}{RMSE\_{species}}\$\$

where:

- `RMSE_eval`: RMSE of the evaluated version, for the variable, on that
  USM.

- `RMSE_ref`: RMSE of the reference version, for the variable, on that
  USM.

- `RMSE_species`: RMSE of the evaluated version, for the variable,
  computed on the whole species (all USMs together).

A USM is considered as **not passing the test** if, for at least one
variable:

- `RMSE_ratio > ratio_threshold` (default 50%), OR

- more than `max_degraded_vars` (default 3) variables have
  `RMSE_ratio > degraded_threshold` (default 20%) on that USM.

There is no minimum number of observations per USM. However, a variable
that is not tested at the species level (fewer than 10 observations over
the whole species, see `RRmseComparison`) is not tested on the USMs of
that species either.

The class can be used in two ways:

- Driven by `eval_workspace`/`workspace`, like `SpeciesEvaluation`:
  create with `eval_workspace` (or a pre-built `workspace`), then call
  `run()`.

- Manually, by directly providing `stats_usm` and `stats_species` (or a
  pre-computed `data`) for a single species, useful for unit testing.

## Active bindings

- `degraded_vars`:

  Data frame of variable/USM combinations (across all species) where
  `rmse_ratio` exceeds `degraded_threshold` (default 20%).

- `critical_vars`:

  Data frame of variable/USM combinations (across all species) where
  `rmse_ratio` exceeds `ratio_threshold` (default 50%).

- `failed_usms`:

  Data frame with columns `species`/`situation` for USMs that do not
  pass the test: either at least one variable with a ratio above
  `ratio_threshold`, or more than `max_degraded_vars` variables above
  `degraded_threshold`.

- `success`:

  TRUE if the USM evaluation is successful, i.e. no USM failed the test
  for any species.

- `is_empty`:

  TRUE if there is no data to evaluate.

- `skip_reason`:

  Why no USM was evaluated (e.g. `"no reference data"`), or NULL if at
  least one species was evaluated.

## Methods

### Public methods

- [`USMEvaluation$new()`](#method-USMEvaluation-initialize)

- [`USMEvaluation$run()`](#method-USMEvaluation-run)

- [`USMEvaluation$get_data()`](#method-USMEvaluation-get_data)

- [`USMEvaluation$export()`](#method-USMEvaluation-export)

- [`USMEvaluation$summary()`](#method-USMEvaluation-summary)

- [`USMEvaluation$clone()`](#method-USMEvaluation-clone)

------------------------------------------------------------------------

### `USMEvaluation$new()`

Create a new USMEvaluation object.

#### Usage

    USMEvaluation$new(
      eval_workspace = NULL,
      species_filter = NULL,
      usms = NULL,
      var2exclude = NULL,
      percentage = 5,
      output_dir = NULL,
      parallel = FALSE,
      cores = NA,
      workspace = NULL,
      backend = NULL,
      logger = default_logger,
      species_evaluation = NULL,
      ratio_threshold = 50,
      degraded_threshold = 20,
      max_degraded_vars = 3,
      species = NULL,
      stats_usm = NULL,
      stats_species = NULL,
      data = NULL
    )

#### Arguments

- `eval_workspace`:

  Path to the evaluation workspace. Only used to build a default
  `workspace` when one isn't supplied. Providing this or `workspace`
  selects the evaluation mode; otherwise the object is built in manual
  mode from `data`/`stats_usm`+`stats_species`.

- `species_filter`:

  Optional character vector of species to evaluate.

- `usms`:

  Optional character vector of USMs to evaluate.

- `var2exclude`:

  Optional character vector of variables to exclude.

- `percentage`:

  Threshold (%) above which a variable is flagged as deteriorated vs.
  the reference.

- `output_dir`:

  Output directory for the CSV export.

- `parallel, cores`:

  Parallel execution options. Only used to build a default `backend`
  when one isn't supplied.

- `workspace`:

  An optional EvalWorkspace object. If not provided, a new EvalWorkspace
  will be created from `eval_workspace`.

- `backend`:

  An optional ParallelBackend object for parallel processing. If not
  provided, a new ParallelBackend will be created from
  `parallel`/`cores`.

- `logger`:

  An optional logger object for logging messages. If not provided, the
  default logger will be used.

- `species_evaluation`:

  Optional `SpeciesEvaluation` instance. If provided and already
  `run()`, its per-species statistics are reused instead of being
  recomputed from scratch (avoids re-reading the sim/obs/ref data and
  re-running CroPlotR's
  [`summary()`](https://rdrr.io/r/base/summary.html) for each species).
  Falls back to computing statistics independently for any species not
  found in `species_evaluation` (e.g. if it hasn't been run yet).

- `ratio_threshold`:

  Numeric threshold (%) above which a single variable makes the USM
  fail. Default 50.

- `degraded_threshold`:

  Numeric threshold (%) above which a variable is considered degraded.
  Default 20.

- `max_degraded_vars`:

  Maximum number of degraded variables (ratio \> `degraded_threshold`)
  tolerated per USM before it is considered failed. Default 3 (i.e.
  fails starting at 4 degraded variables).

- `species`:

  Optional species name, used in manual mode (ignored in evaluation
  mode).

- `stats_usm`:

  Manual mode: data frame of per-USM stats (RMSE, n_obs), for
  `evaluated` and `reference` groups, e.g. as returned by CroPlotR's
  summary method with
  `all_situations = FALSE, stats = c("RMSE", "n_obs")`.

- `stats_species`:

  Manual mode: data frame of species-level stats (RMSE, n_obs), e.g. as
  returned by CroPlotR's summary method with `all_situations = TRUE`
  (default).

- `data`:

  Manual mode: optional pre-computed data frame (bypasses
  `stats_usm`/`stats_species` computation).

------------------------------------------------------------------------

### `USMEvaluation$run()`

Run the USM evaluation. This method computes, for each species found in
the workspace (filtered according to `species_filter`/`usms`), the
RMSE_ratio per variable and per USM, and stores the results internally.

#### Usage

    USMEvaluation$run()

------------------------------------------------------------------------

### `USMEvaluation$get_data()`

Return the underlying data frame (one row per variable/USM/species
combination, with columns: species, situation, variable, rmse_eval,
rmse_ref, rmse_species, ratio, n_obs).

#### Usage

    USMEvaluation$get_data()

------------------------------------------------------------------------

### `USMEvaluation$export()`

Export the USM evaluation data for deteriorated USMs, and a per-species
USM pass/fail count summary, to CSV files.

The exported files are written to the `csv` subdirectory of the
configured output directory: `usm_thresholds.csv` (the
`ratio_threshold`/`degraded_threshold`/ `max_degraded_vars` this
evaluation was configured with, so downstream reports can explain what
"failed" means for this run), `Deteriorated_USM.csv` (only if at least
one USM has deteriorated data), `usm_summary.csv` (species, total USMs,
failed USMs, passed USMs), and `failed_usms.csv` (species, situation -
the same USMs counted as "failed" in `usm_summary.csv` and reported by
[`summary()`](https://rdrr.io/r/base/summary.html), i.e.
`self$failed_usms`; only written if at least one USM failed).

#### Usage

    USMEvaluation$export()

------------------------------------------------------------------------

### `USMEvaluation$summary()`

Print a summary of the USM evaluation results, species by species.

#### Usage

    USMEvaluation$summary()

------------------------------------------------------------------------

### `USMEvaluation$clone()`

The objects of this class are cloneable with this method.

#### Usage

    USMEvaluation$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
