# SpeciesEvaluation class

Internal class. Evaluates the performance of a model across different
species: computes statistics, generates comparisons, and produces
reports for each species. Instantiated internally by
[`evaluate`](https://sticsrpacks.github.io/SticsREval/reference/evaluate.md).

## Details

The class uses a backend for parallel processing and a workspace to
access simulation and observation data. It provides methods to run
evaluations, summarize results, and export findings.

## Active bindings

- `success`:

  A logical value indicating whether the species evaluation was
  successful. The evaluation is considered successful if all species
  comparisons were performed and there are no critical variables
  identified in any of the comparisons.

- `skip_reason`:

  Why no species rRMSE comparison was done (e.g. `"no reference data"`),
  or NULL if at least one was done.

## Methods

### Public methods

- [`SpeciesEvaluation$new()`](#method-SpeciesEvaluation-initialize)

- [`SpeciesEvaluation$get_species_stats()`](#method-SpeciesEvaluation-get_species_stats)

- [`SpeciesEvaluation$run()`](#method-SpeciesEvaluation-run)

- [`SpeciesEvaluation$summary()`](#method-SpeciesEvaluation-summary)

- [`SpeciesEvaluation$export()`](#method-SpeciesEvaluation-export)

- [`SpeciesEvaluation$clone()`](#method-SpeciesEvaluation-clone)

------------------------------------------------------------------------

### `SpeciesEvaluation$new()`

Create a new SpeciesEvaluation object.

#### Usage

    SpeciesEvaluation$new(
      eval_workspace = NULL,
      species = NULL,
      usms = NULL,
      var2exclude = NULL,
      percentage = 5,
      output_dir = NULL,
      parallel = FALSE,
      cores = NA,
      workspace = NULL,
      backend = NULL,
      logger = default_logger
    )

#### Arguments

- `eval_workspace`:

  Path to the evaluation workspace. Only used to build a default
  `workspace` when one isn't supplied.

- `species`:

  Optional character vector of species to evaluate.

- `usms`:

  Optional character vector of USMs to evaluate.

- `var2exclude`:

  Optional character vector of variables to exclude.

- `percentage`:

  Threshold (%) above which a variable is flagged as deteriorated vs.
  the reference.

- `output_dir`:

  Output directory for CSV/plot exports.

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

------------------------------------------------------------------------

### `SpeciesEvaluation$get_species_stats()`

Return the CroPlotR statistics already computed by `run()` for a given
species, so that other evaluation classes (e.g. `USMEvaluation`) can
reuse them instead of recomputing the same summary statistics from
scratch.

#### Usage

    SpeciesEvaluation$get_species_stats(species)

#### Arguments

- `species`:

  The species to get statistics for.

#### Returns

A list with elements `stats` (species-level summary,
`all_situations = TRUE`) and `stats_usm` (per-USM summary with `RMSE`,
`rRMSE` and `n_obs`). Elements are `NULL` if `run()` has not been called
yet or produced no result for this species.

------------------------------------------------------------------------

### `SpeciesEvaluation$run()`

Run the species evaluation. This method performs the species evaluation
by generating statistics and comparisons for each species based on the
provided configuration and workspace.

#### Usage

    SpeciesEvaluation$run()

------------------------------------------------------------------------

### `SpeciesEvaluation$summary()`

Print a summary of the species evaluation results.

#### Usage

    SpeciesEvaluation$summary()

------------------------------------------------------------------------

### `SpeciesEvaluation$export()`

Export the species evaluation results to CSV files and RDS files. This
method exports the statistics, rRMSE comparisons (including the
species-level rRMSE ratio per variable, in
`species_rrmse_comparison .csv`), and a per-species degradation status
summary (`species_summary .csv`) to CSV files in the "csv" subdirectory.
For each species with degraded variables, it also saves their
observations and their evaluated and reference simulations over the
whole simulated period, for the USMs with observations of these
variables (a list of `sim`, `obs`, `ref_sim` and the degraded `vars`),
to `<species>_degraded_vars.rds` in the "data" subdirectory. The rRMSE
comparison can be plotted with
[`plot_rrmse_comparison()`](https://sticsrpacks.github.io/SticsREval/reference/plot_rrmse_comparison.md).

#### Usage

    SpeciesEvaluation$export()

------------------------------------------------------------------------

### `SpeciesEvaluation$clone()`

The objects of this class are cloneable with this method.

#### Usage

    SpeciesEvaluation$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
