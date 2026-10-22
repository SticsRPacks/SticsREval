# GlobalEvaluation class

Internal class. Evaluates the global performance of the STICS model
across multiple USMs (User Simulation Models): computes global
statistics and performs a comparison of the relative Root Mean Square
Error (rRMSE) for the specified variables. Instantiated internally by
[`evaluate`](https://sticsrpacks.github.io/SticsREval/reference/evaluate.md).

## Details

The evaluation process involves the following steps:

1.  Generating global statistics by comparing simulated and observed
    data.

2.  Performing a comparison of rRMSE values for the specified variables.

3.  Logging the results and providing a summary of the evaluation.

## Active bindings

- `success`:

  A logical value indicating whether the global evaluation was
  successful. The evaluation is considered successful if the rRMSE
  comparison was performed and there are no critical variables
  identified in the comparison.

- `skip_reason`:

  Why the rRMSE comparison was not done (e.g. `"no reference data"`), or
  NULL if it was done.

## Methods

### Public methods

- [`GlobalEvaluation$new()`](#method-GlobalEvaluation-initialize)

- [`GlobalEvaluation$run()`](#method-GlobalEvaluation-run)

- [`GlobalEvaluation$summary()`](#method-GlobalEvaluation-summary)

- [`GlobalEvaluation$export()`](#method-GlobalEvaluation-export)

- [`GlobalEvaluation$clone()`](#method-GlobalEvaluation-clone)

------------------------------------------------------------------------

### `GlobalEvaluation$new()`

Create a new GlobalEvaluation object.

#### Usage

    GlobalEvaluation$new(
      eval_workspace = NULL,
      usms = NULL,
      var2exclude = NULL,
      percentage = 5,
      output_dir = NULL,
      workspace = NULL,
      logger = default_logger
    )

#### Arguments

- `eval_workspace`:

  Path to the evaluation workspace. Only used to build a default
  `workspace` when one isn't supplied.

- `usms`:

  Optional character vector of USMs to evaluate.

- `var2exclude`:

  Optional character vector of variables to exclude.

- `percentage`:

  Threshold (%) above which a variable is flagged as deteriorated vs.
  the reference.

- `output_dir`:

  Output directory for the CSV export.

- `workspace`:

  An optional EvalWorkspace object. If not provided, a new EvalWorkspace
  will be created from `eval_workspace`.

- `logger`:

  An optional logger object for logging messages. If not provided, the
  default logger will be used.

------------------------------------------------------------------------

### `GlobalEvaluation$run()`

Run the global evaluation. This method performs the global evaluation by
generating global statistics and comparing the rRMSE values for the
specified variables.

#### Usage

    GlobalEvaluation$run()

------------------------------------------------------------------------

### `GlobalEvaluation$summary()`

Print a summary of the global evaluation results.

#### Usage

    GlobalEvaluation$summary()

------------------------------------------------------------------------

### `GlobalEvaluation$export()`

Export the global evaluation results to CSV files. This method exports
the global statistics (`global_stats.csv`) and, if a comparison against
a reference version was performed, the per-variable rRMSE comparison
(`global_rrmse_comparison.csv`, with each variable's status:
Critical/Warning/Improved/Not tested/Other) to the `csv` subdirectory.
The rRMSE comparison can be plotted with
[`plot_rrmse_comparison()`](https://sticsrpacks.github.io/SticsREval/reference/plot_rrmse_comparison.md).

#### Usage

    GlobalEvaluation$export()

------------------------------------------------------------------------

### `GlobalEvaluation$clone()`

The objects of this class are cloneable with this method.

#### Usage

    GlobalEvaluation$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
