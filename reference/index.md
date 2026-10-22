# Package index

## Prepare data

Build a STICS workspace from SMS data and run the simulations of a STICS
version on it.

- [`gen_workspace_from_sms()`](https://sticsrpacks.github.io/SticsREval/reference/gen_workspace_from_sms.md)
  : Generate a Stics workspace from SMS data, for evaluation and
  calibration.
- [`read_usms_files()`](https://sticsrpacks.github.io/SticsREval/reference/read_usms_files.md)
  : Read one or more USM list files.
- [`get_var_from_obs()`](https://sticsrpacks.github.io/SticsREval/reference/get_var_from_obs.md)
  : Derive variable names from observation data
- [`run_simulations()`](https://sticsrpacks.github.io/SticsREval/reference/run_simulations.md)
  : Run Stics simulations for a text workspace.

## Evaluate simulations

Compare a STICS version against observations and a reference version,
and check balance closure.

- [`evaluate()`](https://sticsrpacks.github.io/SticsREval/reference/evaluate.md)
  : Run All Evaluations
- [`balance_closure_test()`](https://sticsrpacks.github.io/SticsREval/reference/balance_closure_test.md)
  : Run the Balance Closure Test

## Reports and plots

Visualize the evaluation results.

- [`plot_rrmse_comparison()`](https://sticsrpacks.github.io/SticsREval/reference/plot_rrmse_comparison.md)
  : Plot the rRMSE comparison
- [`overview_heatmap()`](https://sticsrpacks.github.io/SticsREval/reference/overview_heatmap.md)
  : Overview heatmap of the species-level status of every variable
- [`usm_ratio_heatmap()`](https://sticsrpacks.github.io/SticsREval/reference/usm_ratio_heatmap.md)
  : Heatmap of the per-USM RMSE ratio
- [`render_report()`](https://sticsrpacks.github.io/SticsREval/reference/render_report.md)
  : Render an HTML evaluation site
