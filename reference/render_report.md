# Render an HTML evaluation site

Renders an HTML site summarizing the evaluation results written to
`output_dir` by
[`evaluate`](https://sticsrpacks.github.io/SticsREval/reference/evaluate.md)
(called with `output_dir` set): a homepage (`index.html`) showing the
evaluation summary (pass/fail status, and per-species status/USM
counts), a global page (`global.html`) showing the degraded variables,
rRMSE comparison plot and global stats (`global_stats.csv`) from
`output_dir/csv`, and one page per species
(`output_dir/species/<species>.html`) with that species' statistics,
degraded variables, deteriorated/failed USMs, rRMSE comparison plot and
the scatter plots of its degraded variables (from
`output_dir/data/<species>_degraded_vars.rds`). It's a single Quarto
website project (a generated `_quarto.yml`), so every page shares the
same navigation sidebar - but each species page is still rendered and
loaded separately, so a browser only has to load one species' plots at a
time, instead of every species' plots at once on a single page. No
statistic is recomputed: only the files
[`evaluate()`](https://sticsrpacks.github.io/SticsREval/reference/evaluate.md)
already wrote are read.

None of the pages are standalone files: Quarto writes its own JS/CSS
assets to a shared `site_libs/` subfolder, linked via relative paths, so
keep the rendered site intact when sharing or moving results.

Requires the Quarto CLI (<https://quarto.org>) to be installed, along
with the quarto and DT R packages.

## Usage

``` r
render_report(output_dir, open = interactive(), parallel = FALSE, cores = NA)
```

## Arguments

- output_dir:

  path to a directory previously populated by
  [`evaluate`](https://sticsrpacks.github.io/SticsREval/reference/evaluate.md)
  (i.e. `evaluate(..., output_dir = output_dir)` must have been run
  beforehand). The rendered site is written there.

- open:

  Boolean. Open the rendered dashboard in a browser once done. Defaults
  to `TRUE` in interactive sessions.

- parallel:

  Boolean. Render the per-species pages in parallel ? Default `FALSE`.
  The homepage and global page are always rendered first and
  sequentially.

- cores:

  Number of cores to use when `parallel = TRUE`.

## Value

invisibly, the path to the rendered `index.html` file
