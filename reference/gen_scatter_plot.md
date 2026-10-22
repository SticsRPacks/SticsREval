# Generate one independent scatter plot HTML file per variable

Unlike a single combined page, each variable in `vars` gets its own
standalone HTML file, named `<var>_scatter_plot.html` in `output_dir`,
so it can be viewed, shared or embedded independently of the others
(e.g. one per tab/card in a dashboard).

## Usage

``` r
gen_scatter_plot(output_dir, sim, obs, ref_sim, vars)
```
