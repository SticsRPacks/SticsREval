# Heatmap of the per-USM RMSE ratio

One row per USM, one column per variable, colored by the ratio used by
the USM test: (RMSE - reference RMSE) / RMSE of the species over all its
USMs, in %. USMs are sorted by the number of variables above the
degraded threshold, then by their highest ratio, and only the `max_usms`
most degraded ones are kept.

## Usage

``` r
usm_ratio_heatmap(
  usm_ratio,
  variables = unique(usm_ratio$variable),
  degraded_threshold = 20,
  ratio_threshold = 50,
  max_usms = 50
)
```

## Arguments

- usm_ratio:

  Data frame with one row per USM and variable, with columns
  `situation`, `variable`, `ratio`, `RMSE_eval`, `RMSE_ref`,
  `RMSE_species` and `n_obs`.

- variables:

  Variables to show, in column order.

- degraded_threshold:

  Ratio (%) above which a variable is degraded.

- ratio_threshold:

  Ratio (%) above which a variable is critical.

- max_usms:

  Maximum number of USMs shown.

## Value

A `plotly` htmlwidget.

## Details

The color scale is capped at +/- `ratio_threshold` so that a single huge
ratio doesn't wash out every other cell (the exact value is in the hover
text). It follows the test's thresholds: green = improved, white =
unchanged, orange = degraded, red = critical.

## Examples

``` r
usm_ratio <- data.frame(
  situation = rep(c("usm_1", "usm_2", "usm_3"), each = 2),
  variable = rep(c("lai_n", "masec_n"), 3),
  ratio = c(65.2, 24.8, -12.5, 3.1, 21.0, -30.4),
  RMSE_eval = c(1.10, 2.35, 0.48, 1.62, 0.95, 1.05),
  RMSE_ref = c(0.55, 1.90, 0.62, 1.57, 0.70, 1.60),
  RMSE_species = c(0.84, 1.81, 1.12, 1.61, 1.19, 1.81),
  n_obs = c(6L, 4L, 5L, 4L, 7L, 3L)
)
usm_ratio_heatmap(usm_ratio)

{"x":{"visdat":{"1c25536317a5":["function () ","plotlyVisDat"]},"cur_data":"1c25536317a5","attrs":{"1c25536317a5":{"x":["lai_n","masec_n"],"y":["usm_1","usm_3","usm_2"],"z":[[50,24.800000000000001],[21,-30.399999999999999],[-12.5,3.1000000000000001]],"text":[["USM: usm_1<br>Variable: lai_n<br>Ratio: +65.2%<br>RMSE: 1.1 (ref 0.55, species 0.84)<br>n_obs: 6","USM: usm_1<br>Variable: masec_n<br>Ratio: +24.8%<br>RMSE: 2.35 (ref 1.9, species 1.81)<br>n_obs: 4"],["USM: usm_3<br>Variable: lai_n<br>Ratio: +21.0%<br>RMSE: 0.95 (ref 0.7, species 1.19)<br>n_obs: 7","USM: usm_3<br>Variable: masec_n<br>Ratio: -30.4%<br>RMSE: 1.05 (ref 1.6, species 1.81)<br>n_obs: 3"],["USM: usm_2<br>Variable: lai_n<br>Ratio: -12.5%<br>RMSE: 0.48 (ref 0.62, species 1.12)<br>n_obs: 5","USM: usm_2<br>Variable: masec_n<br>Ratio: +3.1%<br>RMSE: 1.62 (ref 1.57, species 1.61)<br>n_obs: 4"]],"hoverinfo":"text","xgap":1,"ygap":1,"zmin":-50,"zmax":50,"colorscale":[["0","#1e8449"],["0.5","#ffffff"],["0.7","#f5b041"],["1","#c0392b"]],"colorbar":{"title":"RMSE ratio (%)"},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"heatmap"}},"layout":{"height":198,"margin":{"b":40,"l":60,"t":25,"r":10},"xaxis":{"domain":[0,1],"automargin":true,"side":"top","tickangle":-45,"title":[]},"yaxis":{"domain":[0,1],"automargin":true,"autorange":"reversed","type":"category","title":[]},"plot_bgcolor":"#f2f3f4","scene":{"zaxis":{"title":[]}},"hovermode":"closest","showlegend":false,"legend":{"yanchor":"top","y":0.5}},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"displaylogo":false},"data":[{"colorbar":{"title":"RMSE ratio (%)","ticklen":2,"len":0.5,"lenmode":"fraction","y":1,"yanchor":"top"},"colorscale":[["0","#1e8449"],["0.5","#ffffff"],["0.7","#f5b041"],["1","#c0392b"]],"showscale":true,"x":["lai_n","masec_n"],"y":["usm_1","usm_3","usm_2"],"z":[[50,24.800000000000001],[21,-30.399999999999999],[-12.5,3.1000000000000001]],"text":[["USM: usm_1<br>Variable: lai_n<br>Ratio: +65.2%<br>RMSE: 1.1 (ref 0.55, species 0.84)<br>n_obs: 6","USM: usm_1<br>Variable: masec_n<br>Ratio: +24.8%<br>RMSE: 2.35 (ref 1.9, species 1.81)<br>n_obs: 4"],["USM: usm_3<br>Variable: lai_n<br>Ratio: +21.0%<br>RMSE: 0.95 (ref 0.7, species 1.19)<br>n_obs: 7","USM: usm_3<br>Variable: masec_n<br>Ratio: -30.4%<br>RMSE: 1.05 (ref 1.6, species 1.81)<br>n_obs: 3"],["USM: usm_2<br>Variable: lai_n<br>Ratio: -12.5%<br>RMSE: 0.48 (ref 0.62, species 1.12)<br>n_obs: 5","USM: usm_2<br>Variable: masec_n<br>Ratio: +3.1%<br>RMSE: 1.62 (ref 1.57, species 1.61)<br>n_obs: 4"]],"hoverinfo":"text","xgap":1,"ygap":1,"zmin":-50,"zmax":50,"type":"heatmap","xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
