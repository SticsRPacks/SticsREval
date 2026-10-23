# Overview heatmap of the species-level status of every variable

Variables in rows (there are many more of them than species), species in
columns. Species are sorted most degraded first, variables by the number
of species where they are critical, then warning.

## Usage

``` r
overview_heatmap(
  overview,
  status_levels = c("Not tested", "Improved", "Warning", "Critical"),
  href = NULL
)
```

## Arguments

- overview:

  Data frame of the per-species rRMSE comparison (as exported to
  `species_rrmse_comparison.csv`), with columns `species`, `variable`,
  `status`, `rrmse_ratio`, `rrmse_eval`, `rrmse_ref` and `n_obs`.

- status_levels:

  Statuses to show, in increasing order of severity.

- href:

  Optional function of `(species, variable, status)` returning the URL
  opened when clicking each row's cell (`NA` for none).

## Value

A `plotly` htmlwidget.

## Examples

``` r
overview <- data.frame(
  species = rep(c("wheat", "maize"), each = 3),
  variable = rep(c("lai_n", "masec_n", "mafruit"), 2),
  status = c("Critical", "Improved", "Warning", "Improved", "Not tested",
             "Warning"),
  rrmse_ratio = c(12.4, -3.1, 2.7, -1.5, 0.8, 4.2),
  rrmse_eval = c(0.42, 0.25, 0.31, 0.18, 0.40, 0.29),
  rrmse_ref = c(0.37, 0.26, 0.30, 0.18, 0.40, 0.28),
  n_obs = c(120L, 85L, 64L, 97L, 8L, 51L)
)
overview_heatmap(overview)

{"x":{"visdat":{"195f65fd79c1":["function () ","plotlyVisDat"]},"cur_data":"195f65fd79c1","attrs":{"195f65fd79c1":{"x":["wheat","maize"],"y":["lai_n","mafruit","masec_n"],"z":[[4,2],[3,3],[2,1]],"text":[["Species: wheat<br>Variable: lai_n<br>Status: Critical<br>rRMSE ratio: +12.40%<br>rRMSE: 0.42 (ref 0.37)<br>n_obs: 120","Species: maize<br>Variable: lai_n<br>Status: Improved<br>rRMSE ratio: -1.50%<br>rRMSE: 0.18 (ref 0.18)<br>n_obs: 97"],["Species: wheat<br>Variable: mafruit<br>Status: Warning<br>rRMSE ratio: +2.70%<br>rRMSE: 0.31 (ref 0.3)<br>n_obs: 64","Species: maize<br>Variable: mafruit<br>Status: Warning<br>rRMSE ratio: +4.20%<br>rRMSE: 0.29 (ref 0.28)<br>n_obs: 51"],["Species: wheat<br>Variable: masec_n<br>Status: Improved<br>rRMSE ratio: -3.10%<br>rRMSE: 0.25 (ref 0.26)<br>n_obs: 85","Species: maize<br>Variable: masec_n<br>Status: Not tested<br>rRMSE ratio: +0.80%<br>rRMSE: 0.4 (ref 0.4)<br>n_obs: 8"]],"hoverinfo":"text","xgap":1,"ygap":1,"zmin":0.5,"zmax":4.5,"colorscale":[[0,"#d5d8dc"],[0.25,"#d5d8dc"],[0.25,"#82e0aa"],[0.5,"#82e0aa"],[0.5,"#f5b041"],[0.75,"#f5b041"],[0.75,"#c0392b"],[1,"#c0392b"]],"colorbar":{"tickvals":[1,2,3,4],"ticktext":["Not tested","Improved","Warning","Critical"],"len":1,"thickness":15},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"heatmap"}},"layout":{"height":174,"margin":{"b":40,"l":60,"t":60,"r":10},"xaxis":{"domain":[0,1],"automargin":true,"side":"top","type":"category","title":[]},"yaxis":{"domain":[0,1],"automargin":true,"autorange":"reversed","type":"category","title":[]},"plot_bgcolor":"#ffffff","scene":{"zaxis":{"title":[]}},"hovermode":"closest","showlegend":false,"legend":{"yanchor":"top","y":0.5}},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"displaylogo":false},"data":[{"colorbar":{"title":"","ticklen":2,"tickvals":[1,2,3,4],"ticktext":["Not tested","Improved","Warning","Critical"],"len":1,"thickness":15,"lenmode":"fraction","y":1,"yanchor":"top"},"colorscale":[[0,"#d5d8dc"],[0.25,"#d5d8dc"],[0.25,"#82e0aa"],[0.5,"#82e0aa"],[0.5,"#f5b041"],[0.75,"#f5b041"],[0.75,"#c0392b"],[1,"#c0392b"]],"showscale":true,"x":["wheat","maize"],"y":["lai_n","mafruit","masec_n"],"z":[[4,2],[3,3],[2,1]],"text":[["Species: wheat<br>Variable: lai_n<br>Status: Critical<br>rRMSE ratio: +12.40%<br>rRMSE: 0.42 (ref 0.37)<br>n_obs: 120","Species: maize<br>Variable: lai_n<br>Status: Improved<br>rRMSE ratio: -1.50%<br>rRMSE: 0.18 (ref 0.18)<br>n_obs: 97"],["Species: wheat<br>Variable: mafruit<br>Status: Warning<br>rRMSE ratio: +2.70%<br>rRMSE: 0.31 (ref 0.3)<br>n_obs: 64","Species: maize<br>Variable: mafruit<br>Status: Warning<br>rRMSE ratio: +4.20%<br>rRMSE: 0.29 (ref 0.28)<br>n_obs: 51"],["Species: wheat<br>Variable: masec_n<br>Status: Improved<br>rRMSE ratio: -3.10%<br>rRMSE: 0.25 (ref 0.26)<br>n_obs: 85","Species: maize<br>Variable: masec_n<br>Status: Not tested<br>rRMSE ratio: +0.80%<br>rRMSE: 0.4 (ref 0.4)<br>n_obs: 8"]],"hoverinfo":"text","xgap":1,"ygap":1,"zmin":0.5,"zmax":4.5,"type":"heatmap","xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
# Open each species page when clicking a cell
overview_heatmap(
  overview,
  href = function(species, variable, status) {
    paste0("species/", species, ".html")
  }
)

{"x":{"visdat":{"195f1ef100bc":["function () ","plotlyVisDat"]},"cur_data":"195f1ef100bc","attrs":{"195f1ef100bc":{"x":["wheat","maize"],"y":["lai_n","mafruit","masec_n"],"z":[[4,2],[3,3],[2,1]],"text":[["Species: wheat<br>Variable: lai_n<br>Status: Critical<br>rRMSE ratio: +12.40%<br>rRMSE: 0.42 (ref 0.37)<br>n_obs: 120","Species: maize<br>Variable: lai_n<br>Status: Improved<br>rRMSE ratio: -1.50%<br>rRMSE: 0.18 (ref 0.18)<br>n_obs: 97"],["Species: wheat<br>Variable: mafruit<br>Status: Warning<br>rRMSE ratio: +2.70%<br>rRMSE: 0.31 (ref 0.3)<br>n_obs: 64","Species: maize<br>Variable: mafruit<br>Status: Warning<br>rRMSE ratio: +4.20%<br>rRMSE: 0.29 (ref 0.28)<br>n_obs: 51"],["Species: wheat<br>Variable: masec_n<br>Status: Improved<br>rRMSE ratio: -3.10%<br>rRMSE: 0.25 (ref 0.26)<br>n_obs: 85","Species: maize<br>Variable: masec_n<br>Status: Not tested<br>rRMSE ratio: +0.80%<br>rRMSE: 0.4 (ref 0.4)<br>n_obs: 8"]],"hoverinfo":"text","xgap":1,"ygap":1,"zmin":0.5,"zmax":4.5,"colorscale":[[0,"#d5d8dc"],[0.25,"#d5d8dc"],[0.25,"#82e0aa"],[0.5,"#82e0aa"],[0.5,"#f5b041"],[0.75,"#f5b041"],[0.75,"#c0392b"],[1,"#c0392b"]],"colorbar":{"tickvals":[1,2,3,4],"ticktext":["Not tested","Improved","Warning","Critical"],"len":1,"thickness":15},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"heatmap"}},"layout":{"height":174,"margin":{"b":40,"l":60,"t":60,"r":10},"xaxis":{"domain":[0,1],"automargin":true,"side":"top","type":"category","title":[]},"yaxis":{"domain":[0,1],"automargin":true,"autorange":"reversed","type":"category","title":[]},"plot_bgcolor":"#ffffff","scene":{"zaxis":{"title":[]}},"hovermode":"closest","showlegend":false,"legend":{"yanchor":"top","y":0.5}},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"displaylogo":false},"data":[{"colorbar":{"title":"","ticklen":2,"tickvals":[1,2,3,4],"ticktext":["Not tested","Improved","Warning","Critical"],"len":1,"thickness":15,"lenmode":"fraction","y":1,"yanchor":"top"},"colorscale":[[0,"#d5d8dc"],[0.25,"#d5d8dc"],[0.25,"#82e0aa"],[0.5,"#82e0aa"],[0.5,"#f5b041"],[0.75,"#f5b041"],[0.75,"#c0392b"],[1,"#c0392b"]],"showscale":true,"x":["wheat","maize"],"y":["lai_n","mafruit","masec_n"],"z":[[4,2],[3,3],[2,1]],"text":[["Species: wheat<br>Variable: lai_n<br>Status: Critical<br>rRMSE ratio: +12.40%<br>rRMSE: 0.42 (ref 0.37)<br>n_obs: 120","Species: maize<br>Variable: lai_n<br>Status: Improved<br>rRMSE ratio: -1.50%<br>rRMSE: 0.18 (ref 0.18)<br>n_obs: 97"],["Species: wheat<br>Variable: mafruit<br>Status: Warning<br>rRMSE ratio: +2.70%<br>rRMSE: 0.31 (ref 0.3)<br>n_obs: 64","Species: maize<br>Variable: mafruit<br>Status: Warning<br>rRMSE ratio: +4.20%<br>rRMSE: 0.29 (ref 0.28)<br>n_obs: 51"],["Species: wheat<br>Variable: masec_n<br>Status: Improved<br>rRMSE ratio: -3.10%<br>rRMSE: 0.25 (ref 0.26)<br>n_obs: 85","Species: maize<br>Variable: masec_n<br>Status: Not tested<br>rRMSE ratio: +0.80%<br>rRMSE: 0.4 (ref 0.4)<br>n_obs: 8"]],"hoverinfo":"text","xgap":1,"ygap":1,"zmin":0.5,"zmax":4.5,"type":"heatmap","xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":{"render":[{"code":"function(el, x, links) {\n      el.classList.add('clickable-heatmap');\n      el.on('plotly_click', function(data) {\n        var cell = data.points[0].pointIndex;\n        var url = links[cell[0]][cell[1]];\n        if (url) window.location.href = url;\n      });\n    }","data":[["species/wheat.html","species/maize.html"],["species/wheat.html","species/maize.html"],["species/wheat.html","species/maize.html"]]}]}}
```
