#' Plot the rRMSE comparison
#'
#' Plots the rRMSE of the new version against the reference version, one
#' point per variable, colored by status. Each point is labeled with its
#' variable name and number of observations (`ggrepel` keeps labels apart
#' and away from their point).
#'
#' @param rrmse_comparison Data frame of the per-variable rRMSE comparison
#'   (as exported to `global_rrmse_comparison.csv` or
#'   `species_rrmse_comparison.csv`), with columns `variable`, `n_obs`,
#'   `rrmse_ref`, `rrmse_eval` and `status`.
#' @param percentage Threshold (%) above which a variable is flagged as
#'   critical, drawn as a dashed line above the identity line.
#'
#' @return A `ggplot` object.
#' @export
plot_rrmse_comparison <- function(rrmse_comparison, percentage = 5) {
  rrmse_comparison |>
    dplyr::mutate(
      label = paste0(.data$variable, " (n=", .data$n_obs, ")")
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$rrmse_ref,
        y = .data$rrmse_eval,
        color = .data$status
      )
    ) +
    ggplot2::labs(x = "Ref rRMSE", y = "New rRMSE", color = "Status") +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = c(
      Critical = "red",
      Warning = "orange",
      Improved = "forestgreen",
      `Not tested` = "grey75",
      Other = "grey50"
    )) +
    ggplot2::geom_abline(intercept = 0, slope = 1) +
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1 + percentage / 100,
      linetype = "dashed"
    ) +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = .data$label),
      na.rm = TRUE,
      show.legend = FALSE,
      max.overlaps = 100
    ) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle("rRMSE New Version vs Ref Version")
}

severity <- function(overview, status, by) {
  tapply(overview$status == status, overview[[by]], sum)
}

#' Overview heatmap of the species-level status of every variable
#'
#' Variables in rows (there are many more of them than species), species in
#' columns. Species are sorted most degraded first, variables by the number
#' of species where they are critical, then warning.
#'
#' @param overview Data frame of the per-species rRMSE comparison (as
#'   exported to `species_rrmse_comparison.csv`), with columns `species`,
#'   `variable`, `status`, `rrmse_ratio`, `rrmse_eval`, `rrmse_ref` and
#'   `n_obs`.
#' @param status_levels Statuses to show, in increasing order of severity.
#' @param href Optional function of `(species, variable, status)` returning
#'   the URL opened when clicking each row's cell (`NA` for none).
#'
#' @return A `plotly` htmlwidget.
#' @export
overview_heatmap <- function(
  overview,
  status_levels = c("Not tested", "Improved", "Warning", "Critical"),
  href = NULL
) {
  species <- names(sort(
    severity(overview, "Critical", "species") * 1000 +
      severity(overview, "Warning", "species"),
    decreasing = TRUE
  ))
  n_crit <- severity(overview, "Critical", "variable")
  n_warn <- severity(overview, "Warning", "variable")
  vars <- names(n_crit)[order(-n_crit, -n_warn, names(n_crit))]

  # One flat color band per status, matching the badges.
  status_colors <- c("#d5d8dc", "#82e0aa", "#f5b041", "#c0392b")
  bands <- seq(0, 1, length.out = length(status_levels) + 1)
  colorscale <- do.call(c, lapply(seq_along(status_colors), function(i) {
    list(
      list(bands[i], status_colors[i]), list(bands[i + 1], status_colors[i])
    )
  }))

  long_heatmap(
    rows = overview$variable, cols = overview$species,
    row_levels = vars, col_levels = species,
    z = match(overview$status, status_levels),
    hover = sprintf(
      "Species: %s<br>Variable: %s<br>Status: %s<br>rRMSE ratio: %+.2f%%<br>rRMSE: %.3g (ref %.3g)<br>n_obs: %d", # nolint: line_length_linter
      overview$species, overview$variable, overview$status,
      overview$rrmse_ratio, overview$rrmse_eval, overview$rrmse_ref,
      overview$n_obs
    ),
    empty_hover = "Not evaluated",
    links = if (!is.null(href)) {
      href(overview$species, overview$variable, overview$status)
    },
    # No fixed width: the plot fills the content column, so its color bar
    # isn't cut off next to the sidebar and table of contents.
    height = 120 + 18 * length(vars),
    layout = list(
      xaxis = list(type = "category"),
      plot_bgcolor = "#ffffff",
      margin = list(t = 60)
    ),
    zmin = 0.5, zmax = length(status_levels) + 0.5,
    colorscale = colorscale,
    colorbar = list(
      tickvals = seq_along(status_levels), ticktext = status_levels,
      len = 1, thickness = 15
    )
  )
}

#' Heatmap of the per-USM RMSE ratio
#'
#' One row per USM, one column per variable, colored by the ratio used by
#' the USM test: (RMSE - reference RMSE) / RMSE of the species over all its
#' USMs, in %. USMs are sorted by the number of variables above the
#' degraded threshold, then by their highest ratio, and only the
#' `max_usms` most degraded ones are kept.
#'
#' The color scale is capped at +/- `ratio_threshold` so that a single huge
#' ratio doesn't wash out every other cell (the exact value is in the hover
#' text). It follows the test's thresholds: green = improved, white =
#' unchanged, orange = degraded, red = critical.
#'
#' @param usm_ratio Data frame with one row per USM and variable, with
#'   columns `situation`, `variable`, `ratio`, `RMSE_eval`, `RMSE_ref`,
#'   `RMSE_species` and `n_obs`.
#' @param variables Variables to show, in column order.
#' @param degraded_threshold Ratio (%) above which a variable is degraded.
#' @param ratio_threshold Ratio (%) above which a variable is critical.
#' @param max_usms Maximum number of USMs shown.
#'
#' @return A `plotly` htmlwidget.
#' @export
usm_ratio_heatmap <- function(
  usm_ratio,
  variables = unique(usm_ratio$variable),
  degraded_threshold = 20,
  ratio_threshold = 50,
  max_usms = 50
) {
  n_degraded <- tapply(
    usm_ratio$ratio > degraded_threshold, usm_ratio$situation, sum
  )
  max_ratio <- tapply(usm_ratio$ratio, usm_ratio$situation, max)
  usms <- names(n_degraded)[order(-n_degraded, -max_ratio)]
  usms <- utils::head(usms, max_usms)
  vars <- intersect(variables, usm_ratio$variable)

  long_heatmap(
    rows = usm_ratio$situation, cols = usm_ratio$variable,
    row_levels = usms, col_levels = vars,
    z = pmax(pmin(usm_ratio$ratio, ratio_threshold), -ratio_threshold),
    hover = sprintf(
      "USM: %s<br>Variable: %s<br>Ratio: %+.1f%%<br>RMSE: %.3g (ref %.3g, species %.3g)<br>n_obs: %d", # nolint: line_length_linter
      usm_ratio$situation, usm_ratio$variable, usm_ratio$ratio,
      usm_ratio$RMSE_eval, usm_ratio$RMSE_ref, usm_ratio$RMSE_species,
      usm_ratio$n_obs
    ),
    empty_hover = "No data",
    height = 150 + 16 * length(usms),
    layout = list(
      xaxis = list(tickangle = -45),
      plot_bgcolor = "#f2f3f4"
    ),
    zmin = -ratio_threshold, zmax = ratio_threshold,
    colorscale = list(
      c(0, "#1e8449"), c(0.5, "#ffffff"),
      c(0.5 + degraded_threshold / (2 * ratio_threshold), "#f5b041"),
      c(1, "#c0392b")
    ),
    colorbar = list(title = "RMSE ratio (%)")
  )
}

# Heatmap of long data (one element of `rows`, `cols`, `z`, `hover` and
# `links` per cell), with rows and columns in the order of `row_levels` and
# `col_levels` (cells outside them are dropped). Cells without data show
# `empty_hover`. `links` are the URLs opened when clicking each cell (`NA`
# for none). `layout` is merged into the common layout and `...` is passed
# to `plotly::plot_ly()` (color scale and bar).
long_heatmap <- function(
  rows, cols, row_levels, col_levels, z, hover, empty_hover,
  links = NULL, height = NULL, layout = list(), ...
) {
  cells <- cbind(match(rows, row_levels), match(cols, col_levels))
  keep <- stats::complete.cases(cells)
  cells <- cells[keep, , drop = FALSE]
  dims <- c(length(row_levels), length(col_levels))

  z_mat <- matrix(NA_real_, dims[1], dims[2])
  z_mat[cells] <- z[keep]
  hover_mat <- matrix(empty_hover, dims[1], dims[2])
  hover_mat[cells] <- hover[keep]

  widget <- plotly::plot_ly(
    x = col_levels, y = row_levels, z = z_mat,
    type = "heatmap", text = hover_mat, hoverinfo = "text",
    xgap = 1, ygap = 1, height = height, ...
  )
  widget <- do.call(plotly::layout, c(list(widget), utils::modifyList(
    list(
      xaxis = list(side = "top"),
      yaxis = list(autorange = "reversed", type = "category")
    ),
    layout
  ))) |>
    plotly::config(displaylogo = FALSE)

  if (is.null(links)) {
    return(widget)
  }
  links_mat <- matrix("", dims[1], dims[2])
  links_mat[cells] <- ifelse(is.na(links[keep]), "", links[keep])
  # `links_mat` is passed to the JS callback as its third argument (plotly
  # drops a matrix `customdata` on heatmaps), indexed by the clicked
  # cell's [row, column].
  htmlwidgets::onRender(
    widget,
    "function(el, x, links) {
      el.classList.add('clickable-heatmap');
      el.on('plotly_click', function(data) {
        var cell = data.points[0].pointIndex;
        var url = links[cell[0]][cell[1]];
        if (url) window.location.href = url;
      });
    }",
    data = links_mat
  )
}
