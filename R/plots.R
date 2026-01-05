gen_scatter_plot <- function(sim, ref_sim, obs, vars, output_dir) {
  plot_list <- lapply(vars, function(var) {
    plots <- CroPlotR:::plot.cropr_simulation(
      sim,
      ref_sim,
      obs = obs,
      type = "scatter",
      select_scat = "sim",
      var = var
    )
    plotly::ggplotly(plots[[1]])
  })
  page <- htmltools::tagList(plot_list)
  htmltools::save_html(
    page,
    file = file.path(
      output_dir,
      "scatter_interactive.html"
    )
  )
}

gen_comparison_plot <- function(
  new_stats,
  ref_stats,
  output_dir,
  pct = 0.05
) {
  ref <- ref_stats %>%
    dplyr::select(variable, rRMSE) %>%
    dplyr::mutate(
      rRMSE = suppressWarnings(as.numeric(sub(",", ".", rRMSE, fixed = TRUE))),
      group = "Ref Version"
    )
  new <- new_stats %>%
    dplyr::select(variable, rRMSE) %>%
    dplyr::mutate(
      rRMSE = suppressWarnings(as.numeric(sub(",", ".", rRMSE, fixed = TRUE))),
      group = "New Version"
    )
  stats_all <- dplyr::bind_rows(ref, new)
  tmp <- tidyr::pivot_wider(
    stats_all[, c("group", "variable", "rRMSE")],
    names_from = group,
    values_from = rRMSE
  )
  tmp <- tmp %>%
    dplyr::mutate(
      abs_ref = abs(`Ref Version`),
      abs_new = abs(`New Version`),
      ratio = dplyr::case_when(
        is.na(abs_ref) | is.na(abs_new) ~ NA_real_,
        abs_ref == 0 ~ NA_real_,
        TRUE ~ abs_new / abs_ref
      ),
      colours = dplyr::case_when(
        is.na(ratio) ~ "grey50",
        ratio > (1 + pct) ~ "red",
        ratio > 1 & ratio <= (1 + pct) ~ "orange",
        ratio <= 1 ~ "green",
        TRUE ~ "grey50"
      )
    )
  p <- ggplot2::ggplot(
    tmp,
    ggplot2::aes(x = `Ref Version`, y = `New Version`, color = colours)
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(intercept = 0, slope = 1) +
    ggplot2::geom_abline(intercept = 0, slope = 1 + pct, linetype = "dashed") +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = .data$variable),
      na.rm = TRUE,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(
      breaks = c("red", "orange", "green", "grey50"),
      values = c("red", "orange", "green", "grey50"),
      na.value = "grey50"
    ) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle("rRMSE New Version vs Ref Version")
  run_with_log_control(
    ggplot2::ggsave(filename = file.path(output_dir, "variables.png"), plot = p)
  )
}
