gen_scatter_plot <- function(sim, ref_sim, obs, vars) {
  lapply(vars, function(var) {
    plots <- CroPlotR:::plot.cropr_simulation(
      "New version" = sim,
      "Ref version" = ref_sim,
      obs = obs,
      type = "scatter",
      select_scat = "sim",
      var = var
    )
    plotly::ggplotly(plots[[1]])
  })
}

gen_comparison_plot <- function(
  comparison,
  pct = get_config_env()$percentage
) {
  stats_all <- dplyr::bind_rows(comparison) %>%
    dplyr::mutate(
      status = dplyr::case_when(
        is_critical(ratio) ~ "Critical",
        is_warning(ratio) ~ "Warning",
        is_improved(ratio) ~ "Improved",
        TRUE ~ "Other"
      ),
    )
  plot <- ggplot2::ggplot(
    stats_all,
    ggplot2::aes(
      x = ref_rmse,
      y = new_rmse,
      color = status,
      text = variable
    ),
    ggplot2::labs(
      x = "Ref RMSE",
      y = "New RMSE",
      status = "Status",
      variable = "Variable"
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = c(
      "Critical" = "red",
      "Warning"  = "orange",
      "Improved" = "green",
      "Other"    = "grey50"
    )) +
    ggplot2::geom_abline(intercept = 0, slope = 1) +
    ggplot2::geom_abline(intercept = 0, slope = 1 + pct, linetype = "dashed") +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = variable),
      na.rm = TRUE,
      show.legend = FALSE
    ) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle("rRMSE New Version vs Ref Version")
  plotly::ggplotly(plot)
}

gen_plots_file <- function(
  species,
  output_dir,
  comparison,
  sim,
  obs
) {
  logger::log_debug("Generating ", species, " comparison plot")
  plots <- list(gen_comparison_plot(comparison))
  logger::log_debug(species, " comparison plot generated")
  logger::log_debug("Generating ", species, " scatter plots")
  ref_sim <- read_ref_sim(species)
  deteriorated <- c(get_crit_vars(comparison), get_warn_vars(comparison))
  scat_plots <- gen_scatter_plot(
    sim,
    ref_sim,
    obs,
    deteriorated
  )
  plots <- append(plots, scat_plots)
  logger::log_debug(species, " scatter plots generated")
  page <- htmltools::tagList(plots)
  htmltools::save_html(
    page,
    file = file.path(
      output_dir,
      "plots.html"
    )
  )
}