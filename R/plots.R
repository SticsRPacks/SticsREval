gen_scatter_plot <- function(output_dir, sim, obs, ref_sim, vars) {
  plots <- CroPlotR:::plot.cropr_simulation(
    "New version" = sim,
    "Ref version" = ref_sim,
    obs = obs,
    type = "scatter",
    select_scat = "sim",
    var = vars
  )
  page <- htmltools::tagList(
    lapply(vars, function(var) {
      suppressWarnings(
        plotly::ggplotly(
          CroPlotR::extract_plot(plots, var = var)[[1]]
        )
      )
    })
  )
  htmltools::save_html(
    page,
    file = file.path(
      output_dir,
      "scatter_plots.html"
    )
  )
}

#' @importFrom rlang .data
gen_comparison_plot <- function(
  output_dir,
  comparison,
  percentage
) {
  p <- comparison %>%
    dplyr::mutate(
      status = dplyr::case_when(
        is_critical(.data$ratio, percentage) ~ "Critical",
        is_warning(.data$ratio, percentage) ~ "Warning",
        is_improved(.data$ratio) ~ "Improved",
        TRUE ~ "Other"
      )
    ) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$rmse_ref,
        y = .data$rmse_new,
        color = .data$status,
        text  = .data$variable
      ),
      ggplot2::labs(
        x = "Ref RMSE",
        y = "New RMSE",
        color = "Status"
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
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1 + percentage / 100,
      linetype = "dashed"
    ) +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = .data$variable),
      na.rm = TRUE,
      show.legend = FALSE,
      max.overlaps = 100
    ) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle("rRMSE New Version vs Ref Version")
  CroPlotR::save_plot_png(p, out_dir = output_dir, suffix = "scatter_")
}