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
