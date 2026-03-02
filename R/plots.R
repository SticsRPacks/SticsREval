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
  p <- comparison |>
    dplyr::mutate(
      status = dplyr::case_when(
        is_critical(.data$ratio, percentage) ~ "Critical",
        is_warning(.data$ratio, percentage) ~ "Warning",
        is_improved(.data$ratio) ~ "Improved",
        TRUE ~ "Other"
      )
    ) |>
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

#' Generate comparison and scatter plots for each species
#'
#' This function generates diagnostic plots for each species found in the
#' evaluation workspace. It produces comparison plots and, when possible,
#' scatter plots highlighting deteriorated variables based on a given threshold.
#'
#' For each species, the function:
#' \itemize{
#'   \item Retrieves comparison data between simulation and observation.
#'   \item Generates comparison plots.
#'   \item Identifies deteriorated variables using a threshold percentage.
#'   \item Optionally generates scatter plots if reference simulation data
#'      are available.
#' }
#'
#' Scatter plots are only generated when both reference simulation data and
#' deteriorated variables are available.
#'
#' @param config List. Configuration object created by `make_config()`,
#'    containing all parameters required for the plots generation
#'
#' @return NULL. This function is called for its side effects (writing plot
#' files).
#'
#' @details
#' For each species:
#' \itemize{
#'   \item Creates a species-specific output directory.
#'   \item Generates comparison plots using `gen_comparison_plot()`.
#'   \item Identifies critical and warning variables using the given percentage.
#'   \item Generates scatter plots using `gen_scatter_plot()` if reference data
#'   and deteriorated variables are available.
#' }
#'
#' @examples
#' \dontrun{
#' gen_plots("results/", percentage = 0.2, eval_workspace = "workspace/")
#' }
#'
#' @export
gen_plots <- function(config) {
  validate_export_config(config)
  valide_plots_config(config)
  species <- get_species(config$eval_workspace)
  for (spec in species) {
    o_dir <- prepare_species_output_dir(config$output_dir, spec)
    spec_comparison <- get_species_comparison(config$eval_workspace, spec, TRUE)
    if (is.null(spec_comparison)) {
      logger::log_info("Skipping plot generation for species {spec}")
      next
    }
    logger::log_info("Generating variable comparison plot for species {spec}")
    gen_comparison_plot(o_dir, spec_comparison, config$percentage)
    ref_sim <- read_ref_sim(config$reference_data_dir, spec, TRUE)
    deteriorated <- c(
      get_crit_vars(spec_comparison, config$percentage),
      get_warn_vars(spec_comparison, config$percentage)
    )
    if (!is.null(ref_sim) && length(deteriorated) > 0) {
      logger::log_info("Generating scatter plots for species {spec}")
      sim <- get_by_species(config$eval_workspace, spec, "sim", TRUE)
      obs <- get_by_species(config$eval_workspace, spec, "obs", TRUE)
      gen_scatter_plot(
        o_dir,
        CroPlotR::split_df2sim(sim),
        CroPlotR::split_df2sim(obs),
        CroPlotR::split_df2sim(ref_sim),
        deteriorated
      )
      rm(sim, obs, ref_sim)
      gc()
    }
  }
}