RmseComparison <- R6::R6Class("RmseComparison",
  private = list(
    data = NULL,
    percentage = NULL,

    compare_rmse = function(species, ref_stats, new_stats) {
      new_stats |>
        dplyr::left_join(ref_stats, by = c("situation", "variable")) |>
        dplyr::mutate(
          rmse_new = as.numeric(.data$rRMSE.x),
          rmse_ref = as.numeric(.data$rRMSE.y)
        ) |>
        dplyr::filter(
          is.finite(.data$rmse_new), is.finite(.data$rmse_ref),
          !is.na(.data$variable), !is.na(.data$situation)
        ) |>
        dplyr::mutate(
          species = species,
          ratio   = round(
            (abs(.data$rmse_new) - abs(.data$rmse_ref)) / abs(.data$rmse_ref) * 100,
            2
          )
        ) |>
        dplyr::filter(is.finite(.data$ratio)) |>
        dplyr::select("species", "situation", "variable", "rmse_new", "rmse_ref", "ratio")
    }
  ),

  active = list(
    critical_vars = function() {
      private$data |>
        dplyr::filter(.data$ratio >= private$percentage) |>
        dplyr::pull("variable")
    },
    warning_vars = function() {
      private$data |>
        dplyr::filter(.data$ratio < private$percentage, .data$ratio > 0) |>
        dplyr::pull("variable")
    },
    improved_vars = function() {
      private$data |>
        dplyr::filter(.data$ratio <= 0) |>
        dplyr::pull("variable")
    },
    is_empty = function() isTRUE(nrow(private$data) == 0)
  ),

  public = list(
    initialize = function(
      percentage, species = NULL, ref_stats = NULL,
      eval_stats = NULL, data = NULL
    ) {
      private$percentage <- percentage
      
      if (!is.null(data)) {
        private$data <- data |>
          dplyr::arrange(dplyr::desc(.data$ratio)) |>
          dplyr::collect()
      } else if (!is.null(species) && !is.null(ref_stats) && !is.null(eval_stats)) {
        private$data <- private$compare_rmse(species, ref_stats, eval_stats) |>
          dplyr::arrange(dplyr::desc(.data$ratio)) |>
          dplyr::collect()
      } else {
        stop("`data`, or `species` + `ref_stats` + `eval_stats` must be defined")
      }
    },

    log = function() {
      if (self$is_empty) return(invisible(self))
      logger::log_info(strrep("-", 65))
      logger::log_info("Species: ", private$data$species[1])
      logger::log_info("Total number of variables: ", nrow(private$data))
      logger::log_info(length(self$critical_vars),
        " deteriorated variables (>={private$percentage}%): ")
      if (length(self$critical_vars) > 0)
        logger::log_info(toString(self$critical_vars))
      logger::log_info(length(self$warning_vars),
        " deteriorated variables (>0%, <{private$percentage}%): ")
      if (length(self$warning_vars) > 0)
        logger::log_info(toString(self$warning_vars))
      logger::log_info(length(self$improved_vars), " improved variables (<=0%): ")
      if (length(self$improved_vars) > 0)
        logger::log_info(toString(self$improved_vars))
      logger::log_info(strrep("-", 65))
      invisible(self)
    },

    get_data = function() private$data,

    plot_comparison = function(output_dir) {
      p <- private$data |>
        dplyr::mutate(
          status = dplyr::case_when(
            .data$ratio >= private$percentage ~ "Critical",
            .data$ratio > 0 & .data$ratio < private$percentage ~ "Warning",
            .data$ratio <= 0 ~ "Improved",
            TRUE ~ "Other"
          )
        ) |>
        ggplot2::ggplot(
          ggplot2::aes(
            x = .data$rmse_ref,
            y = .data$rmse_new,
            color = .data$status,
            text = .data$variable
          ),
          ggplot2::labs(x = "Ref RMSE", y = "New RMSE", color = "Status")
        ) +
        ggplot2::geom_point() +
        ggplot2::scale_color_manual(values = c(
          Critical = "red",
          Warning = "orange",
          Improved = "green",
          Other = "grey50"
        )) +
        ggplot2::geom_abline(intercept = 0, slope = 1) +
        ggplot2::geom_abline(
          intercept = 0,
          slope = 1 + private$percentage / 100,
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
      invisible(self)
    }
  )
)

DeterioratedUSMComparison <- R6::R6Class("DeterioratedUSMComparison",
  inherit = RmseComparison,
  public = list(
    initialize = function(
      percentage,
      species = NULL,
      ref_stats = NULL,
      eval_stats = NULL,
      data = NULL
    ) {
      super$initialize(
        species = species,
        ref_stats = ref_stats,
        eval_stats = eval_stats,
        percentage = percentage,
        data = data
      )
      private$data <- private$data |>
        dplyr::filter(.data$ratio > 0) |>
        dplyr::arrange(dplyr::desc(.data$ratio))
    }
  )
)
