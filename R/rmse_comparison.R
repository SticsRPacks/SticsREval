RRmseComparison <- R6::R6Class("RRmseComparison", # nolint: object_name_linter
  private = list(
    data = NULL,
    percentage = NULL,

    compare_rrmse = function(species, stats) {
      ref_stats <- dplyr::filter(stats, .data$group == "reference")
      new_stats <- dplyr::filter(stats, .data$group == "evaluated")

      res <- new_stats |>
        dplyr::left_join(ref_stats, by = c("situation", "variable")) |>
        dplyr::mutate(
          rrmse_new = as.numeric(.data$rRMSE.x),
          rrmse_ref = as.numeric(.data$rRMSE.y),
          n_obs = as.numeric(.data$n_obs.y)
        ) |>
        dplyr::filter(
          is.finite(.data$rrmse_new), is.finite(.data$rrmse_ref),
          !is.na(.data$variable), !is.na(.data$situation)
        ) |>
        dplyr::mutate(
          ratio = round(
            (abs(.data$rrmse_new) - abs(.data$rrmse_ref)) / abs(.data$rrmse_ref) * 100, # nolint: line_length_linter
            2
          )
        ) |>
        dplyr::filter(is.finite(.data$ratio)) |>
        dplyr::select(
          "situation", "variable", "rrmse_new", "rrmse_ref", "ratio", "n_obs"
        )
      if (!is.null(species)) {
        res <- dplyr::mutate(res, species = species)
      }
      res
    }
  ),

  active = list(
    critical_vars = function() {
      private$data |>
        dplyr::filter(.data$ratio >= private$percentage, .data$n_obs > 10) |>
        dplyr::pull("variable")
    },
    warning_vars = function() {
      private$data |>
        dplyr::filter(
          .data$ratio < private$percentage, .data$ratio > 0,
          .data$n_obs > 10
        ) |>
        dplyr::pull("variable")
    },
    improved_vars = function() {
      private$data |>
        dplyr::filter(.data$ratio <= 0, .data$n_obs > 10) |>
        dplyr::pull("variable")
    },
    is_empty = function() isTRUE(nrow(private$data) == 0)
  ),

  public = list(
    initialize = function(
      percentage, species = NULL, stats = NULL, data = NULL
    ) {
      private$percentage <- percentage
      if (!is.null(data)) {
        private$data <- data |>
          dplyr::arrange(dplyr::desc(.data$ratio)) |>
          dplyr::collect()
      } else if (!is.null(stats)) {
        private$data <- private$compare_rrmse(species, stats) |>
          dplyr::arrange(dplyr::desc(.data$ratio)) |>
          dplyr::collect()
      } else {
        stop(
          "`data`, or `stats` must be defined",
          call. = FALSE
        )
      }
    },

    log = function() {
      if (self$is_empty) return(invisible(self))

      if (!is.null(private$data$species[1])) {
        cli::cli_rule(left = "Species: {private$data$species[1]}")
      }

      n_crit <- length(self$critical_vars)
      n_warn <- length(self$warning_vars)
      n_imp <- length(self$improved_vars)

      cli::cli_dl(c(
        "Total variables" = "{n_crit + n_warn + n_imp}"
      ))

      cli::cli_li("{.strong {n_crit} critical} (>= {private$percentage}%)")
      if (n_crit > 0) {
        cli::cli_text(cli::col_red("  {toString(self$critical_vars)}"))
      }
      cli::cli_li("{.strong {n_warn} warning} (> 0%, < {private$percentage}%)")
      if (n_warn > 0) {
        cli::cli_text(cli::col_yellow("  {toString(self$warning_vars)}"))
      }

      cli::cli_li("{.strong {n_imp} improved} (<= 0%)")
      if (n_imp > 0) {
        cli::cli_text(cli::col_green("  {toString(self$improved_vars)}"))
      }

      invisible(self)
    },

    get_data = function() private$data,

    plot_comparison = function(output_path) {
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
            x = .data$rrmse_ref,
            y = .data$rrmse_new,
            color = .data$status,
            text = .data$variable
          ),
          ggplot2::labs(x = "Ref rRMSE", y = "New rRMSE", color = "Status")
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
      ggplot2::ggsave(
        filename = output_path,
        plot = p,
        width = 17,
        height = 12,
        units = "cm",
        dpi = 200,
        scale = 1.5
      )
      invisible(self)
    }
  )
)

DeterioratedUSMComparison <- R6::R6Class("DeterioratedUSMComparison", # nolint: object_name_linter
  inherit = RRmseComparison,
  public = list(
    initialize = function(
      percentage,
      species = NULL,
      stats = NULL,
      data = NULL
    ) {
      super$initialize(
        species = species,
        stats = stats,
        percentage = percentage,
        data = data
      )
      private$data <- private$data |>
        dplyr::filter(.data$ratio > 0) |>
        dplyr::arrange(dplyr::desc(.data$ratio))
    }
  )
)
