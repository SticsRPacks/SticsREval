# Minimum number of observations a variable needs,
# at the global or species level, to be tested. Below it, the variable is
# reported with the "Not tested" status, and, for the per-USM test, its
# situations are not tested either for that species.
min_obs_threshold <- 10

has_enough_obs <- function(n_obs) {
  !is.na(n_obs) & n_obs >= min_obs_threshold
}

RRmseComparison <- R6::R6Class("RRmseComparison", # nolint: object_name_linter
  private = list(
    data = NULL,
    percentage = NULL,
    check_n_obs = TRUE,

    compare_rrmse = function(species, stats) {
      ref_stats <- dplyr::filter(stats, .data$group == "reference")
      new_stats <- dplyr::filter(stats, .data$group == "evaluated")

      res <- new_stats |>
        dplyr::left_join(ref_stats, by = c("situation", "variable")) |>
        dplyr::mutate(
          rrmse_eval = as.numeric(.data$rRMSE.x),
          rrmse_ref = as.numeric(.data$rRMSE.y),
          n_obs = as.numeric(.data$n_obs.y)
        ) |>
        dplyr::filter(
          is.finite(.data$rrmse_eval), is.finite(.data$rrmse_ref),
          !is.na(.data$variable), !is.na(.data$situation)
        ) |>
        dplyr::mutate(
          rrmse_ratio = round(
            (abs(.data$rrmse_eval) - abs(.data$rrmse_ref)) / abs(.data$rrmse_ref) * 100, # nolint: line_length_linter
            2
          )
        ) |>
        dplyr::filter(is.finite(.data$rrmse_ratio)) |>
        dplyr::select(
          "situation", "variable", "rrmse_eval",
          "rrmse_ref", "rrmse_ratio", "n_obs"
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
        dplyr::filter(.data$status == "Critical") |>
        dplyr::pull("variable")
    },
    warning_vars = function() {
      private$data |>
        dplyr::filter(.data$status == "Warning") |>
        dplyr::pull("variable")
    },
    improved_vars = function() {
      private$data |>
        dplyr::filter(.data$status == "Improved") |>
        dplyr::pull("variable")
    },
    not_tested_vars = function() {
      private$data |>
        dplyr::filter(.data$status == "Not tested") |>
        dplyr::pull("variable")
    },
    is_empty = function() isTRUE(nrow(private$data) == 0)
  ),

  public = list(
    initialize = function(
      percentage, species = NULL, stats = NULL, data = NULL,
      check_n_obs = TRUE
    ) {
      private$percentage <- percentage
      private$check_n_obs <- check_n_obs
      if (!is.null(data)) {
        private$data <- data |>
          dplyr::arrange(dplyr::desc(.data$rrmse_ratio)) |>
          dplyr::collect()
      } else if (!is.null(stats)) {
        private$data <- private$compare_rrmse(species, stats) |>
          dplyr::arrange(dplyr::desc(.data$rrmse_ratio)) |>
          dplyr::collect()
      } else {
        stop(
          "`data`, or `stats` must be defined",
          call. = FALSE
        )
      }
      # Exposed via get_data() (and thus species_rrmse_comparison.csv) so
      # consumers (e.g. the dashboard) can tell critical/warning variables
      # apart without duplicating this threshold logic. Variables without
      # enough observations (see `min_obs_threshold`) are not tested.
      check_n_obs <- private$check_n_obs
      private$data <- private$data |>
        dplyr::mutate(status = dplyr::case_when(
          check_n_obs & !has_enough_obs(.data$n_obs) ~ "Not tested",
          .data$rrmse_ratio >= private$percentage ~ "Critical",
          .data$rrmse_ratio > 0 & .data$rrmse_ratio < private$percentage ~ "Warning", # nolint: line_length_linter
          .data$rrmse_ratio <= 0 ~ "Improved",
          TRUE ~ "Other"
        ))
    },

    log = function() {
      if (self$is_empty) return(invisible(self))

      div_id <- NULL
      if (!is.null(private$data$species[1])) {
        cli_species_rule(private$data$species[1])
        div_id <- cli_indent_start()
      }

      n_crit <- length(self$critical_vars)
      n_warn <- length(self$warning_vars)
      n_imp <- length(self$improved_vars)

      cli::cli_dl(c(
        "Total variables" = "{n_crit + n_warn + n_imp}"
      ))

      cli::cli_li("{.strong {n_crit} critical} (>= {private$percentage}%)")
      if (n_crit > 0) {
        cli::cli_text(cli::col_red("{toString(self$critical_vars)}"))
      }
      cli::cli_li("{.strong {n_warn} warning} (> 0%, < {private$percentage}%)")
      if (n_warn > 0) {
        cli::cli_text(cli::col_yellow("{toString(self$warning_vars)}"))
      }

      cli::cli_li("{.strong {n_imp} improved} (<= 0%)")
      if (n_imp > 0) {
        cli::cli_text(cli::col_green("{toString(self$improved_vars)}"))
      }

      n_not_tested <- length(self$not_tested_vars)
      if (n_not_tested > 0) {
        cli::cli_li(
          "{.strong {n_not_tested} not tested} (< {min_obs_threshold} obs)"
        )
        cli::cli_text(cli::col_grey("{toString(self$not_tested_vars)}"))
      }

      if (!is.null(div_id)) cli_indent_end(div_id)

      invisible(self)
    },

    get_data = function() private$data
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
      # The per-USM test has no minimum number of observations (the
      # species-level criterion is applied upstream, by USMEvaluation).
      super$initialize(
        species = species,
        stats = stats,
        percentage = percentage,
        data = data,
        check_n_obs = FALSE
      )
      private$data <- private$data |>
        dplyr::filter(.data$rrmse_ratio > 0) |>
        dplyr::arrange(dplyr::desc(.data$rrmse_ratio))
    }
  )
)
