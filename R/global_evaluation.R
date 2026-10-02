#' GlobalEvaluation class
#'
#' @description
#' Internal class. Evaluates the global performance of the STICS model
#' across multiple USMs (User Simulation Models): computes global
#' statistics and performs a comparison of the relative Root Mean Square
#' Error (rRMSE) for the specified variables. Instantiated internally by
#' \code{\link{evaluate}}.
#'
#' @details
#' The evaluation process involves the following steps:
#' \enumerate{
#'   \item Generating global statistics by comparing simulated and observed
#'      data.
#'   \item Performing a comparison of rRMSE values for the specified variables.
#'   \item Logging the results and providing a summary of the evaluation.
#' }
#'
#' @keywords internal
GlobalEvaluation <- R6::R6Class("GlobalEvaluation", # nolint: object_name_linter
  private = list(
    usms = NULL,
    var2exclude = NULL,
    percentage = NULL,
    output_dir = NULL,
    rrmse_comparison = NULL,
    stats = NULL,
    no_comparison_reason = NULL,
    workspace = NULL,
    logger = NULL,

    gen_global_stats = function() {
      eval_data <- read_split_eval_data(
        private$workspace, usms = private$usms,
        var2exclude = private$var2exclude
      )
      if (is.null(eval_data$sim) || is.null(eval_data$obs)) {
        private$logger$warn(
          "No simulation or observation data, skipping global statistics"
        )
        private$no_comparison_reason <- "no data to evaluate"
        return(invisible(NULL))
      }

      private$logger$info("Generating global statistics")
      private$stats <- compute_eval_stats(eval_data)
      rm(eval_data)
      gc()
    },

    gen_global_comparison = function() {
      private$gen_global_stats()
      if (is.null(private$stats)) {
        return(invisible(NULL))
      }
      if (!has_reference_stats(private$stats)) {
        private$logger$warn(
          "No reference simulation data, skipping global rRMSE comparison"
        )
        private$no_comparison_reason <- "no reference data"
        return(invisible(NULL))
      }
      private$logger$info("Comparing global rRMSE")
      private$rrmse_comparison <- RRmseComparison$new(
        stats = private$stats,
        percentage = private$percentage
      )
      private$logger$info("Global comparison generated")
    },

    evaluate_global = function() {
      private$logger$info("Generating global comparison...")
      private$gen_global_comparison()
    }
  ),

  active = list(
    #' @field success
    #' A logical value indicating whether the global evaluation was successful.
    #' The evaluation is considered successful if the rRMSE comparison was
    #' performed and there are no critical variables identified in the
    #' comparison.
    success = function() {
      !is.null(private$rrmse_comparison) &&
        length(private$rrmse_comparison$critical_vars) == 0
    },

    #' @field skip_reason
    #' Why the rRMSE comparison was not done (e.g. \code{"no reference
    #' data"}), or NULL if it was done.
    skip_reason = function() {
      if (!is.null(private$rrmse_comparison)) return(NULL)
      private$no_comparison_reason %||% "no data to evaluate"
    }
  ),

  public = list(
    #' @description
    #' Create a new GlobalEvaluation object.
    #' @param eval_workspace Path to the evaluation workspace. Only used to
    #' build a default `workspace` when one isn't supplied.
    #' @param usms Optional character vector of USMs to evaluate.
    #' @param var2exclude Optional character vector of variables to exclude.
    #' @param percentage Threshold (%) above which a variable is flagged as
    #' deteriorated vs. the reference.
    #' @param output_dir Output directory for the CSV export.
    #' @param workspace An optional EvalWorkspace object. If not provided, a
    #' new EvalWorkspace will be created from `eval_workspace`.
    #' @param logger An optional logger object for logging messages. If not
    #' provided, the default logger will be used.
    initialize = function(
      eval_workspace = NULL,
      usms = NULL,
      var2exclude = NULL,
      percentage = 5,
      output_dir = NULL,
      workspace = NULL,
      logger = default_logger
    ) {
      private$usms <- usms
      private$var2exclude <- var2exclude
      private$percentage <- percentage
      private$output_dir <- output_dir
      private$workspace <- workspace %||% EvalWorkspace$new(eval_workspace)
      private$logger <- logger
    },

    #' @description
    #' Run the global evaluation.
    #' This method performs the global evaluation by generating global
    #' statistics and comparing the rRMSE values for the specified variables.
    run = function() {
      on.exit({
        end_time <- Sys.time()
        private$logger$info(
          "Global evaluation time: ",
          format_duration(start_time, end_time)
        )
      }, add = TRUE)
      start_time <- Sys.time()
      tryCatch({
        private$logger$info("Starting global evaluation...")

        private$evaluate_global()

      }, error = function(e) {
        private$logger$error(conditionMessage(e))
        private$logger$debug(
          paste(capture.output(rlang::last_trace()), collapse = "\n")
        )
        rlang::abort(conditionMessage(e), parent = e)
      })
    },

    #' @description
    #' Print a summary of the global evaluation results.
    summary = function() {
      cli::cli_h1("Global comparison")

      if (is.null(private$rrmse_comparison)) {
        cli::cli_alert_warning("No comparison done ({self$skip_reason}).")
        return(invisible(self))
      }
      private$rrmse_comparison$log()

      cli::cli_rule()
    },

    #' @description
    #' Export the global evaluation results to CSV files and a comparison
    #' plot. This method exports the global statistics
    #' (`global_stats.csv`) and, if a comparison against a reference version
    #' was performed, the per-variable rRMSE comparison
    #' (`global_rrmse_comparison.csv`, with each variable's status:
    #' Critical/Warning/Improved/Other) to the `csv` subdirectory, plus a
    #' static rRMSE comparison plot (`plots/global_comparison.png`).
    export = function() {
      private$logger$info("Exporting global evaluation data")
      if (is.null(private$stats)) {
        private$logger$info("No data to export for global evaluation")
        return()
      }
      safe_write_csv(
        private$stats,
        csv_output_path(private$output_dir, "global_stats.csv")
      )

      if (!is.null(private$rrmse_comparison) &&
            !private$rrmse_comparison$is_empty) {
        safe_write_csv(
          private$rrmse_comparison$get_data(),
          csv_output_path(private$output_dir, "global_rrmse_comparison.csv")
        )
        plots_dir <- file.path(private$output_dir, "plots")
        dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
        private$rrmse_comparison$plot_comparison(
          file.path(plots_dir, "global_comparison.png")
        )
      }

      private$logger$info("Global evaluation export done")
    }
  )
)
