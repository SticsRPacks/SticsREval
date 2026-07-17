#' GlobalEvaluation class
#'
#' @description
#' The \code{GlobalEvaluation} class is responsible for evaluating the
#' global performance of the STICS model across multiple USMs (User
#' Simulation Models).
#' It computes global statistics and performs a comparison of the relative
#' Root Mean Square Error (rRMSE) for the specified variables.
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
#' @examples
#' \dontrun{
#' config <- Configuration$new(
#'   eval_workspace = "path/to/eval_workspace",
#'   usms_workspace = "path/to/usms_workspace",
#'   usms = c("USM1", "USM2"),
#'   var2exclude = c("var1", "var2"),
#'   percentage = 0.1,
#'   output_dir = "path/to/output_dir"
#' )
#' global_eval <- GlobalEvaluation$new(config = config)
#' global_eval$run()
#' global_eval$summary()
#' global_eval$export()
#' }
#' @export
GlobalEvaluation <- R6::R6Class("GlobalEvaluation", # nolint: object_name_linter
  private = list(
    config = NULL,
    rrmse_comparison = NULL,
    stats = NULL,
    workspace = NULL,
    logger = NULL,

    gen_global_stats = function() {
      exclude <- c("version", "species", private$config$var2exclude)

      splited_sim <- CroPlotR::split_df2sim(
        private$workspace$get_sim(
          species = NULL, usms = private$config$usms, var2exclude = exclude
        )
      )
      splited_ref_sim <- CroPlotR::split_df2sim(
        private$workspace$get_ref_sim(
          species = NULL, usms = private$config$usms, var2exclude = exclude
        )
      )
      splited_obs <- CroPlotR::split_df2sim(
        private$workspace$get_obs(
          species = NULL, usms = private$config$usms, var2exclude = exclude
        )
      )

      private$logger$info("Generating global statistics")
      loadNamespace("CroPlotR")
      summary_method <- utils::getS3method("summary", class(splited_sim))

      private$stats <- run_with_log_control(
        summary_method(
          evaluated = splited_sim,
          reference = splited_ref_sim,
          obs = splited_obs
        )
      )
      rm(splited_sim, splited_ref_sim, splited_obs)
      gc()
    },

    gen_global_comparison = function() {
      private$gen_global_stats()
      if (is.null(private$stats)) {
        return(invisible(NULL))
      }
      private$logger$info("Comparing global rRMSE")
      private$rrmse_comparison <- RRmseComparison$new(
        stats = private$stats,
        percentage = private$config$percentage
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
    }
  ),

  public = list(
    #' @description
    #' Create a new GlobalEvaluation object.
    #' @param config A Configuration object containing the necessary parameters
    #' for the evaluation.
    #' @param workspace An optional EvalWorkspace object. If not provided, a new
    #' EvalWorkspace will be created using the provided configuration.
    #' @param logger An optional logger object for logging messages. If not
    #' provided, the default logger will be used.
    initialize = function(
      config, workspace = NULL, logger = default_logger
    ) {
      config$validate_eval()
      private$config <- config
      private$workspace <- workspace %||% EvalWorkspace$new(
        config$eval_workspace
      )
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
        cli::cli_alert_warning("No comparison done.")
        return(invisible(self))
      }
      private$rrmse_comparison$log()
    },

    #' @description
    #' Export the global evaluation results to CSV files.
    #' This method exports the global statistics and rRMSE comparison results
    #' to CSV files in the specified output directory.
    export = function() {
      private$logger$info("Exporting global evaluation data")
      if (is.null(private$stats)) {
        private$logger$info("No data to export for global evaluation")
        return()
      }
      safe_write_csv(
        private$stats,
        file.path(private$config$output_dir, "global_stats.csv")
      )
      private$logger$info("Global evaluation export done")
    }
  )
)
