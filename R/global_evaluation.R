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
    success = function() {
      !is.null(private$rrmse_comparison) &&
        length(private$rrmse_comparison$critical_vars) == 0
    }
  ),

  public = list(
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

    summary = function() {
      cli::cli_h1("Global comparison")

      if (is.null(private$rrmse_comparison)) {
        cli::cli_alert_warning("No comparison done.")
        return(invisible(self))
      }
      private$rrmse_comparison$log()
    },

    export = function() {
      if (!is.null(private$stats)) {
        safe_write_csv(
          private$stats,
          file.path(private$config$output_dir, "global_stats.csv")
        )
      }
    }
  )
)
