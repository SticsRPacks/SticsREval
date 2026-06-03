#'
#' Evaluation class
#'
#' Run the evaluation workflow
#'
#' @name Evaluation
#' @docType class
#'
#'
#' @examples
#' \dontrun{
#' config <- Configuration$new(
#'   stics_exe = "/path/to/stics",
#'   eval_workspace = "workspace/",
#'   metadata_file = "metadata.csv"
#' )
#' Evaluation$new(config)$run()
#' }
#'
#' @export
Evaluation <- R6::R6Class("Evaluation", # nolint: object_name_linter
  private = list(
    config = NULL,
    backend = NULL,
    workspace = NULL,
    logger = NULL,
    summary_class = NULL,

    filter_species_config = function(species) {
      if (is.null(private$config$species)) return(species)
      intersect(species, private$config$species)
    },

    filter_species_usms = function(species) {
      if (is.null(private$config$usms)) return(species)

      species <- species[
        vapply(species, function(sp) {
          private$logger$debug(sprintf("Checking USMs for species %s...", sp))
          species_usms <- private$workspace$get_species_usm(
            sp, private$config$usms
          )
          private$logger$debug(sprintf(
            "Species %s has USMs: %s",
            sp, toString(species_usms)
          ))
          length(species_usms) > 0
        }, FUN.VALUE = logical(1))
      ]
      species
    },

    get_species_to_evaluate = function() {
      private$logger$debug("Getting species to evaluate...")
      species <- private$workspace$get_species()

      private$logger$debug("Filtering species based on config...")
      species <- private$filter_species_config(species)
      private$logger$debug("Filtering species based on USMs in config...")
      species <- private$filter_species_usms(species)

      private$logger$debug("Species to evaluate: ")
      private$logger$debug(toString(species))

      species
    },

    gen_species_stats = function(species) {
      results <- private$backend$run(
        length(species),
        function(i) {
          spec <- species[i]
          private$logger$debug(
            "Splitting simulations and observations data for species ", spec
          )

          exclude <- c("version", "species", private$config$var2exclude)

          splited_sim <- CroPlotR::split_df2sim(
            private$workspace$get_sim(
              spec, usms = private$config$usms, var2exclude = exclude
            )
          )
          splited_obs <- CroPlotR::split_df2sim(
            private$workspace$get_obs(
              spec, usms = private$config$usms, var2exclude = exclude
            )
          )

          private$logger$info("Generating statistics for ", spec)
          loadNamespace("CroPlotR")
          summary_method <- utils::getS3method("summary", class(splited_sim))

          stats <- run_with_log_control(
            summary_method(splited_sim, obs = splited_obs)
          )
          rmse_per_usm <- run_with_log_control(
            summary_method(
              splited_sim, obs = splited_obs,
              all_situations = FALSE, stats = "rRMSE"
            )
          )
          list(species = spec, stats = stats, rmse_per_usm = rmse_per_usm)
        }
      )

      for (res in results) {
        private$logger$info("Saving statistics for ", res$species)
        private$workspace$save_stats(res$species, res$stats)
        private$logger$info("Saving RMSE per USM for species ", res$species)
        private$workspace$save_rmse_per_usm(res$species, res$rmse_per_usm)
      }
    },

    gen_global_stats = function() {
      exclude <- c("version", "species", private$config$var2exclude)

      splited_sim <- CroPlotR::split_df2sim(
        private$workspace$get_sim(
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

      stats <- run_with_log_control(
        summary_method(splited_sim, obs = splited_obs)
      )
      private$logger$info("Saving global statistics")
      private$workspace$save_global_stats(stats)
    },

    gen_species_deteriorated_usm = function(species) {
      ref_workspace <- private$workspace$with_version(
        private$config$reference_version
      )

      for (spec in species) {
        private$logger$info("Reading reference RMSE per USM for species ", spec)
        ref_stats <- ref_workspace$get_rmse_per_usm(
          spec,
          usms = private$config$usms,
          var2exclude = private$config$var2exclude
        )

        if (is.null(ref_stats)) next

        stats <- private$workspace$get_rmse_per_usm(
          spec,
          usms = private$config$usms,
          var2exclude = private$config$var2exclude
        )
        private$logger$info("Comparing RMSE per usm for species ", spec)
        deteriorated_usm <- DeterioratedUSMComparison$new(
          species = spec,
          ref_stats = ref_stats,
          eval_stats = stats,
          percentage = private$config$percentage
        )

        if (is.null(deteriorated_usm$get_data())) next

        private$logger$info("Saving deteriorated USM for species ", spec)
        private$workspace$save_deteriorated_usm(deteriorated_usm)
        private$logger$info("Deteriorated USM saved for species ", spec)
      }
    },

    gen_species_comparison = function(species) {
      ref_workspace <- private$workspace$with_version(
        private$config$reference_version
      )
      for (spec in species) {
        ref_stats <- ref_workspace$get_stats(spec)
        if (is.null(ref_stats)) {
          next
        }
        private$logger$info("Reading stats file for species ", spec)
        stats <- private$workspace$get_stats(spec)
        if (is.null(stats)) {
          next
        }
        private$logger$info("Comparing RMSE for species ", spec)
        comparison <- RmseComparison$new(
          species = spec,
          ref_stats = ref_stats,
          eval_stats = stats,
          percentage = private$config$percentage
        )
        private$logger$info("Saving RMSE comparison for species ", spec)
        private$workspace$save_species_comparison(comparison)
        private$logger$info("Species comparison saved for species ", spec)
      }
    },

    gen_global_comparison = function() {
      ref_workspace <- private$workspace$with_version(
        private$config$reference_version
      )
      ref_stats <- ref_workspace$get_global_stats()
      if (is.null(ref_stats)) {
        return(invisible(NULL))
      }
      private$logger$info("Reading global stats file")
      stats <- private$workspace$get_global_stats()
      if (is.null(stats)) {
        return(invisible(NULL))
      }
      private$logger$info("Comparing global RMSE")
      comparison <- RmseComparison$new(
        ref_stats = ref_stats,
        eval_stats = stats,
        percentage = private$config$percentage
      )
      private$logger$info("Saving global RMSE comparison")
      private$workspace$save_global_comparison(comparison)
      private$logger$info("Global comparison saved")
    },

    evaluate_species = function(species) {
      private$logger$info("Generating stats for species.")
      private$gen_species_stats(species)

      if (is.null(private$config$reference_version)) {
        private$logger$info(
          "No reference version defined. ",
          "Skipping deteriorated usm generation and comparison"
        )
        return()
      }

      private$logger$info("Computing deteriorated USM for species.")
      private$gen_species_deteriorated_usm(species)

      private$logger$info("Computing species comparison.")
      private$gen_species_comparison(species)
    },

    evaluate_global = function() {
      private$logger$info("Generating global stats.")
      private$gen_global_stats()
      if (!is.null(private$config$reference_version)) {
        private$logger$info(
          "Reference version defined: ", private$config$reference_version,
          ". Starting deteriorated USM generation and comparison..."
        )
        private$gen_global_comparison()
      } else {
        private$logger$info(
          "No reference version defined. ",
          "Skipping global deteriorated usm generation and comparison"
        )
      }
    },

    build_summary = function(species) {
      private$summary_class$new(
        workspace = private$workspace,
        species = species,
        percentage = private$config$percentage
      )$display()
    },

    init_workspace = function() {
      logger::log_info(
        "Initializing workspace {private$config$eval_workspace}
        for evaluation..."
      )
      if (!dir.exists(private$config$eval_workspace) &&
          !dir.create(private$config$eval_workspace, recursive = TRUE)
      ) {
        stop("Can't create evaluation workspace", call. = FALSE)
      }
      USMSWorkspace$new(
        workspace = private$workspace,
        backend = private$backend,
        config = private$config
      )$load()
    }
  ),

  public = list(
    #' @description
    #' Create an evaluation workflow
    #'
    #' @param config the configuration of the evaluation workflow
    #' @param workspace an object of class `EvalWorkspace` to access the
    #' evaluation data (default: `EvalWorkspace$new(config$eval_workspace)`)
    #' @param backend an object of class `ParallelBackend` to run parallel
    #' computations (default:
    #' `ParallelBackend$new(config$parallel, config$cores)`)
    #' @param logger a logger object with `info`, `debug`, `warn` and `error`
    #' methods (default: uses the logger package)
    #' @param summary_class a class to build the summary of the evaluation
    #'  (default: `EvaluationSummary`)
    initialize = function(
      config, workspace = NULL, backend = NULL, logger = default_logger,
      summary_class = EvaluationSummary
    ) {
      config$validate_eval()
      private$config <- config
      private$backend <- backend %||% ParallelBackend$new(
        config$parallel, config$cores
      )
      private$workspace <- workspace %||% EvalWorkspace$new(
        config$eval_workspace
      )
      private$logger <- logger
      private$summary_class <- summary_class
    },

    #' @description
    #' Run the evaluation workflow
    #' This function orchestrates the full evaluation workflow based on a given
    #' configuration object. It initializes logging, optionally prepares the
    #' evaluation workspace, runs the evaluation globally and then for all
    #' species, and displays summary information.
    run = function() {
      on.exit({
        private$workspace$cleanup()
        end_time <- Sys.time()
        private$logger$info(
          "Evaluation time: ",
          format_duration(start_time, end_time)
        )
      }, add = TRUE)
      start_time <- Sys.time()
      tryCatch({
        private$logger$info("Starting evaluation...")

        private$init_workspace()

        private$evaluate_global()

        species <- private$get_species_to_evaluate()

        if (length(species) == 0) {
          private$logger$info("No species found to evaluate in the workspace.")
          return(invisible(NULL))
        }

        private$logger$info(
          "Found ", length(species), " species in workspace ",
          private$config$eval_workspace, ": ", format_species(species)
        )

        private$evaluate_species(species)
        private$build_summary(species)
      }, error = function(e) {
        private$logger$error(conditionMessage(e))
        private$logger$debug(
          paste(capture.output(rlang::last_trace()), collapse = "\n")
        )
        rlang::abort(conditionMessage(e), parent = e)
      })
    }
  )
)
