GlobalEvaluation <- R6::R6Class("GlobalEvaluation", # nolint: object_name_linter
  private = list(
    config = NULL,
    backend = NULL,
    workspace = NULL,
    logger = NULL,

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
      private$logger$info("Comparing global rRMSE")
      comparison <- RRmseComparison$new(
        ref_stats = ref_stats,
        eval_stats = stats,
        percentage = private$config$percentage
      )
      private$logger$info("Saving global rRMSE comparison")
      private$workspace$save_global_comparison(comparison)
      private$logger$info("Global comparison saved")
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
    }
  ),

  active = list(
    success = function() {
      global_comparison <- private$workspace$get_global_comparison(
        private$config$percentage
      )
      length(global_comparison$critical_vars) == 0
    }
  ),

  public = list(
    initialize = function(
      config, workspace = NULL, backend = NULL, logger = default_logger
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
      global_comparison <- private$workspace$get_global_comparison(
        private$config$percentage
      )
      cli::cli_h1("Global comparison")

      if (is.null(global_comparison)) {
        cli::cli_alert_warning("No comparison done.")
        return(invisible(self))
      }
      global_comparison$log()
    }
  )

)

SpeciesEvaluation <- R6::R6Class("SpeciesEvaluation", # nolint: object_name_linter

  private = list(
    config = NULL,
    backend = NULL,
    workspace = NULL,
    logger = NULL,

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

    gen_species_deteriorated_usm = function(species) {
      ref_workspace <- private$workspace$with_version(
        private$config$reference_version
      )

      for (spec in species) {
        private$logger$info(
          "Reading reference rRMSE per USM for species ",
          spec
        )
        ref_stats <- ref_workspace$get_rrmse_per_usm(
          spec,
          usms = private$config$usms,
          var2exclude = private$config$var2exclude
        )

        if (is.null(ref_stats)) next

        stats <- private$workspace$get_rrmse_per_usm(
          spec,
          usms = private$config$usms,
          var2exclude = private$config$var2exclude
        )
        private$logger$info("Comparing rRMSE per usm for species ", spec)
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
        private$logger$info("Comparing rRMSE for species ", spec)
        comparison <- RRmseComparison$new(
          species = spec,
          ref_stats = ref_stats,
          eval_stats = stats,
          percentage = private$config$percentage
        )
        private$logger$info("Saving rRMSE comparison for species ", spec)
        private$workspace$save_species_comparison(comparison)
        private$logger$info("Species comparison saved for species ", spec)
      }
    },

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
          rrmse_per_usm <- run_with_log_control(
            summary_method(
              splited_sim, obs = splited_obs,
              all_situations = FALSE, stats = c("rRMSE", "n_obs")
            )
          )
          list(species = spec, stats = stats, rrmse_per_usm = rrmse_per_usm)
        }
      )

      for (res in results) {
        private$logger$info("Saving statistics for ", res$species)
        private$workspace$save_stats(res$species, res$stats)
        private$logger$info("Saving rRMSE per USM for species ", res$species)
        private$workspace$save_rrmse_per_usm(res$species, res$rrmse_per_usm)
      }
    }
  ),

  active = list(
    success = function() {
      species <- private$get_species_to_evaluate()
      comparisons <- lapply(
        species,
        private$workspace$get_species_comparison,
        private$config$percentage
      )
      comparisons <- Filter(Negate(is.null), comparisons)
      all_crit <- unique(unlist(lapply(comparisons, function(c) {
        if (length(c$critical_vars) > 0) c$get_data()$species[1]
      })))
      length(all_crit) == 0
    }
  ),

  public = list(
    initialize = function(
      config, workspace = NULL, backend = NULL, logger = default_logger
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
    },

    run = function() {
      on.exit({
        end_time <- Sys.time()
        private$logger$info(
          "Species evaluation time: ",
          format_duration(start_time, end_time)
        )
      }, add = TRUE)
      start_time <- Sys.time()
      tryCatch({
        private$logger$info("Starting species evaluation...")

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
      }, error = function(e) {
        private$logger$error(conditionMessage(e))
        private$logger$debug(
          paste(capture.output(rlang::last_trace()), collapse = "\n")
        )
        rlang::abort(conditionMessage(e), parent = e)
      })
    },

    summary = function() {
      species <- private$get_species_to_evaluate()
      comparisons <- lapply(
        species,
        private$workspace$get_species_comparison,
        private$config$percentage
      )
      comparisons <- Filter(Negate(is.null), comparisons)

      cli::cli_h1("Species comparisons")
      if (length(comparisons) == 0) {
        cli::cli_alert_warning("No comparison done.")
        return(invisible(self))
      }

      all_crit <- unique(unlist(lapply(comparisons, function(c) {
        if (length(c$critical_vars) > 0) c$get_data()$species[1]
      })))
      all_warn <- unique(unlist(lapply(comparisons, function(c) {
        if (length(c$warning_vars) > 0) c$get_data()$species[1]
      })))
      all_warn <- setdiff(all_warn, all_crit)
      all_ok <- unique(unlist(lapply(comparisons, function(c) {
        if (length(c$critical_vars) == 0 && length(c$warning_vars) == 0) {
          c$get_data()$species[1]
        }
      })))

      for (comp in comparisons) comp$log()

      cli::cli_h2("Summary")
      cli::cli_text("The following species show at least one variable with:")
      cli::cli_ul()
      cli::cli_li(
        "{.strong Major degradation} (> {private$percentage}% rRMSE increase):
        {format_species(all_crit)}"
      )
      cli::cli_li(
        "{.strong Minor degradation} (<= {private$percentage}% rRMSE increase):
        {format_species(all_warn)}"
      )
      cli::cli_li(
        "{.strong No degradation} (rRMSE stable or improved):
        {format_species(all_ok)}"
      )
      cli::cli_end()

      if (length(all_crit) > 0) {
        cli::cli_alert_danger(
          "Found at least one critical deteriorated variable"
        )
      } else if (length(all_warn) > 0) {
        cli::cli_alert_warning("Found at least one deteriorated variable")
      } else {
        cli::cli_alert_success("All species stable or improved")
      }
      invisible(self)
    }
  )
)

#' Run All Evaluations
#'
#' Initializes the evaluation workspace, runs global and species evaluations,
#' displays a summary of results, and stops with an error if any test failed.
#'
#' @param config A list containing the evaluation configuration. Must include at
#'   least the following element:
#'   \describe{
#'     \item{eval_workspace}{Character. Path to the directory used as evaluation
#'       workspace. Created recursively if it does not already exist.}
#'   }
#'
#' @return Invisibly \code{NULL}. Called for its side effects: workspace
#'   creation, evaluation runs, and console reporting.
#'
#' @details
#' The function proceeds in the following steps:
#' \enumerate{
#'   \item Creates \code{config$eval_workspace} if it does not exist, and stops
#'     with an error if creation fails.
#'   \item Loads the USMS workspace via \code{USMSWorkspace$new(config)$load()}.
#'   \item Instantiates and runs a \code{GlobalEvaluation} then a
#'     \code{SpeciesEvaluation}.
#'   \item Prints their respective summaries.
#'   \item Displays a CLI report listing each evaluation as
#'     \strong{success} (green ✔) or \strong{failed} (red ✗).
#'   \item Stops with an error if at least one evaluation did not succeed.
#' }
#'
#' @export
run_all_evaluations <- function(config) {
  logger::log_info(
    "Initializing workspace {config$eval_workspace}
    for evaluation..."
  )
  if (!dir.exists(config$eval_workspace) &&
      !dir.create(config$eval_workspace, recursive = TRUE)
  ) {
    stop("Can't create evaluation workspace", call. = FALSE)
  }
  USMSWorkspace$new(config = config)$load()
  global_eval <- GlobalEvaluation$new(config = config)
  global_eval$run()
  species_eval <- SpeciesEvaluation$new(config = config)
  species_eval$run()
  global_eval$summary()
  species_eval$summary()

  ok  <- paste(cli::col_green(cli::symbol$tick), cli::col_green("success"))
  nok <- paste(cli::col_red(cli::symbol$cross),  cli::col_red("failed"))

  cli::cli_h1("Tests results")
  cli::cli_ul()
  cli::cli_li("Global evaluation: {if (global_eval$success) ok else nok}")
  cli::cli_li("Species evaluation: {if (species_eval$success) ok else nok}")
  cli::cli_end()
  if (!global_eval$success || !species_eval$success) {
    stop("At least one test failed, see details above.", call. = FALSE)
  }
}
