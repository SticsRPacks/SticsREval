#' SpeciesEvaluation class
#'
#' @description
#' Internal class. Evaluates the performance of a model across different
#' species: computes statistics, generates comparisons, and produces
#' reports for each species. Instantiated internally by
#' \code{\link{evaluate}}.
#'
#' @details
#' The class uses a backend for parallel processing and a workspace to
#' access simulation and observation data. It provides methods to run
#' evaluations, summarize results, and export findings.
#'
#' @keywords internal
SpeciesEvaluation <- R6::R6Class("SpeciesEvaluation", # nolint: object_name_linter

  private = list(
    species = NULL,
    usms = NULL,
    var2exclude = NULL,
    percentage = NULL,
    output_dir = NULL,
    eval_workspace = NULL,
    backend = NULL,
    workspace = NULL,
    logger = NULL,
    rrmse_comparisons = list(),
    stats = list(),
    rrmse_per_usm = list(),

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

    gen_species_comparison = function(species) {
      private$logger$info(
        "Generating stats species comparison for ",
        length(species),
        " species."
      )
      all_species_stats <- private$gen_species_stats(species)
      for (spec in species) {
        private$logger$info("Reading stats for species ", spec)
        stats <- Find(function(x) x$species == spec, all_species_stats)
        if (is.null(stats)) {
          next
        }
        private$stats[[spec]] <- stats$stats
        private$rrmse_per_usm[[spec]] <- stats$rrmse_per_usm
        if (!has_reference_stats(stats$stats)) {
          private$logger$warn(
            "No reference simulation data for species ", spec,
            ", skipping rRMSE comparison"
          )
          next
        }
        private$logger$info("Comparing rRMSE for species ", spec)
        comparison <- RRmseComparison$new(
          species = spec,
          stats = stats$stats,
          percentage = private$percentage
        )
        private$rrmse_comparisons[[spec]] <- comparison
        private$logger$info(
          "Species comparison for species ", spec, " generated"
        )
      }
    },

    filter_species_config = function(species) {
      if (is.null(private$species)) return(species)
      intersect(species, private$species)
    },

    filter_species_usms = function(species) {
      if (is.null(private$usms)) return(species)

      species <- species[
        vapply(species, function(sp) {
          private$logger$debug(sprintf("Checking USMs for species %s...", sp))
          species_usms <- private$workspace$get_species_situations(
            sp, private$usms
          )
          private$logger$debug(sprintf(
            "Species %s has USMs: %s",
            sp, toString(unique(species_usms$situation))
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
          eval_data <- read_split_eval_data(
            private$workspace, spec, usms = private$usms,
            var2exclude = private$var2exclude
          )
          if (is.null(eval_data$sim) || is.null(eval_data$obs)) {
            private$logger$warn(
              "No simulation or observation data for species ", spec,
              ", skipping it"
            )
            return(NULL)
          }

          private$logger$info("Generating statistics for ", spec)
          stats <- compute_eval_stats(eval_data)
          rrmse_per_usm <- compute_eval_stats(
            eval_data,
            all_situations = FALSE, stats = c("RMSE", "rRMSE", "n_obs")
          )
          rm(eval_data)
          gc()
          list(species = spec, stats = stats, rrmse_per_usm = rrmse_per_usm)
        }
      )
      results <- Filter(Negate(is.null), results)
      results
    },

    # Species-level degradation status ("Critical"/"Warning"/"OK"), used by
    # both `summary()` (console report) and `export()` (`species_summary
    # .csv`).
    species_status_table = function() {
      comparisons <- Filter(Negate(is.null), private$rrmse_comparisons)
      if (length(comparisons) == 0) {
        return(data.frame(
          species = character(0), status = character(0),
          stringsAsFactors = FALSE
        ))
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

      data.frame(
        species = c(all_crit, all_warn, all_ok),
        status = c(
          rep("Critical", length(all_crit)),
          rep("Warning", length(all_warn)),
          rep("OK", length(all_ok))
        ),
        stringsAsFactors = FALSE
      )
    }
  ),

  active = list(
    #' @field success
    #' A logical value indicating whether the species evaluation was successful.
    #' The evaluation is considered successful if all species comparisons were
    #' performed and there are no critical variables identified in any of the
    #' comparisons.
    success = function() {
      comparisons <- Filter(Negate(is.null), private$rrmse_comparisons)
      all_crit <- unique(unlist(lapply(comparisons, function(c) {
        if (length(c$critical_vars) > 0) c$get_data()$species[1]
      })))
      length(all_crit) == 0
    },

    #' @field skip_reason
    #' Why no species rRMSE comparison was done (e.g. \code{"no reference
    #' data"}), or NULL if at least one was done.
    skip_reason = function() {
      comparisons <- Filter(Negate(is.null), private$rrmse_comparisons)
      if (length(comparisons) > 0) return(NULL)
      if (length(private$stats) > 0) "no reference data" else "no data to evaluate" # nolint: line_length_linter
    }
  ),

  public = list(
    #' @description
    #' Create a new SpeciesEvaluation object.
    #' @param eval_workspace Path to the evaluation workspace. Only used to
    #' build a default `workspace` when one isn't supplied.
    #' @param species Optional character vector of species to evaluate.
    #' @param usms Optional character vector of USMs to evaluate.
    #' @param var2exclude Optional character vector of variables to exclude.
    #' @param percentage Threshold (%) above which a variable is flagged as
    #' deteriorated vs. the reference.
    #' @param output_dir Output directory for CSV/plot exports.
    #' @param parallel,cores Parallel execution options. Only used to build
    #' a default `backend` when one isn't supplied.
    #' @param workspace An optional EvalWorkspace object. If not provided, a
    #' new EvalWorkspace will be created from `eval_workspace`.
    #' @param backend An optional ParallelBackend object for parallel
    #' processing. If not provided, a new ParallelBackend will be created
    #' from `parallel`/`cores`.
    #' @param logger An optional logger object for logging messages. If not
    #' provided, the default logger will be used.
    initialize = function(
      eval_workspace = NULL,
      species = NULL,
      usms = NULL,
      var2exclude = NULL,
      percentage = 5,
      output_dir = NULL,
      parallel = FALSE,
      cores = NA,
      workspace = NULL,
      backend = NULL,
      logger = default_logger
    ) {
      private$eval_workspace <- eval_workspace
      private$species <- species
      private$usms <- usms
      private$var2exclude <- var2exclude
      private$percentage <- percentage
      private$output_dir <- output_dir
      private$backend <- backend %||% ParallelBackend$new(parallel, cores)
      private$workspace <- workspace %||% EvalWorkspace$new(eval_workspace)
      private$logger <- logger
    },

    #' @description
    #' Return the CroPlotR statistics already computed by `run()` for a
    #' given species, so that other evaluation classes (e.g.
    #' \code{USMEvaluation}) can reuse them instead of recomputing the same
    #' summary statistics from scratch.
    #' @param species The species to get statistics for.
    #' @returns A list with elements \code{stats} (species-level summary,
    #' \code{all_situations = TRUE}) and \code{stats_usm} (per-USM summary
    #' with \code{RMSE}, \code{rRMSE} and \code{n_obs}). Elements are
    #' \code{NULL} if \code{run()} has not been called yet or produced no
    #' result for this species.
    get_species_stats = function(species) {
      list(
        stats = private$stats[[species]],
        stats_usm = private$rrmse_per_usm[[species]]
      )
    },

    #' @description
    #' Run the species evaluation.
    #' This method performs the species evaluation by generating statistics and
    #' comparisons for each species based on the provided configuration and
    #' workspace.
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
          private$eval_workspace, ": ", format_species(species)
        )

        private$logger$info("Computing species comparison.")
        private$gen_species_comparison(species)
      }, error = function(e) {
        private$logger$error(conditionMessage(e))
        private$logger$debug(
          paste(capture.output(rlang::last_trace()), collapse = "\n")
        )
        rlang::abort(conditionMessage(e), parent = e)
      })
    },

    #' @description
    #' Print a summary of the species evaluation results.
    summary = function() {
      comparisons <- Filter(Negate(is.null), private$rrmse_comparisons)
      cli::cli_h1("Species comparisons")
      if (length(comparisons) == 0) {
        cli::cli_alert_warning("No comparison done ({self$skip_reason}).")
        return(invisible(self))
      }

      status <- private$species_status_table()
      all_crit <- status$species[status$status == "Critical"]
      all_warn <- status$species[status$status == "Warning"]
      all_ok <- status$species[status$status == "OK"]

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
      no_ref <- setdiff(names(private$stats), names(comparisons))
      if (length(no_ref) > 0) {
        cli::cli_li(
          "{.strong No reference data} (not compared): {format_species(no_ref)}"
        )
      }
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

      cli::cli_rule()
      invisible(self)
    },

    #' @description
    #' Export the species evaluation results to CSV files and plots.
    #' This method exports the statistics, rRMSE comparisons (including the
    #' species-level rRMSE ratio per variable, in `species_rrmse_comparison
    #' .csv`), and a per-species degradation status summary (`species_summary
    #' .csv`) to CSV files in the "csv" subdirectory. It also generates a
    #' static rRMSE comparison plot and interactive per-variable scatter
    #' plots for each species, saved in their own subdirectory under "plots"
    #' (one per species).
    export = function() {
      private$logger$info("Exporting species evaluation data")
      plots_dir <- file.path(private$output_dir, "plots")
      dir.create(
        plots_dir,
        recursive = TRUE,
        showWarnings = FALSE
      )
      private$backend$run(
        length(names(private$rrmse_comparisons)),
        function(i) {
          spec <- names(private$rrmse_comparisons)[i]
          comp <- private$rrmse_comparisons[[spec]]
          if (is.null(comp) || comp$is_empty) return()

          spec_plots_dir <- file.path(plots_dir, spec)
          dir.create(spec_plots_dir, recursive = TRUE, showWarnings = FALSE)

          comp$plot_comparison(
            file.path(spec_plots_dir, "species_comparison.png")
          )
          deteriorated <- c(
            comp$critical_vars,
            comp$warning_vars
          )
          if (length(deteriorated) > 0) {
            spec_usms <- private$workspace$get_species_situations(spec)
            eval_data <- read_split_eval_data(
              private$workspace, spec, usms = private$usms,
              var2exclude = private$var2exclude
            )
            gen_scatter_plot(
              spec_plots_dir,
              eval_data$sim[spec_usms$situation],
              Filter(Negate(is.null), eval_data$obs[spec_usms$situation]),
              eval_data$ref_sim[spec_usms$situation],
              deteriorated
            )
          }

        }
      )
      safe_write_csv(
        dplyr::bind_rows(private$stats, .id = "species"),
        csv_output_path(private$output_dir, "species_stats.csv")
      )
      safe_write_csv(
        dplyr::bind_rows(private$rrmse_per_usm, .id = "species"),
        csv_output_path(private$output_dir, "rRMSE_per_usm.csv")
      )
      safe_write_csv(
        dplyr::bind_rows(
          lapply(private$rrmse_comparisons, function(comp) comp$get_data())
        ),
        csv_output_path(private$output_dir, "species_rrmse_comparison.csv")
      )
      safe_write_csv(
        private$species_status_table(),
        csv_output_path(private$output_dir, "species_summary.csv")
      )
      private$logger$info("Species evaluation export done")
    }
  )
)
