#' SpeciesEvaluation class
#'
#' @description
#' The \code{SpeciesEvaluation} class is responsible for evaluating the
#' performance of a model across different species. It computes statistics,
#' generates comparisons, and produces reports for each species based on the
#' provided configuration and workspace.
#'
#' @details
#' The class uses a backend for parallel processing and a workspace to
#' access simulation and observation data. It provides methods to run
#' evaluations, summarize results, and export findings.
#'
#' @examples
#' \dontrun{
#' config <- list(
#'    eval_workspace = "path/to/eval_workspace",
#'    usms = c("USM1", "USM2"),
#'    species = c("Species1", "Species2"),
#'    var2exclude = c("var1", "var2"),
#'    percentage = 10,
#'    parallel = TRUE,
#'    cores = 4,
#'    output_dir = "path/to/output"
#' )
#' species_eval <- SpeciesEvaluation$new(config = config)
#' species_eval$run()
#' }
#' @export
SpeciesEvaluation <- R6::R6Class("SpeciesEvaluation", # nolint: object_name_linter

  private = list(
    config = NULL,
    backend = NULL,
    workspace = NULL,
    logger = NULL,
    rrmse_comparisons = list(),
    deteriorated_usm = list(),
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
        private$logger$info("Comparing rRMSE for species ", spec)
        comparison <- RRmseComparison$new(
          species = spec,
          stats = stats$stats,
          percentage = private$config$percentage
        )
        private$rrmse_comparisons[[spec]] <- comparison
        private$logger$info(
          "Species comparison for species ", spec, " generated"
        )

        private$logger$info("Comparing rRMSE per usm for species ", spec)
        deteriorated_usm <- DeterioratedUSMComparison$new(
          species = spec,
          stats = stats$rrmse_per_usm,
          percentage = private$config$percentage
        )
        if (is.null(deteriorated_usm$get_data())) next
        private$deteriorated_usm[[spec]] <- deteriorated_usm
        private$logger$info("Deteriorated USM for species ", spec, " generated")
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
          species_usms <- private$workspace$get_species_situations(
            sp, private$config$usms
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

          exclude <- c("version", "species", private$config$var2exclude)

          splited_sim <- CroPlotR::split_df2sim(
            private$workspace$get_sim(
              spec, usms = private$config$usms, var2exclude = exclude
            )
          )
          splited_ref_sim <- CroPlotR::split_df2sim(
            private$workspace$get_ref_sim(
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
            summary_method(
              evaluated = splited_sim,
              reference = splited_ref_sim,
              obs = splited_obs
            )
          )
          rrmse_per_usm <- run_with_log_control(
            summary_method(
              evaluated = splited_sim,
              reference = splited_ref_sim,
              obs = splited_obs,
              all_situations = FALSE, stats = c("rRMSE", "n_obs")
            )
          )
          rm(splited_sim, splited_ref_sim, splited_obs)
          gc()
          list(species = spec, stats = stats, rrmse_per_usm = rrmse_per_usm)
        }
      )
      results <- Filter(Negate(is.null), results)
      results
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
    }
  ),

  public = list(
    #' @description
    #' Create a new SpeciesEvaluation object.
    #' @param config A Configuration object containing the necessary parameters
    #' for the evaluation.
    #' @param workspace An optional EvalWorkspace object. If not provided, a new
    #' EvalWorkspace will be created using the provided configuration.
    #' @param backend An optional ParallelBackend object for parallel
    #' processing.
    #' If not provided, a new ParallelBackend will be created using the provided
    #' configuration.
    #' @param logger An optional logger object for logging messages. If not
    #' provided, the default logger will be used.
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
          private$config$eval_workspace, ": ", format_species(species)
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
    },

    #' @description
    #' Export the species evaluation results to CSV files and plots.
    #' This method exports the statistics, rRMSE comparisons, and deteriorated
    #' USM data to CSV files in the specified output directory. It also
    #' generates plots for each species and saves them in the "plots"
    #' subdirectory.
    export = function() {
      private$logger$info("Exporting species evaluation data")
      plots_dir <- file.path(private$config$output_dir, "plots")
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
          if (is.null(comp$get_data())) return()

          comp$plot_comparison(
            file.path(
              plots_dir,
              paste0(spec, "_species_comparison.png")
            )
          )
          deteriorated <- c(
            comp$critical_vars,
            comp$warning_vars
          )
          if (length(deteriorated) > 0) {
            spec_usms <- private$workspace$get_species_situations(spec)
            var2exclude <- c("version", "species", private$config$var2exclude)
            sim <- CroPlotR::split_df2sim(
              private$workspace$get_sim(
                spec, usms = private$config$usms, var2exclude = var2exclude
              )
            )
            ref_sim <- CroPlotR::split_df2sim(
              private$workspace$get_ref_sim(
                spec, usms = private$config$usms, var2exclude = var2exclude
              )
            )
            obs <- CroPlotR::split_df2sim(
              private$workspace$get_obs(
                spec, usms = private$config$usms, var2exclude = var2exclude
              )
            )
            gen_scatter_plot(
              file.path(plots_dir, paste0(spec, "_scatter_plots.html")),
              sim[spec_usms$situation],
              Filter(Negate(is.null), obs[spec_usms$situation]),
              ref_sim[spec_usms$situation],
              deteriorated
            )
          }

        }
      )
      safe_write_csv(
        dplyr::bind_rows(private$stats, .id = "species"),
        file.path(private$config$output_dir, "species_stats.csv")
      )
      safe_write_csv(
        dplyr::bind_rows(private$rrmse_per_usm, .id = "species"),
        file.path(private$config$output_dir, "rRMSE_per_usm.csv")
      )
      safe_write_csv(
        do.call(rbind, lapply(private$deteriorated_usm, function(x) {
          if (!is.null(x$get_data())) x$get_data()
        })),
        file.path(private$config$output_dir, "Deteriorated_USM.csv")
      )
      private$logger$info("Species evaluation export done")
    }
  )
)
