#' USMEvaluation class
#'
#' @description
#' The \code{USMEvaluation} class detects USMs (situations) where the model
#' performance on a given variable is very degraded compared to a reference
#' version, relative to the overall performance of the species on that
#' variable.
#'
#' @details
#' For each variable and each USM, the following ratio is computed:
#'
#' \deqn{RMSE\_ratio = 100 \times
#' \frac{RMSE_{eval} - RMSE_{ref}}{RMSE_{species}}}
#'
#' where:
#' \itemize{
#'   \item \code{RMSE_eval}: RMSE of the evaluated version, for the variable,
#'   on that USM.
#'   \item \code{RMSE_ref}: RMSE of the reference version, for the variable,
#'   on that USM.
#'   \item \code{RMSE_species}: RMSE of the evaluated version, for the
#'   variable, computed on the whole species (all USMs together).
#' }
#'
#' A USM is considered as **not passing the test** if, for at least one
#' variable:
#' \itemize{
#'   \item \code{RMSE_ratio > ratio_threshold} (default 50%), OR
#'   \item more than \code{max_degraded_vars} (default 3) variables have
#'   \code{RMSE_ratio > degraded_threshold} (default 20%) on that USM.
#' }
#'
#' USMs/variables with less than 10 observations (\code{n_obs}) are ignored,
#' consistently with \code{RRmseComparison}/\code{DeterioratedUSMComparison}.
#'
#' The class can be used in two ways:
#' \itemize{
#'   \item Driven by a \code{Configuration} + workspace, like
#'   \code{SpeciesEvaluation}: create with \code{config} (and optionally
#'   \code{workspace}/\code{backend}/\code{logger}), then call \code{run()}.
#'   \item Manually, by directly providing \code{stats_usm} and
#'   \code{stats_species} (or a pre-computed \code{data}) for a single
#'   species, useful for unit testing.
#' }
#'
#' @examples
#' \dontrun{
#' # Driven by config/workspace, like SpeciesEvaluation
#' config <- list(
#'   eval_workspace = "path/to/eval_workspace",
#'   usms = c("USM1", "USM2"),
#'   species = c("Species1", "Species2"),
#'   var2exclude = c("var1", "var2"),
#'   parallel = TRUE,
#'   cores = 4,
#'   output_dir = "path/to/output"
#' )
#' usm_eval <- USMEvaluation$new(config = config)
#' usm_eval$run()
#' usm_eval$summary()
#' usm_eval$get_data()
#' usm_eval$failed_usms
#'
#' # Manual mode (unit tests)
#' usm_eval <- USMEvaluation$new(
#'   species = "Species1",
#'   stats_usm = stats_usm,
#'   stats_species = stats_species
#' )
#' usm_eval$summary()
#' }
#' @export
USMEvaluation <- R6::R6Class("USMEvaluation", # nolint: object_name_linter

  private = list(
    config = NULL,
    backend = NULL,
    workspace = NULL,
    logger = NULL,
    data = list(),
    deteriorated_usm = list(),
    ratio_threshold = NULL,
    degraded_threshold = NULL,
    max_degraded_vars = NULL,

    compute_ratio = function(species, stats_usm, stats_species) {
      ref_usm <- dplyr::filter(stats_usm, .data$group == "reference")
      eval_usm <- dplyr::filter(stats_usm, .data$group == "evaluated")

      species_rmse <- stats_species |>
        dplyr::filter(.data$group == "evaluated") |>
        dplyr::mutate(rmse_species = as.numeric(.data$RMSE)) |>
        dplyr::select("variable", "rmse_species")

      res <- eval_usm |>
        dplyr::left_join(ref_usm, by = c("situation", "variable")) |>
        dplyr::mutate(
          rmse_eval = as.numeric(.data$RMSE.x),
          rmse_ref = as.numeric(.data$RMSE.y),
          n_obs = as.numeric(.data$n_obs.y)
        ) |>
        dplyr::left_join(species_rmse, by = "variable") |>
        dplyr::filter(
          is.finite(.data$rmse_eval), is.finite(.data$rmse_ref),
          is.finite(.data$rmse_species), .data$rmse_species != 0,
          !is.na(.data$variable), !is.na(.data$situation)
        ) |>
        dplyr::mutate(
          rmse_ratio = round(
            100 * (.data$rmse_eval - .data$rmse_ref) / .data$rmse_species, # nolint: line_length_linter
            2
          )
        ) |>
        dplyr::filter(is.finite(.data$rmse_ratio)) |>
        dplyr::select(
          "situation", "variable",
          "rmse_eval", "rmse_ref", "rmse_species",
          "rmse_ratio", "n_obs"
        )

      if (!is.null(species)) {
        res <- dplyr::mutate(res, species = species)
      }
      res |>
        dplyr::arrange(dplyr::desc(.data$rmse_ratio)) |>
        dplyr::collect()
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

    gen_usm_stats = function(species) {
      private$logger$info(
        "Generating RMSE and rRMSE per USM for species ",
        species
      )
      exclude <- c("version", "species", private$config$var2exclude)

      splited_sim <- CroPlotR::split_df2sim(
        private$workspace$get_sim(
          species, usms = private$config$usms, var2exclude = exclude
        )
      )
      splited_ref_sim <- CroPlotR::split_df2sim(
        private$workspace$get_ref_sim(
          species, usms = private$config$usms, var2exclude = exclude
        )
      )
      splited_obs <- CroPlotR::split_df2sim(
        private$workspace$get_obs(
          species, usms = private$config$usms, var2exclude = exclude
        )
      )
      private$logger$info("Generating RMSE statistics for ", species)
      loadNamespace("CroPlotR")
      summary_method <- utils::getS3method("summary", class(splited_sim))

      stats_species <- run_with_log_control(
        summary_method(
          evaluated = splited_sim,
          reference = splited_ref_sim,
          obs = splited_obs
        )
      )
      private$logger$info("Generating RMSE and rRMSE per USM for ", species)
      stats_usm <- run_with_log_control(
        summary_method(
          evaluated = splited_sim,
          reference = splited_ref_sim,
          obs = splited_obs,
          all_situations = FALSE, stats = c("RMSE", "rRMSE", "n_obs")
        )
      )
      rm(splited_obs, splited_sim, splited_ref_sim)
      gc()
      list(
        stats_species = stats_species,
        stats_usm = stats_usm
      )
    },

    gen_usm_comparisons = function(species) {
      private$logger$info(
        "Generating RMSE per USM comparison for ",
        length(species),
        " species."
      )
      results <- private$backend$run(
        length(species),
        function(i) {
          spec <- species[i]
          stats <- private$gen_usm_stats(spec)
          private$logger$info("Comparing rRMSE per usm for species ", spec)
          deteriorated_usm <- DeterioratedUSMComparison$new(
            species = spec,
            stats = stats$stats_usm,
            percentage = private$config$percentage
          )
          ratio <- private$compute_ratio(
            spec, stats$stats_usm, stats$stats_species
          )
          list(
            species = spec,
            deteriorated_usm = deteriorated_usm,
            data = ratio
          )
        }
      )
      results <- Filter(Negate(is.null), results)
      for (res in results) {
        private$data[[res$species]] <- res$data
        if (!is.null(res$deteriorated_usm$get_data())) {
          private$deteriorated_usm[[res$species]] <- res$deteriorated_usm
          private$logger$info(
            "Deteriorated USM for species ", res$species, " generated"
          )
        }
      }
      invisible(NULL)
    }
  ),

  active = list(
    #' @field degraded_vars
    #' Data frame of variable/USM combinations (across all species) where
    #' \code{rmse_ratio} exceeds \code{degraded_threshold} (default 20%).
    degraded_vars = function() {
      self$get_data() |>
        dplyr::filter(
          .data$rmse_ratio > private$degraded_threshold,
          .data$n_obs >= 10
        )
    },

    #' @field critical_vars
    #' Data frame of variable/USM combinations (across all species) where
    #' \code{rmse_ratio} exceeds \code{ratio_threshold} (default 50%).
    critical_vars = function() {
      self$get_data() |>
        dplyr::filter(
          .data$rmse_ratio > private$ratio_threshold,
          .data$n_obs >= 10
        )
    },

    #' @field failed_usms
    #' Data frame with columns \code{species}/\code{situation} for USMs that
    #' do not pass the test: either at least one variable with a ratio above
    #' \code{ratio_threshold}, or more than \code{max_degraded_vars} variables
    #' above \code{degraded_threshold}.
    failed_usms = function() {
      if (self$is_empty) {
        return(
          data.frame(
            species = character(0),
            situation = character(0),
            stringsAsFactors = FALSE
          )
        )
      }

      critical_usms <- dplyr::distinct(
        self$critical_vars, .data$species, .data$situation
      )

      many_degraded_usms <- self$degraded_vars |>
        dplyr::count(.data$species, .data$situation) |>
        dplyr::filter(.data$n > private$max_degraded_vars) |>
        dplyr::distinct(.data$species, .data$situation)

      dplyr::distinct(dplyr::bind_rows(critical_usms, many_degraded_usms))
    },

    #' @field success
    #' TRUE if the USM evaluation is successful, i.e. no USM failed the test
    #' for any species.
    success = function() {
      nrow(self$failed_usms) == 0
    },

    #' @field is_empty
    #' TRUE if there is no data to evaluate.
    is_empty = function() isTRUE(length(private$data) == 0)
  ),

  public = list(
    #' @description
    #' Create a new USMEvaluation object.
    #' @param config A Configuration object containing the necessary
    #' parameters for the evaluation (same as \code{SpeciesEvaluation}).
    #' Required to use \code{run()}.
    #' @param workspace An optional EvalWorkspace object. If not provided, a
    #' new EvalWorkspace will be created using the provided configuration.
    #' @param backend An optional ParallelBackend object for parallel
    #' processing. If not provided, a new ParallelBackend will be created
    #' using the provided configuration.
    #' @param logger An optional logger object for logging messages. If not
    #' provided, the default logger will be used.
    #' @param ratio_threshold Numeric threshold (%) above which a single
    #' variable makes the USM fail. Default 50.
    #' @param degraded_threshold Numeric threshold (%) above which a variable
    #' is considered degraded. Default 20.
    #' @param max_degraded_vars Maximum number of degraded variables (ratio >
    #' \code{degraded_threshold}) tolerated per USM before it is considered
    #' failed. Default 3 (i.e. fails starting at 4 degraded variables).
    #' @param species Optional species name, used in manual mode (ignored if
    #' \code{config} is provided).
    #' @param stats_usm Manual mode: data frame of per-USM stats (RMSE,
    #' n_obs), for \code{evaluated} and \code{reference} groups, e.g. as
    #' returned by CroPlotR's summary method with \code{all_situations =
    #' FALSE, stats = c("RMSE", "n_obs")}.
    #' @param stats_species Manual mode: data frame of species-level stats
    #' (RMSE), e.g. as returned by CroPlotR's summary method with
    #' \code{all_situations = TRUE} (default).
    #' @param data Manual mode: optional pre-computed data frame (bypasses
    #' \code{stats_usm}/\code{stats_species} computation).
    initialize = function(
      config = NULL,
      workspace = NULL,
      backend = NULL,
      logger = default_logger,
      ratio_threshold = 50,
      degraded_threshold = 20,
      max_degraded_vars = 3,
      species = NULL,
      stats_usm = NULL,
      stats_species = NULL,
      data = NULL
    ) {
      private$ratio_threshold <- ratio_threshold
      private$degraded_threshold <- degraded_threshold
      private$max_degraded_vars <- max_degraded_vars

      if (!is.null(config)) {
        config$validate_eval()
        private$config <- config
        private$backend <- backend %||% ParallelBackend$new(
          config$parallel, config$cores
        )
        private$workspace <- workspace %||% EvalWorkspace$new(
          config$eval_workspace
        )
        private$logger <- logger
        return(invisible(self))
      }

      if (!is.null(data)) {
        private$data[[species %||% "1"]] <- data |>
          dplyr::arrange(dplyr::desc(.data$rmse_ratio)) |>
          dplyr::collect()
      } else if (!is.null(stats_usm) && !is.null(stats_species)) {
        private$data[[species %||% "1"]] <- private$compute_ratio(
          species, stats_usm, stats_species
        )
      } else {
        stop(
          "`config`, `data`, or `stats_usm` and `stats_species`",
          "must be defined",
          call. = FALSE
        )
      }
    },

    #' @description
    #' Run the USM evaluation.
    #' This method computes, for each species found in the workspace
    #' (filtered according to the configuration), the RMSE_ratio per variable
    #' and per USM, and stores the results internally.
    run = function() {
      if (is.null(private$config)) {
        stop(
          "`run()` requires the object to be initialized with a `config`",
          call. = FALSE
        )
      }

      on.exit({
        end_time <- Sys.time()
        private$logger$info(
          "USM evaluation time: ",
          format_duration(start_time, end_time)
        )
      }, add = TRUE)
      start_time <- Sys.time()
      tryCatch({
        private$logger$info("Starting USM evaluation...")

        species <- private$get_species_to_evaluate()

        if (length(species) == 0) {
          private$logger$info("No species found to evaluate in the workspace.")
          return(invisible(NULL))
        }

        private$logger$info(
          "Found ", length(species), " species in workspace ",
          private$config$eval_workspace, ": ", format_species(species)
        )

        private$logger$info("Computing RMSE per USM comparison.")
        private$gen_usm_comparisons(species)
      }, error = function(e) {
        private$logger$error(conditionMessage(e))
        private$logger$debug(
          paste(capture.output(rlang::last_trace()), collapse = "\n")
        )
        rlang::abort(conditionMessage(e), parent = e)
      })
    },

    #' @description
    #' Return the underlying data frame (one row per variable/USM/species
    #' combination, with columns: species, situation, variable, rmse_eval,
    #' rmse_ref, rmse_species, ratio, n_obs).
    get_data = function() {
      if (self$is_empty) return(NULL)
      dplyr::bind_rows(private$data)
    },

    #' @description
    #' Export the USM evaluation data for deteriorated USMs to a CSV file.
    #'
    #' The exported file is written to the configured output directory and is
    #' named `Deteriorated_USM.csv`. Only USMs that contain evaluation data are
    #' included in the export.
    export = function() {
      private$logger$info("Exporting USM evaluation data")

      deteriorated_data <- do.call(rbind, lapply(private$deteriorated_usm, function(x) { # nolint
        if (!is.null(x$get_data())) x$get_data()
      }))

      usm_eval_data <- self$get_data()

      merged_data <- dplyr::left_join(
        deteriorated_data,
        usm_eval_data,
        by = c("species", "situation", "variable")
      )

      safe_write_csv(
        merged_data,
        file.path(private$config$output_dir, "Deteriorated_USM.csv")
      )
    },

    #' @description
    #' Print a summary of the USM evaluation results, species by species.
    summary = function() {
      cli::cli_h1("USM comparisons (RMSE_ratio)")
      if (self$is_empty) {
        cli::cli_alert_warning("No USM evaluation data.")
        return(invisible(self))
      }

      failed <- self$failed_usms
      all_species <- names(private$data)
      failed_species <- unique(failed$species)
      passed_species <- setdiff(all_species, failed_species)

      for (spec in all_species) {
        spec_data <- private$data[[spec]]
        spec_failed <- failed |>
          dplyr::filter(.data$species == spec) |>
          dplyr::pull("situation")
        all_usms <- unique(spec_data$situation)
        spec_passed <- setdiff(all_usms, spec_failed)

        cli::cli_rule(left = "Species: {spec}")
        cli::cli_dl(c("Total USMs" = "{length(all_usms)}"))
        cli::cli_li("{.strong {length(spec_failed)} failed}")
        if (length(spec_failed) > 0) {
          cli::cli_text(cli::col_red("  {toString(spec_failed)}"))
        }
        cli::cli_li("{.strong {length(spec_passed)} passed}")
        if (length(spec_passed) > 0) {
          cli::cli_text(cli::col_green("  {toString(spec_passed)}"))
        }
      }

      cli::cli_h2("Summary")
      cli::cli_text("The following species have at least one failed USM:")
      cli::cli_ul()
      cli::cli_li(
        "{.strong Failed} (RMSE_ratio > {private$ratio_threshold}%, or more
        than {private$max_degraded_vars} variables >
        {private$degraded_threshold}%): {format_species(failed_species)}"
      )
      cli::cli_li(
        "{.strong Passed} (all USMs ok): {format_species(passed_species)}"
      )
      cli::cli_end()

      if (length(failed_species) > 0) {
        cli::cli_alert_danger("Found at least one failed USM")
      } else {
        cli::cli_alert_success("All USMs passed for all species")
      }

      invisible(self)
    }
  )
)