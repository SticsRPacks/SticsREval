evaluate_species <- function(
  eval_workspace,
  species,
  reference_workspace,
  percentage,
  parallel,
  cores,
  usms = NULL,
  var2exclude = NULL
) {
  logger::log_debug("Generating stats for species.")
  gen_species_stats(
    eval_workspace, species, parallel, cores,
    usms = usms, var2exclude = var2exclude
  )
  if (is.null(reference_workspace)) {
    logger::log_info(
      "No reference workspace defined.
      Skipping deteriorated usm generation and comparison"
    )
    return()
  }
  logger::log_debug("Computing deteriorated USM for species.")
  gen_deteriorated_usm(
    eval_workspace, species, reference_workspace, percentage,
    usms = usms, var2exclude = var2exclude
  )
  logger::log_debug("Computing species comparison.")
  gen_species_comparison(
    eval_workspace, species, reference_workspace, percentage
  )
}

#' Run the evaluation workflow
#'
#' This function orchestrates the full evaluation workflow based on a given
#' configuration object. It initializes logging, optionally prepares the
#' evaluation workspace, runs the evaluation for all species, and displays
#' summary information.
#'
#' The total execution time is measured and logged at the end of the process.
#' Errors occurring during evaluation are caught and logged.
#'
#' @param config List. Configuration object created by `make_config()`,
#' containing all parameters required for the evaluation workflow.
#'
#' @return NULL. This function is called for its side effects (running
#' evaluation and logging results).
#'
#' @details
#' The workflow includes the following steps:
#' \itemize{
#'   \item Initializes the logger using the specified verbosity level.
#'   \item Optionally initializes the evaluation workspace (copying data,
#'   preparing inputs, and running simulations if required).
#'   \item Runs evaluation for all species using `evaluate_species()`.
#'   \item Displays comparison summaries using `display_comparisons_info()`.
#' }
#'
#' Errors occurring during the evaluation phase are captured and logged using
#' `logger::log_error()`, allowing the process to fail gracefully.
#'
#' @examples
#' \dontrun{
#' config <- make_config(
#'   stics_exe = "/path/to/stics",
#'   workspace = "workspace/",
#'   metadata_file = "metadata.csv"
#' )
#'
#' evaluate(config)
#' }
#'
#' @export
evaluate <- function(config) {
  on.exit({
    end_time <- Sys.time()
    logger::log_info("Evaluation time: ", format_duration(start_time, end_time))
  }, add = TRUE)
  start_time <- Sys.time()
  validate_eval_configuration(config)
  if (config$init_workspace) {
    init_eval_workspace(
      config$workspace,
      config$eval_workspace,
      config$metadata_file,
      config$stics_exe,
      config$run_simulations,
      config$parallel,
      config$cores,
      force = config$force
    )
  }
  logger::log_info("Starting evaluation...")
  tryCatch({
    species <- get_species(config$eval_workspace)
    if (!is.null(config$species)) {
      species <- intersect(config$species, species)
    }
    if (!is.null(config$usms)) {
      species <- species[
        vapply(species, function(sp) {
          length(get_species_usm(config$eval_workspace, sp, config$usms)) > 0
        }, FUN.VALUE = logical(1))
      ]
    }
    if (length(species) == 0) {
      return()
    }
    logger::log_debug(
      "Found {length(species)} species in workspace
      {config$eval_workspace}: {format_species(species)}"
    )
    evaluate_species(
      config$eval_workspace,
      species,
      config$reference_workspace,
      config$percentage,
      config$parallel,
      config$cores,
      usms = config$usms,
      var2exclude = config$var2exclude
    )
    display_comparisons_info(
      config$eval_workspace,
      species,
      config$percentage
    )
  }, error = function(e) {
    logger::log_error(e$message)
  })
}
