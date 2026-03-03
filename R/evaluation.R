prepare_species_workspace <- function(eval_workspace, species) {
  for (spec in species) {
    logger::log_info("Starting evaluation of species {spec}")
    species_usms <- get_species_usm(eval_workspace, spec)
    if (!length(species_usms)) {
      logger::log_warn("No USM for species {spec}.")
      next
    }
    logger::log_info("Found {length(species_usms)} USMs for species {spec}.")
    rm(species_usms)
    gc()
    species_workspace <- file.path(eval_workspace, spec)
    if (!dir.exists(species_workspace) &&
        !dir.create(species_workspace, recursive = TRUE)
    ) {
      stop(paste0("Error while creating ", spec, " output directory"))
    }
    logger::log_debug(
      "Exporting {spec} evaluation results in {species_workspace}"
    )
  }
}

evaluate_all_species <- function(
  eval_workspace,
  reference_data_dir,
  percentage,
  parallel,
  cores
) {
  species <- get_species(eval_workspace)
  logger::log_debug(
    "Found {length(species)} species in workspace {eval_workspace}:
    {format_species(species)}"
  )
  logger::log_debug("Preparing species workspaces.")
  prepare_species_workspace(eval_workspace, species)
  logger::log_debug("Generating stats for species.")
  gen_species_stats(eval_workspace, species, parallel, cores)
  logger::log_debug("Computing deteriorated USM for species.")
  gen_deteriorated_usm(
    eval_workspace, species, reference_data_dir, percentage
  )
  logger::log_debug("Computing species comparison.")
  gen_species_comparison(
    eval_workspace, species, reference_data_dir, percentage
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
#'   \item Runs evaluation for all species using `evaluate_all_species()`.
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
      config$cores
    )
  }
  logger::log_info("Starting evaluation...")
  tryCatch({
    evaluate_all_species(
      config$eval_workspace,
      config$reference_data_dir,
      config$percentage,
      config$parallel,
      config$cores
    )
    display_comparisons_info(config$eval_workspace, config$percentage)
  }, error = function(e) {
    logger::log_error(e$message)
  })
}
