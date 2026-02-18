`%||%` <- function(a, b) if (!is.null(a)) a else b

#' @title Getting/setting a list of parameters with initialized fields for
#'  evaluation
#'
#' @param stics_exe path to the STICS executable
#' @param workspace path to the simulation and observation data
#' @param reference_data_dir path to the reference data to use for comparison
#' @param output_dir path where output files will be saved
#' @param run_simulations Logical value for running simulation or not
#' @param verbose Number value for displaying information.
#'  2 = DEBUG, 1 = INFO, 0 = WARNING
#' @param parallel Boolean. Is the computation to be done in parallel ?
#' @param cores Number of cores to use for parallel computation.
#' @param rotation_file path to the CSV which contains the information about
#'  rotations
#' @param exports a list of strings to use to define what to export. Values can
#' be "plots", "sim", "stats", "rmse_per_usm".
#' @param percentage the percentage threshold used to detect critical
#' deteriorated variables
#'
#' @returns A list containing parameters that can be used in `evaluate()`
#'  function.
#'
#' @seealso [set_config_default_values()] to get default values
#' @seealso [validate_configuration()] to get more information about valid
#'  configuration
#'
#' @export
make_config <- function(...) {
  config <- list(...)
  config <- set_config_default_values(config)
  validate_configuration(config)
  list(
    stics_exe = config$stics_exe,
    workspace = config$workspace,
    run_simulations = config$run_simulations,
    verbose = config$verbose,
    parallel = config$parallel,
    cores = config$cores,
    output_dir = config$output_dir,
    reference_data_dir = config$reference_data_dir,
    rotation_file = config$rotation_file,
    exports = config$exports,
    percentage = config$percentage
  )
}

#' @title Setting default values for null parameters in a configuration list
#'
#' @details
#'  Default values:
#'   - run_simulations -> TRUE
#'   - verbose -> 1
#'   - parallel -> FALSE
#'   - cores -> NA
#'   - percentage -> 5
#'
#' @returns A configuration list with default values
set_config_default_values <- function(config) {
  config$run_simulations <- config$run_simulations %||% TRUE
  config$verbose <- config$verbose %||% 1
  config$parallel <- config$parallel %||% FALSE
  config$cores <- config$cores %||% NA
  config$percentage <- config$percentage %||% 5
  config
}

#' @title Validating evaluation configuration
#'
#' @description
#' The configuration must follow these rules to be considered as valid:
#'  - `stics_exe`, `workspace` must be defined
#'  - if `reference_data_dir` is defined, it must be a valid path
#'  - `data_source` must be either `sms` or `local`
#'  - if `data_source` is `local`:
#'    - `rotation_file` must be defined
#'  - if `data_source` is `sms`:
#'    - `sms_path` and `stics_path` must be defined and valid paths
#'    - `run_simulations` must be `TRUE`
validate_configuration <- function(config) {
  if (is.null(config$stics_exe)) stop("Stics executable path must be defined")
  if (is.null(config$workspace)) stop("Workspace path must be defined")
  if (
    !is.null(config$reference_data_dir) &&
      !dir.exists(config$reference_data_dir)
  ) {
    stop("Reference data directory must be a valid path if defined")
  }
  if (!is.null(config$exports) && is.null(config$output_dir)) {
    stop("Output dir must be defined when exports is defined")
  }
  if (!is.null(config$output_dir) && !dir.exists(config$output_dir)) {
    dir.create(config$output_dir, recursive = TRUE)
  }
  if (is.null(config$rotation_file) || !file.exists(config$rotation_file)) {
    stop("Rotation file must be a valid path")
  }
}
