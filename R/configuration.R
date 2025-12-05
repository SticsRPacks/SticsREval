#' @title Getting/setting a list of parameters with initialized fields for
#'  evaluation
#'
#' @param stics_exe path to the STICS executable
#' @param workspace path to the simulation and observation data
#' @param reference_data_dir path to the reference data to use for comparison
#' @param output_dir path where output files will be saved
#' @param run_simulations Logical value for running simulation or not
#' @param do_evaluation Logical value for running evaluation or not
#' @param verbose Logical value for displaying or not information while running
#' @param parallel Boolean. Is the computation to be done in parallel ?
#' @param cores Number of cores to use for parallel computation.
#' @param data_source the source of the data to use for the evaluation. The
#'  source can be "sms" or "local".
#' @param rotation_file path to the CSV which contains the information about
#'  rotations
#' @param sms_path path to the directory which contains the SMS repository
#' @param stics_path path to the STICS repository
#'
#' @returns A list containing parameters that can be used in `evaluate()`
#'  function.
#'
#' @seealso [set_config_default_values()] to get default values
#' @seealso [validate_configuration()] to get more information about valid
#'  configurations
#'
#' @export
make_config <- function(...) {
  config <- list(...)
  config <- set_config_default_values(config)
  validate_configuration(config)
  list(
    stics_exe = config$stics_exe,
    workspace = config$workspace,
    data_source = config$data_source,
    run_simulations = config$run_simulations,
    do_evaluation = config$do_evaluation,
    verbose = config$verbose,
    parallel = config$parallel,
    cores = config$cores,
    output_dir = config$output_dir,
    reference_data_dir = config$reference_data_dir,
    rotation_file = config$rotation_file,
    sms_path = config$sms_path,
    stics_path = config$stics_path
  )
}

#' @title Setting default values for null parameters in a configuration list
#'
#' @details
#'  Default values:
#'   - run_simulations -> TRUE
#'   - do_evaluation -> TRUE
#'   - verbose -> FALSE
#'   - parallel -> FALSE
#'   - cores -> NA
#'
#' @returns A configuration list with default values
set_config_default_values <- function(config) {
  if (is.null(config$run_simulations)) {
    config$run_simulations <- TRUE
  }
  if (is.null(config$do_evaluation)) {
    config$do_evaluation <- TRUE
  }
  if (is.null(config$verbose)) {
    config$verbose <- FALSE
  }
  if (is.null(config$parallel)) {
    config$parallel <- FALSE
  }
  if (is.null(config$cores)) {
    config$cores <- NA
  }
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
  if (!is.null(config$output_dir) && !file.exists(config$output_dir)) {
    dir.create(config$output_dir, recursive = TRUE)
  }
  if (config$data_source == "sms") {
    if (is.null(config$sms_path) || !dir.exists(config$sms_path)) {
      stop("SMS path must be a valid path when data source is sms")
    }
    if (is.null(config$stics_path) || !dir.exists(config$stics_path)) {
      stop("Stics path must be a valid path when data source is sms")
    }
    if (!config$run_simulations) {
      stop(
        "run_simulations flag must be True when data source is sms"
      )
    }
  } else if (config$data_source == "local") {
    if (is.null(config$rotation_file) || !file.exists(config$rotation_file)) {
      stop("Rotation file must be a valid path when data source is local")
    }
  } else {
    stop("Invalid data source (", config$data_source, "): source must be 'sms' or 'local'")
  }
}
