#' Create configuration for the evaluation workflow
#'
#' This function builds a configuration list containing all parameters
#' required to run simulations and evaluation workflows.
#'
#' The resulting configuration object can be passed to downstream functions
#' to ensure consistent parameter handling.
#'
#' @param stics_exe Character. Path to the STICS executable.
#' @param workspace Character. Path to the working directory containing
#'   simulation inputs.
#' @param metadata_file Character. Path to the metadata file describing
#'   simulations.
#' @param run_simulations Logical. Whether to run simulations.
#'   Defaults to TRUE.
#' @param verbose Integer. Verbosity level for logging. Defaults to 1.
#' @param parallel Logical. Whether to enable parallel execution.
#'   Defaults to FALSE.
#' @param cores Integer or NA. Number of cores to use for parallel
#'   execution. If NA, the number of available cores may be used.
#' @param reference_data_dir Character or NULL. Path to reference
#'   simulation data. If NULL, reference-based analyses may be skipped.
#' @param percentage Numeric. Threshold used for evaluation metrics
#'   (e.g., detecting deteriorated variables). Defaults to 5.
#' @param eval_workspace Character. Path to the evaluation workspace.
#'   Defaults to `DEFAULT_WORKSPACE`.
#' @param init_workspace Logical. Whether to initialize the evaluation
#'   workspace. Defaults to TRUE.
#' @param output_dir Character or NULL. Path to the output directory
#'   for exported files. Defaults to NULL.
#' @param species Character vector or NULL. Optional list of species to
#'   evaluate. If NULL, all available species are evaluated.
#'   Defaults to NULL.
#' @param usms Character vector or NULL. Optional list of USMs to
#'   evaluate. If NULL, all available USMs are evaluated.
#'   Defaults to NULL.
#' @param force Logical. Whether to overwrite an existing non-empty
#'   evaluation workspace without prompting. Defaults to FALSE.
#'
#' @return A named list containing the configuration parameters.
#'
#' @examples
#' \dontrun{
#' config <- make_config(
#'   stics_exe = "/path/to/stics",
#'   workspace = "workspace/",
#'   metadata_file = "metadata.csv"
#' )
#' }
#'
#' @export
make_config <- function(
  stics_exe = NULL,
  workspace  = NULL,
  metadata_file  = NULL,
  run_simulations = TRUE,
  verbose = 1,
  parallel = FALSE,
  cores = NA,
  reference_data_dir = NULL,
  percentage = 5,
  eval_workspace = DEFAULT_WORKSPACE,
  init_workspace = TRUE,
  output_dir = NULL,
  species = NULL,
  usms = NULL,
  force = FALSE
) {
  config <- list(
    stics_exe = stics_exe,
    workspace = workspace,
    run_simulations = run_simulations,
    verbose = verbose,
    parallel = parallel,
    cores = cores,
    reference_data_dir = reference_data_dir,
    metadata_file = metadata_file,
    percentage = percentage,
    eval_workspace = eval_workspace,
    init_workspace = init_workspace,
    output_dir = output_dir,
    species = species,
    usms = usms,
    force = force
  )
  init_logger(config$verbose)
  config
}

#' @title Validating evaluation configuration
#'
#' @description
#' The configuration must follow these rules to be considered as valid:
#'  - if `init_workspace` is TRUE, `stics_exe`, `workspace` and
#'    `metadata_file` must be defined and `metadata_file` must be a valid path
#'  - if `reference_data_dir` is defined, it must be a valid path
#'  - `eval_workspace` must be defined
#'
#' @keywords internal
validate_eval_configuration <- function(config) {
  if (config$init_workspace) {
    if (is.null(config$stics_exe)) {
      stop("Stics executable path must be defined", call. = FALSE)
    }
    if (is.null(config$workspace)) {
      stop("Workspace path must be defined", call. = FALSE)
    }
    if (is.null(config$metadata_file) || !file.exists(config$metadata_file)) {
      stop("Metadata file must be a valid path", call. = FALSE)
    }
  }
  if (
    !is.null(config$reference_data_dir) &&
      !dir.exists(config$reference_data_dir)
  ) {
    stop(
      "Reference data directory must be a valid path if defined",
      call. = FALSE
    )
  }
  if (is.null(config$eval_workspace)) {
    stop("Eval workspace path must be defined", call. = FALSE)
  }
}

validate_export_config <- function(config) {
  if (is.null(config$output_dir)) {
    stop("Output dir path must be defined", call. = FALSE)
  }
  if (!dir.exists(config$output_dir) &&
        !dir.create(config$output_dir)) {
    stop(
      "Can't create ", config$output_dir, " directory",
      call. = FALSE
    )
  }
  if (is.null(config$eval_workspace)) {
    stop("Eval workspace path must be defined", call. = FALSE)
  }
}

validate_plots_config <- function(config) {
  if (
    !is.null(config$reference_data_dir) &&
      !dir.exists(config$reference_data_dir)
  ) {
    stop(
      "Reference data directory must be a valid path if defined",
      call. = FALSE
    )
  }
}
