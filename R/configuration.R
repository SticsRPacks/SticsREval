#' 
#' Configuration class
#'
#' Configuration of the evaluation workflow
#'
#' This class is used to configure the evaluation workflow.
#'
#' @name Configuration
#' @docType class
#'
#' @field stics_exe Character. Path to the STICS executable.
#' @field usms_workspace Character. Path to the working directory
#'   containing simulation inputs.
#' @field metadata_file Character. Path to the metadata file describing
#'   simulations.
#' @field run_simulations Logical. Whether to run simulations.
#'   Defaults to TRUE.
#' @field verbose Integer. Verbosity level for logging. Defaults to 1.
#' @field parallel Logical. Whether to enable parallel execution.
#'   Defaults to FALSE.
#' @field cores Integer or NA. Number of cores to use for parallel
#'   execution. If NA, the number of available cores may be used.
#' @field reference_version Character or NULL. Version of the reference
#'   evaluation data. Must be present in evaluation workspace.
#'   If NULL, reference-based analyses may be skipped.
#' @field percentage Numeric. Threshold used for evaluation metrics
#'   (e.g., detecting deteriorated variables). Defaults to 5.
#' @field eval_workspace Character. Path to the evaluation workspace.
#'   Defaults to NULL.
#' @field init_workspace Logical. Whether to initialize the evaluation
#'   workspace. Defaults to TRUE.
#' @field output_dir Character or NULL. Path to the output directory
#'   for exported files. Defaults to NULL.
#' @field species Character vector or NULL. Optional list of species to
#'   evaluate. If NULL, all available species are evaluated.
#'   Defaults to NULL.
#' @field usms Character vector or NULL. Optional list of USMs to
#'   evaluate. If NULL, all available USMs are evaluated.
#'   Defaults to NULL.
#' @field var2exclude Character vector or NULL. Optional list of variables
#'   to exclude from evaluation. If NULL, all available variables are
#'   evaluated.
#'   Defaults to NULL.
#'
#' @return A named list containing the configuration parameters.
#'
#' @examples
#' \dontrun{
#' config <- Configuration$new(
#'   stics_exe = "/path/to/stics",
#'   eval_workspace = "workspace/",
#'   metadata_file = "metadata.csv"
#' )
#' }
#'
#' @export
Configuration <- R6::R6Class("Configuration",
  public = list(
    stics_exe = NULL,
    usms_workspace = NULL,
    metadata_file = NULL,
    run_simulations = TRUE,
    verbose = 1,
    parallel = FALSE,
    cores = NA,
    reference_version = NULL,
    percentage = 5,
    eval_workspace = NULL,
    init_workspace = TRUE,
    output_dir = NULL,
    species = NULL,
    usms = NULL,
    var2exclude = NULL,

    #' @description
    #' Create a configuration object
    #'
    #' @param stics_exe Character. Path to the STICS executable.
    #' @param usms_workspace Character. Path to the working directory
    #'   containing simulation inputs.
    #' @param metadata_file Character. Path to the metadata file describing
    #'   simulations.
    #' @param run_simulations Logical. Whether to run simulations.
    #'   Defaults to TRUE.
    #' @param verbose Integer. Verbosity level for logging. Defaults to 1.
    #' @param parallel Logical. Whether to enable parallel execution.
    #'   Defaults to FALSE.
    #' @param cores Integer or NA. Number of cores to use for parallel
    #'   execution. If NA, the number of available cores may be used.
    #' @param reference_version Character or NULL. Version of the reference
    #'   evaluation data. Must be present in evaluation workspace.
    #'   If NULL, reference-based analyses may be skipped.
    #' @param percentage Numeric. Threshold used for evaluation metrics
    #'   (e.g., detecting deteriorated variables). Defaults to 5.
    #' @param eval_workspace Character. Path to the evaluation workspace.
    #'   Defaults to NULL.
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
    #' @param var2exclude Character vector or NULL. Optional list of variables
    #'   to exclude from evaluation. If NULL, all available variables are
    #'   evaluated.
    #'   Defaults to NULL.
    #'
    #' @return A configuration object
    initialize = function(
      stics_exe = NULL, usms_workspace = NULL, metadata_file = NULL,
      run_simulations = TRUE, verbose = 1, parallel = FALSE, cores = NA,
      reference_version = NULL, percentage = 5, eval_workspace = NULL,
      init_workspace = TRUE, output_dir = NULL, species = NULL,
      usms = NULL, var2exclude = NULL
    ) {
      self$stics_exe <- stics_exe
      self$usms_workspace <- usms_workspace
      self$metadata_file <- metadata_file
      self$run_simulations <- run_simulations
      self$verbose <- verbose
      self$parallel <- parallel
      self$cores <- cores
      self$reference_version <- reference_version
      self$percentage <- percentage
      self$eval_workspace <- eval_workspace
      self$init_workspace <- init_workspace
      self$output_dir <- output_dir
      self$species <- species
      self$usms <- usms
      self$var2exclude <- var2exclude
      init_logger(self$verbose)
    },

    #' @description
    #' Validating evaluation configuration
    #' The configuration must follow these rules to be considered as valid:
    #'  - if `init_workspace` is TRUE, `stics_exe`, `workspace` and
    #'    `metadata_file` must be defined and `metadata_file` must be a valid path
    #'  - `eval_workspace` must be defined
    #'  - if `reference_version` is defined, it must be a version present in
    #'    evaluation workspace
    #'
    #' @keywords internal
    validate_eval = function() {
      if (self$init_workspace) {
        if (is.null(self$stics_exe))
          stop("Stics executable path must be defined", call. = FALSE)
        if (is.null(self$usms_workspace))
          stop("USMs workspace path must be defined", call. = FALSE)
        if (is.null(self$metadata_file) || !file.exists(self$metadata_file))
          stop("Metadata file must be a valid path", call. = FALSE)
      }
      if (is.null(self$eval_workspace))
        stop("Eval workspace path must be defined", call. = FALSE)
      if (!is.null(self$reference_version)) {
        private$check_reference_version()
      }
      invisible(self)
    },

    #' @description
    #' Validating export configuration
    #' The configuration must follow these rules to be considered as valid:
    #'  - `output_dir` must be defined and be a valid path
    #'  - `eval_workspace` must be defined
    #'
    #' @keywords internal
    validate_export = function() {
      if (is.null(self$output_dir))
        stop("Output dir path must be defined", call. = FALSE)
      if (!dir.exists(self$output_dir) && !dir.create(self$output_dir))
        stop("Can't create ", self$output_dir, " directory", call. = FALSE)
      if (is.null(self$eval_workspace))
        stop("Eval workspace path must be defined", call. = FALSE)
      invisible(self)
    },

    #' @description
    #' Validating plots configuration
    #' The configuration must follow these rules to be considered as valid:
    #'  - `output_dir` must be defined and be a valid path
    #'  - `eval_workspace` must be defined
    #'  - if `reference_version` is defined, it must be a version present in
    #'    evaluation workspace
    #'
    #' @keywords internal
    validate_plots = function() {
      if (!is.null(self$reference_version)) {
        private$check_reference_version()
      }
      if (is.null(self$eval_workspace)) {
        stop("Eval workspace path must be defined", call. = FALSE)
      }
      invisible(self)
    }
  ),

  private = list(
    check_reference_version = function() {
      ws <- EvalWorkspace$new(self$eval_workspace)
      versions <- ws$get_all_versions()
      if (is.null(versions) || !(self$reference_version %in% versions)) {
        stop(
          "Reference version is not present in evaluation workspace. ",
          "Available versions: ", toString(versions),
          call. = FALSE
        )
      }
    }
  )
)
