#' @include schema.R
#' @keywords internal
validate_cores <- function(val) {
  if (identical(val, NA) || identical(val, NA_integer_)) return(TRUE)
  if (!is.numeric(val) || val < 1) return("must be an integer >= 1 or NA")
  TRUE
}

#' @keywords internal
validate_nonempty_chr <- function(val) {
  if (is.null(val)) return(TRUE)
  if (length(val) == 0) return("must be a non-empty character vector or NULL")
  TRUE
}

#' @keywords internal
validate_rds_path <- function(val) {
  if (is.null(val)) return(TRUE)
  if (!is.character(val) || length(val) != 1)
    return("must be a single file path (character)")
  if (!grepl("\\.rds$", val, ignore.case = TRUE))
    return("must point to a .rds file")
  TRUE
}

# Cross-field constraint checkers
# These express dependencies BETWEEN fields, which is different from
# "this field is required for the eval workflow" (see required_for in
# field_spec() and validate_for() below). Both mechanisms are needed:
# required_for is static (depends only on which workflow is being run),
# while these cross-validators are dynamic (depend on other field values).
#
# These checkers are purely structural (no filesystem access): they only
# ask "is this field set when it needs to be". Whether a given path
# actually exists on disk is a separate, I/O-bound question, handled by
# check_path_exists() / config_schema$filesystem_checks below, so that
# validate_schema() (structural, run at construction) never touches the
# filesystem.

#' @keywords internal
check_parallel_cores <- function(s) {
  if (!isTRUE(s$parallel)) return(TRUE)
  cores_missing <- is.null(s$cores) ||
    identical(s$cores, NA) ||
    identical(s$cores, NA_integer_)
  if (cores_missing)
    return("cores must be an integer >= 1 when parallel = TRUE")
  TRUE
}

#' @keywords internal
ref_sim_rds_given <- function(self) {
  !is.null(self$ref_sim_rds)
}

config_schema <- list(

  fields = list(

    # --- paths ---------------------------------------------------------
    stics_exe = field_spec(
      default = NULL,
      type = "character",
      nullable = TRUE,
      required_for = "balance_closure",
      group = "paths"
    ),

    usms_workspace = field_spec(
      default = NULL,
      type = "character",
      nullable = TRUE,
      required_for = c("eval", "balance_closure"),
      group = "paths"
    ),

    metadata_file = field_spec(
      default = NULL,
      type = "character",
      nullable = TRUE,
      required_for = "balance_closure",
      group = "paths"
    ),

    eval_workspace = field_spec(
      default = tempfile(pattern = "eval_workspace_"),
      type = "character",
      nullable = FALSE,
      required_for = "eval",
      group = "paths"
    ),

    output_dir = field_spec(
      default = NULL,
      type = "character",
      nullable = TRUE,
      group = "paths"
    ),

    # --- execution -------------------------------------------------------
    parallel = field_spec(
      default = FALSE,
      type = "logical",
      nullable = FALSE,
      group = "execution"
    ),

    verbose = field_spec(
      default = 1L,
      type = "integer",
      nullable = FALSE,
      min = 0L,
      group = "execution"
    ),

    cores = field_spec(
      default = NA,
      nullable = TRUE,
      validator = validate_cores,
      group = "execution"
    ),

    # --- filtering / export ----------------------------------------------
    percentage = field_spec(
      default = 5,
      type = "numeric",
      nullable = FALSE,
      min = 0,
      max = 100,
      group = "filtering"
    ),

    species = field_spec(
      default = NULL,
      type = "character",
      nullable = TRUE,
      validator = validate_nonempty_chr,
      group = "filtering"
    ),

    usms = field_spec(
      default = NULL,
      type = "character",
      nullable = TRUE,
      validator = validate_nonempty_chr,
      group = "filtering"
    ),

    var2exclude = field_spec(
      default = NULL,
      type = "character",
      nullable = TRUE,
      validator = validate_nonempty_chr,
      group = "filtering"
    ),

    # --- data --------------------------------------------------------
    sim_rds = field_spec(
      default = NULL,
      type = "character",
      nullable = TRUE,
      validator = validate_rds_path,
      group = "data",
      required_for = "eval"
    ),

    obs_rds = field_spec(
      default = NULL,
      type = "character",
      nullable = TRUE,
      validator = validate_rds_path,
      group = "data",
      required_for = "eval"
    ),

    ref_sim_rds = field_spec(
      default = NULL,
      type = "character",
      nullable = TRUE,
      validator = validate_rds_path,
      group = "data",
      required_for = "eval"
    )
  ),

  # Structural checks: no filesystem access, run automatically at
  # construction time (see validate_schema()).
  cross_validators = list(
    list(
      desc = "If parallel = TRUE, cores must be an integer >= 1",
      check = check_parallel_cores
    )
  ),

  # I/O checks: only run on demand via check_filesystem() (called from
  # validate_eval() / validate_balance_closure()), never at construction.
  filesystem_checks = list(
    list(
      desc = "stics_exe must point to an existing file",
      check = check_path_exists("stics_exe")
    ),
    list(
      desc = "metadata_file must point to an existing file",
      check = check_path_exists("metadata_file")
    ),
    list(
      desc = "sim_rds must point to an existing file",
      check = check_path_exists("sim_rds")
    ),
    list(
      desc = "obs_rds must point to an existing file",
      check = check_path_exists("obs_rds")
    ),
    list(
      desc = "ref_sim_rds must point to an existing file",
      check = check_path_exists("ref_sim_rds")
    )
  )
)

#' Configuration class
#' Encapsulates all configuration parameters for the package, with validation.
#' The same object is used for all workflows, with
#' workflow-specific validation methods.
#'
#' @field stics_exe Path to STICS executable (required for balance_closure)
#' @field usms_workspace Path to USMs workspace (required)
#' @field metadata_file Path to metadata file (required for balance_closure)
#' @field eval_workspace Path to evaluation workspace (required)
#' @field output_dir Path to output directory for export workflow (required for
#'  export)
#' @field parallel Logical. Whether to run workflow in parallel.
#' @field verbose Integer. Verbosity level (0 = silent, 1 = info, 2 = debug).
#' @field cores Integer or NA. Number of CPU cores to use for parallel
#'  processing (only if parallel = TRUE). NA means auto-detect.
#' @field percentage Numeric. Percentage of simulations to consider for export
#'  and plots (between 0 and 100).
#' @field species Character vector or NULL. If specified, only these species
#'  will be included in export and plots.
#' @field usms Character vector or NULL. If specified, only these USMs will
#'  be included in export and plots.
#' @field var2exclude Character vector or NULL. If specified, these variables
#'  will be excluded from export and plots.
#' @field sim_rds Character. Path to an .rds file containing pre-computed
#'  simulation results (required for evaluation). Use
#'  \code{\link{run_simulations}} to produce it.
#' @field obs_rds Character. Path to an .rds file containing pre-computed
#'  observation data (required for evaluation). Use
#'  \code{\link{run_simulations}} to produce it.
#' @field ref_sim_rds Character or NULL. Path to an .rds file containing
#'  reference simulation results used for evaluation and plots. Required for
#'  evaluation.
#' @export
Configuration <- R6::R6Class("Configuration", # nolint: object_name_linter
  public = c(

    # Public fields generated from the schema
    schema_public_fields(),

    list(
      #' @description
      #' Create a Configuration object.
      #' Values are validated against a declarative schema.
      #'
      #' @param ... Named configuration fields. Must match names defined in
      #'   `config_schema$fields`. Unspecified fields use their default values.
      #'   Invalid or unknown fields will trigger validation errors.
      initialize = function(...) {
        schema_initialize(self, list(...))
        invisible(self)
      },

      #' @description
      #' Check that filesystem-dependent fields point to files that exist
      #' on disk. Separate from schema validation (structural, checked at
      #' construction): this is I/O-bound and only runs when called
      #' explicitly, e.g. from `validate_eval()` /
      #' `validate_balance_closure()`.
      check_filesystem = function() {
        validate_filesystem(self)
        invisible(self)
      },

      #' @description
      #' TRUE if `ref_sim_rds` was supplied, meaning reference simulation
      #' results are available for evaluation.
      has_reference_sim = function() ref_sim_rds_given(self),

      #' @description
      #' Validate configuration for evaluation workflow. Evaluation always
      #' reads simulation and observation data from `sim_rds` / `obs_rds`
      #' — see `\link{run_simulations}` to produce them.
      validate_eval = function() {
        validate_for(self, "eval")
        self$check_filesystem()
        if (
          !is.null(self$output_dir) &&
            !dir.exists(self$output_dir) &&
            !dir.create(self$output_dir)
        ) {
          stop("Can't create ", self$output_dir, " directory", call. = FALSE)
        }
        invisible(self)
      },

      #' @description Validate configuration for balance closure test
      validate_balance_closure = function() {
        validate_for(self, "balance_closure")
        self$check_filesystem()
        invisible(self)
      },

      #' @description
      #' Print the configuration, with fields grouped by theme (paths,
      #' execution, filtering, ...) rather than as a flat list. Makes it
      #' much easier to eyeball the current state of a large Configuration.
      #'
      #' @param ... Ignored. For compatibility with R6 print() generic.
      print = function(...) {
        cat("<Configuration>\n")
        groups <- fields_by_group()
        for (group_name in names(groups)) {
          cat(sprintf("  [%s]\n", group_name))
          for (field_name in groups[[group_name]]) {
            val <- self[[field_name]]
            val_str <- if (is.null(val)) "NULL" else toString(val)
            cat(sprintf("    %-20s %s\n", field_name, val_str))
          }
        }
        invisible(self)
      }
    )
  )
)
