#' Describe a configuration field
#'
#' @param default Default value for the field. Use \code{required()} for
#'   fields that must be explicitly provided and have no sensible default.
#' @param type Character vector. Accepted R types (e.g. "character", "logical").
#' @param nullable Logical. Is NULL a valid value? Defaults to TRUE.
#' @param choices Vector. If provided, the value must belong to this set.
#' @param min Numeric. Minimum allowed value (for numeric types).
#' @param max Numeric. Maximum allowed value (for numeric types).
#' @param validator Function. Custom validation: function(val) returns
#'   TRUE if valid, or a character error message otherwise.
#' @return A list of class "field_spec"
field_spec <- function(
  default = NULL,
  type = NULL,
  nullable = TRUE,
  choices = NULL,
  min = NULL,
  max = NULL,
  validator = NULL
) {
  is_required <- inherits(default, "required_field")
  if (is_required) nullable <- FALSE

  field <- list(
    default = if (is_required) NULL else default,
    required = is_required,
    type = type,
    nullable = nullable,
    choices = choices,
    min = min,
    max = max,
    validator = validator
  )
  class(field) <- "field_spec"
  field
}

#' Sentinel for fields with no default value
#' @return An object of class "required_field"
required <- function() {
  req <- list()
  class(req) <- "required_field"
  req
}

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

# Cross-field constraint checkers

#' @keywords internal
check_init_ws_stics_exe <- function(s) {
  if (
    isTRUE(s$init_workspace) &&
      isTRUE(s$run_simulations) &&
      is.null(s$stics_exe)
  ) {
    return(
      "stics_exe is required when init_workspace = TRUE and
      run_simulations = TRUE"
    )
  }
  TRUE
}

#' @keywords internal
check_init_ws_usms_workspace <- function(s) {
  if (isTRUE(s$init_workspace) && is.null(s$usms_workspace))
    return("usms_workspace is required when init_workspace = TRUE")
  TRUE
}

#' @keywords internal
check_init_ws_metadata_file <- function(s) {
  if (!isTRUE(s$init_workspace)) return(TRUE)
  if (is.null(s$metadata_file))
    return("metadata_file is required when init_workspace = TRUE")
  if (!file.exists(s$metadata_file))
    return(paste0("metadata_file not found: ", s$metadata_file))
  TRUE
}

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

config_schema <- list(

  fields = list(

    stics_exe = field_spec(
      default = NULL,
      type = "character",
      nullable = TRUE
    ),

    usms_workspace = field_spec(
      default = NULL,
      type = "character",
      nullable = TRUE
    ),

    metadata_file = field_spec(
      default = NULL,
      type = "character",
      nullable = TRUE
    ),

    eval_workspace = field_spec(
      default = required(),
      type = "character"
    ),

    output_dir = field_spec(
      default = NULL,
      type = "character",
      nullable = TRUE
    ),

    run_simulations = field_spec(
      default = FALSE,
      type = "logical",
      nullable = FALSE
    ),

    init_workspace = field_spec(
      default = FALSE,
      type = "logical",
      nullable = FALSE
    ),

    parallel = field_spec(
      default = FALSE,
      type = "logical",
      nullable = FALSE
    ),

    verbose = field_spec(
      default = 1L,
      type = "integer",
      nullable = FALSE,
      min = 0L
    ),

    cores = field_spec(
      default = NA,
      nullable = TRUE,
      validator = validate_cores
    ),

    percentage = field_spec(
      default = 5,
      type = "numeric",
      nullable = FALSE,
      min = 0,
      max = 100
    ),

    reference_version = field_spec(
      default = NULL,
      type = "character",
      nullable = TRUE
    ),

    species = field_spec(
      default = NULL,
      type = "character",
      nullable = TRUE,
      validator = validate_nonempty_chr
    ),

    usms = field_spec(
      default = NULL,
      type = "character",
      nullable = TRUE,
      validator = validate_nonempty_chr
    ),

    var2exclude = field_spec(
      default = NULL,
      type = "character",
      nullable = TRUE,
      validator = validate_nonempty_chr
    )
  ),

  cross_validators = list(
    list(
      desc = "If init_workspace = TRUE and run_simulations = TRUE,
        stics_exe must be defined",
      check = check_init_ws_stics_exe
    ),
    list(
      desc = "If init_workspace = TRUE, usms_workspace must be defined",
      check = check_init_ws_usms_workspace
    ),
    list(
      desc = "If init_workspace = TRUE, metadata_file must be a valid path",
      check = check_init_ws_metadata_file
    ),
    list(
      desc = "If parallel = TRUE, cores must be an integer >= 1",
      check = check_parallel_cores
    )
  )
)

# Validate a single NULL value against its spec (required / nullable rules).
# Returns an error string or NULL.
#' @keywords internal
validate_null <- function(field_name, spec) {
  if (spec$required)
    return(sprintf("- %s: required field", field_name))
  if (!spec$nullable)
    return(sprintf("- %s: NULL is not allowed", field_name))
  NULL
}

# Validate the type of a non-NULL value against its spec.
# Returns an error string or NULL.
#' @keywords internal
validate_type <- function(field_name, val, spec) {
  if (is.null(spec$type)) return(NULL)
  type_ok <- any(vapply(spec$type, function(t) methods::is(val, t), logical(1)))
  if (type_ok) return(NULL)
  sprintf(
    "- %s: expected type [%s], got [%s]",
    field_name,
    paste(spec$type, collapse = " | "),
    toString(class(val))
  )
}

#' @keywords internal
validate_choices <- function(field_name, val, spec) {
  if (is.null(spec$choices) || val %in% spec$choices) return(NULL)
  sprintf(
    "- %s: value '%s' is not allowed, valid choices: %s",
    field_name, val, toString(spec$choices)
  )
}

#' @keywords internal
validate_min <- function(field_name, val, spec) {
  if (is.null(spec$min) || !is.numeric(val) || val >= spec$min) return(NULL)
  sprintf(
    "- %s: value %s is below the minimum allowed (%s)",
    field_name, val, spec$min
  )
}

#' @keywords internal
validate_max <- function(field_name, val, spec) {
  if (is.null(spec$max) || !is.numeric(val) || val <= spec$max) return(NULL)
  sprintf(
    "- %s: value %s exceeds the maximum allowed (%s)",
    field_name, val, spec$max
  )
}

#' @keywords internal
validate_custom <- function(field_name, val, spec) {
  if (is.null(spec$validator)) return(NULL)
  result <- spec$validator(val)
  if (!isTRUE(result)) return(sprintf("- %s: %s", field_name, result))
  NULL
}

# Validate choices, min, max and custom validator for a non-NULL value.
# Returns a character vector of error strings (may be empty).
#' @keywords internal
validate_constraints <- function(field_name, val, spec) {
  c(
    validate_choices(field_name, val, spec),
    validate_min(field_name, val, spec),
    validate_max(field_name, val, spec),
    validate_custom(field_name, val, spec)
  )
}

# Validate a single field. Returns a character vector of errors (may be empty).
#' @keywords internal
validate_field <- function(field_name, val, spec) {
  if (is.null(val)) {
    err <- validate_null(field_name, spec)
    return(err %||% character(0))
  }
  c(
    validate_type(field_name, val, spec),
    validate_constraints(field_name, val, spec)
  )
}

# Validate all cross-field constraints. Returns a character vector of errors.
#' @keywords internal
validate_cross <- function(self, cross_validators) {
  errors <- character(0)
  for (cv in cross_validators) {
    result <- cv$check(self)
    if (!isTRUE(result))
      errors <- c(errors, paste0("- [constraint] ", result))
  }
  errors
}

#' Validate an object against a schema
#'
#' Collects ALL errors before raising them, rather than stopping at the first.
#'
#' @param self A named list or R6 object.
#' @param schema A schema produced by config_schema.
#' @return invisible(TRUE) if valid, otherwise stop() with all errors.
#' @keywords internal
validate_schema <- function(self, schema = config_schema) {
  field_errors <- unlist(lapply(
    names(schema$fields),
    function(field_name) {
      validate_field(
        field_name,
        self[[field_name]],
        schema$fields[[field_name]]
      )
    }
  ))

  errors <- c(field_errors, validate_cross(self, schema$cross_validators))

  if (length(errors) > 0)
    stop(
      "Invalid configuration:\n", paste(errors, collapse = "\n"),
      call. = FALSE
    )

  invisible(TRUE)
}

#' Generate the R6 public fields list from the schema
#' (all initialised to NULL — actual values are set inside initialize)
#' @keywords internal
schema_public_fields <- function(schema = config_schema) {
  fields <- vector("list", length(schema$fields))
  names(fields) <- names(schema$fields)
  fields
}

#' Body of initialize(): assign args or defaults, then validate
#' @keywords internal
schema_initialize <- function(self, args, schema = config_schema) {
  for (field_name in names(schema$fields)) {
    spec <- schema$fields[[field_name]]
    val <- if (field_name %in% names(args)) {
      args[[field_name]]
    } else {
      spec$default
    }
    self[[field_name]] <- val
  }
  validate_schema(self, schema)
  invisible(self)
}

#' Configuration class
#' Encapsulates all configuration parameters for the package, with validation.
#' The same object is used for all workflows (evaluation, export, plots), with
#' workflow-specific validation methods.
#'
#' @field stics_exe Path to STICS executable (required if init_workspace = TRUE)
#' @field usms_workspace Path to USMs workspace (required
#'  if init_workspace = TRUE)
#' @field metadata_file Path to metadata file (required if
#'  init_workspace = TRUE)
#' @field eval_workspace Path to evaluation workspace (required)
#' @field output_dir Path to output directory for export workflow (required for
#'  export)
#' @field run_simulations Logical. Whether to run simulations or just prepare
#'  the workspace.
#' @field init_workspace Logical. Whether to initialize the evaluation workspace
#'  (run simulations, prepare metadata, etc.) or assume it's already set up.
#' @field parallel Logical. Whether to run simulations in parallel (only if
#'  init_workspace = TRUE).
#' @field verbose Integer. Verbosity level (0 = silent, 1 = info, 2 = debug).
#' @field cores Integer or NA. Number of CPU cores to use for parallel
#'  processing (only if parallel = TRUE). NA means auto-detect.
#' @field percentage Numeric. Percentage of simulations to consider for export
#'  and plots (between 0 and 100).
#' @field reference_version Character or NULL. If specified, the version in the
#'  evaluation workspace to use as reference for export and plots. Must be
#'  present in the workspace.
#' @field species Character vector or NULL. If specified, only these species
#'  will be included in export and plots.
#' @field usms Character vector or NULL. If specified, only these USMs will
#'  be included in export and plots.
#' @field var2exclude Character vector or NULL. If specified, these variables
#'  will be excluded from export and plots.
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
        init_logger(self$verbose)
        invisible(self)
      },

      #' @description Validate configuration for evaluation workflow
      validate_eval = function() {
        if (is.null(self$eval_workspace))
          stop("Eval workspace path must be defined", call. = FALSE)
        if (!is.null(self$reference_version))
          private$check_reference_version()
        invisible(self)
      },

      #' @description Validate configuration for export
      validate_export = function() {
        if (is.null(self$output_dir))
          stop("Output dir path must be defined", call. = FALSE)
        if (!dir.exists(self$output_dir) && !dir.create(self$output_dir))
          stop("Can't create ", self$output_dir, " directory", call. = FALSE)
        if (is.null(self$eval_workspace))
          stop("Eval workspace path must be defined", call. = FALSE)
        invisible(self)
      },

      #' @description Validate configuration for plots
      validate_plots = function() {
        if (!is.null(self$reference_version))
          private$check_reference_version()
        if (is.null(self$eval_workspace))
          stop("Eval workspace path must be defined", call. = FALSE)
        invisible(self)
      }
    )
  ),

  private = list(
    check_reference_version = function() {
      ws <- EvalWorkspace$new(self$eval_workspace)
      versions <- ws$get_all_versions()
      if (is.null(versions) || !(self$reference_version %in% versions))
        stop(
          "Reference version is not present in evaluation workspace. ",
          "Available versions: ", toString(versions),
          call. = FALSE
        )
    }
  )
)
