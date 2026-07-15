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
#' @param required_for Character vector. Names of workflow contexts (e.g.
#'   "eval", "export", "balance_closure") in which this field must be
#'   non-NULL. Checked by \code{validate_for()}. This is independent of
#'   \code{nullable}, which only governs whether NULL is an acceptable
#'   value in general (outside of any specific workflow).
#' @param required_unless Function(self, context) or NULL. If provided and
#'   it returns TRUE for the context currently being validated, the
#'   \code{required_for} requirement is waived for that context (the field
#'   may be NULL). General escape hatch for conditional requirements that
#'   depend on other field values (e.g. "field X is required for context Y,
#'   UNLESS some other field Z is set"). Not currently used by any field in
#'   this schema, but kept available for future cases like this. Evaluated
#'   only when \code{context \%in\% required_for}; ignored otherwise.
#' @param group Character scalar. Purely organisational: which thematic
#'   group this field belongs to (e.g. "paths", "execution", "filtering").
#'   Has no effect on validation; used to group fields when printing a
#'   Configuration, generating docs, or browsing the schema. Defaults to
#'   "other" if not specified.
#' @return A list of class "field_spec"
#'
#' @keywords internal
field_spec <- function(
  default = NULL,
  type = NULL,
  nullable = TRUE,
  choices = NULL,
  min = NULL,
  max = NULL,
  validator = NULL,
  required_for = character(0),
  required_unless = NULL,
  group = "other"
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
    validator = validator,
    required_for = required_for,
    required_unless = required_unless,
    group = group
  )
  class(field) <- "field_spec"
  field
}

#' Sentinel for fields with no default value
#' @return An object of class "required_field"
#'
#' @keywords internal
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

#' @keywords internal
validate_rds_path <- function(val) {
  if (is.null(val)) return(TRUE)
  if (!is.character(val) || length(val) != 1)
    return("must be a single file path (character)")
  if (!grepl("\\.rds$", val, ignore.case = TRUE))
    return("must point to a .rds file")
  if (!file.exists(val))
    return(paste0("file not found: ", val))
  TRUE
}

# Cross-field constraint checkers
# These express dependencies BETWEEN fields (e.g. "metadata_file is
# required only if run_simulations is TRUE"), which is different from
# "this field is required for the eval workflow" (see required_for in
# field_spec() and validate_for() below). Both mechanisms are needed:
# required_for is static (depends only on which workflow is being run),
# while these cross-validators are dynamic (depend on other field values).

#' @keywords internal
check_metadata_file <- function(s) {
  if (!s$run_simulations) return(TRUE)
  if (sim_rds_given(s)) return(TRUE) # sim_rds bypasses the simulation step
  if (is.null(s$metadata_file))
    return("metadata_file is required")
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

#' @keywords internal
sim_rds_given <- function(self) {
  !is.null(self$sim_rds)
}

config_schema <- list(

  fields = list(

    # --- paths ---------------------------------------------------------
    stics_exe = field_spec(
      default = NULL,
      type = "character",
      nullable = TRUE,
      required_for = c("eval", "balance_closure"),
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
      group = "paths"
    ),

    eval_workspace = field_spec(
      default = NULL,
      type = "character",
      nullable = TRUE,
      required_for = c("eval", "export"),
      group = "paths"
    ),

    output_dir = field_spec(
      default = NULL,
      type = "character",
      nullable = TRUE,
      required_for = "export",
      group = "paths"
    ),

    # --- execution -------------------------------------------------------
    run_simulations = field_spec(
      default = FALSE,
      type = "logical",
      nullable = FALSE,
      group = "execution"
    ),

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
      group = "data"
    ),

    obs_rds = field_spec(
      default = NULL,
      type = "character",
      nullable = TRUE,
      validator = validate_rds_path,
      group = "data"
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

  cross_validators = list(
    list(
      desc = "If run_simulations = TRUE, metadata_file must be a valid path",
      check = check_metadata_file
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

#' Validate that all fields required for a given workflow context are set
#'
#' Looks up, for every field in the schema, whether \code{context} appears
#' in that field's \code{required_for}. If so, the field must be non-NULL
#' on \code{self} — unless the field's \code{required_unless(self, context)}
#' function is defined and returns TRUE, in which case the requirement is
#' waived for that field/context pair.
#' This replaces the hand-written \code{if (is.null(...)) stop(...)}
#' checks that used to live in each \code{validate_*} method, so that
#' "which fields are required for which workflow, and under which
#' exceptions" has a single source of truth: the schema itself.
#'
#' @param self A Configuration object (or any named list/R6 with the
#'   relevant fields).
#' @param context Character scalar naming the workflow, e.g. "eval",
#'   "balance_closure".
#' @param schema A schema produced by config_schema.
#' @return invisible(TRUE) if valid, otherwise stop() with all errors found.
#' @keywords internal
validate_for <- function(self, context, schema = config_schema) {
  errors <- unlist(lapply(
    names(schema$fields),
    function(field_name) {
      spec <- schema$fields[[field_name]]
      if (!context %in% spec$required_for) return(NULL)
      waived <- !is.null(spec$required_unless) &&
        isTRUE(spec$required_unless(self, context))
      if (waived) return(NULL)
      if (is.null(self[[field_name]]))
        sprintf("- %s: required for the '%s' workflow", field_name, context)
      else
        NULL
    }
  ))

  if (length(errors) > 0)
    stop(
      "Invalid configuration for '", context, "':\n",
      paste(errors, collapse = "\n"),
      call. = FALSE
    )

  invisible(TRUE)
}

#' List field names grouped by their `group` attribute
#'
#' @param schema A schema produced by config_schema.
#' @return A named list: group name -> character vector of field names,
#'   in the order groups first appear in the schema.
#' @keywords internal
fields_by_group <- function(schema = config_schema) {
  groups <- vapply(schema$fields, function(spec) spec$group, character(1))
  split(names(schema$fields), factor(groups, levels = unique(groups)))
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
#' The same object is used for all workflows, with
#' workflow-specific validation methods.
#'
#' @field stics_exe Path to STICS executable (required)
#' @field usms_workspace Path to USMs workspace (required)
#' @field metadata_file Path to metadata file (required if
#'  run_simulations = TRUE)
#' @field eval_workspace Path to evaluation workspace (required)
#' @field output_dir Path to output directory for export workflow (required for
#'  export)
#' @field run_simulations Logical. Whether to run simulations or just prepare
#'  the workspace.
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
#' @field sim_rds Character or NULL. Path to an .rds file containing
#'  pre-computed simulation results. If supplied, bypasses the need to
#'  run simulations (see `validate_eval()`). Independent of `obs_rds`.
#' @field obs_rds Character or NULL. Path to an .rds file containing
#'  observation data used as reference for evaluation, plots and balance
#'  closure. Independent of `sim_rds` — has no effect on required fields.
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
        init_logger(self$verbose)
        invisible(self)
      },

      #' @description
      #' Validate configuration for evaluation workflow.
      #' Note: if `sim_rds` is supplied, it bypasses the need to run
      #' simulations (see `check_metadata_file`). `obs_rds` is independent
      #' and has no effect on required fields. `usms_workspace` remains
      #' required for eval regardless of `sim_rds`/`obs_rds`.
      validate_eval = function() {
        validate_for(self, "eval")
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
