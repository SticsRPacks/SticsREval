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
#'   "eval", "balance_closure") in which this field must be
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

# Generic filesystem checker factory. Not tied to any specific schema:
# builds a check(s) function that verifies a named field, if set, points
# to an existing file. Only run on demand (see validate_filesystem()),
# never as a side effect of validate_schema().
#' @keywords internal
check_path_exists <- function(field_name, needed = function(s) TRUE) {
  function(s) {
    if (!needed(s)) return(TRUE)
    val <- s[[field_name]]
    if (is.null(val)) return(TRUE)
    if (!file.exists(val))
      return(paste0(field_name, " not found: ", val))
    TRUE
  }
}

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
#' Purely structural: never touches the filesystem (see
#' \code{validate_filesystem()} for that).
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

#' Validate filesystem-dependent fields (I/O)
#'
#' Checks that path fields point to files that actually exist on disk.
#' Kept separate from \code{validate_schema()} so that building a
#' configuration never touches the filesystem: callers decide when this
#' I/O-bound check runs (see \code{check_filesystem()}).
#'
#' @param self A named list (typically a configuration built by
#'   \code{\link{Configuration}}).
#' @param schema A schema produced by config_schema.
#' @return invisible(TRUE) if valid, otherwise stop() with all errors.
#' @keywords internal
validate_filesystem <- function(self, schema = config_schema) {
  errors <- character(0)
  for (cv in schema$filesystem_checks) {
    result <- cv$check(self)
    if (!isTRUE(result))
      errors <- c(errors, paste0("- [filesystem] ", result))
  }

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
#' @param self A named list (typically a configuration built by
#'   \code{\link{Configuration}}).
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

#' Build a validated configuration list from named arguments
#'
#' For every field in the schema, takes the corresponding value from
#' \code{args} if provided, or the field's default otherwise, then
#' validates the resulting list against the schema (see
#' \code{\link{validate_schema}}).
#'
#' @param args A named list of field values, e.g. from \code{list(...)}.
#' @param schema A schema produced by config_schema.
#' @return The validated, fully-populated configuration list.
#' @keywords internal
schema_initialize <- function(args, schema = config_schema) {
  self <- lapply(names(schema$fields), function(field_name) {
    if (field_name %in% names(args)) {
      args[[field_name]]
    } else {
      schema$fields[[field_name]]$default
    }
  })
  names(self) <- names(schema$fields)
  validate_schema(self, schema)
  self
}
