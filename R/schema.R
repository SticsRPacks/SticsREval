#' Describe a function argument's validation rules
#'
#' @param default Default value for the field. Use \code{required()} for
#'   fields that must be explicitly provided and have no sensible default.
#'   Purely informational unless the caller applies it itself — validation
#'   functions here only look at the value actually supplied.
#' @param type Character vector. Accepted R types (e.g. "character", "logical").
#' @param nullable Logical. Is NULL a valid value? Defaults to TRUE.
#' @param choices Vector. If provided, the value must belong to this set.
#' @param min Numeric. Minimum allowed value (for numeric types).
#' @param max Numeric. Maximum allowed value (for numeric types).
#' @param validator Function. Custom validation: function(val) returns
#'   TRUE if valid, or a character error message otherwise.
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
check_path_exists <- function(field_name) {
  function(s) {
    val <- s[[field_name]]
    if (is.null(val)) return(TRUE)
    if (!file.exists(val))
      return(paste0(field_name, " not found: ", val))
    TRUE
  }
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
  TRUE
}

#' Cross-field check: if parallel = TRUE, cores must be an integer >= 1
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

#' Validate a named list of values against a local schema
#'
#' Collects ALL errors before raising them, rather than stopping at the
#' first. Purely structural: never touches the filesystem (see
#' \code{validate_filesystem()} for that). Callers build a small,
#' function-scoped \code{schema} (\code{list(fields = list(...),
#' cross_validators = list(...))}) for just the arguments they need to
#' validate — there is no single shared schema across the package.
#'
#' @param self A named list, typically \code{as.list(environment())} taken
#'   at the top of the calling function, i.e. its already-resolved
#'   arguments.
#' @param schema A list with a \code{fields} element (named list of
#'   \code{field_spec()}) and an optional \code{cross_validators} element
#'   (list of \code{list(desc = ..., check = function(self) ...)}).
#' @return invisible(TRUE) if valid, otherwise stop() with all errors.
#' @keywords internal
validate_schema <- function(self, schema) {
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
      "Invalid arguments:\n", paste(errors, collapse = "\n"),
      call. = FALSE
    )

  invisible(TRUE)
}

#' Validate filesystem-dependent fields (I/O)
#'
#' Checks that path fields point to files that actually exist on disk.
#' Kept separate from \code{validate_schema()} so that plain structural
#' validation never touches the filesystem: callers decide when this
#' I/O-bound check runs.
#'
#' @param self A named list, typically \code{as.list(environment())} taken
#'   at the top of the calling function.
#' @param schema A list with a \code{filesystem_checks} element: a list of
#'   \code{list(desc = ..., check = function(self) ...)}, e.g. built with
#'   \code{check_path_exists()}.
#' @return invisible(TRUE) if valid, otherwise stop() with all errors.
#' @keywords internal
validate_filesystem <- function(self, schema) {
  errors <- character(0)
  for (cv in schema$filesystem_checks) {
    result <- cv$check(self)
    if (!isTRUE(result))
      errors <- c(errors, paste0("- [filesystem] ", result))
  }

  if (length(errors) > 0)
    stop(
      "Invalid arguments:\n", paste(errors, collapse = "\n"),
      call. = FALSE
    )

  invisible(TRUE)
}
