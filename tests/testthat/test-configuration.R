# ---------------------------------------------------------------------------
# Helpers / stubs # nolint: commented_code_linter
# ---------------------------------------------------------------------------

# Stub init_logger so tests don't depend on it
init_logger <- function(verbose) invisible(NULL)

# Stub EvalWorkspace so check_reference_version can be exercised
EvalWorkspace <- R6::R6Class("EvalWorkspace", # nolint: object_name_linter
  public = list(
    path = NULL,
    initialize = function(path) self$path <- path,
    get_all_versions = function() c("v1.0", "v2.0")
  )
)

build_metadata_file <- function() {
  f <- tempfile()
  file.create(f)
  f
}

# ---------------------------------------------------------------------------
# field_spec
# ---------------------------------------------------------------------------

test_that("field_spec returns a list of class field_spec", {
  spec <- field_spec()
  expect_s3_class(spec, "field_spec")
  expect_type(spec, "list")
})

test_that("field_spec stores all arguments correctly", {
  validator_fn <- function(val) TRUE
  spec <- field_spec(
    default = 42,
    type = "numeric",
    nullable = FALSE,
    choices = c(1, 42, 99),
    min = 1,
    max = 100,
    validator = validator_fn
  )
  expect_identical(spec$default, 42)
  expect_identical(spec$type, "numeric")
  expect_false(spec$nullable)
  expect_identical(spec$choices, c(1, 42, 99))
  expect_identical(spec$min, 1)
  expect_identical(spec$max, 100)
  expect_identical(spec$validator, validator_fn)
  expect_false(spec$required)
})

test_that(
  "field_spec with required() sets required = TRUE and nullable = FALSE",
  {
    spec <- field_spec(default = required())
    expect_true(spec$required)
    expect_false(spec$nullable)
    expect_null(spec$default)
  }
)

test_that("field_spec default nullable is TRUE for non-required fields", {
  spec <- field_spec(default = "hello")
  expect_true(spec$nullable)
})

# ---------------------------------------------------------------------------
# required
# ---------------------------------------------------------------------------

test_that("required() returns an object of class required_field", {
  req <- required()
  expect_s3_class(req, "required_field")
})

# ---------------------------------------------------------------------------
# validate_cores
# ---------------------------------------------------------------------------

test_that("validate_cores accepts NA", {
  expect_true(validate_cores(NA))
  expect_true(validate_cores(NA_integer_))
})

test_that("validate_cores accepts integers >= 1", {
  expect_true(validate_cores(1))
  expect_true(validate_cores(4))
})

test_that("validate_cores rejects 0 and negative integers", {
  expect_type(validate_cores(0), "character")
  expect_type(validate_cores(-1), "character")
})

test_that("validate_cores rejects non-numeric values", {
  expect_type(validate_cores("four"), "character")
})

# ---------------------------------------------------------------------------
# validate_nonempty_chr
# ---------------------------------------------------------------------------

test_that("validate_nonempty_chr accepts NULL", {
  expect_true(validate_nonempty_chr(NULL))
})

test_that("validate_nonempty_chr accepts non-empty character vectors", {
  expect_true(validate_nonempty_chr(c("a", "b")))
})

test_that("validate_nonempty_chr rejects empty character vector", {
  expect_type(validate_nonempty_chr(character(0)), "character")
})

# ---------------------------------------------------------------------------
# validate_null
# ---------------------------------------------------------------------------

test_that("validate_null returns error for required field", {
  spec <- field_spec(default = required())
  result <- validate_null("my_field", spec)
  expect_match(result, "required field")
})

test_that("validate_null returns error when nullable = FALSE", {
  spec <- field_spec(default = "x", nullable = FALSE)
  result <- validate_null("my_field", spec)
  expect_match(result, "NULL is not allowed")
})

test_that("validate_null returns NULL when nullable = TRUE", {
  spec <- field_spec(default = NULL, nullable = TRUE)
  result <- validate_null("my_field", spec)
  expect_null(result)
})

# ---------------------------------------------------------------------------
# validate_type
# ---------------------------------------------------------------------------

test_that("validate_type returns NULL when no type constraint", {
  spec <- field_spec()
  expect_null(validate_type("f", 42, spec))
})

test_that("validate_type returns NULL when type matches", {
  spec <- field_spec(type = "character")
  expect_null(validate_type("f", "hello", spec))
})

test_that("validate_type returns error string on type mismatch", {
  spec <- field_spec(type = "character")
  result <- validate_type("f", 42, spec)
  expect_type(result, "character")
  expect_match(result, "expected type")
})

test_that("validate_type accepts multiple allowed types", {
  spec <- field_spec(type = c("character", "numeric"))
  expect_null(validate_type("f", "hello", spec))
  expect_null(validate_type("f", 3.14, spec))
  result <- validate_type("f", TRUE, spec)
  # logical is not in allowed types
  expect_type(result, "character")
})

# ---------------------------------------------------------------------------
# validate_choices
# ---------------------------------------------------------------------------

test_that("validate_choices returns NULL when choices is NULL", {
  spec <- field_spec()
  expect_null(validate_choices("f", "anything", spec))
})

test_that("validate_choices returns NULL when value is in choices", {
  spec <- field_spec(choices = c("a", "b", "c"))
  expect_null(validate_choices("f", "b", spec))
})

test_that("validate_choices returns error when value not in choices", {
  spec <- field_spec(choices = c("a", "b"))
  result <- validate_choices("f", "z", spec)
  expect_type(result, "character")
  expect_match(result, "not allowed")
})

# ---------------------------------------------------------------------------
# validate_min / validate_max # nolint: commented_code_linter
# ---------------------------------------------------------------------------

test_that("validate_min returns NULL when no min", {
  spec <- field_spec()
  expect_null(validate_min("f", -999, spec))
})

test_that("validate_min returns NULL when value >= min", {
  spec <- field_spec(min = 0)
  expect_null(validate_min("f", 0, spec))
  expect_null(validate_min("f", 5, spec))
})

test_that("validate_min returns error when value < min", {
  spec <- field_spec(min = 0)
  result <- validate_min("f", -1, spec)
  expect_type(result, "character")
  expect_match(result, "below the minimum")
})

test_that("validate_max returns NULL when no max", {
  spec <- field_spec()
  expect_null(validate_max("f", 9999, spec))
})

test_that("validate_max returns NULL when value <= max", {
  spec <- field_spec(max = 100)
  expect_null(validate_max("f", 100, spec))
  expect_null(validate_max("f", 50, spec))
})

test_that("validate_max returns error when value > max", {
  spec <- field_spec(max = 100)
  result <- validate_max("f", 101, spec)
  expect_type(result, "character")
  expect_match(result, "exceeds the maximum")
})

# ---------------------------------------------------------------------------
# validate_custom
# ---------------------------------------------------------------------------

test_that("validate_custom returns NULL when no validator", {
  spec <- field_spec()
  expect_null(validate_custom("f", "val", spec))
})

test_that("validate_custom returns NULL when validator returns TRUE", {
  spec <- field_spec(validator = function(v) TRUE)
  expect_null(validate_custom("f", "val", spec))
})

test_that(
  "validate_custom returns error string when validator returns message",
  {
    spec <- field_spec(validator = function(v) "bad value")
    result <- validate_custom("f", "val", spec)
    expect_type(result, "character")
    expect_match(result, "bad value")
  }
)

# ---------------------------------------------------------------------------
# validate_field
# ---------------------------------------------------------------------------

test_that(
  "validate_field returns no errors for a valid nullable field with NULL",
  {
    spec <- field_spec(nullable = TRUE)
    expect_length(validate_field("f", NULL, spec), 0)
  }
)

test_that("validate_field returns error for required field with NULL", {
  spec <- field_spec(default = required())
  errs <- validate_field("f", NULL, spec)
  expect_gt(length(errs), 0)
})

test_that("validate_field returns no errors for valid value", {
  spec <- field_spec(type = "numeric", min = 0, max = 100)
  errs <- validate_field("f", 50, spec)
  expect_length(errs, 0)
})

test_that("validate_field accumulates multiple errors", {
  spec <- field_spec(type = "character", choices = c("a", "b"))
  # value is numeric AND not in choices
  errs <- validate_field("f", 99, spec)
  expect_gte(length(errs), 1)
})

# ---------------------------------------------------------------------------
# Cross-field validators
# ---------------------------------------------------------------------------

make_state <- function(...) {
  defaults <- list(
    init_workspace = FALSE,
    stics_exe = NULL,
    usms_workspace = NULL,
    metadata_file = NULL,
    parallel = FALSE,
    cores = NA
  )
  overrides <- list(...)
  for (nm in names(overrides)) defaults[[nm]] <- overrides[[nm]]
  defaults
}

test_that("check_metadata_file passes when run_simulations = FALSE", {
  expect_true(check_metadata_file(make_state(run_simulations = FALSE)))
})

test_that("check_metadata_file fails when metadata_file is NULL", {
  s <- make_state(run_simulations = TRUE, metadata_file = NULL)
  result <- check_metadata_file(s)
  expect_type(result, "character")
  expect_match(result, "metadata_file")
})

test_that(
  "check_metadata_file fails when metadata_file does not exist",
  {
    s <- make_state(
      run_simulations = TRUE,
      metadata_file = file.path("nonexistent", "path.csv")
    )
    result <- check_metadata_file(s)
    expect_type(result, "character")
    expect_match(result, "not found")
  }
)

test_that("check_metadata_file passes with an existing file", {
  tmp <- tempfile()
  file.create(tmp)
  on.exit(unlink(tmp))
  s <- make_state(run_simulations = TRUE, metadata_file = tmp)
  expect_true(check_metadata_file(s))
})

test_that("check_parallel_cores passes when parallel = FALSE", {
  expect_true(check_parallel_cores(make_state(parallel = FALSE)))
})

test_that("check_parallel_cores passes when parallel = TRUE and cores set", {
  s <- make_state(parallel = TRUE, cores = 4)
  expect_true(check_parallel_cores(s))
})

test_that("check_parallel_cores fails when parallel = TRUE and cores is NA", {
  s <- make_state(parallel = TRUE, cores = NA)
  result <- check_parallel_cores(s)
  expect_type(result, "character")
  expect_match(result, "cores")
})

test_that("check_parallel_cores fails when parallel = TRUE and cores is NULL", {
  s <- make_state(parallel = TRUE, cores = NULL)
  result <- check_parallel_cores(s)
  expect_type(result, "character")
})

# ---------------------------------------------------------------------------
# validate_schema (integration)
# ---------------------------------------------------------------------------

make_valid_list <- function(...) {
  base <- list(
    eval_workspace = "ws",
    stics_exe = NULL,
    usms_workspace = NULL,
    metadata_file = NULL,
    output_dir = NULL,
    run_simulations = FALSE,
    init_workspace = FALSE,
    parallel = FALSE,
    verbose = 1L,
    cores = NA,
    percentage = 5,
    reference_version = NULL,
    species = NULL,
    usms = NULL,
    var2exclude = NULL
  )
  overrides <- list(...)
  for (nm in names(overrides)) base[[nm]] <- overrides[[nm]]
  base
}

test_that("validate_schema returns invisible TRUE for a valid config", {
  result <- validate_schema(make_valid_list())
  expect_true(result)
})

test_that("validate_schema stops with message for wrong type", {
  cfg <- make_valid_list(run_simulations = "yes")
  expect_error(validate_schema(cfg), "invalid argument type")
})

test_that("validate_schema stops when percentage out of range", {
  cfg <- make_valid_list(percentage = 150)
  expect_error(validate_schema(cfg), "exceeds the maximum")
})

test_that(
  "validate_schema reports cross-field error for parallel without cores",
  {
    cfg <- make_valid_list(parallel = TRUE, cores = NA)
    expect_error(validate_schema(cfg), "cores")
  }
)

test_that("validate_schema collects multiple errors before stopping", {
  cfg <- make_valid_list()
  cfg$eval_workspace <- NULL
  cfg$run_simulations <- "bad"
  err <- tryCatch(validate_schema(cfg), error = function(e) e$message)
  # Both errors should appear in the same message
  expect_match(err, "invalid argument type")
})

# ---------------------------------------------------------------------------
# schema_public_fields
# ---------------------------------------------------------------------------

test_that("schema_public_fields returns a named list with all schema fields", {
  fields <- schema_public_fields()
  expect_type(fields, "list")
  expect_true(all(names(config_schema$fields) %in% names(fields)))
})

test_that("schema_public_fields all values are NULL", {
  fields <- schema_public_fields()
  expect_true(all(vapply(fields, is.null, logical(1))))
})

# ---------------------------------------------------------------------------
# schema_initialize
# ---------------------------------------------------------------------------

test_that("schema_initialize applies defaults for unspecified fields", {
  obj <- new.env(parent = emptyenv())
  schema_initialize(
    obj,
    list(eval_workspace = "/path"),
    config_schema
  )
  expect_false(obj$run_simulations)
  expect_false(obj$parallel)
  expect_identical(obj$percentage, 5)
})

test_that("schema_initialize uses provided values over defaults", {
  obj <- new.env(parent = emptyenv())
  schema_initialize(
    obj,
    list(eval_workspace = "/path", percentage = 10),
    config_schema
  )
  expect_identical(obj$percentage, 10)
})

# ---------------------------------------------------------------------------
# Configuration R6 class
# ---------------------------------------------------------------------------

test_that("Configuration initializes with minimal required args", {
  cfg <- Configuration$new(
    eval_workspace = "ws",
    usms_workspace = "usms_ws",
    metadata_file = build_metadata_file()
  )
  expect_s3_class(cfg, "R6")
  expect_identical(cfg$eval_workspace, "ws")
})

test_that("Configuration applies defaults correctly", {
  cfg <- Configuration$new(eval_workspace = "ws")
  expect_false(cfg$run_simulations)
  expect_false(cfg$parallel)
  expect_identical(cfg$percentage, 5)
  expect_identical(cfg$verbose, 1L)
})

test_that("Configuration raises error on invalid type", {
  expect_error(
    Configuration$new(eval_workspace = "/p", verbose = "loud"),
    "expected type"
  )
})

test_that("Configuration raises error when percentage is out of range", {
  expect_error(
    Configuration$new(eval_workspace = "/p", percentage = -1),
    "below the minimum"
  )
  expect_error(
    Configuration$new(eval_workspace = "/p", percentage = 101),
    "exceeds the maximum"
  )
})

test_that("Configuration raises error when parallel = TRUE and cores is NA", {
  expect_error(
    Configuration$new(eval_workspace = "/p", parallel = TRUE, cores = NA),
    "cores"
  )
})

test_that("Configuration accepts valid cores when parallel = TRUE", {
  cfg <- Configuration$new(
    eval_workspace = "ws",
    parallel = TRUE,
    cores = 2L
  )
  expect_identical(cfg$cores, 2L)
})

# ---------------------------------------------------------------------------
# validate_eval
# ---------------------------------------------------------------------------

test_that(
  "validate_eval passes when eval_workspace, stics_exe and usms_workspace is set",
  {
    cfg <- Configuration$new(eval_workspace = "ws", stics_exe = "stics", usms_workspace = "usms_ws")
    expect_r6_class(cfg$validate_eval(), "Configuration")
  }
)

# ---------------------------------------------------------------------------
# validate_export
# ---------------------------------------------------------------------------

test_that("validate_export stops when output_dir is NULL", {
  cfg <- Configuration$new(eval_workspace = "ws")
  expect_error(cfg$validate_export(), "Output dir")
})

test_that("validate_export passes when output_dir exists", {
  tmp <- tempdir()
  cfg <- Configuration$new(
    eval_workspace = "wp",
    output_dir = tmp
  )
  expect_r6_class(cfg$validate_export(), "Configuration")
})

test_that("validate_export creates output_dir if it does not exist", {
  tmp <- file.path(tempdir(), paste0("test_out_", sample.int(1e6, 1)))
  on.exit(unlink(tmp, recursive = TRUE))
  cfg <- Configuration$new(
    eval_workspace = "ws",
    output_dir = tmp
  )
  cfg$validate_export()
  expect_true(dir.exists(tmp))
})

# ---------------------------------------------------------------------------
# validate_balance_closure
# ---------------------------------------------------------------------------

test_that("validate_balance_closure stops when usms_workspace is NULL", {
  cfg <- Configuration$new(eval_workspace = "ws")
  expect_error(cfg$validate_balance_closure(), "USMs workspace")
})

test_that("validate_balance_closure stops when stics_exe is NULL", {
  cfg <- Configuration$new(
    eval_workspace = "ws",
    usms_workspace = "usms_ws"
  )
  expect_error(cfg$validate_balance_closure(), "STICS executable")
})
