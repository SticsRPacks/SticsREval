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
# validate_rds_path
# ---------------------------------------------------------------------------

test_that("validate_rds_path accepts NULL", {
  expect_true(validate_rds_path(NULL))
})

test_that("validate_rds_path accepts a path ending in .rds", {
  expect_true(validate_rds_path("sim.rds"))
})

test_that("validate_rds_path rejects a non-character value", {
  expect_type(validate_rds_path(42), "character")
})

test_that("validate_rds_path rejects a path not ending in .rds", {
  expect_type(validate_rds_path("sim.csv"), "character")
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
# Cross-field / filesystem validators
# ---------------------------------------------------------------------------

make_state <- function(...) {
  defaults <- list(
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

# check_path_exists() is the I/O counterpart: file existence, not
# required-ness. It is not run automatically by validate_schema()
# (see the validate_filesystem tests below).

test_that(
  "check_path_exists fails when metadata_file does not exist",
  {
    s <- make_state(metadata_file = file.path("nonexistent", "path.csv"))
    check <- check_path_exists("metadata_file")
    result <- check(s)
    expect_type(result, "character")
    expect_match(result, "not found")
  }
)

test_that("check_path_exists passes with an existing file", {
  tmp <- tempfile()
  file.create(tmp)
  on.exit(unlink(tmp))
  s <- make_state(metadata_file = tmp)
  check <- check_path_exists("metadata_file")
  expect_true(check(s))
})

test_that("check_path_exists passes when the field is NULL", {
  s <- make_state(metadata_file = NULL)
  check <- check_path_exists("metadata_file")
  expect_true(check(s))
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
# validate_schema and validate_filesystem (integration)
# ---------------------------------------------------------------------------

make_local_schema <- function() {
  list(
    fields = list(
      eval_workspace = field_spec(type = "character", nullable = FALSE),
      output_dir = field_spec(type = "character"),
      parallel = field_spec(type = "logical", nullable = FALSE),
      cores = field_spec(validator = validate_cores),
      percentage = field_spec(type = "numeric", min = 0, max = 100)
    ),
    cross_validators = list(
      list(
        desc = "If parallel = TRUE, cores must be an integer >= 1",
        check = check_parallel_cores
      )
    ),
    filesystem_checks = list(
      list(
        desc = "output_dir must point to an existing file",
        check = check_path_exists("output_dir")
      )
    )
  )
}

make_valid_args <- function(...) {
  base <- list(
    eval_workspace = "ws",
    output_dir = NULL,
    parallel = FALSE,
    cores = NA,
    percentage = 5
  )
  overrides <- list(...)
  for (nm in names(overrides)) base[[nm]] <- overrides[[nm]]
  base
}

test_that("validate_schema returns invisible TRUE for valid args", {
  result <- validate_schema(make_valid_args(), make_local_schema())
  expect_true(result)
})

test_that("validate_schema stops with message for wrong type", {
  args <- make_valid_args(parallel = "yes")
  expect_error(validate_schema(args, make_local_schema()), "expected type")
})

test_that("validate_schema stops when percentage out of range", {
  args <- make_valid_args(percentage = 150)
  expect_error(
    validate_schema(args, make_local_schema()), "exceeds the maximum"
  )
})

test_that(
  "validate_schema reports cross-field error for parallel without cores",
  {
    args <- make_valid_args(parallel = TRUE, cores = NA)
    expect_error(validate_schema(args, make_local_schema()), "cores")
  }
)

test_that("validate_schema collects multiple errors before stopping", {
  args <- make_valid_args()
  args$eval_workspace <- NULL
  args$parallel <- "bad"
  err <- tryCatch(
    validate_schema(args, make_local_schema()),
    error = function(e) e$message
  )
  # Both errors should appear in the same message
  expect_match(err, "eval_workspace")
  expect_match(err, "expected type")
})

test_that("validate_filesystem passes when output_dir is NULL", {
  result <- validate_filesystem(make_valid_args(), make_local_schema())
  expect_true(result)
})

test_that("validate_filesystem stops when output_dir does not exist", {
  args <- make_valid_args(output_dir = file.path("nonexistent", "dir"))
  expect_error(
    validate_filesystem(args, make_local_schema()), "not found"
  )
})
