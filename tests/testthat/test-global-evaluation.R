test_that("GlobalEvaluation initializes correctly", {

  config <- list(
    eval_workspace = tempdir(),
    usms = NULL,
    var2exclude = NULL,
    percentage = 10,
    output_dir = tempdir()
  )
  config$validate_eval <- function() {}

  eval <- GlobalEvaluation$new(
    config = config,
    workspace = mock()
  )

  expect_s3_class(eval, "GlobalEvaluation")
})


test_that("success is FALSE before running evaluation", {

  config <- list(
    eval_workspace = tempdir(),
    usms = NULL,
    var2exclude = NULL,
    percentage = 10,
    output_dir = tempdir()
  )
  config$validate_eval <- function() {}

  eval <- GlobalEvaluation$new(
    config = config,
    workspace = mock()
  )

  expect_false(eval$success)
})


test_that("summary works when no comparison exists", {

  config <- list(
    eval_workspace = tempdir(),
    usms = NULL,
    var2exclude = NULL,
    percentage = 10,
    output_dir = tempdir()
  )
  config$validate_eval <- function() {}

  eval <- GlobalEvaluation$new(
    config = config,
    workspace = mock()
  )

  expect_no_error(eval$summary())
})


test_that("export returns when no statistics are available", {

  output_dir <- file.path(tempdir(), "global_eval")
  unlink(output_dir, recursive = TRUE)

  config <- list(
    eval_workspace = tempdir(),
    usms = NULL,
    var2exclude = NULL,
    percentage = 10,
    output_dir = output_dir
  )
  config$validate_eval <- function() {}

  eval <- GlobalEvaluation$new(
    config = config,
    workspace = mock()
  )

  expect_no_error(eval$export())
})


test_that("run propagates workspace errors", {

  workspace <- list(
    get_sim = function(...) stop("workspace failure", call. = FALSE)
  )

  config <- list(
    eval_workspace = tempdir(),
    usms = NULL,
    var2exclude = NULL,
    percentage = 10,
    output_dir = tempdir()
  )
  config$validate_eval <- function() {}

  eval <- GlobalEvaluation$new(
    config = config,
    workspace = workspace
  )

  expect_error(
    eval$run(),
    "workspace failure"
  )
})


test_that("export does not create csv when stats are NULL", {

  output_dir <- file.path(tempdir(), "global_export")
  unlink(output_dir, recursive = TRUE)

  dir.create(output_dir, recursive = TRUE)

  config <- list(
    eval_workspace = tempdir(),
    usms = NULL,
    var2exclude = NULL,
    percentage = 10,
    output_dir = output_dir
  )
  config$validate_eval <- function() {}

  eval <- GlobalEvaluation$new(
    config = config,
    workspace = mock()
  )

  eval$export()

  expect_false(file.exists(
    file.path(output_dir, "global_stats.csv")
  ))
})
