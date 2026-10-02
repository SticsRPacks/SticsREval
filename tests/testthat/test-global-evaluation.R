test_that("GlobalEvaluation initializes correctly", {

  eval <- GlobalEvaluation$new(
    eval_workspace = tempdir(),
    usms = NULL,
    var2exclude = NULL,
    percentage = 10,
    output_dir = tempdir(),
    workspace = mock()
  )

  expect_s3_class(eval, "GlobalEvaluation")
})


test_that("success is FALSE before running evaluation", {

  eval <- GlobalEvaluation$new(
    eval_workspace = tempdir(),
    usms = NULL,
    var2exclude = NULL,
    percentage = 10,
    output_dir = tempdir(),
    workspace = mock()
  )

  expect_false(eval$success)
})


test_that("summary works when no comparison exists", {

  eval <- GlobalEvaluation$new(
    eval_workspace = tempdir(),
    usms = NULL,
    var2exclude = NULL,
    percentage = 10,
    output_dir = tempdir(),
    workspace = mock()
  )

  expect_no_error(eval$summary())
})


test_that("export returns when no statistics are available", {

  output_dir <- file.path(tempdir(), "global_eval")
  unlink(output_dir, recursive = TRUE)

  eval <- GlobalEvaluation$new(
    eval_workspace = tempdir(),
    usms = NULL,
    var2exclude = NULL,
    percentage = 10,
    output_dir = output_dir,
    workspace = mock()
  )

  expect_no_error(eval$export())
})


test_that("run propagates workspace errors", {

  workspace <- list(
    get_sim = function(...) stop("workspace failure", call. = FALSE)
  )

  eval <- GlobalEvaluation$new(
    eval_workspace = tempdir(),
    usms = NULL,
    var2exclude = NULL,
    percentage = 10,
    output_dir = tempdir(),
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

  eval <- GlobalEvaluation$new(
    eval_workspace = tempdir(),
    usms = NULL,
    var2exclude = NULL,
    percentage = 10,
    output_dir = output_dir,
    workspace = mock()
  )

  eval$export()

  expect_false(file.exists(
    file.path(output_dir, "csv", "global_stats.csv")
  ))
})


test_that("run skips the comparison when there is no reference data", {
  d <- make_eval_data()
  eval <- GlobalEvaluation$new(
    workspace = mock_data_workspace(d$sim, d$obs)
  )

  expect_no_error(eval$run())
  expect_no_error(eval$summary())
})

test_that("run compares only USMs having reference data", {
  d <- make_eval_data()
  output_dir <- withr::local_tempdir()
  eval <- GlobalEvaluation$new(
    workspace = mock_data_workspace(d$sim, d$obs, d$ref_sim),
    output_dir = output_dir
  )

  expect_no_error(eval$run())
  eval$export()

  stats <- read.csv(file.path(output_dir, "csv", "global_stats.csv"))
  # Only the 2 wheat USMs (12 dates each) have reference simulations.
  expect_identical(unique(stats$n_obs), 24L)
  expect_setequal(stats$group, c("evaluated", "reference"))
})

test_that("skip_reason reports missing reference data", {
  d <- make_eval_data()
  eval <- GlobalEvaluation$new(
    workspace = mock_data_workspace(d$sim, d$obs)
  )
  eval$run()

  expect_identical(eval$skip_reason, "no reference data")
  expect_identical(evaluation_status(eval), "not evaluated")

  eval_ref <- GlobalEvaluation$new(
    workspace = mock_data_workspace(d$sim, d$obs, d$ref_sim)
  )
  eval_ref$run()
  expect_null(eval_ref$skip_reason)
})
