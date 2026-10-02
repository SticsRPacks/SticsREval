test_that("SpeciesEvaluation initializes correctly", {
  eval <- SpeciesEvaluation$new(
    parallel = FALSE,
    cores = 1,
    eval_workspace = tempdir(),
    species = NULL,
    usms = NULL,
    percentage = 10,
    workspace = mock(),
    backend = mock()
  )

  expect_s3_class(eval, "SpeciesEvaluation")
})


test_that("success is TRUE before any comparison is run", {
  eval <- SpeciesEvaluation$new(
    parallel = FALSE,
    cores = 1,
    eval_workspace = tempdir(),
    species = NULL,
    usms = NULL,
    percentage = 10,
    workspace = mock(),
    backend = mock()
  )

  expect_true(eval$success)
})


test_that("summary works when no comparison was generated", {
  eval <- SpeciesEvaluation$new(
    parallel = FALSE,
    cores = 1,
    eval_workspace = tempdir(),
    species = NULL,
    usms = NULL,
    percentage = 10,
    workspace = mock(),
    backend = mock()
  )

  expect_no_error(eval$summary())
})


test_that("run returns without error when no species are available", {
  workspace <- list(
    get_species = function() character(0)
  )

  backend <- list()

  eval <- SpeciesEvaluation$new(
    parallel = FALSE,
    cores = 1,
    eval_workspace = tempdir(),
    species = NULL,
    usms = NULL,
    percentage = 10,
    var2exclude = NULL,
    workspace = workspace,
    backend = backend
  )

  expect_no_error(eval$run())
})


test_that("export works when no comparison exists", {
  output_dir <- file.path(tempdir(), "species_eval")
  unlink(output_dir, recursive = TRUE)

  backend <- list(
    run = function(...) NULL
  )

  eval <- SpeciesEvaluation$new(
    parallel = FALSE,
    cores = 1,
    output_dir = output_dir,
    eval_workspace = tempdir(),
    species = NULL,
    usms = NULL,
    percentage = 10,
    var2exclude = NULL,
    workspace = mock(),
    backend = backend
  )

  expect_no_error(eval$export())

  expect_true(dir.exists(file.path(output_dir, "plots")))
})


test_that("run propagates backend errors", {

  backend <- list(
    run = function(...) {
      stop("backend failure", call. = FALSE)
    }
  )

  workspace <- list(
    get_species = function() "wheat",
    get_species_situations = function(...) {
      data.frame(situation = "USM1", stringsAsFactors = FALSE)
    }
  )

  eval <- SpeciesEvaluation$new(
    parallel = FALSE,
    cores = 1,
    eval_workspace = tempdir(),
    species = NULL,
    usms = NULL,
    percentage = 10,
    var2exclude = NULL,
    workspace = workspace,
    backend = backend
  )

  expect_error(
    eval$run(),
    "backend failure"
  )
})


test_that("run skips the comparison of species without reference data", {
  d <- make_eval_data()
  eval <- SpeciesEvaluation$new(
    percentage = 10,
    workspace = mock_data_workspace(d$sim, d$obs, d$ref_sim),
    backend = ParallelBackend$new(FALSE, NA)
  )

  expect_no_error(eval$run())
  expect_false(is.null(eval$get_species_stats("maize")$stats))
  expect_false(is.null(eval$get_species_stats("wheat")$stats))
  expect_no_error(eval$summary())

  output_dir <- withr::local_tempdir()
  eval_out <- SpeciesEvaluation$new(
    percentage = 10,
    output_dir = output_dir,
    workspace = mock_data_workspace(d$sim, d$obs, d$ref_sim),
    backend = ParallelBackend$new(FALSE, NA)
  )
  eval_out$run()
  expect_no_error(eval_out$export())
  comparison <- read.csv(
    file.path(output_dir, "csv", "species_rrmse_comparison.csv")
  )
  expect_identical(unique(comparison$species), "wheat")
})


test_that("skip_reason reports missing reference data", {
  d <- make_eval_data()
  eval <- SpeciesEvaluation$new(
    workspace = mock_data_workspace(d$sim, d$obs),
    backend = ParallelBackend$new(FALSE, NA)
  )
  eval$run()

  expect_identical(eval$skip_reason, "no reference data")
  expect_identical(evaluation_status(eval), "not evaluated")
})
