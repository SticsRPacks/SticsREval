test_that("USMEvaluation computes RMSE ratios correctly", {
  stats_usm <- data.frame(
    situation = c("USM1", "USM1"),
    variable = c("lai", "lai"),
    group = c("reference", "evaluated"),
    RMSE = c(2, 4),
    n_obs = c(20, 20),
    stringsAsFactors = FALSE
  )

  stats_species <- data.frame(
    variable = "lai",
    group = "evaluated",
    RMSE = 4,
    stringsAsFactors = FALSE
  )

  eval <- USMEvaluation$new(
    species = "wheat",
    stats_usm = stats_usm,
    stats_species = stats_species
  )

  data <- eval$get_data()

  expect_identical(nrow(data), 1L)
  expect_identical(data$rmse_ratio, 50)
  expect_identical(data$rmse_eval, 4)
  expect_identical(data$rmse_ref, 2)
  expect_identical(data$rmse_species, 4)
  expect_identical(data$species, "wheat")
})

test_that("critical_vars returns only variables above ratio threshold", {
  data <- data.frame(
    species = "wheat",
    situation = c("A", "A", "B"),
    variable = c("lai", "yield", "lai"),
    rmse_ratio = c(60, 30, 80),
    n_obs = 20,
    stringsAsFactors = FALSE
  )

  eval <- USMEvaluation$new(
    species = "wheat",
    data = data
  )

  critical <- eval$critical_vars

  expect_identical(nrow(critical), 2L)
  expect_true(all(critical$rmse_ratio > 50))
})

test_that("degraded_vars returns only degraded variables", {
  data <- data.frame(
    species = "wheat",
    situation = c("A", "A", "B"),
    variable = c("lai", "yield", "lai"),
    rmse_ratio = c(10, 25, 60),
    n_obs = 20,
    stringsAsFactors = FALSE
  )

  eval <- USMEvaluation$new(
    species = "wheat",
    data = data
  )

  degraded <- eval$degraded_vars

  expect_identical(nrow(degraded), 2L)
  expect_true(all(degraded$rmse_ratio > 20))
})

test_that("USM fails when one variable exceeds critical threshold", {

  data <- data.frame(
    species = "wheat",
    situation = c("USM1", "USM2"),
    variable = c("lai", "lai"),
    rmse_ratio = c(55, 10),
    n_obs = 20,
    stringsAsFactors = FALSE
  )

  eval <- USMEvaluation$new(
    species = "wheat",
    data = data
  )

  failed <- eval$failed_usms

  expect_identical(failed$situation, "USM1")
})

test_that("USM fails when one variable exceeds critical threshold", {

  data <- data.frame(
    species = "wheat",
    situation = c("USM1", "USM2"),
    variable = c("lai", "lai"),
    rmse_ratio = c(55, 10),
    n_obs = 20,
    stringsAsFactors = FALSE
  )

  eval <- USMEvaluation$new(
    species = "wheat",
    data = data
  )

  failed <- eval$failed_usms

  expect_identical(failed$situation, "USM1")
})

test_that("USM fails when too many degraded variables are present", {

  data <- data.frame(
    species = "wheat",
    situation = rep("USM1", 4),
    variable = c("lai", "yield", "biomass", "height"),
    rmse_ratio = c(21, 22, 25, 30),
    n_obs = 20,
    stringsAsFactors = FALSE
  )

  eval <- USMEvaluation$new(
    species = "wheat",
    data = data
  )

  failed <- eval$failed_usms

  expect_identical(nrow(failed), 1L)
  expect_identical(failed$situation, "USM1")
})

test_that("success is TRUE when no USM fails", {

  data <- data.frame(
    species = "wheat",
    situation = c("A", "B"),
    variable = c("lai", "lai"),
    rmse_ratio = c(5, 10),
    n_obs = 20,
    stringsAsFactors = FALSE
  )

  eval <- USMEvaluation$new(
    species = "wheat",
    data = data
  )

  expect_true(eval$success)
})

test_that("success is FALSE when one USM fails", {

  data <- data.frame(
    species = "wheat",
    situation = "A",
    variable = "lai",
    rmse_ratio = 70,
    n_obs = 20,
    stringsAsFactors = FALSE
  )

  eval <- USMEvaluation$new(
    species = "wheat",
    data = data
  )

  expect_false(eval$success)
})

test_that("data are sorted by decreasing RMSE ratio", {

  data <- data.frame(
    species = "wheat",
    situation = c("A", "B"),
    variable = c("lai", "yield"),
    rmse_ratio = c(10, 80),
    n_obs = 20,
    stringsAsFactors = FALSE
  )

  eval <- USMEvaluation$new(
    species = "wheat",
    data = data
  )

  expect_identical(eval$get_data()$rmse_ratio, c(80, 10))
})

test_that("initialize fails without inputs", {
  expect_error(
    USMEvaluation$new(),
    "must be defined"
  )
})

test_that("variables with fewer than 10 observations are ignored", {

  stats_usm <- data.frame(
    situation = c("USM1", "USM1"),
    variable = c("lai", "lai"),
    group = c("reference", "evaluated"),
    RMSE = c(2, 5),
    n_obs = c(5, 5),
    stringsAsFactors = FALSE
  )

  stats_species <- data.frame(
    variable = "lai",
    group = "evaluated",
    RMSE = 4,
    stringsAsFactors = FALSE
  )

  eval <- USMEvaluation$new(
    species = "wheat",
    stats_usm = stats_usm,
    stats_species = stats_species
  )

  expect_identical(nrow(eval$failed_usms), 0L)
})

test_that("get_data returns an empty data frame with the expected columns when there is no data", { # nolint: line_length_linter
  workspace <- list(get_species = function() character(0))
  backend <- list()

  eval <- USMEvaluation$new(
    eval_workspace = "ws", workspace = workspace, backend = backend
  )
  eval$run()

  data <- eval$get_data()

  expect_s3_class(data, "data.frame")
  expect_identical(nrow(data), 0L)
  expect_setequal(
    names(data),
    c(
      "species", "situation", "variable", "rmse_eval", "rmse_ref",
      "rmse_species", "rmse_ratio", "n_obs"
    )
  )
})

test_that("export does not error when no species were evaluated at all", {

  output_dir <- file.path(tempdir(), "usm_eval_no_species")
  unlink(output_dir, recursive = TRUE)
  dir.create(output_dir, recursive = TRUE)

  workspace <- list(get_species = function() character(0))
  backend <- list()

  eval <- USMEvaluation$new(
    eval_workspace = "ws", output_dir = output_dir,
    workspace = workspace, backend = backend
  )
  eval$run()

  expect_no_error(eval$export())
})

test_that("export does not error when no species has a deteriorated USM", {

  output_dir <- file.path(tempdir(), "usm_eval_no_deteriorated")
  unlink(output_dir, recursive = TRUE)
  dir.create(output_dir, recursive = TRUE)

  eval <- USMEvaluation$new(
    species = "wheat",
    data = data.frame(
      species = "wheat",
      situation = "USM1",
      variable = "lai",
      rmse_eval = 2,
      rmse_ref = 2,
      rmse_species = 4,
      rmse_ratio = 0,
      n_obs = 20,
      stringsAsFactors = FALSE
    )
  )
  # Manual mode never runs gen_usm_comparisons(), so `deteriorated_usm`
  # stays empty here just like it would for a real run() where every
  # species passed: inject output_dir/logger the same way run() would.
  replace_private(eval, "output_dir", output_dir)
  replace_private(eval, "logger", default_logger)

  expect_no_error(eval$export())
  expect_false(
    file.exists(file.path(output_dir, "csv", "Deteriorated_USM.csv"))
  )
})

test_that("export writes failed_usms.csv matching `failed_usms`", {
  output_dir <- file.path(tempdir(), "usm_eval_failed_usms")
  unlink(output_dir, recursive = TRUE)
  dir.create(output_dir, recursive = TRUE)

  eval <- USMEvaluation$new(
    species = "wheat",
    data = data.frame(
      species = "wheat",
      situation = c("USM1", "USM2"),
      variable = "lai",
      rmse_eval = 2,
      rmse_ref = 2,
      rmse_species = 4,
      rmse_ratio = c(60, 0),
      n_obs = 20,
      stringsAsFactors = FALSE
    )
  )
  replace_private(eval, "output_dir", output_dir)
  replace_private(eval, "logger", default_logger)

  eval$export()

  written <- read.csv(
    file.path(output_dir, "csv", "failed_usms.csv"), stringsAsFactors = FALSE
  )
  expect_identical(written, eval$failed_usms)
})

test_that("export does not write failed_usms.csv when no USM failed", {
  output_dir <- file.path(tempdir(), "usm_eval_no_failed_usms")
  unlink(output_dir, recursive = TRUE)
  dir.create(output_dir, recursive = TRUE)

  eval <- USMEvaluation$new(
    species = "wheat",
    data = data.frame(
      species = "wheat",
      situation = "USM1",
      variable = "lai",
      rmse_eval = 2,
      rmse_ref = 2,
      rmse_species = 4,
      rmse_ratio = 0,
      n_obs = 20,
      stringsAsFactors = FALSE
    )
  )
  replace_private(eval, "output_dir", output_dir)
  replace_private(eval, "logger", default_logger)

  eval$export()

  expect_false(file.exists(file.path(output_dir, "csv", "failed_usms.csv")))
})
