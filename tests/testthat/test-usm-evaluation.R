test_that("USMEvaluation computes RMSE ratios correctly", {
  stats_usm <- data.frame(
    situation = c("USM1", "USM1"),
    variable = c("lai", "lai"),
    group = c("reference", "evaluated"),
    RMSE = c(2, 4),
    n_obs = c(20, 20)
  )

  stats_species <- data.frame(
    variable = "lai",
    group = "evaluated",
    RMSE = 4
  )

  eval <- USMEvaluation$new(
    species = "wheat",
    stats_usm = stats_usm,
    stats_species = stats_species
  )

  data <- eval$get_data()

  expect_equal(nrow(data), 1)
  expect_equal(data$rmse_ratio, 50)
  expect_equal(data$rmse_eval, 4)
  expect_equal(data$rmse_ref, 2)
  expect_equal(data$rmse_species, 4)
  expect_equal(data$species, "wheat")
})

test_that("critical_vars returns only variables above ratio threshold", {
  data <- data.frame(
    species = "wheat",
    situation = c("A", "A", "B"),
    variable = c("lai", "yield", "lai"),
    rmse_ratio = c(60, 30, 80),
    n_obs = 20
  )

  eval <- USMEvaluation$new(
    species = "wheat",
    data = data
  )

  critical <- eval$critical_vars

  expect_equal(nrow(critical), 2)
  expect_true(all(critical$rmse_ratio > 50))
})

test_that("degraded_vars returns only degraded variables", {
  data <- data.frame(
    species = "wheat",
    situation = c("A", "A", "B"),
    variable = c("lai", "yield", "lai"),
    rmse_ratio = c(10, 25, 60),
    n_obs = 20
  )

  eval <- USMEvaluation$new(
    species = "wheat",
    data = data
  )

  degraded <- eval$degraded_vars

  expect_equal(nrow(degraded), 2)
  expect_true(all(degraded$rmse_ratio > 20))
})

test_that("USM fails when one variable exceeds critical threshold", {

  data <- data.frame(
    species = "wheat",
    situation = c("USM1", "USM2"),
    variable = c("lai", "lai"),
    rmse_ratio = c(55, 10),
    n_obs = 20
  )

  eval <- USMEvaluation$new(
    species = "wheat",
    data = data
  )

  failed <- eval$failed_usms

  expect_equal(failed$situation, "USM1")
})

test_that("USM fails when one variable exceeds critical threshold", {

  data <- data.frame(
    species = "wheat",
    situation = c("USM1", "USM2"),
    variable = c("lai", "lai"),
    rmse_ratio = c(55, 10),
    n_obs = 20
  )

  eval <- USMEvaluation$new(
    species = "wheat",
    data = data
  )

  failed <- eval$failed_usms

  expect_equal(failed$situation, "USM1")
})

test_that("USM fails when too many degraded variables are present", {

  data <- data.frame(
    species = "wheat",
    situation = rep("USM1", 4),
    variable = c("lai", "yield", "biomass", "height"),
    rmse_ratio = c(21, 22, 25, 30),
    n_obs = 20
  )

  eval <- USMEvaluation$new(
    species = "wheat",
    data = data
  )

  failed <- eval$failed_usms

  expect_equal(nrow(failed), 1)
  expect_equal(failed$situation, "USM1")
})

test_that("success is TRUE when no USM fails", {

  data <- data.frame(
    species = "wheat",
    situation = c("A", "B"),
    variable = c("lai", "lai"),
    rmse_ratio = c(5, 10),
    n_obs = 20
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
    n_obs = 20
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
    n_obs = 20
  )

  eval <- USMEvaluation$new(
    species = "wheat",
    data = data
  )

  expect_equal(eval$get_data()$rmse_ratio, c(80, 10))
})

test_that("initialize fails without inputs", {
  expect_error(
    {
      USMEvaluation$new()
    },
    "must be defined"
  )
})

test_that("variables with fewer than 10 observations are ignored", {

  stats_usm <- data.frame(
    situation = c("USM1", "USM1"),
    variable = c("lai", "lai"),
    group = c("reference", "evaluated"),
    RMSE = c(2, 5),
    n_obs = c(5, 5)
  )

  stats_species <- data.frame(
    variable = "lai",
    group = "evaluated",
    RMSE = 4
  )

  eval <- USMEvaluation$new(
    species = "wheat",
    stats_usm = stats_usm,
    stats_species = stats_species
  )

  expect_equal(nrow(eval$failed_usms), 0)
})
