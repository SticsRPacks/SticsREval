library(testthat)
library(mockery)
library(withr)

# -----------------------------
# prepare_species_output_dir
# -----------------------------

test_that("prepare_species_output_dir creates directory when missing", {
  tmp <- local_tempdir()

  out <- prepare_species_output_dir(tmp, "wheat")

  expect_true(dir.exists(out))
  expect_equal(basename(out), "wheat")
})

test_that("prepare_species_output_dir returns existing directory", {
  tmp <- local_tempdir()
  dir.create(file.path(tmp, "barley"))

  out <- prepare_species_output_dir(tmp, "barley")

  expect_true(dir.exists(out))
  expect_true(grepl("barley$", out))
})

test_that("prepare_species_output_dir fails when dir cannot be created", {
  tmp <- local_tempdir()

  stub(prepare_species_output_dir, "dir.create", function(...) FALSE)

  expect_error(
    prepare_species_output_dir(tmp, "maize"),
    "Can't create output directory"
  )
})

# -----------------------------
# export_stats_to_csv
# -----------------------------

test_that("export_stats_to_csv runs with minimal valid config", {

  tmp <- local_tempdir()

  config <- list(
    output_dir = tmp,
    percentage = 10,
    eval_workspace = "dummy"
  )

  config$validate_export <- function() NULL

  # ---- Mock EvalWorkspace ----
  ew <- R6::R6Class(
  "FakeWorkspace",
    public = list(
      get_species = function() c("wheat"),
      get_stats = function(...) data.frame(a = 1),
      get_rmse_per_usm = function(...) data.frame(b = 2),
      get_deteriorated_usm = function(...) {
        list(get_data = function() data.frame(c = 3))
      }
    )
  )$new()

  stub(export_stats_to_csv, "EvalWorkspace$new", function(...) ew)

  # avoid real logging noise
  stub(export_stats_to_csv, "logger::log_info", function(...) NULL)
  stub(export_stats_to_csv, "format_duration", function(...) "0s")

  # avoid real file writing
  stub(export_stats_to_csv, "safe_write_csv", function(data, path) {
    # simulate file creation
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    write.csv(data, path, row.names = FALSE)
  })

  expect_silent(export_stats_to_csv(config))

  expect_true(file.exists(file.path(tmp, "wheat", "Criteres_stats.csv")))
  expect_true(file.exists(file.path(tmp, "wheat", "RMSE_per_usm.csv")))
  expect_true(file.exists(file.path(tmp, "wheat", "Deteriorated_USM.csv")))
})

# -----------------------------
# export_stats_to_csv: null cases
# -----------------------------

test_that("export_stats_to_csv skips NULL datasets", {

  tmp <- local_tempdir()

  config <- list(
    output_dir = tmp,
    percentage = 10,
    eval_workspace = "dummy"
  )

  config$validate_export <- function() NULL

  ew <- R6::R6Class(
    "FakeWorkspace",
    public = list(
      get_species = function() c("wheat"),
      get_stats = function(...) NULL,
      get_rmse_per_usm = function(...) NULL,
      get_deteriorated_usm = function(...) {
        list(get_data = function() NULL)
      }
    )
  )$new()

  stub(export_stats_to_csv, "EvalWorkspace$new", function(...) ew)

  stub(export_stats_to_csv, "logger::log_info", function(...) NULL)
  stub(export_stats_to_csv, "format_duration", function(...) "0s")
  stub(export_stats_to_csv, "safe_write_csv", function(...) stop("should not be called"))

  expect_silent(export_stats_to_csv(config))
})
