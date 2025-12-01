library(testthat)
library(mockery)

test_that("evaluate_species returns NULL when reference file is missing", {

  species <- "Wheat"
  sim <- data.frame(a = 1)
  obs <- data.frame(b = 2)
  workspace <- tempdir()
  reference_data_dir <- tempdir()
  verbose <- FALSE
  stub(evaluate_species, "summary", function(sim, obs) {
    data.frame(RMSE = 1)
  })
  stub(evaluate_species, "safe_write_csv", function(x, path) TRUE)

  # Ensure reference file does not exist
  ref_file <- file.path(reference_data_dir, "Criteres_stats_Wheat.csv")
  expect_false(file.exists(ref_file))

  result <- evaluate_species(
    species, sim, obs,
    workspace,
    reference_data_dir,
    verbose
  )

  expect_null(result)
})


test_that("evaluate_species calls compare_rmse when reference file exists", {

  species <- "Corn"
  sim <- data.frame(a = 10)
  obs <- data.frame(b = 20)
  workspace <- tempdir()
  reference_data_dir <- tempdir()
  verbose <- FALSE
  stub(evaluate_species, "summary", function(sim, obs) {
    data.frame(RMSE = 10)
  })
  stub(evaluate_species, "safe_write_csv", function(x, path) TRUE)
  stub(evaluate_species, "read_csv", function(path) {
    data.frame(RMSE = 12)
  })

  compare_called <- FALSE
  compare_result <- "result_of_compare"

  stub(evaluate_species, "compare_rmse", function(species, ref, stats) {
    compare_called <<- TRUE
    return(compare_result)
  })

  # Create reference file
  ref_file <- file.path(reference_data_dir, "Criteres_stats_Corn.csv")
  write.csv(data.frame(RMSE = 12), ref_file, row.names = FALSE)

  result <- evaluate_species(
    species, sim, obs,
    workspace,
    reference_data_dir,
    verbose
  )

  expect_true(compare_called)
  expect_equal(result, compare_result)
})


test_that("evaluate_species writes stats when output_dir is not NULL", {

  species <- "Barley"
  sim <- data.frame(a = 1)
  obs <- data.frame(b = 1)
  output_dir <- tempdir()
  reference_data_dir <- tempdir()
  verbose <- FALSE

  # Mock summary
  stub(evaluate_species, "summary", function(...) data.frame(RMSE = 5))

  write_called <- FALSE
  write_path <- NULL

  stub(evaluate_species, "safe_write_csv", function(x, path) {
    write_called <<- TRUE
    write_path <<- path
  })

  # Create reference file
  ref_file <- file.path(reference_data_dir, "Criteres_stats_Barley.csv")
  write.csv(data.frame(RMSE = 5), ref_file, row.names = FALSE)

  stub(evaluate_species, "read_csv", function(...) data.frame(RMSE = 5))
  stub(evaluate_species, "compare_rmse", function(...) "ok")

  evaluate_species(
    species, sim, obs,
    output_dir,
    reference_data_dir,
    verbose
  )

  expect_true(write_called)
  expect_true(grepl("Criteres_stats_Barley.csv", write_path))
})


test_that("evaluate_species does not write when output_dir is NULL", {

  species <- "Rice"
  sim <- data.frame(a = 1)
  obs <- data.frame(b = 2)
  output_dir <- NULL
  reference_data_dir <- tempdir()
  verbose <- FALSE

  stub(evaluate_species, "summary", function(...) data.frame(RMSE = 5))

  stub(evaluate_species, "safe_write_csv", function(...) {
    stop("safe_write_csv should not be called")
  })

  # Create reference file
  ref_file <- file.path(reference_data_dir, "Criteres_stats_Rice.csv")
  write.csv(data.frame(RMSE = 5), ref_file, row.names = FALSE)

  stub(evaluate_species, "read_csv", function(...) data.frame(RMSE = 5))
  stub(evaluate_species, "compare_rmse", function(...) "ok")

  result <- evaluate_species(
    species, sim, obs,
    output_dir,
    reference_data_dir,
    verbose
  )

  expect_equal(result, "ok")
})
