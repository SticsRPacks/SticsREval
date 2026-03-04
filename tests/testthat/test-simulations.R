library(testthat)
library(mockery)

# ---- Source the functions ----
source(here::here("R/utils.R"))
source(here::here("R/simulations.R"))  # run_simulations, read_ref_sim

# ===========================================================================
# Tests: run_simulations
# ===========================================================================

test_that(
  "run_simulations calls stics_wrapper_options with correct arguments",
  {
    mock_options <- mock("fake_options")
    mock_wrapper <- mock(list(sim_list = list()))

    stub(run_simulations, "SticsOnR::stics_wrapper_options", mock_options)
    stub(run_simulations, "SticsOnR::stics_wrapper",         mock_wrapper)

    run_simulations(
      stics_exe  = "/path/to/stics",
      workspace  = "/path/to/workspace",
      usm_names  = c("usm1", "usm2"),
      successive = list(c("usm1", "usm2")),
      verbose    = FALSE
    )

    expect_called(mock_options, 1)
    args <- mock_args(mock_options)[[1]]
    expect_equal(args$stics_exe,  "/path/to/stics")
    expect_equal(args$workspace,  "/path/to/workspace")
    expect_equal(args$successive, list(c("usm1", "usm2")))
    expect_equal(args$verbose,    FALSE)
    expect_equal(args$parallel,   FALSE)
    expect_equal(args$cores,      NA)
  }
)

test_that("run_simulations passes usm_names as situation to stics_wrapper", {
  mock_options <- mock("fake_options")
  mock_wrapper <- mock(list(sim_list = list()))

  stub(run_simulations, "SticsOnR::stics_wrapper_options", mock_options)
  stub(run_simulations, "SticsOnR::stics_wrapper",         mock_wrapper)

  run_simulations(
    stics_exe  = "/path/to/stics",
    workspace  = "/path/to/workspace",
    usm_names  = c("usm1", "usm2"),
    successive = NULL,
    verbose    = FALSE
  )

  expect_called(mock_wrapper, 1)
  args <- mock_args(mock_wrapper)[[1]]
  expect_equal(args$situation, c("usm1", "usm2"))
})

test_that("run_simulations returns sim_list from stics_wrapper result", {
  fake_sim_list <- list(usm1 = data.frame(x = 1), usm2 = data.frame(x = 2))

  stub(run_simulations, "SticsOnR::stics_wrapper_options", mock(NULL))
  stub(
    run_simulations,
    "SticsOnR::stics_wrapper",
    mock(list(sim_list = fake_sim_list))
  )

  result <- run_simulations(
    stics_exe  = "/path/to/stics",
    workspace  = "/path/to/workspace",
    usm_names  = c("usm1", "usm2"),
    successive = NULL,
    verbose    = FALSE
  )

  expect_equal(result, fake_sim_list)
})

test_that(
  "run_simulations passes parallel and cores to stics_wrapper_options",
  {
    mock_options <- mock("fake_options")
    stub(run_simulations, "SticsOnR::stics_wrapper_options", mock_options)
    stub(
      run_simulations,
      "SticsOnR::stics_wrapper",
      mock(list(sim_list = list()))
    )

    run_simulations(
      stics_exe  = "/path/to/stics",
      workspace  = "/path/to/workspace",
      usm_names  = c("usm1"),
      successive = NULL,
      verbose    = FALSE,
      parallel   = TRUE,
      cores      = 4
    )

    args <- mock_args(mock_options)[[1]]
    expect_equal(args$parallel, TRUE)
    expect_equal(args$cores,    4)
  }
)

test_that(
  "run_simulations passes verbose to time_display in stics_wrapper_options",
  {
    mock_options <- mock("fake_options")
    stub(run_simulations, "SticsOnR::stics_wrapper_options", mock_options)
    stub(
      run_simulations,
      "SticsOnR::stics_wrapper",
      mock(list(sim_list = list()))
    )

    run_simulations(
      stics_exe  = "/path/to/stics",
      workspace  = "/path/to/workspace",
      usm_names  = c("usm1"),
      successive = NULL,
      verbose    = TRUE
    )

    args <- mock_args(mock_options)[[1]]
    expect_equal(args$time_display, TRUE)
  }
)

# ===========================================================================
# Helpers: read_ref_sim
# ===========================================================================

make_parquet_dir <- function(species) {
  base <- file.path(tempdir(), basename(tempfile()))
  dir  <- file.path(base, species)
  dir.create(dir, recursive = TRUE)

  # Create a minimal parquet file with arrow
  df <- data.frame(x = 1:3, y = c("a", "b", "c"))
  arrow::write_parquet(df, file.path(dir, "Simulations.parquet"))

  base
}

# ===========================================================================
# Tests: read_ref_sim
# ===========================================================================

test_that("read_ref_sim returns NULL when reference_data_dir is NULL", {
  result <- read_ref_sim(NULL, "wheat")
  expect_null(result)
})

test_that("read_ref_sim returns NULL when species directory does not exist", {
  ref_dir <- tempfile()
  dir.create(ref_dir)

  result <- read_ref_sim(ref_dir, "unknown_species")
  expect_null(result)
})

test_that("read_ref_sim returns NULL when parquet file does not exist", {
  ref_dir <- tempfile()
  dir.create(
    file.path(ref_dir, "wheat"),
    recursive = TRUE
  )

  result <- read_ref_sim(ref_dir, "wheat")
  expect_null(result)
})

test_that(
  "read_ref_sim returns an arrow Dataset when file exists and collect = FALSE",
  {
    ref_dir <- make_parquet_dir("wheat")

    result <- read_ref_sim(ref_dir, "wheat", collect = FALSE)

    expect_true(inherits(result, "Dataset") || inherits(result, "ArrowObject"))
  }
)

test_that("read_ref_sim returns a data frame when collect = TRUE", {
  ref_dir <- make_parquet_dir("wheat")

  result <- read_ref_sim(ref_dir, "wheat", collect = TRUE)

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_equal(nrow(result), 3)
})

test_that("read_ref_sim collect = FALSE is the default", {
  ref_dir <- make_parquet_dir("maize")

  result <- read_ref_sim(ref_dir, "maize")

  # By default, we expect a lazy object (not a data frame)
  expect_false(is.data.frame(result))
})

test_that("read_ref_sim reads data for the correct species", {
  ref_dir <- make_parquet_dir("soy")

  result <- read_ref_sim(ref_dir, "soy", collect = TRUE)

  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("x", "y"))
})