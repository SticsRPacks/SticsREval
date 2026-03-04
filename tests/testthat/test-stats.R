# ===========================================================================
# Helpers
# ===========================================================================

# Create a temporary directory with a minimal CSV file
make_ref_dir <- function(species, filename, content = c("a,b", "1,2")) {
  base <- file.path(tempdir(), basename(tempfile()))
  dir  <- file.path(base, species)
  dir.create(dir, recursive = TRUE)
  writeLines(content, file.path(dir, filename))
  base
}

# ===========================================================================
# Tests: read_ref_stats
# ===========================================================================
test_that("read_ref_stats returns a data frame when the file exists", {
  ref_dir <- make_ref_dir("wheat", "Criteres_stats.csv")
  stub(read_ref_stats, "is_debug", function() FALSE)

  result <- read_ref_stats("wheat", ref_dir)

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_equal(nrow(result), 1)
})

test_that("read_ref_stats returns NULL when file is missing", {
  ref_dir <- tempfile()
  dir.create(file.path(ref_dir, "wheat"), recursive = TRUE)
  stub(read_ref_stats, "logger::log_warn", function(...) NULL)

  result <- read_ref_stats("wheat", ref_dir)
  expect_null(result)
})

test_that("read_ref_stats logs a warning when file is missing", {
  ref_dir <- tempfile()
  dir.create(file.path(ref_dir, "wheat"), recursive = TRUE)
  mock_warn <- mock(NULL)
  stub(read_ref_stats, "logger::log_warn", mock_warn)

  read_ref_stats("wheat", ref_dir)
  expect_called(mock_warn, 1)
})

test_that("read_ref_stats returns NULL when species directory does not exist", {
  ref_dir <- tempfile()
  dir.create(ref_dir)

  stub(read_ref_stats, "logger::log_warn", function(...) NULL)

  result <- read_ref_stats("unknown_species", ref_dir)
  expect_null(result)
})

test_that("read_ref_stats reads the correct file for the given species", {
  ref_dir <- make_ref_dir("maize", "Criteres_stats.csv", c("x,y,z", "1,2,3"))
  stub(read_ref_stats, "is_debug", function() FALSE)

  result <- read_ref_stats("maize", ref_dir)
  expect_equal(names(result), c("x", "y", "z"))
})

# ===========================================================================
# Tests: read_ref_rmse_per_usm
# ===========================================================================
test_that("read_ref_rmse_per_usm returns a data frame when the file exists", {
  ref_dir <- make_ref_dir("wheat", "RMSE_per_usm.csv")
  stub(read_ref_rmse_per_usm, "is_debug", function() FALSE)

  result <- read_ref_rmse_per_usm("wheat", ref_dir)

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
})

test_that("read_ref_rmse_per_usm returns NULL when file is missing", {
  ref_dir <- tempfile()
  dir.create(file.path(ref_dir, "wheat"), recursive = TRUE)

  stub(read_ref_rmse_per_usm, "logger::log_warn", function(...) NULL)

  result <- read_ref_rmse_per_usm("wheat", ref_dir)
  expect_null(result)
})

test_that(
  "read_ref_rmse_per_usm returns NULL when species directory does not exist",
  {
    ref_dir <- tempfile()
    dir.create(ref_dir)

    stub(read_ref_rmse_per_usm, "logger::log_warn", function(...) NULL)

    result <- read_ref_rmse_per_usm("unknown_species", ref_dir)
    expect_null(result)
  }
)

test_that(
  "read_ref_rmse_per_usm reads the correct file for the given species",
  {
    ref_dir <- make_ref_dir(
      "soy",
      "RMSE_per_usm.csv",
      c("usm,rmse", "usm1,0.5")
    )
    stub(read_ref_rmse_per_usm, "is_debug", function() FALSE)

    result <- read_ref_rmse_per_usm("soy", ref_dir)
    expect_equal(names(result), c("usm", "rmse"))
    expect_equal(nrow(result), 1)
  }
)

# ===========================================================================
# Tests: gen_species_stats
# ===========================================================================

# Helpers pour gen_species_stats
fake_stats        <- data.frame(var = "LAI", stat = 0.9)
fake_rmse_per_usm <- data.frame(usm = "usm1", rRMSE = 0.1)

make_loop_result <- function(species) {
  list(list(
    species     = species,
    stats       = fake_stats,
    rmse_per_usm = fake_rmse_per_usm
  ))
}

test_that(
  "gen_species_stats calls save_stats and save_rmse_per_usm for each species",
  {
    mock_loop     <- mock(make_loop_result("wheat"))
    mock_save     <- mock(NULL)
    mock_save_rmse <- mock(NULL)

    stub(gen_species_stats, "parallelizable_loop", mock_loop)
    stub(gen_species_stats, "save_stats",          mock_save)
    stub(gen_species_stats, "save_rmse_per_usm",   mock_save_rmse)

    gen_species_stats(
      eval_workspace = list(),
      species        = "wheat",
      parallel       = FALSE,
      cores          = 1
    )

    expect_called(mock_save,      1)
    expect_called(mock_save_rmse, 1)
  }
)

test_that("gen_species_stats calls save_stats once per species", {
  species_list <- c("wheat", "maize", "soy")
  loop_results <- lapply(species_list, function(s) {
    list(
      species      = s,
      stats        = fake_stats,
      rmse_per_usm = fake_rmse_per_usm
    )
  })

  mock_loop      <- mock(loop_results)
  mock_save      <- mock(NULL, cycle = TRUE)
  mock_save_rmse <- mock(NULL, cycle = TRUE)

  stub(gen_species_stats, "parallelizable_loop", mock_loop)
  stub(gen_species_stats, "save_stats",          mock_save)
  stub(gen_species_stats, "save_rmse_per_usm",   mock_save_rmse)

  gen_species_stats(
    eval_workspace = list(),
    species        = species_list,
    parallel       = FALSE,
    cores          = 1
  )

  expect_called(mock_save,      3)
  expect_called(mock_save_rmse, 3)
})

test_that("gen_species_stats passes correct species to save_stats", {
  mock_loop      <- mock(make_loop_result("maize"))
  mock_save      <- mock(NULL)
  mock_save_rmse <- mock(NULL)

  stub(gen_species_stats, "parallelizable_loop", mock_loop)
  stub(gen_species_stats, "save_stats",          mock_save)
  stub(gen_species_stats, "save_rmse_per_usm",   mock_save_rmse)

  gen_species_stats(
    eval_workspace = list(),
    species        = "maize",
    parallel       = FALSE,
    cores          = 1
  )

  call_args <- mock_args(mock_save)[[1]]
  expect_equal(call_args[[2]], "maize")   # 2e argument = species
})

test_that("gen_species_stats passes stats data frame to save_stats", {
  mock_loop      <- mock(make_loop_result("wheat"))
  mock_save      <- mock(NULL)
  mock_save_rmse <- mock(NULL)

  stub(gen_species_stats, "parallelizable_loop", mock_loop)
  stub(gen_species_stats, "save_stats",          mock_save)
  stub(gen_species_stats, "save_rmse_per_usm",   mock_save_rmse)

  gen_species_stats(
    eval_workspace = list(),
    species        = "wheat",
    parallel       = FALSE,
    cores          = 1
  )

  call_args <- mock_args(mock_save)[[1]]
  expect_equal(call_args[[3]], fake_stats)   # 3e argument = stats
})