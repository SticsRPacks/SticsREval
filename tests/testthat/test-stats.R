# ===========================================================================
# Tests: gen_species_stats
# ===========================================================================

# Helpers for gen_species_stats
fake_stats <- data.frame(var = "LAI", stat = 0.9, stringsAsFactors = FALSE)
fake_rmse_per_usm <- data.frame(
  usm = "usm1", rRMSE = 0.1, stringsAsFactors = FALSE
)

make_loop_result <- function(species) {
  list(list(
    species = species,
    stats = fake_stats,
    rmse_per_usm = fake_rmse_per_usm
  ))
}

test_that(
  "gen_species_stats calls save_stats and save_rmse_per_usm for each species",
  {
    mock_loop <- mock(make_loop_result("wheat"))
    mock_save <- mock(NULL)
    mock_save_rmse <- mock(NULL)

    stub(gen_species_stats, "parallelizable_loop", mock_loop)
    stub(gen_species_stats, "save_stats", mock_save)
    stub(gen_species_stats, "save_rmse_per_usm", mock_save_rmse)

    gen_species_stats(
      eval_workspace = list(),
      species = "wheat",
      parallel = FALSE,
      cores = 1
    )

    expect_called(mock_save, 1)
    expect_called(mock_save_rmse, 1)
  }
)

test_that("gen_species_stats calls save_stats once per species", {
  species_list <- c("wheat", "maize", "soy")
  loop_results <- lapply(species_list, function(s) {
    list(
      species = s,
      stats = fake_stats,
      rmse_per_usm = fake_rmse_per_usm
    )
  })

  mock_loop <- mock(loop_results)
  mock_save <- mock(NULL, cycle = TRUE)
  mock_save_rmse <- mock(NULL, cycle = TRUE)

  stub(gen_species_stats, "parallelizable_loop", mock_loop)
  stub(gen_species_stats, "save_stats", mock_save)
  stub(gen_species_stats, "save_rmse_per_usm", mock_save_rmse)

  gen_species_stats(
    eval_workspace = list(),
    species = species_list,
    parallel = FALSE,
    cores = 1
  )

  expect_called(mock_save, 3)
  expect_called(mock_save_rmse, 3)
})

test_that("gen_species_stats passes correct arguments to save_stats", {
  mock_loop <- mock(make_loop_result("maize"))
  mock_save <- mock(NULL)
  mock_save_rmse <- mock(NULL)
  mock_version <- mock("1.0.0")

  stub(gen_species_stats, "parallelizable_loop", mock_loop)
  stub(gen_species_stats, "get_stics_version", mock_version)
  stub(gen_species_stats, "save_stats", mock_save)
  stub(gen_species_stats, "save_rmse_per_usm", mock_save_rmse)

  gen_species_stats(
    eval_workspace = list(),
    species = "maize",
    parallel = FALSE,
    cores = 1
  )

  call_args <- mock_args(mock_save)[[1]]
  expect_identical(call_args[[2]], "1.0.0")
  expect_identical(call_args[[3]], "maize")
  expect_identical(call_args[[4]], fake_stats)
})
