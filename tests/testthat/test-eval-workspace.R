# ===========================================================================
# Helpers
# ===========================================================================

make_parquet <- function(dir, df = data.frame(x = 1:3)) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, "part-0.parquet")
  arrow::write_parquet(df, path)
  dir
}

make_species_parquet <- function(species, filename) {
  base <- file.path(tempdir(), basename(tempfile()))
  dir.create(file.path(base, species), recursive = TRUE)
  path <- file.path(base, species, filename)
  arrow::write_parquet(data.frame(x = 1:3), path)
  base
}

# ===========================================================================
# Tests: path helpers
# ===========================================================================

test_that("sim_ds_path returns data_dir/sim", { # nolint: nonportable_path_linter
  expect_identical(sim_ds_path("/base"), file.path("base", "sim"))
})

test_that("obs_ds_path returns data_dir/obs", { # nolint: nonportable_path_linter
  expect_identical(obs_ds_path("/base"), file.path("base", "obs"))
})

test_that("stats_ds_path returns correct path", {
  expect_identical(
    stats_ds_path("/base", "wheat"),
    file.path("base", "wheat", "Criteres_stats.parquet")
  )
})

test_that("rmse_per_usm_ds_path returns correct path", {
  expect_identical(
    rmse_per_usm_ds_path("/base", "wheat"),
    file.path("base", "wheat", "RMSE_per_USM.parquet")
  )
})

test_that("deteriorated_ds_path returns correct path", {
  expect_identical(
    deteriorated_ds_path("/base", "wheat"),
    file.path("base", "wheat", "Deteriorated_RMSE_per_usm.parquet")
  )
})

test_that("comparison_ds_path returns correct path", {
  expect_identical(
    comparison_ds_path("/base", "wheat"),
    file.path("base", "wheat", "Comparison.parquet")
  )
})

# ===========================================================================
# Tests: open_parquet_or_null
# ===========================================================================

test_that("open_parquet_or_null returns NULL when file missing", {
  mock_warn <- mock(NULL)
  stub(open_parquet_or_null, "logger::log_warn", mock_warn)

  result <- open_parquet_or_null(
    "/nonexistent.parquet",
    collect = FALSE,
    warn_msg = "missing"
  )

  expect_null(result)
  expect_called(mock_warn, 1)
})

test_that("open_parquet_or_null logs the warn_msg when file missing", {
  mock_warn <- mock(NULL)
  stub(open_parquet_or_null, "logger::log_warn", mock_warn)

  open_parquet_or_null(
    "/nonexistent.parquet",
    collect = FALSE,
    warn_msg = "custom warning message"
  )

  expect_identical(mock_args(mock_warn)[[1]][[1]], "custom warning message")
})

test_that("open_parquet_or_null returns lazy dataset when collect = FALSE", {
  base <- make_species_parquet("wheat", "test.parquet")
  path <- file.path(base, "wheat", "test.parquet")

  result <- open_parquet_or_null(path, collect = FALSE, warn_msg = "")
  expect_false(is.data.frame(result))
})

test_that("open_parquet_or_null returns data frame when collect = TRUE", {
  base <- make_species_parquet("wheat", "test.parquet")
  path <- file.path(base, "wheat", "test.parquet")

  result <- open_parquet_or_null(path, collect = TRUE, warn_msg = "")
  expect_s3_class(result, "data.frame")
})

# ===========================================================================
# Tests: init_eval_workspace
# ===========================================================================

test_that("init_eval_workspace creates workspace directory", {
  base <- file.path(tempdir(), basename(tempfile()))
  stub(init_eval_workspace, "extract_species_from_usms", mock(
    data.frame(usm = "usm1", species = "wheat", stringsAsFactors = FALSE)
  ))
  stub(init_eval_workspace, "get_rotation_list", mock(list()))
  stub(init_eval_workspace, "load_workspace_sim", mock(NULL))
  stub(init_eval_workspace, "load_workspace_obs", mock(NULL))
  stub(init_eval_workspace, "list.dirs", mock("usm1"))

  init_eval_workspace(
    data_workspace = "/data",
    eval_workspace = base,
    metadata_file = "/meta.csv",
    stics_exe = "/stics",
    must_run_simulations = FALSE,
    parallel = FALSE,
    cores = NA
  )

  expect_true(dir.exists(base))
})

test_that(
  "init_eval_workspace errors when workspace not empty and force = FALSE",
  {
    base <- file.path(tempdir(), basename(tempfile()))
    dir.create(base)
    writeLines("old", file.path(base, "old.txt"))

    expect_error(
      init_eval_workspace(
        data_workspace = "/data",
        eval_workspace = base,
        metadata_file = "/meta.csv",
        stics_exe = "/stics",
        must_run_simulations = FALSE,
        parallel = FALSE,
        cores = NA,
        force = FALSE
      ),
      regexp = "force = TRUE"
    )
  }
)

test_that(
  "init_eval_workspace clears existing workspace when force = TRUE",
  {
    base <- file.path(tempdir(), basename(tempfile()))
    dir.create(base)
    old_file <- file.path(base, "old.txt")
    writeLines("old", old_file)

    stub(init_eval_workspace, "extract_species_from_usms", mock(
      data.frame(usm = "usm1", species = "wheat", stringsAsFactors = FALSE)
    ))
    stub(init_eval_workspace, "get_rotation_list", mock(list()))
    stub(init_eval_workspace, "load_workspace_sim", mock(NULL))
    stub(init_eval_workspace, "load_workspace_obs", mock(NULL))
    stub(init_eval_workspace, "list.dirs", mock("usm1"))

    init_eval_workspace(
      data_workspace = "/data",
      eval_workspace = base,
      metadata_file = "/meta.csv",
      stics_exe = "/stics",
      must_run_simulations = FALSE,
      parallel = FALSE,
      cores = NA,
      force = TRUE
    )

    expect_false(file.exists(old_file))
  }
)

test_that(
  "init_eval_workspace proceeds when workspace is empty and force = FALSE",
  {
    base <- file.path(tempdir(), basename(tempfile()))
    dir.create(base)

    stub(init_eval_workspace, "extract_species_from_usms", mock(
      data.frame(usm = "usm1", species = "wheat", stringsAsFactors = FALSE)
    ))
    stub(init_eval_workspace, "get_rotation_list", mock(list()))
    stub(init_eval_workspace, "load_workspace_sim", mock(NULL))
    stub(init_eval_workspace, "load_workspace_obs", mock(NULL))
    stub(init_eval_workspace, "list.dirs", mock("usm1"))

    expect_no_error(
      init_eval_workspace(
        data_workspace = "/data",
        eval_workspace = base,
        metadata_file = "/meta.csv",
        stics_exe = "/stics",
        must_run_simulations = FALSE,
        parallel = FALSE,
        cores = NA,
        force = FALSE
      )
    )
  }
)

test_that("init_eval_workspace throws error when dir cannot be created", {
  stub(init_eval_workspace, "list.dirs", mock(character(0)))

  expect_error(
    suppressWarnings(
      init_eval_workspace(
        data_workspace = "/data",
        eval_workspace = "/proc/invalid_path", # nolint: nonportable_path_linter
        metadata_file = "/meta.csv",
        stics_exe = "/stics",
        must_run_simulations = FALSE,
        parallel = FALSE,
        cores = NA
      )
    ),
    regexp = "Can't create evaluation workspace"
  )
})

test_that("init_eval_workspace calls load_workspace_sim", {
  base <- file.path(tempdir(), basename(tempfile()))
  mock_load_sim <- mock(NULL)

  stub(init_eval_workspace, "extract_species_from_usms", mock(
    data.frame(usm = "usm1", species = "wheat", stringsAsFactors = FALSE)
  ))
  stub(init_eval_workspace, "get_rotation_list", mock(list()))
  stub(init_eval_workspace, "load_workspace_sim", mock_load_sim)
  stub(init_eval_workspace, "load_workspace_obs", mock(NULL))
  stub(init_eval_workspace, "list.dirs", mock("usm1"))

  init_eval_workspace(
    data_workspace = "/data",
    eval_workspace = base,
    metadata_file = "/meta.csv",
    stics_exe = "/stics",
    must_run_simulations = TRUE,
    parallel = FALSE,
    cores = NA
  )

  expect_called(mock_load_sim, 1)
})

test_that("init_eval_workspace calls load_workspace_obs", {
  base <- file.path(tempdir(), basename(tempfile()))
  mock_load_obs <- mock(NULL)

  stub(init_eval_workspace, "extract_species_from_usms", mock(
    data.frame(usm = "usm1", species = "wheat", stringsAsFactors = FALSE)
  ))
  stub(init_eval_workspace, "get_rotation_list", mock(list()))
  stub(init_eval_workspace, "load_workspace_sim", mock(NULL))
  stub(init_eval_workspace, "load_workspace_obs", mock_load_obs)
  stub(init_eval_workspace, "list.dirs", mock("usm1"))

  init_eval_workspace(
    data_workspace = "/data",
    eval_workspace = base,
    metadata_file = "/meta.csv",
    stics_exe = "/stics",
    must_run_simulations = FALSE,
    parallel = FALSE,
    cores = NA
  )

  expect_called(mock_load_obs, 1)
})

# ===========================================================================
# Tests: get_sim_ds / get_obs_ds # nolint
# ===========================================================================

test_that("get_sim_ds throws error when sim directory does not exist", {
  base <- file.path(tempdir(), basename(tempfile()))
  dir.create(base)

  expect_error(get_sim_ds(base), regexp = "does not exist")
})

test_that("get_obs_ds throws error when obs directory does not exist", {
  base <- file.path(tempdir(), basename(tempfile()))
  dir.create(base)

  expect_error(get_obs_ds(base), regexp = "does not exist")
})

test_that("get_sim_ds returns an arrow Dataset when path exists", {
  base <- file.path(tempdir(), basename(tempfile()))
  make_parquet(file.path(base, "sim"))

  result <- get_sim_ds(base)
  expect_true(
    inherits(result, "Dataset") || inherits(result, "ArrowObject")
  )
})

test_that("get_obs_ds returns an arrow Dataset when path exists", {
  base <- file.path(tempdir(), basename(tempfile()))
  make_parquet(file.path(base, "obs"))

  result <- get_obs_ds(base)
  expect_true(
    inherits(result, "Dataset") || inherits(result, "ArrowObject")
  )
})

# ===========================================================================
# Tests: save_sim / save_obs # nolint
# ===========================================================================

test_that("save_sim writes a partitioned parquet dataset", {
  base <- file.path(tempdir(), basename(tempfile()))
  dir.create(base)
  sim <- data.frame(situation = "usm1", LAI = 1.2, stringsAsFactors = FALSE)
  usms_species <- data.frame(
    usm = "usm1",
    species = "wheat",
    stringsAsFactors = FALSE
  )

  mock_bind <- mock(sim)
  mock_join <- mock(sim)
  mock_write <- mock(NULL)

  stub(save_sim, "CroPlotR::bind_rows", mock_bind)
  stub(save_sim, "dplyr::inner_join", mock_join)
  stub(save_sim, "arrow::write_dataset", mock_write)

  save_sim(base, list(usm1 = sim), usms_species)

  expect_called(mock_write, 1)
  args <- mock_args(mock_write)[[1]]
  expect_identical(args$path, sim_ds_path(base))
  expect_identical(args$partitioning, "species")
})

test_that("save_obs writes a partitioned parquet dataset", {
  base <- file.path(tempdir(), basename(tempfile()))
  dir.create(base)
  obs <- data.frame(situation = "usm1", LAI = 1.0, stringsAsFactors = FALSE)
  usms_species <- data.frame(
    usm = "usm1",
    species = "wheat",
    stringsAsFactors = FALSE
  )

  mock_bind <- mock(obs)
  mock_join <- mock(obs)
  mock_write <- mock(NULL)

  stub(save_obs, "CroPlotR::bind_rows", mock_bind)
  stub(save_obs, "dplyr::inner_join", mock_join)
  stub(save_obs, "arrow::write_dataset", mock_write)

  save_obs(base, list(usm1 = obs), usms_species)

  expect_called(mock_write, 1)
  args <- mock_args(mock_write)[[1]]
  expect_identical(args$path, obs_ds_path(base))
  expect_identical(args$partitioning, "species")
})

# ===========================================================================
# Tests: get_species / get_species_usm / get_by_species # nolint
# ===========================================================================

test_that("get_species returns sorted species names", {
  base <- file.path(tempdir(), basename(tempfile()))
  df <- data.frame(
    situation = c("usm1", "usm2", "usm3"),
    species = c("wheat", "maize", "soy"),
    stringsAsFactors = FALSE
  )
  make_parquet(file.path(base, "obs"), df)

  result <- get_species(base)
  expect_identical(result, sort(c("wheat", "maize", "soy")))
})

test_that("get_species_usm returns USMs for given species", {
  base <- file.path(tempdir(), basename(tempfile()))
  df <- data.frame(
    situation = c("usm1", "usm2", "usm3"),
    species = c("wheat", "wheat", "maize"),
    stringsAsFactors = FALSE
  )
  make_parquet(file.path(base, "obs"), df)

  result <- get_species_usm(base, "wheat")
  expect_setequal(result, c("usm1", "usm2"))
})

test_that("get_by_species returns lazy dataset when collect = FALSE", {
  base <- file.path(tempdir(), basename(tempfile()))
  df <- data.frame(
    situation = "usm1",
    species = "wheat",
    LAI = 1.2,
    stringsAsFactors = FALSE
  )
  make_parquet(file.path(base, "sim"), df)

  result <- get_by_species(base, "wheat", "sim", collect = FALSE)
  expect_false(is.data.frame(result))
})

test_that("get_by_species returns data frame when collect = TRUE", {
  base <- file.path(tempdir(), basename(tempfile()))
  df <- data.frame(
    situation = c("usm1", "usm2"),
    species = c("wheat", "wheat"),
    LAI = c(1.2, 1.5),
    stringsAsFactors = FALSE
  )
  make_parquet(file.path(base, "sim"), df)

  result <- get_by_species(base, "wheat", "sim", collect = TRUE)
  expect_s3_class(result, "data.frame")
  expect_identical(nrow(result), 2)
})

test_that("get_by_species filters by species", {
  base <- file.path(tempdir(), basename(tempfile()))
  df <- data.frame(
    situation = c("usm1", "usm2"),
    species = c("wheat", "maize"),
    LAI = c(1.2, 0.8),
    stringsAsFactors = FALSE
  )
  make_parquet(file.path(base, "obs"), df)

  result <- get_by_species(base, "wheat", "obs", collect = TRUE)
  expect_identical(nrow(result), 1)
  expect_identical(result$situation, "usm1")
})

# ===========================================================================
# Tests: save_stats / get_stats # nolint
# ===========================================================================

test_that("save_stats writes parquet to correct path", {
  base <- file.path(tempdir(), basename(tempfile()))
  dir.create(file.path(base, "wheat"), recursive = TRUE)
  df <- data.frame(var = "LAI", stat = 0.9, stringsAsFactors = FALSE)

  save_stats(base, "wheat", df)

  expect_true(file.exists(stats_ds_path(base, "wheat")))
})

test_that("get_stats delegates to open_parquet_or_null", {
  mock_open <- mock(NULL)
  stub(get_stats, "open_parquet_or_null", mock_open)

  get_stats("/base", "wheat", collect = TRUE)

  expect_called(mock_open, 1)
  args <- mock_args(mock_open)[[1]]
  expect_identical(args$path, stats_ds_path("/base", "wheat"))
  expect_true(args$collect)
})

test_that("get_stats returns NULL when file missing", {
  stub(get_stats, "open_parquet_or_null", mock(NULL))
  expect_null(get_stats("/base", "wheat"))
})

test_that("get_stats returns lazy dataset when collect = FALSE", {
  base <- make_species_parquet("wheat", "Criteres_stats.parquet")
  result <- get_stats(base, "wheat", collect = FALSE)
  expect_false(is.data.frame(result))
})

test_that("get_stats returns data frame when collect = TRUE", {
  base <- make_species_parquet("wheat", "Criteres_stats.parquet")
  result <- get_stats(base, "wheat", collect = TRUE)
  expect_s3_class(result, "data.frame")
})

# ===========================================================================
# Tests: save_rmse_per_usm / get_rmse_per_usm # nolint
# ===========================================================================

test_that("save_rmse_per_usm writes parquet to correct path", {
  base <- file.path(tempdir(), basename(tempfile()))
  dir.create(file.path(base, "wheat"), recursive = TRUE)
  df <- data.frame(usm = "usm1", rmse = 0.1, stringsAsFactors = FALSE)

  save_rmse_per_usm(base, "wheat", df)

  expect_true(file.exists(rmse_per_usm_ds_path(base, "wheat")))
})

test_that("get_rmse_per_usm delegates to open_parquet_or_null", {
  mock_open <- mock(NULL)
  stub(get_rmse_per_usm, "open_parquet_or_null", mock_open)

  get_rmse_per_usm("/base", "wheat", collect = FALSE)

  expect_called(mock_open, 1)
  args <- mock_args(mock_open)[[1]]
  expect_identical(args$path, rmse_per_usm_ds_path("/base", "wheat"))
})

test_that("get_rmse_per_usm returns NULL when file missing", {
  stub(get_rmse_per_usm, "open_parquet_or_null", mock(NULL))
  expect_null(get_rmse_per_usm("/base", "wheat"))
})

test_that("get_rmse_per_usm returns data frame when collect = TRUE", {
  base <- make_species_parquet("wheat", "RMSE_per_USM.parquet")
  result <- get_rmse_per_usm(base, "wheat", collect = TRUE)
  expect_s3_class(result, "data.frame")
})

# ===========================================================================
# Tests: save_deteriorated_usm / get_deteriorated_usm # nolint
# ===========================================================================

test_that("save_deteriorated_usm writes parquet to correct path", {
  base <- file.path(tempdir(), basename(tempfile()))
  dir.create(file.path(base, "wheat"), recursive = TRUE)
  df <- data.frame(usm = "usm1", rRMSE = 0.5, stringsAsFactors = FALSE)

  save_deteriorated_usm(base, "wheat", df)

  expect_true(
    file.exists(deteriorated_ds_path(base, "wheat"))
  )
})

test_that("get_deteriorated_usm delegates to open_parquet_or_null", {
  mock_open <- mock(NULL)
  stub(get_deteriorated_usm, "open_parquet_or_null", mock_open)

  get_deteriorated_usm("/base", "wheat", collect = FALSE)

  expect_called(mock_open, 1)
  args <- mock_args(mock_open)[[1]]
  expect_identical(args$path, deteriorated_ds_path("/base", "wheat"))
})

test_that("get_deteriorated_usm returns NULL when file missing", {
  stub(get_deteriorated_usm, "open_parquet_or_null", mock(NULL))
  expect_null(get_deteriorated_usm("/base", "wheat"))
})

test_that("get_deteriorated_usm returns data frame when collect = TRUE", {
  base <- make_species_parquet(
    "wheat", "Deteriorated_RMSE_per_usm.parquet"
  )
  result <- get_deteriorated_usm(base, "wheat", collect = TRUE)
  expect_s3_class(result, "data.frame")
})

# ===========================================================================
# Tests: save_species_comparison / get_species_comparison # nolint
# ===========================================================================

test_that("save_species_comparison writes parquet to correct path", {
  base <- file.path(tempdir(), basename(tempfile()))
  dir.create(file.path(base, "wheat"), recursive = TRUE)
  df <- data.frame(variable = "LAI", ratio = 1.1, stringsAsFactors = FALSE)

  save_species_comparison(base, "wheat", df)

  expect_true(
    file.exists(comparison_ds_path(base, "wheat"))
  )
})

test_that("get_species_comparison delegates to open_parquet_or_null", {
  mock_open <- mock(NULL)
  stub(get_species_comparison, "open_parquet_or_null", mock_open)

  get_species_comparison("/base", "wheat", collect = TRUE)

  expect_called(mock_open, 1)
  args <- mock_args(mock_open)[[1]]
  expect_identical(args$path, comparison_ds_path("/base", "wheat"))
  expect_true(args$collect)
})

test_that("get_species_comparison returns NULL when file missing", {
  stub(get_species_comparison, "open_parquet_or_null", mock(NULL))
  expect_null(get_species_comparison("/base", "wheat"))
})

test_that(
  "get_species_comparison returns data frame when collect = TRUE",
  {
    base <- make_species_parquet("wheat", "Comparison.parquet")
    result <- get_species_comparison(base, "wheat", collect = TRUE)
    expect_s3_class(result, "data.frame")
  }
)
