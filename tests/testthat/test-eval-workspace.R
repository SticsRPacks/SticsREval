# ===========================================================================
# Helpers
# ===========================================================================

make_parquet <- function(dir, df = data.frame(x = 1:3)) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, "part-0.parquet")
  arrow::write_parquet(df, path)
  dir
}

make_species_parquet <- function(species, dirtype, df = data.frame(x = 1:3)) {
  df$species <- species
  base <- file.path(tempdir(), basename(tempfile()))
  arrow::write_dataset(df, file.path(base, dirtype), partitioning = "species")
  base
}

# ===========================================================================
# Tests: path helpers
# ===========================================================================

test_that("sim_ds_path returns data_dir/sim", { # nolint: nonportable_path_linter
  expect_identical(sim_ds_path("base"), file.path("base", "sim"))
})

test_that("obs_ds_path returns data_dir/obs", { # nolint: nonportable_path_linter
  expect_identical(obs_ds_path("base"), file.path("base", "obs"))
})

test_that("stats_ds_path returns correct path", {
  expect_identical(
    stats_ds_path("base"),
    file.path("base", "Criteres_stats")
  )
})

test_that("rmse_per_usm_ds_path returns correct path", {
  expect_identical(
    rmse_per_usm_ds_path("base"),
    file.path("base", "RMSE_per_USM")
  )
})

test_that("deteriorated_ds_path returns correct path", {
  expect_identical(
    deteriorated_ds_path("base"),
    file.path("base", "Deteriorated_RMSE_per_usm")
  )
})

test_that("comparison_ds_path returns correct path", {
  expect_identical(
    comparison_ds_path("base"),
    file.path("base", "comparison")
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
  path <- file.path(base, "test.parquet")

  result <- open_parquet_or_null(path, collect = TRUE, warn_msg = "")
  expect_s3_class(result, "data.frame")
})

test_that("init_eval_workspace creates workspace directory", {
  base <- file.path(tempdir(), basename(tempfile()))
  stub(init_eval_workspace, "extract_species_from_usms", mock(
    data.frame(usm = "usm1", species = "wheat", stringsAsFactors = FALSE)
  ))
  stub(init_eval_workspace, "get_rotation_list", mock(list()))
  stub(init_eval_workspace, "load_workspace_sim", mock(NULL))
  stub(init_eval_workspace, "load_workspace_obs", mock(NULL))
  stub(init_eval_workspace, "remove_init_obs", mock(NULL))
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
    stub(init_eval_workspace, "remove_init_obs", mock(NULL))
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
    stub(init_eval_workspace, "remove_init_obs", mock(NULL))
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
        eval_workspace = "/proc/invalid_path",  # nolint: nonportable_path_linter
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
  stub(init_eval_workspace, "remove_init_obs", mock(NULL))
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
  stub(init_eval_workspace, "remove_init_obs", mock(NULL))
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

test_that("init_eval_workspace calls remove_init_obs", {
  base <- file.path(tempdir(), basename(tempfile()))
  mock_remove <- mock(NULL)

  stub(init_eval_workspace, "extract_species_from_usms", mock(
    data.frame(usm = "usm1", species = "wheat", stringsAsFactors = FALSE)
  ))
  stub(init_eval_workspace, "get_rotation_list", mock(list()))
  stub(init_eval_workspace, "load_workspace_sim", mock(NULL))
  stub(init_eval_workspace, "load_workspace_obs", mock(NULL))
  stub(init_eval_workspace, "remove_init_obs", mock_remove)
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

  expect_called(mock_remove, 1)
  expect_identical(mock_args(mock_remove)[[1]][[1]], base)
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
  expect_identical(nrow(result), 2L)
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
  expect_identical(nrow(result), 1L)
  expect_identical(result$situation, "usm1")
})

test_that("get_by_species filters by usms when provided", {
  base <- file.path(tempdir(), basename(tempfile()))
  df <- data.frame(
    situation = c("usm1", "usm2", "usm3"),
    species = c("wheat", "wheat", "wheat"),
    LAI = c(1.2, 1.5, 1.8),
    stringsAsFactors = FALSE
  )
  make_parquet(file.path(base, "sim"), df)
  result <- get_by_species(
    base, "wheat", "sim", collect = TRUE, usms = c("usm1", "usm3")
  )
  expect_identical(nrow(result), 2L)
  expect_true(all(result$situation %in% c("usm1", "usm3")))
  expect_false("usm2" %in% result$situation)
})

test_that("get_by_species returns empty data frame when usms matches nothing", {
  base <- file.path(tempdir(), basename(tempfile()))
  df <- data.frame(
    situation = c("usm1", "usm2"),
    species = c("wheat", "wheat"),
    LAI = c(1.2, 1.5),
    stringsAsFactors = FALSE
  )
  make_parquet(file.path(base, "sim"), df)
  result <- get_by_species(
    base, "wheat", "sim", collect = TRUE, usms = "usm_inexistant"
  )
  expect_identical(nrow(result), 0L)
})

test_that("get_by_species usms filter does not leak across species", {
  base <- file.path(tempdir(), basename(tempfile()))
  df <- data.frame(
    situation = c("usm1", "usm1"),
    species = c("wheat", "maize"),
    LAI = c(1.2, 0.8),
    stringsAsFactors = FALSE
  )
  make_parquet(file.path(base, "sim"), df)
  result <- get_by_species(base, "wheat", "sim", collect = TRUE, usms = "usm1")
  expect_identical(nrow(result), 1L)
  expect_identical(result$species, "wheat")
})

test_that("get_by_species excludes column listed in var2exclude", {
  base <- file.path(tempdir(), basename(tempfile()))
  df <- data.frame(
    situation = "usm1",
    species = "wheat",
    LAI = 1.2,
    bias = 0.05,
    stringsAsFactors = FALSE
  )
  make_parquet(file.path(base, "sim"), df)
  result <- get_by_species(
    base, "wheat", "sim", collect = TRUE, var2exclude = "bias"
  )
  expect_false("bias" %in% names(result))
  expect_true("LAI" %in% names(result))
})

test_that("get_by_species excludes multiple columns listed in var2exclude", {
  base <- file.path(tempdir(), basename(tempfile()))
  df <- data.frame(
    situation = "usm1",
    species = "wheat",
    LAI = 1.2,
    bias = 0.05,
    n = 10L,
    stringsAsFactors = FALSE
  )
  make_parquet(file.path(base, "sim"), df)
  result <- get_by_species(
    base, "wheat", "sim", collect = TRUE, var2exclude = c("bias", "n")
  )
  expect_false(any(c("bias", "n") %in% names(result)))
  expect_true(all(c("situation", "LAI") %in% names(result)))
})

test_that("get_by_species applies both usms and var2exclude together", {
  base <- file.path(tempdir(), basename(tempfile()))
  df <- data.frame(
    situation = c("usm1", "usm2", "usm3"),
    species = c("wheat", "wheat", "wheat"),
    LAI = c(1.2, 1.5, 1.8),
    bias = c(0.01, 0.02, 0.03),
    stringsAsFactors = FALSE
  )
  make_parquet(file.path(base, "sim"), df)
  result <- get_by_species(
    base, "wheat", "sim", collect = TRUE,
    usms = c("usm1", "usm2"),
    var2exclude = "bias"
  )
  expect_identical(nrow(result), 2L)
  expect_false("usm3" %in% result$situation)
  expect_false("bias" %in% names(result))
})

test_that("get_by_species applies usms and var2exclude with obs type", {
  base <- file.path(tempdir(), basename(tempfile()))
  df <- data.frame(
    situation = c("usm1", "usm2"),
    species = c("wheat", "wheat"),
    LAI = c(1.2, 1.5),
    bias = c(0.01, 0.02),
    stringsAsFactors = FALSE
  )
  make_parquet(file.path(base, "obs"), df)
  result <- get_by_species(
    base, "wheat", "obs", collect = TRUE,
    usms = "usm1",
    var2exclude = "bias"
  )
  expect_identical(nrow(result), 1L)
  expect_false("bias" %in% names(result))
})

# ===========================================================================
# Tests: save_stats / get_stats # nolint
# ===========================================================================

test_that("save_stats writes parquet to correct path", {
  base <- file.path(tempdir(), basename(tempfile()))
  dir.create(base, recursive = TRUE)
  df <- data.frame(var = "LAI", stat = 0.9, stringsAsFactors = FALSE)

  save_stats(base, "wheat", df)

  expect_true(file.exists(stats_ds_path(base)))
})

test_that("get_stats delegates to open_parquet_or_null", {
  mock_open <- mock(NULL)
  stub(get_stats, "open_parquet_or_null", mock_open)

  get_stats("/base", "wheat", collect = TRUE)

  expect_called(mock_open, 1)
  args <- mock_args(mock_open)[[1]]
  expect_identical(args$path, stats_ds_path("/base"))
  expect_true(args$collect)
})

test_that("get_stats returns NULL when file missing", {
  stub(get_stats, "open_parquet_or_null", mock(NULL))
  expect_null(get_stats("/base", "wheat"))
})

test_that("get_stats returns lazy dataset when collect = FALSE", {
  base <- make_species_parquet("wheat", "Criteres_stats")
  result <- get_stats(base, "wheat", collect = FALSE)
  expect_false(is.data.frame(result))
})

test_that("get_stats returns data frame when collect = TRUE", {
  base <- make_species_parquet("wheat", "Criteres_stats")
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

  expect_true(file.exists(rmse_per_usm_ds_path(base)))
})

test_that("get_rmse_per_usm delegates to open_parquet_or_null", {
  mock_open <- mock(NULL)
  stub(get_rmse_per_usm, "open_parquet_or_null", mock_open)

  get_rmse_per_usm("/base", "wheat", collect = FALSE)

  expect_called(mock_open, 1)
  args <- mock_args(mock_open)[[1]]
  expect_identical(args$path, rmse_per_usm_ds_path("/base"))
})

test_that("get_rmse_per_usm returns NULL when file missing", {
  stub(get_rmse_per_usm, "open_parquet_or_null", mock(NULL))
  expect_null(get_rmse_per_usm("/base", "wheat"))
})

test_that("get_rmse_per_usm returns data frame when collect = TRUE", {
  base <- make_species_parquet("wheat", "RMSE_per_USM")
  result <- get_rmse_per_usm(base, "wheat", collect = TRUE)
  expect_s3_class(result, "data.frame")
})

test_that("get_rmse_per_usm filters by usms when provided", {
  base <- make_species_parquet(
    "wheat", "RMSE_per_USM",
    df = data.frame(
      situation = c("usm1", "usm2", "usm3"),
      rmse = c(0.1, 0.2, 0.3),
      stringsAsFactors = FALSE
    )
  )
  result <- get_rmse_per_usm(
    base, "wheat", collect = TRUE, usms = c("usm1", "usm3")
  )
  expect_identical(nrow(result), 2L)
  expect_true(all(result$situation %in% c("usm1", "usm3")))
  expect_false("usm2" %in% result$situation)
})

test_that(
  "get_rmse_per_usm returns empty data frame when usms matches nothing",
  {
    base <- make_species_parquet(
      "wheat", "RMSE_per_USM",
      df = data.frame(
        situation = c("usm1", "usm2"),
        rmse = c(0.1, 0.2),
        stringsAsFactors = FALSE
      )
    )
    result <- get_rmse_per_usm(
      base, "wheat", collect = TRUE, usms = "usm_inexistant"
    )
    expect_identical(nrow(result), 0L)
  }
)

test_that("get_rmse_per_usm excludes columns listed in var2exclude", {
  base <- make_species_parquet(
    "wheat", "RMSE_per_USM",
    df = data.frame(
      situation = "usm1",
      rmse = 0.1,
      bias = 0.05,
      stringsAsFactors = FALSE
    )
  )
  result <- get_rmse_per_usm(
    base, "wheat", collect = TRUE, var2exclude = "bias"
  )
  expect_false("bias" %in% names(result))
  expect_true("rmse" %in% names(result))
  expect_true("situation" %in% names(result))
})

test_that("get_rmse_per_usm excludes multiple columns listed in var2exclude", {
  base <- make_species_parquet(
    "wheat", "RMSE_per_USM",
    df = data.frame(
      situation = "usm1",
      rmse = 0.1,
      bias = 0.05,
      n = 10L,
      stringsAsFactors = FALSE
    )
  )
  result <- get_rmse_per_usm(
    base, "wheat", collect = TRUE, var2exclude = c("bias", "n")
  )
  expect_false(any(c("bias", "n") %in% names(result)))
  expect_true(all(c("situation", "rmse") %in% names(result)))
})

test_that("get_rmse_per_usm applies both usms and var2exclude together", {
  base <- make_species_parquet(
    "wheat", "RMSE_per_USM",
    df = data.frame(
      situation = c("usm1", "usm2", "usm3"),
      rmse = c(0.1, 0.2, 0.3),
      bias = c(0.01, 0.02, 0.03),
      stringsAsFactors = FALSE
    )
  )
  result <- get_rmse_per_usm(
    base, "wheat", collect = TRUE,
    usms = c("usm1", "usm2"),
    var2exclude = "bias"
  )
  expect_identical(nrow(result), 2L)
  expect_false("usm3" %in% result$situation)
  expect_false("bias" %in% names(result))
})

test_that("get_rmse_per_usm ignores usms and var2exclude when data is NULL", {
  stub(get_rmse_per_usm, "open_parquet_or_null", mock(NULL))
  expect_null(get_rmse_per_usm(
    "/base", "wheat", usms = "usm1", var2exclude = "bias"
  ))
})

# ===========================================================================
# Tests: save_deteriorated_usm / get_deteriorated_usm # nolint
# ===========================================================================

test_that("save_deteriorated_usm writes parquet to correct path", {
  base <- file.path(tempdir(), basename(tempfile()))
  df <- data.frame(
    usm = "usm1",
    species = "wheat",
    rRMSE = 0.5,
    stringsAsFactors = FALSE
  )
  save_deteriorated_usm(base, df)
  expect_true(
    file.exists(deteriorated_ds_path(base))
  )
})

test_that("get_deteriorated_usm delegates to open_parquet_or_null", {
  mock_open <- mock(NULL)
  stub(get_deteriorated_usm, "open_parquet_or_null", mock_open)

  get_deteriorated_usm("/base", "wheat", collect = FALSE)

  expect_called(mock_open, 1)
  args <- mock_args(mock_open)[[1]]
  expect_identical(args$path, deteriorated_ds_path("/base"))
})

test_that("get_deteriorated_usm returns NULL when file missing", {
  stub(get_deteriorated_usm, "open_parquet_or_null", mock(NULL))
  expect_null(get_deteriorated_usm("/base", "wheat"))
})

test_that("get_deteriorated_usm returns data frame when collect = TRUE", {
  base <- make_species_parquet(
    "wheat", "Deteriorated_RMSE_per_usm"
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
  df <- data.frame(variable = "LAI", species = "wheat", ratio = 1.1, stringsAsFactors = FALSE)

  save_species_comparison(base, df)

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
    species <- "wheat"
    base <- make_species_parquet(species, "comparison")
    result <- get_species_comparison(base, species, collect = TRUE)
    expect_s3_class(result, "data.frame")
  }
)

# ===========================================================================
# Tests: remove_init_obs
# ===========================================================================

# Helper to create fake simulation data
make_sim_data <- function() {
  data.frame(
    situation = c("A", "A", "B", "B"),
    species = c("sp1", "sp1", "sp2", "sp2"),
    Date = as.Date(
      c("2023-01-01", "2023-01-02", "2023-01-01", "2023-01-03")
    ),
    value = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )
}

# Helper to create fake observation data
make_obs_data <- function() {
  data.frame(
    situation = c("A", "A", "B", "B"),
    species = c("sp1", "sp1", "sp2", "sp2"),
    Date = as.Date(
      c("2023-01-01", "2023-01-02", "2023-01-01", "2023-01-03")
    ),
    measure1 = c(1.1, 2.2, 3.3, 4.4),
    measure2 = c(5.5, 6.6, 7.7, 8.8),
    stringsAsFactors = FALSE
  )
}

test_that("values at init date are replaced with NA", {
  stub(remove_init_obs, "get_sim_ds", function(...) make_sim_data())
  stub(remove_init_obs, "get_obs_ds", function(...) make_obs_data())
  stub(remove_init_obs, "obs_ds_path", function(...) tempdir())
  stub(remove_init_obs, "arrow::write_dataset", function(data, ...) data)

  result <- remove_init_obs(file.path("fake", "path"))

  # Situation A: init_date = 2023-01-01
  row_init_a <- result |>
    dplyr::filter(situation == "A", Date == as.Date("2023-01-01"))
  expect_true(is.na(row_init_a$measure1))
  expect_true(is.na(row_init_a$measure2))

  # Situation A: next date is preserved
  row_next_a <- result |>
    dplyr::filter(situation == "A", Date == as.Date("2023-01-02"))
  expect_equal(row_next_a$measure1, 2.2)
  expect_equal(row_next_a$measure2, 6.6)
})

test_that("values after init date are unchanged", {
  stub(remove_init_obs, "get_sim_ds", function(...) make_sim_data())
  stub(remove_init_obs, "get_obs_ds", function(...) make_obs_data())
  stub(remove_init_obs, "obs_ds_path", function(...) tempdir())
  stub(remove_init_obs, "arrow::write_dataset", function(data, ...) data)

  result <- remove_init_obs(file.path("fake", "path"))

  row_b <- result |>
    dplyr::filter(situation == "B", Date == as.Date("2023-01-03"))
  expect_equal(row_b$measure1, 4.4)
  expect_equal(row_b$measure2, 8.8)
})

test_that("init_date is correctly computed as the minimum date per situation", {
  sim_data <- data.frame(
    situation = c("A", "A", "A"),
    species = c("sp1", "sp1", "sp1"),
    Date = as.Date(c("2023-01-03", "2023-01-01", "2023-01-02")),
    value = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  obs_data <- data.frame(
    situation = c("A", "A", "A"),
    species = c("sp1", "sp1", "sp1"),
    Date = as.Date(c("2023-01-03", "2023-01-01", "2023-01-02")),
    measure1 = c(1.0, 2.0, 3.0),
    stringsAsFactors = FALSE
  )

  stub(remove_init_obs, "get_sim_ds", function(...) sim_data)
  stub(remove_init_obs, "get_obs_ds", function(...) obs_data)
  stub(remove_init_obs, "obs_ds_path", function(...) tempdir())
  stub(remove_init_obs, "arrow::write_dataset", function(data, ...) data)

  result <- remove_init_obs(file.path("fake", "path"))

  # Only 2023-01-01 (min) should be NA
  init_measure1 <- result |>
    dplyr::filter(Date == as.Date("2023-01-01")) |>
    dplyr::pull(measure1)
  expect_true(is.na(init_measure1))
  next_measure1 <- result |>
    dplyr::filter(Date == as.Date("2023-01-02")) |>
    dplyr::pull(measure1)
  expect_false(is.na(next_measure1))
  next_measure1 <- result |>
    dplyr::filter(Date == as.Date("2023-01-03")) |>
    dplyr::pull(measure1)
  expect_false(is.na(next_measure1))
})

test_that("init_date column is absent from the final result", {
  stub(remove_init_obs, "get_sim_ds", function(...) make_sim_data())
  stub(remove_init_obs, "get_obs_ds", function(...) make_obs_data())
  stub(remove_init_obs, "obs_ds_path", function(...) tempdir())
  stub(remove_init_obs, "arrow::write_dataset", function(data, ...) data)

  result <- remove_init_obs(file.path("fake", "path"))
  expect_false("init_date" %in% names(result))
})

test_that("situation, species and Date columns are not modified", {
  stub(remove_init_obs, "get_sim_ds", function(...) make_sim_data())
  stub(remove_init_obs, "get_obs_ds", function(...) make_obs_data())
  stub(remove_init_obs, "obs_ds_path", function(...) tempdir())
  stub(remove_init_obs, "arrow::write_dataset", function(data, ...) data)

  result <- remove_init_obs(file.path("fake", "path"))
  obs <- make_obs_data()

  expect_identical(result$situation, obs$situation)
  expect_identical(result$species, obs$species)
  expect_identical(result$Date, obs$Date)
})

test_that(
  "write_dataset is called with the correct path, format and partitioning",
  {
    stub(remove_init_obs, "get_sim_ds", function(...) make_sim_data())
    stub(remove_init_obs, "get_obs_ds", function(...) make_obs_data())
    stub(
      remove_init_obs,
      "obs_ds_path",
      function(...) file.path("expected", "path")
    )

    mock_write <- mock()
    stub(remove_init_obs, "arrow::write_dataset", mock_write)

    remove_init_obs(file.path("fake", "path"))

    expect_called(mock_write, 1)
    call_args <- mock_args(mock_write)[[1]]
    expect_identical(call_args[[2]], file.path("expected", "path"))
    expect_identical(call_args$format, "parquet")
    expect_identical(call_args$partitioning, "species")
  }
)

test_that(
  "NA dates in simulation data are ignored when computing the minimum",
  {
    sim_data <- data.frame(
      situation = c("A", "A"),
      species   = c("sp1", "sp1"),
      Date      = as.Date(c(NA, "2023-01-05")),
      value     = c(10, 20),
      stringsAsFactors = FALSE
    )
    obs_data <- data.frame(
      situation = c("A", "A"),
      species   = c("sp1", "sp1"),
      Date      = as.Date(c("2023-01-05", "2023-01-06")),
      measure1  = c(9.9, 8.8),
      stringsAsFactors = FALSE
    )

    stub(remove_init_obs, "get_sim_ds", function(...) sim_data)
    stub(remove_init_obs, "get_obs_ds", function(...) obs_data)
    stub(remove_init_obs, "obs_ds_path", function(...) tempdir())
    stub(remove_init_obs, "arrow::write_dataset", function(data, ...) data)

    result <- remove_init_obs(file.path("fake", "path"))

    init_measure1 <- result |>
      dplyr::filter(Date == as.Date("2023-01-05")) |>
      dplyr::pull(measure1)
    expect_true(is.na(init_measure1))
    next_measure1 <- result |>
      dplyr::filter(Date == as.Date("2023-01-06")) |>
      dplyr::pull(measure1)
    expect_identical(next_measure1, 8.8)
  }
)
