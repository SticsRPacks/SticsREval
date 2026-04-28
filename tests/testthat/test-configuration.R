# ===========================================================================
# Tests: make_config
# ===========================================================================

test_that("make_config returns a list", {
  stub(make_config, "init_logger", mock(NULL))
  result <- make_config()
  expect_type(result, "list")
})

test_that("make_config includes all expected keys", {
  stub(make_config, "init_logger", mock(NULL))
  result <- make_config()
  expected_keys <- c(
    "stics_exe", "usms_workspace", "run_simulations",
    "verbose", "parallel", "cores",
    "reference_version", "metadata_file",
    "percentage", "eval_workspace",
    "init_workspace", "output_dir", "species", "usms",
    "var2exclude"
  )
  expect_named(result, expected_keys)
})

test_that("make_config uses provided values", {
  stub(make_config, "init_logger", mock(NULL))
  result <- make_config(
    stics_exe = "/stics",
    usms_workspace = "/ws",
    metadata_file = "/meta.csv",
    percentage = 10,
    parallel = TRUE,
    cores = 4
  )
  expect_identical(result$stics_exe, "/stics")
  expect_identical(result$usms_workspace, "/ws")
  expect_identical(result$metadata_file, "/meta.csv")
  expect_identical(result$percentage, 10)
  expect_true(result$parallel)
  expect_identical(result$cores, 4)
})

test_that("make_config uses default values", {
  stub(make_config, "init_logger", mock(NULL))
  result <- make_config()
  expect_true(result$run_simulations)
  expect_identical(result$verbose, 1)
  expect_false(result$parallel)
  expect_true(is.na(result$cores))
  expect_identical(result$percentage, 5)
  expect_true(result$init_workspace)
  expect_null(result$stics_exe)
  expect_null(result$usms_workspace)
  expect_null(result$eval_workspace)
  expect_null(result$output_dir)
  expect_null(result$reference_version)
  expect_null(result$usms)
  expect_null(result$species)
  expect_null(result$var2exclude)
})

test_that("make_config calls init_logger with verbose level", {
  mock_logger <- mock(NULL)
  stub(make_config, "init_logger", mock_logger)
  make_config(verbose = 2)
  expect_called(mock_logger, 1)
  expect_identical(mock_args(mock_logger)[[1]][[1]], 2)
})

# ===========================================================================
# Helpers
# ===========================================================================

make_valid_config <- function(overrides = list()) {
  meta <- tempfile(fileext = ".csv")
  writeLines("usm;rotation;rotation_order", meta)
  cfg <- list(
    stics_exe = "/stics",
    usms_workspace = "/ws",
    metadata_file = meta,
    reference_version = NULL,
    eval_workspace = tempdir(),
    output_dir = tempdir(),
    percentage = 5,
    species = NULL
  )
  for (nm in names(overrides)) cfg[[nm]] <- overrides[[nm]]
  cfg
}

# ===========================================================================
# Tests: validate_eval_configuration
# ===========================================================================

test_that(
  "validate_eval_configuration passes with valid config
  (init_workspace = FALSE)",
  {
    cfg <- make_valid_config(list(init_workspace = FALSE))
    expect_no_error(validate_eval_configuration(cfg))
  }
)

test_that(
  "validate_eval_configuration passes with valid config
  (init_workspace = TRUE)",
  {
    cfg <- make_valid_config(list(init_workspace = TRUE))
    expect_no_error(validate_eval_configuration(cfg))
  }
)

test_that(
  "validate_eval_configuration stops when stics_exe is NULL and
  init_workspace = TRUE",
  {
    cfg <- make_valid_config(list(init_workspace = TRUE, stics_exe = NULL))
    expect_error(
      validate_eval_configuration(cfg),
      regexp = "Stics executable path must be defined"
    )
  }
)

test_that(
  "validate_eval_configuration stops when usms_workspace is NULL and
  init_workspace = TRUE",
  {
    cfg <- make_valid_config(list(init_workspace = TRUE, usms_workspace = NULL))
    expect_error(
      validate_eval_configuration(cfg),
      regexp = "USMs workspace path must be defined"
    )
  }
)

test_that(
  "validate_eval_configuration stops when metadata_file is NULL and
  init_workspace = TRUE",
  {
    cfg <- make_valid_config(list(init_workspace = TRUE, metadata_file = NULL))
    expect_error(
      validate_eval_configuration(cfg),
      regexp = "Metadata file must be a valid path"
    )
  }
)

test_that(
  "validate_eval_configuration stops when metadata_file does not exist
  and init_workspace = TRUE",
  {
    cfg <- make_valid_config(
      list(
        init_workspace = TRUE,
        metadata_file = file.path("nonexistent", "meta.csv")
      )
    )
    expect_error(
      validate_eval_configuration(cfg),
      regexp = "Metadata file must be a valid path"
    )
  }
)

test_that(
  "validate_eval_configuration does not check stics_exe when
  init_workspace = FALSE",
  {
    cfg <- make_valid_config(list(init_workspace = FALSE, stics_exe = NULL))
    expect_no_error(validate_eval_configuration(cfg))
  }
)

test_that(
  "validate_eval_configuration passes when reference_version is NULL",
  {
    cfg <- make_valid_config(
      list(init_workspace = FALSE, reference_version = NULL)
    )
    expect_no_error(validate_eval_configuration(cfg))
  }
)

test_that(
  "validate_eval_configuration passes when reference_workspace exists",
  {
    cfg <- make_valid_config(
      list(init_workspace = FALSE, reference_workspace = tempdir())
    )
    expect_no_error(validate_eval_configuration(cfg))
  }
)

test_that(
  "validate_eval_configuration stops when eval_workspace is NULL",
  {
    cfg <- make_valid_config(
      list(init_workspace = FALSE, eval_workspace = NULL)
    )
    expect_error(
      validate_eval_configuration(cfg),
      regexp = "Eval workspace path must be defined"
    )
  }
)

# ===========================================================================
# Tests: validate_export_config
# ===========================================================================

test_that(
  "validate_export_config passes with valid config",
  {
    cfg <- make_valid_config()
    expect_no_error(validate_export_config(cfg))
  }
)

test_that(
  "validate_export_config errors when output_dir is NULL",
  {
    cfg <- make_valid_config(list(output_dir = NULL))
    expect_error(
      validate_export_config(cfg),
      regexp = "Output dir"
    )
  }
)

test_that(
  "validate_export_config creates output_dir if it does not exist",
  {
    new_dir <- file.path(tempdir(), basename(tempfile()))
    cfg <- make_valid_config(list(output_dir = new_dir))
    validate_export_config(cfg)
    expect_true(dir.exists(new_dir))
  }
)

test_that(
  "validate_export_config errors when output_dir cannot be created",
  {
    cfg <- make_valid_config(
      list(output_dir = "/proc/invalid_output") # nolint: nonportable_path_linter
    )
    expect_error(
      suppressWarnings(validate_export_config(cfg)),
      regexp = "Can't create"
    )
  }
)

test_that(
  "validate_export_config errors when eval_workspace is NULL",
  {
    cfg <- make_valid_config(list(eval_workspace = NULL))
    expect_error(
      validate_export_config(cfg),
      regexp = "Eval workspace"
    )
  }
)

# ===========================================================================
# Tests: validate_plots_config
# ===========================================================================

test_that(
  "validate_plots_config passes when reference_version is NULL",
  {
    cfg <- make_valid_config(list(reference_version = NULL))
    expect_no_error(validate_plots_config(cfg))
  }
)

test_that(
  "validate_plots_config passes when reference_version exists",
  {
    cfg <- make_valid_config(list(reference_version = "1.0.0"))

    mockery::stub(
      validate_plots_config,
      "get_all_versions",
      mockery::mock(c("1.0.0", "2.0.0"))
    )

    expect_no_error(validate_plots_config(cfg))
  }
)

test_that(
  "validate_plots_config fails when reference_version not in metadata",
  {
    cfg <- make_valid_config(list(reference_version = "9.9.9"))

    mockery::stub(
      validate_plots_config,
      "get_all_versions",
      mockery::mock(c("1.0.0", "2.0.0"))
    )

    expect_error(validate_plots_config(cfg))
  }
)

test_that(
  "validate_plots_config fails when no metadata found",
  {
    cfg <- make_valid_config(list(reference_version = "1.0.0"))

    mockery::stub(
      validate_plots_config,
      "get_all_versions",
      mockery::mock(NULL)
    )

    expect_error(validate_plots_config(cfg))
  }
)
