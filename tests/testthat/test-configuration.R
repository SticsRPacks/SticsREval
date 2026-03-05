# ===========================================================================
# Tests: make_config
# ===========================================================================

test_that("make_config returns a list", {
  stub(make_config, "init_logger", mock(NULL))
  result <- make_config()
  expect_type(result, list)
})

test_that("make_config includes all expected keys", {
  stub(make_config, "init_logger", mock(NULL))
  result <- make_config()
  expected_keys <- c(
    "stics_exe", "workspace", "run_simulations",
    "verbose", "parallel", "cores",
    "reference_data_dir", "metadata_file",
    "percentage", "eval_workspace",
    "init_workspace", "output_dir"
  )
  expect_true(all(expected_keys %in% names(result)))
})

test_that("make_config uses provided values", {
  stub(make_config, "init_logger", mock(NULL))
  result <- make_config(
    stics_exe = "/stics",
    workspace = "/ws",
    metadata_file = "/meta.csv",
    percentage = 10,
    parallel = TRUE,
    cores = 4
  )
  expect_identical(result$stics_exe, "/stics")
  expect_identical(result$workspace, "/ws")
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
  expect_null(result$workspace)
  expect_null(result$output_dir)
  expect_null(result$reference_data_dir)
})

test_that("make_config calls init_logger with verbose level", {
  mock_logger <- mock(NULL)
  stub(make_config, "init_logger", mock_logger)
  make_config(verbose = 2)
  expect_called(mock_logger, 1)
  expect_identical(mock_args(mock_logger)[[1]][[1]], 2)
})

test_that("make_config sets eval_workspace to DEFAULT_WORKSPACE", {
  stub(make_config, "init_logger", mock(NULL))
  result <- make_config()
  expect_identical(result$eval_workspace, DEFAULT_WORKSPACE)
})

# ===========================================================================
# Helpers
# ===========================================================================

make_valid_config <- function(overrides = list()) {
  meta <- tempfile(fileext = ".csv")
  writeLines("usm;rotation;rotation_order", meta)
  cfg <- list(
    stics_exe = "/stics",
    workspace = "/ws",
    metadata_file = meta,
    reference_data_dir = NULL,
    eval_workspace = tempdir(),
    output_dir = tempdir(),
    percentage = 5
  )
  for (nm in names(overrides)) cfg[[nm]] <- overrides[[nm]]
  cfg
}

# ===========================================================================
# Tests: validate_eval_configuration
# ===========================================================================

test_that(
  "validate_eval_configuration passes with valid config",
  {
    cfg <- make_valid_config()
    expect_no_error(validate_eval_configuration(cfg))
  }
)

test_that(
  "validate_eval_configuration errors when stics_exe is NULL",
  {
    cfg <- make_valid_config(list(stics_exe = NULL))
    expect_error(
      validate_eval_configuration(cfg),
      regexp = "Stics executable"
    )
  }
)

test_that(
  "validate_eval_configuration errors when workspace is NULL",
  {
    cfg <- make_valid_config(list(workspace = NULL))
    expect_error(
      validate_eval_configuration(cfg),
      regexp = "Workspace"
    )
  }
)

test_that(
  "validate_eval_configuration errors when metadata_file is NULL",
  {
    cfg <- make_valid_config(list(metadata_file = NULL))
    expect_error(
      validate_eval_configuration(cfg),
      regexp = "Metadata file"
    )
  }
)

test_that(
  "validate_eval_configuration errors when metadata_file does not exist",
  {
    cfg <- make_valid_config(
      list(metadata_file = "/nonexistent/meta.csv") # nolint: nonportable_path_linter
    )
    expect_error(
      validate_eval_configuration(cfg),
      regexp = "Metadata file"
    )
  }
)

test_that(
  "validate_eval_configuration errors when eval_workspace is NULL",
  {
    cfg <- make_valid_config(list(eval_workspace = NULL))
    expect_error(
      validate_eval_configuration(cfg),
      regexp = "Eval workspace"
    )
  }
)

test_that(
  "validate_eval_configuration errors when reference_data_dir is invalid",
  {
    cfg <- make_valid_config(
      list(reference_data_dir = "/nonexistent/ref") # nolint: nonportable_path_linter
    )
    expect_error(
      validate_eval_configuration(cfg),
      regexp = "Reference data directory"
    )
  }
)

test_that(
  "validate_eval_configuration passes when reference_data_dir is NULL",
  {
    cfg <- make_valid_config(list(reference_data_dir = NULL))
    expect_no_error(validate_eval_configuration(cfg))
  }
)

test_that(
  "validate_eval_configuration passes when reference_data_dir exists",
  {
    cfg <- make_valid_config(
      list(reference_data_dir = tempdir())
    )
    expect_no_error(validate_eval_configuration(cfg))
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
  "validate_plots_config passes when reference_data_dir is NULL",
  {
    cfg <- make_valid_config(list(reference_data_dir = NULL))
    expect_no_error(validate_plots_config(cfg))
  }
)

test_that(
  "validate_plots_config passes when reference_data_dir exists",
  {
    cfg <- make_valid_config(
      list(reference_data_dir = tempdir())
    )
    expect_no_error(validate_plots_config(cfg))
  }
)

test_that(
  "validate_plots_config errors when reference_data_dir is invalid",
  {
    cfg <- make_valid_config(
      list(reference_data_dir = "/nonexistent/ref") # nolint: nonportable_path_linter
    )
    expect_error(
      validate_plots_config(cfg),
      regexp = "Reference data directory"
    )
  }
)
