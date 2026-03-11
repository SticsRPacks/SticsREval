# ===========================================================================
# Tests: prepare_species_output_dir
# ===========================================================================

test_that(
  "prepare_species_output_dir creates the directory and returns its path",
  {
    base <- file.path(tempdir(), basename(tempfile()))
    result <- prepare_species_output_dir(base, "wheat")

    expect_true(dir.exists(result))
    expect_identical(result, file.path(base, "wheat"))
  }
)

test_that(
  "prepare_species_output_dir returns path when directory already exists",
  {
    base <- file.path(tempdir(), basename(tempfile()))
    dir.create(file.path(base, "wheat"), recursive = TRUE)

    result <- prepare_species_output_dir(base, "wheat")
    expect_identical(result, file.path(base, "wheat"))
  }
)

test_that("prepare_species_output_dir creates nested directories recursively", {
  base <- file.path(tempdir(), basename(tempfile()), "nested")
  result <- prepare_species_output_dir(base, "maize")

  expect_true(dir.exists(result))
})

test_that(
  "prepare_species_output_dir throws an error when directory cannot be created",
  {
    invalid_path <- if (.Platform$OS.type == "windows") {
      "C:/invalid:path/test" # nolint: nonportable_path_linter
    } else {
      "/proc/invalid_root_path" # nolint: nonportable_path_linter
    }
    expect_error(
      suppressWarnings(
        prepare_species_output_dir(invalid_path, "wheat")
      ),
      regexp = "Can't create output directory"
    )
  }
)

# ===========================================================================
# Helpers
# ===========================================================================

make_export_config <- function(overrides = list()) {
  cfg <- list(
    output_dir = file.path(tempdir(), basename(tempfile())),
    eval_workspace = list()
  )
  for (nm in names(overrides)) cfg[[nm]] <- overrides[[nm]]
  cfg
}

# ===========================================================================
# Tests: export_stats_to_csv
# ===========================================================================

test_that("export_stats_to_csv calls validate_export_config", {
  mock_validate <- mock(NULL)

  stub(export_stats_to_csv, "validate_export_config", mock_validate)
  stub(export_stats_to_csv, "get_species", mock(character(0)))

  export_stats_to_csv(make_export_config())
  expect_called(mock_validate, 1)
})

test_that(
  "export_stats_to_csv writes Criteres_stats.csv when stats available",
  {
    mock_write <- mock(NULL)
    cfg        <- make_export_config()

    stub(export_stats_to_csv, "validate_export_config", mock(NULL))
    stub(export_stats_to_csv, "get_species", mock("wheat"))
    stub(
      export_stats_to_csv,
      "prepare_species_output_dir",
      mock(cfg$output_dir)
    )
    stub(export_stats_to_csv, "get_stats", mock(data.frame(x = 1)))
    stub(export_stats_to_csv, "get_rmse_per_usm", mock(NULL))
    stub(export_stats_to_csv, "get_deteriorated_usm", mock(NULL))
    stub(export_stats_to_csv, "safe_write_csv", mock_write)

    export_stats_to_csv(cfg)

    expect_called(mock_write, 1)
    args <- mock_args(mock_write)[[1]]
    expect_match(args[[2]], "Criteres_stats\\.csv$") # nolint: nonportable_path_linter
  }
)

test_that(
  "export_stats_to_csv does not write Criteres_stats.csv when stats is NULL",
  {
    mock_write <- mock(NULL)
    cfg        <- make_export_config()

    stub(export_stats_to_csv, "validate_export_config", mock(NULL))
    stub(export_stats_to_csv, "get_species", mock("wheat"))
    stub(
      export_stats_to_csv,
      "prepare_species_output_dir",
      mock(cfg$output_dir)
    )
    stub(export_stats_to_csv, "get_stats", mock(NULL))
    stub(export_stats_to_csv, "get_rmse_per_usm", mock(NULL))
    stub(export_stats_to_csv, "get_deteriorated_usm", mock(NULL))
    stub(export_stats_to_csv, "safe_write_csv", mock_write)

    export_stats_to_csv(cfg)
    expect_called(mock_write, 0)
  }
)

test_that(
  "export_stats_to_csv writes RMSE_per_usm.csv when rmse_per_usm available",
  {
    mock_write <- mock(NULL, cycle = TRUE)
    cfg <- make_export_config()

    stub(export_stats_to_csv, "validate_export_config", mock(NULL))
    stub(export_stats_to_csv, "get_species", mock("wheat"))
    stub(
      export_stats_to_csv,
      "prepare_species_output_dir",
      mock(cfg$output_dir)
    )
    stub(export_stats_to_csv, "get_stats", mock(NULL))
    stub(
      export_stats_to_csv,
      "get_rmse_per_usm",
      mock(data.frame(usm = "usm1", rmse = 0.1, stringsAsFactors = FALSE))
    )
    stub(export_stats_to_csv, "get_deteriorated_usm", mock(NULL))
    stub(export_stats_to_csv, "safe_write_csv", mock_write)

    export_stats_to_csv(cfg)

    expect_called(mock_write, 1)
    args <- mock_args(mock_write)[[1]]
    expect_match(args[[2]], "RMSE_per_usm\\.csv$") # nolint: nonportable_path_linter
  }
)

test_that(
  "export_stats_to_csv writes Deteriorated_USM.csv when deteriorated_usm
  available",
  {
    mock_write <- mock(NULL, cycle = TRUE)
    cfg <- make_export_config()

    stub(export_stats_to_csv, "validate_export_config", mock(NULL))
    stub(export_stats_to_csv, "get_species", mock("wheat"))
    stub(
      export_stats_to_csv, "prepare_species_output_dir",
      mock(cfg$output_dir)
    )
    stub(export_stats_to_csv, "get_stats", mock(NULL))
    stub(export_stats_to_csv, "get_rmse_per_usm", mock(NULL))
    stub(
      export_stats_to_csv,
      "get_deteriorated_usm",
      mock(data.frame(usm = "usm1", stringsAsFactors = FALSE))
    )
    stub(export_stats_to_csv, "safe_write_csv", mock_write)

    export_stats_to_csv(cfg)

    expect_called(mock_write, 1)
    args <- mock_args(mock_write)[[1]]
    expect_match(args[[2]], "Deteriorated_USM\\.csv$") # nolint: nonportable_path_linter
  }
)

test_that(
  "export_stats_to_csv writes all three files when all data available",
  {
    mock_write <- mock(NULL, cycle = TRUE)
    cfg <- make_export_config()

    stub(export_stats_to_csv, "validate_export_config", mock(NULL))
    stub(export_stats_to_csv, "get_species", mock("wheat"))
    stub(
      export_stats_to_csv,
      "prepare_species_output_dir",
      mock(cfg$output_dir)
    )
    stub(export_stats_to_csv, "get_stats", mock(data.frame(x = 1)))
    stub(
      export_stats_to_csv,
      "get_rmse_per_usm",
      mock(data.frame(usm = "usm1", rmse = 0.1, stringsAsFactors = FALSE))
    )
    stub(
      export_stats_to_csv,
      "get_deteriorated_usm",
      mock(data.frame(usm = "usm1", stringsAsFactors = FALSE))
    )
    stub(export_stats_to_csv, "safe_write_csv", mock_write)

    export_stats_to_csv(cfg)
    expect_called(mock_write, 3)
  }
)

test_that(
  "export_stats_to_csv calls prepare_species_output_dir once per species",
  {
    mock_prepare <- mock(tempdir(), cycle = TRUE)
    cfg <- make_export_config()

    stub(export_stats_to_csv, "validate_export_config", mock(NULL))
    stub(export_stats_to_csv, "get_species", mock(c("wheat", "maize", "soy")))
    stub(export_stats_to_csv, "prepare_species_output_dir", mock_prepare)
    stub(export_stats_to_csv, "get_stats", mock(NULL, cycle = TRUE))
    stub(export_stats_to_csv, "get_rmse_per_usm", mock(NULL, cycle = TRUE))
    stub(export_stats_to_csv, "get_deteriorated_usm", mock(NULL, cycle = TRUE))
    stub(export_stats_to_csv, "safe_write_csv", mock(NULL))

    export_stats_to_csv(cfg)
    expect_called(mock_prepare, 3)
  }
)

test_that(
  "export_stats_to_csv filters species when config$species is set",
  {
    mock_prepare <- mock(tempdir(), cycle = TRUE)
    cfg <- make_export_config(list(species = "wheat"))

    stub(export_stats_to_csv, "validate_export_config", mock(NULL))
    stub(export_stats_to_csv, "get_species", mock(c("wheat", "maize")))
    stub(export_stats_to_csv, "prepare_species_output_dir", mock_prepare)
    stub(export_stats_to_csv, "get_stats", mock(NULL, cycle = TRUE))
    stub(export_stats_to_csv, "get_rmse_per_usm", mock(NULL, cycle = TRUE))
    stub(export_stats_to_csv, "get_deteriorated_usm", mock(NULL, cycle = TRUE))
    stub(export_stats_to_csv, "safe_write_csv", mock(NULL))

    export_stats_to_csv(cfg)

    expect_called(mock_prepare, 1)
    args <- mock_args(mock_prepare)[[1]]
    expect_identical(args[[2]], "wheat")
  }
)

test_that(
  "export_stats_to_csv processes all species when config$species is NULL",
  {
    mock_prepare <- mock(tempdir(), cycle = TRUE)
    cfg <- make_export_config(list(species = NULL))

    stub(export_stats_to_csv, "validate_export_config", mock(NULL))
    stub(export_stats_to_csv, "get_species", mock(c("wheat", "maize")))
    stub(export_stats_to_csv, "prepare_species_output_dir", mock_prepare)
    stub(export_stats_to_csv, "get_stats", mock(NULL, cycle = TRUE))
    stub(export_stats_to_csv, "get_rmse_per_usm", mock(NULL, cycle = TRUE))
    stub(export_stats_to_csv, "get_deteriorated_usm", mock(NULL, cycle = TRUE))
    stub(export_stats_to_csv, "safe_write_csv", mock(NULL))

    export_stats_to_csv(cfg)

    expect_called(mock_prepare, 2)
  }
)

# ===========================================================================
# Tests: export_species_sim
# ===========================================================================

test_that("export_species_sim calls validate_export_config", {
  mock_validate <- mock(NULL)

  stub(export_species_sim, "validate_export_config", mock_validate)
  stub(export_species_sim, "get_species",            mock(character(0)))

  export_species_sim(make_export_config())
  expect_called(mock_validate, 1)
})

test_that("export_species_sim calls write_parquet once per species", {
  mock_parquet <- mock(NULL, cycle = TRUE)
  cfg <- make_export_config()

  stub(export_species_sim, "validate_export_config", mock(NULL))
  stub(export_species_sim, "get_species", mock(c("wheat", "maize")))
  stub(
    export_species_sim,
    "prepare_species_output_dir",
    mock(tempdir(), cycle = TRUE)
  )
  stub(
    export_species_sim,
    "get_by_species",
    mock(data.frame(x = 1), cycle = TRUE)
  )
  stub(export_species_sim, "arrow::write_parquet", mock_parquet)

  export_species_sim(cfg)
  expect_called(mock_parquet, 2)
})

test_that("export_species_sim writes to Simulations.parquet", {
  mock_parquet <- mock(NULL)
  cfg <- make_export_config()

  stub(export_species_sim, "validate_export_config", mock(NULL))
  stub(export_species_sim, "get_species", mock("wheat"))
  stub(export_species_sim, "prepare_species_output_dir", mock(cfg$output_dir))
  stub(export_species_sim, "get_by_species", mock(data.frame(x = 1)))
  stub(export_species_sim, "arrow::write_parquet", mock_parquet)

  export_species_sim(cfg)

  args <- mock_args(mock_parquet)[[1]]
  expect_match(args$sink, "Simulations\\.parquet$") # nolint: nonportable_path_linter
})

test_that("export_species_sim passes sim data to write_parquet", {
  mock_parquet <- mock(NULL)
  fake_sim <- data.frame(
    LAI = c(1.2, 1.5), MASEC = c(100, 200), stringsAsFactors = FALSE
  )
  cfg <- make_export_config()
  stub(export_species_sim, "validate_export_config", mock(NULL))
  stub(export_species_sim, "get_species", mock("wheat"))
  stub(export_species_sim, "prepare_species_output_dir", mock(cfg$output_dir))
  stub(export_species_sim, "get_by_species", mock(fake_sim))
  stub(export_species_sim, "arrow::write_parquet", mock_parquet)
  export_species_sim(cfg)
  args <- mock_args(mock_parquet)[[1]]
  expect_identical(args[[1]], fake_sim)
})

test_that(
  "export_species_sim calls prepare_species_output_dir once per species",
  {
    mock_prepare <- mock(tempdir(), cycle = TRUE)
    cfg <- make_export_config()

    stub(export_species_sim, "validate_export_config", mock(NULL))
    stub(export_species_sim, "get_species", mock(c("wheat", "maize")))
    stub(export_species_sim, "prepare_species_output_dir", mock_prepare)
    stub(export_species_sim, "get_by_species", mock(data.frame(), cycle = TRUE))
    stub(export_species_sim, "arrow::write_parquet", mock(NULL, cycle = TRUE))

    export_species_sim(cfg)
    expect_called(mock_prepare, 2)
  }
)

test_that(
  "export_species_sim filters species when config$species is set",
  {
    mock_prepare <- mock(tempdir(), cycle = TRUE)
    cfg <- make_export_config(list(species = "wheat"))

    stub(export_species_sim, "validate_export_config", mock(NULL))
    stub(export_species_sim, "get_species", mock(c("wheat", "maize")))
    stub(export_species_sim, "prepare_species_output_dir", mock_prepare)
    stub(export_species_sim, "get_by_species", mock(data.frame(), cycle = TRUE))
    stub(export_species_sim, "arrow::write_parquet", mock(NULL, cycle = TRUE))

    export_species_sim(cfg)

    expect_called(mock_prepare, 1)
    args <- mock_args(mock_prepare)[[1]]
    expect_identical(args[[2]], "wheat")
  }
)

test_that(
  "export_species_sim processes all species when config$species is NULL",
  {
    mock_prepare <- mock(tempdir(), cycle = TRUE)
    cfg <- make_export_config(list(species = NULL))

    stub(export_species_sim, "validate_export_config", mock(NULL))
    stub(export_species_sim, "get_species", mock(c("wheat", "maize")))
    stub(export_species_sim, "prepare_species_output_dir", mock_prepare)
    stub(export_species_sim, "get_by_species", mock(data.frame(), cycle = TRUE))
    stub(export_species_sim, "arrow::write_parquet", mock(NULL, cycle = TRUE))

    export_species_sim(cfg)

    expect_called(mock_prepare, 2)
  }
)
