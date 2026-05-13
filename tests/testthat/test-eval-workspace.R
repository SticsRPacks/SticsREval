# ---- Helpers ----

make_ws <- function(dir, version = NULL) {
  EvalWorkspace$new(dir, version = version)
}

write_metadata <- function(dir, rows) {
  arrow::write_parquet(
    data.frame(rows),
    sink = metadata_ds_path(dir)
  )
}

write_parquet_ds <- function(data, path, partitioning = NULL) {
  arrow::write_dataset(
    data,
    path = path,
    format = "parquet",
    partitioning = partitioning
  )
}

# ---- Path helpers ----

test_that("path helpers return correct subpaths", {
  expect_identical(sim_ds_path("ws"), file.path("ws", "sim"))
  expect_identical(obs_ds_path("ws"), file.path("ws", "obs"))
  expect_identical(stats_ds_path("ws"), file.path("ws", "Criteres_stats"))
  expect_identical(rmse_per_usm_ds_path("ws"), file.path("ws", "RMSE_per_USM"))
  expect_identical(
    deteriorated_ds_path("ws"),
    file.path("ws", "Deteriorated_RMSE_per_usm")
  )
  expect_identical(comparison_ds_path("ws"), file.path("ws", "comparison"))
  expect_identical(metadata_ds_path("ws"), file.path("ws", "metadata.parquet"))
})

# ---- initialize / get_stics_version ----

test_that("initialize uses provided version without reading metadata", {
  dir <- withr::local_tempdir()
  ws <- make_ws(dir, version = "v2")
  expect_identical(ws$get_version(), "v2")
})

test_that(
  "initialize reads last_evaluated version from metadata when version is NULL",
  {
    dir <- withr::local_tempdir()
    write_metadata(dir, list(
      stics_version = c("v1", "v2"),
      last_evaluated = c(FALSE, TRUE)
    ))

    ws <- make_ws(dir)
    expect_identical(ws$get_version(), "v2")
  }
)

test_that("initialize sets version to NULL when no metadata exists", {
  dir <- withr::local_tempdir()
  ws <- make_ws(dir)
  expect_null(ws$get_version())
})

# ---- with_version ----

test_that("with_version returns a new EvalWorkspace with the given version", {
  dir <- withr::local_tempdir()
  ws <- make_ws(dir, version = "v1")
  ws$add_evaluated_version("v1")
  ws$add_evaluated_version("v2")
  ws2 <- ws$with_version("v2")

  expect_s3_class(ws2, "EvalWorkspace")
  expect_identical(ws2$get_version(), "v2")
  # original is unchanged
  expect_identical(ws$get_version(), "v1")
})

test_that("with_version fails when the given version is not found", {
  dir <- withr::local_tempdir()
  ws <- make_ws(dir, version = "v1")
  ws$add_evaluated_version("v1")

  expect_error(
    ws$with_version("v2"),
    "Version v2 not found in the workspace. Available versions: v1"
  )
})

# ---- add_evaluated_version ----

test_that("add_evaluated_version creates metadata when none exists", {
  dir <- withr::local_tempdir()
  ws <- make_ws(dir, version = "v1")
  ws$add_evaluated_version("v1")

  meta <- arrow::read_parquet(metadata_ds_path(dir))
  expect_identical(meta$stics_version, "v1")
  expect_true(meta$last_evaluated)
})

test_that("add_evaluated_version marks only new version as last_evaluated", {
  dir <- withr::local_tempdir()
  write_metadata(dir, list(
    stics_version = "v1",
    last_evaluated = TRUE
  ))

  ws <- make_ws(dir, version = "v2")
  ws$add_evaluated_version("v2")

  meta <- arrow::read_parquet(metadata_ds_path(dir))
  expect_false(meta$last_evaluated[meta$stics_version == "v1"])
  expect_true(meta$last_evaluated[meta$stics_version == "v2"])
})

test_that(
  "add_evaluated_version updates existing version instead of duplicating",
  {
    dir <- withr::local_tempdir()
    write_metadata(dir, list(
      stics_version = c("v1", "v2"),
      last_evaluated = c(TRUE, FALSE)
    ))

    ws <- make_ws(dir, version = "v1")
    ws$add_evaluated_version("v1")

    meta <- arrow::read_parquet(metadata_ds_path(dir))
    expect_identical(nrow(meta), 2L)
    expect_true(meta$last_evaluated[meta$stics_version == "v1"])
    expect_false(meta$last_evaluated[meta$stics_version == "v2"])
  }
)

# ---- get_all_versions ----

test_that("get_all_versions returns all versions from metadata", {
  dir <- withr::local_tempdir()
  write_metadata(dir, list(
    stics_version = c("v1", "v2"),
    last_evaluated = c(FALSE, TRUE)
  ))

  ws <- make_ws(dir, version = "v2")
  expect_identical(ws$get_all_versions(), c("v1", "v2"))
})

test_that("get_all_versions returns NULL when no metadata exists", {
  dir <- withr::local_tempdir()
  ws <- make_ws(dir, version = "v1")
  expect_null(ws$get_all_versions())
})

# ---- get_species / get_species_usm ----

test_that("get_species returns sorted distinct species from obs dataset", {
  dir <- withr::local_tempdir()
  obs <- data.frame(
    situation = c("usm1", "usm2", "usm3"),
    species = c("wheat", "barley", "wheat"),
    version = "v1",
    Date = as.Date("2020-01-01"),
    stringsAsFactors = FALSE
  )
  write_parquet_ds(
    obs, obs_ds_path(dir), partitioning = c("version", "species")
  )

  ws <- make_ws(dir, version = "v1")
  expect_identical(ws$get_species(), c("barley", "wheat"))
})

test_that("get_species_usm returns USMs for a species", {
  dir <- withr::local_tempdir()
  obs <- data.frame(
    situation = c("usm1", "usm2", "usm3"),
    species = c("wheat", "wheat", "barley"),
    version = "v1",
    Date = as.Date("2020-01-01"),
    stringsAsFactors = FALSE
  )
  write_parquet_ds(
    obs, obs_ds_path(dir), partitioning = c("version", "species")
  )

  ws <- make_ws(dir, version = "v1")
  expect_setequal(ws$get_species_usm("wheat"), c("usm1", "usm2"))
})

test_that("get_species_usm filters by usms when provided", {
  dir <- withr::local_tempdir()
  obs <- data.frame(
    situation = c("usm1", "usm2", "usm3"),
    species = c("wheat", "wheat", "wheat"),
    version = "v1",
    Date = as.Date("2020-01-01"),
    stringsAsFactors = FALSE
  )
  write_parquet_ds(
    obs, obs_ds_path(dir), partitioning = c("version", "species")
  )

  ws <- make_ws(dir, version = "v1")
  expect_identical(ws$get_species_usm("wheat", usms = "usm1"), "usm1")
})

# ---- save_stats / get_stats ----

test_that("save_stats and get_stats round-trip correctly", {
  dir <- withr::local_tempdir()
  ws <- make_ws(dir, version = "v1")

  stats <- data.frame(variable = "LAI", RMSE = 0.5, stringsAsFactors = FALSE)
  ws$save_stats("wheat", stats)

  result <- ws$get_stats("wheat", collect = TRUE)
  expect_identical(result$variable, "LAI")
  expect_identical(result$version, "v1")
  expect_identical(result$species, "wheat")
})

test_that("get_stats returns NULL when no stats file exists", {
  dir <- withr::local_tempdir()
  ws <- make_ws(dir, version = "v1")
  expect_null(ws$get_stats("wheat"))
})

test_that("get_stats filters by version", {
  dir <- withr::local_tempdir()
  ws_v1 <- make_ws(dir, version = "v1")
  ws_v2 <- make_ws(dir, version = "v2")

  ws_v1$save_stats(
    "wheat",
    data.frame(variable = "LAI", RMSE = 0.5, stringsAsFactors = FALSE)
  )
  ws_v2$save_stats(
    "wheat",
    data.frame(variable = "LAI", RMSE = 1.0, stringsAsFactors = FALSE)
  )

  result_v1 <- ws_v1$get_stats("wheat", collect = TRUE)
  expect_identical(result_v1$RMSE, 0.5)
})

# ---- save_rmse_per_usm / get_rmse_per_usm ----

test_that("save_rmse_per_usm and get_rmse_per_usm round-trip correctly", {
  dir <- withr::local_tempdir()
  ws <- make_ws(dir, version = "v1")

  rmse <- data.frame(
    situation = "usm1", variable = "LAI", rRMSE = 0.1,
    stringsAsFactors = FALSE
  )
  ws$save_rmse_per_usm("wheat", rmse)

  result <- ws$get_rmse_per_usm("wheat", collect = TRUE)
  expect_identical(result$situation, "usm1")
  expect_identical(result$version, "v1")
})

test_that("get_rmse_per_usm returns NULL when no file exists", {
  dir <- withr::local_tempdir()
  ws <- make_ws(dir, version = "v1")
  expect_null(ws$get_rmse_per_usm("wheat"))
})

test_that("get_rmse_per_usm filters by usms", {
  dir <- withr::local_tempdir()
  ws <- make_ws(dir, version = "v1")

  rmse <- data.frame(
    situation = c("usm1", "usm2"),
    variable = "LAI",
    rRMSE = c(0.1, 0.9),
    stringsAsFactors = FALSE
  )
  ws$save_rmse_per_usm("wheat", rmse)

  result <- ws$get_rmse_per_usm("wheat", collect = TRUE, usms = "usm1")
  expect_identical(result$situation, "usm1")
})

test_that("get_rmse_per_usm excludes variables in var2exclude", {
  dir <- withr::local_tempdir()
  ws <- make_ws(dir, version = "v1")

  rmse <- data.frame(
    situation = "usm1", LAI = 0.1, MASEC = 0.5,
    stringsAsFactors = FALSE
  )
  ws$save_rmse_per_usm("wheat", rmse)

  result <- ws$get_rmse_per_usm("wheat", collect = TRUE, var2exclude = "MASEC")
  expect_false("MASEC" %in% names(result))
  expect_true("LAI" %in% names(result))
})
