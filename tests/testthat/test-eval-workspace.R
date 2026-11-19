# ---- Helpers ----

make_ws <- function(dir) {
  EvalWorkspace$new(dir)
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
})

# ---- get_species / get_species_situations ----

test_that("get_species returns sorted distinct species from obs dataset", {
  dir <- withr::local_tempdir()
  species_usm <- data.frame(
    situation = c("usm1", "usm2", "usm3"),
    species = c("wheat", "wheat", "barley"),
    stringsAsFactors = FALSE
  )
  write_parquet_ds(
    species_usm, species_usm_ds_path(dir), partitioning = "species"
  )

  ws <- make_ws(dir)
  expect_identical(ws$get_species(), c("barley", "wheat"))
})

test_that("get_species_situations returns USMs for a species", {
  dir <- withr::local_tempdir()
  species_usm <- data.frame(
    situation = c("usm1", "usm2", "usm3"),
    species = c("wheat", "wheat", "barley"),
    stringsAsFactors = FALSE
  )
  write_parquet_ds(
    species_usm, species_usm_ds_path(dir), partitioning = "species"
  )

  ws <- make_ws(dir)
  expect_identical(
    ws$get_species_situations("wheat"),
    data.frame(
      species = c("wheat", "wheat"),
      situation = c("usm1", "usm2"),
      stringsAsFactors = FALSE
    )
  )
})

# ---- remove_init_obs ----

test_that("remove_init_obs blanks init-date values and keeps all rows", {
  dir <- withr::local_tempdir()
  d1 <- as.POSIXct("2020-01-01", tz = "UTC")
  d2 <- as.POSIXct("2020-02-01", tz = "UTC")

  sim <- data.frame(
    situation = c("usm1", "usm1", "usm2", "usm2"),
    Date = c(d1, d2, d1, d2),
    version = "evaluated",
    species = c("wheat", "wheat", "barley", "barley"),
    stringsAsFactors = FALSE
  )
  write_parquet_ds(sim, sim_ds_path(dir), c("version", "species"))

  obs <- data.frame(
    situation = c("usm1", "usm1", "usm2", "usm2"),
    Date = c(d1, d2, d1, d2),
    HR_1 = c(1L, 2L, 3L, 4L),
    resmes = c(10, 20, 30, 40),
    lai_n = c(0.1, 0.2, 0.3, 0.4),
    species = c("wheat", "wheat", "barley", "barley"),
    stringsAsFactors = FALSE
  )
  write_parquet_ds(obs, obs_ds_path(dir), "species")

  make_ws(dir)$remove_init_obs()

  res <- arrow::open_dataset(obs_ds_path(dir)) |>
    dplyr::collect() |>
    dplyr::arrange(.data$situation, .data$Date)

  expect_identical(nrow(res), 4L)
  expect_identical(res$HR_1, c(NA, 2L, NA, 4L))
  expect_identical(res$resmes, c(NA, 20, NA, 40))
  expect_identical(res$lai_n, c(0.1, 0.2, 0.3, 0.4))
})

test_that("get_species_situations filters by usms when provided", {
  dir <- withr::local_tempdir()
  species_usm <- data.frame(
    species = c("wheat", "wheat", "barley"),
    situation = c("usm1", "usm2", "usm3"),
    stringsAsFactors = FALSE
  )
  write_parquet_ds(
    species_usm, species_usm_ds_path(dir), partitioning = "species"
  )

  ws <- make_ws(dir)
  expect_identical(
    ws$get_species_situations("wheat", usms = "usm1"),
    data.frame(species = "wheat", situation = "usm1", stringsAsFactors = FALSE)
  )
})
