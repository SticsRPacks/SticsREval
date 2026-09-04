# ---- Helpers ----

make_ws <- function(dir) {
  EvalWorkspace$new(dir)
}

write_csv_ds <- function(data, path, partitioning = NULL) {
  EvalDataWriter$new(dirname(path))$write_dataset(
    data, path = path, partitioning = partitioning
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
  write_csv_ds(
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
  write_csv_ds(
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

test_that("get_species_situations filters by usms when provided", {
  dir <- withr::local_tempdir()
  species_usm <- data.frame(
    species = c("wheat", "wheat", "barley"),
    situation = c("usm1", "usm2", "usm3"),
    stringsAsFactors = FALSE
  )
  write_csv_ds(
    species_usm, species_usm_ds_path(dir), partitioning = "species"
  )

  ws <- make_ws(dir)
  expect_identical(
    ws$get_species_situations("wheat", usms = "usm1"),
    data.frame(species = "wheat", situation = "usm1", stringsAsFactors = FALSE)
  )
})
