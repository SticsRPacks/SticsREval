# ===========================================================================
# Tests: render_report
# ===========================================================================

test_that("render_report errors when output_dir does not exist", {
  expect_error(
    render_report(file.path(tempdir(), "does_not_exist_xyz"), open = FALSE),
    regexp = "does not exist"
  )
})

test_that("render_report errors when output_dir has no evaluation results", {
  tmp <- withr::local_tempdir()
  expect_error(
    render_report(tmp, open = FALSE),
    regexp = "No evaluation results found"
  )
})

test_that("render_report renders an index.html from evaluation exports", {
  testthat::skip_if_not_installed("quarto")
  testthat::skip_if_not_installed("DT")
  testthat::skip_if(
    is.null(quarto::quarto_path()), "Quarto CLI not installed"
  )

  tmp <- withr::local_tempdir()
  csv_dir <- file.path(tmp, "csv")
  dir.create(csv_dir)
  write.csv(
    data.frame(variable = "lai", RMSE = 0.5, stringsAsFactors = FALSE),
    file.path(csv_dir, "global_stats.csv"),
    row.names = FALSE
  )
  write.csv(
    data.frame(
      species = c("wheat", "maize"), variable = "lai", RMSE = 0.5,
      stringsAsFactors = FALSE
    ),
    file.path(csv_dir, "species_stats.csv"),
    row.names = FALSE
  )

  html_path <- render_report(tmp, open = FALSE)

  expect_true(file.exists(html_path))
  expect_identical(html_path, file.path(tmp, "index.html"))
  expect_gt(file.info(html_path)$size, 0)

  global_path <- file.path(tmp, "global.html")
  expect_true(file.exists(global_path))
  expect_gt(file.info(global_path)$size, 0)

  species_path <- file.path(tmp, "species", "wheat.html")
  expect_true(file.exists(species_path))
  expect_gt(file.info(species_path)$size, 0)
  expect_false(file.exists(file.path(tmp, "plots", "wheat", "species.html")))
  expect_false(
    file.exists(file.path(tmp, "species", "wheat", "species.html"))
  )
})
