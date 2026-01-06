library(testthat)

test_that("compare_rmse classifies variables correctly", {
  ref_stats <- data.frame(
    variable = c("A", "B", "C"),
    rRMSE = c("10,0", "20,0", "30,0")
  )
  new_stats <- data.frame(
    variable = c("A", "B", "C"),
    rRMSE = c("11,0", "20,5", "25,0")
  )

  result <- compare_rmse(ref_stats, new_stats)

  expect_s4_class(result, "Comparison")

  expect_equal(result@critical, "A")
  expect_equal(result@warning, "B")
  expect_equal(result@improved, "C")
})

test_that("compare_rmse returns only improved when all new RMSE are smaller", {
  ref_stats <- data.frame(
    variable = c("X", "Y"),
    rRMSE = c("50,0", "40,0")
  )
  new_stats <- data.frame(
    variable = c("X", "Y"),
    rRMSE = c("45,0", "35,0")
  )

  result <- compare_rmse(ref_stats, new_stats)

  expect_equal(result@critical, character(0))
  expect_equal(result@warning, character(0))
  expect_equal(result@improved, c("X", "Y"))
})

test_that("compare_rmse returns empty slots when nothing passes numeric filters", {
  ref_stats <- data.frame(
    variable = c("A", "B"),
    rRMSE = c("NA", "abc")
  )
  new_stats <- data.frame(
    variable = c("A", "B"),
    rRMSE = c("NA", "xyz")
  )

  result <- compare_rmse(ref_stats, new_stats)

  expect_equal(result@critical, character(0))
  expect_equal(result@warning, character(0))
  expect_equal(result@improved, character(0))
})

test_that("compare_rmse handles no join matches properly", {
  ref_stats <- data.frame(
    variable = c("A", "B"),
    rRMSE = c("10,0", "20,0")
  )
  new_stats <- data.frame(
    variable = c("C", "D"),
    rRMSE = c("15,0", "25,0")
  )

  result <- compare_rmse(ref_stats, new_stats)

  expect_equal(result@critical, character(0))
  expect_equal(result@warning, character(0))
  expect_equal(result@improved, character(0))
})

test_that("compare_rmse converts comma decimal to numeric", {
  ref_stats <- data.frame(
    variable = "Z",
    rRMSE = "10,5"
  )
  new_stats <- data.frame(
    variable = "Z",
    rRMSE = "11,0"
  )

  result <- compare_rmse(ref_stats, new_stats)

  expect_equal(result@warning, "Z")
})

test_that("compare_rmse returns an invisible value", {
  ref_stats <- data.frame(variable = "A", rRMSE = "10,0")
  new_stats <- data.frame(variable = "A", rRMSE = "9,0")

  res <- expect_invisible(compare_rmse(ref_stats, new_stats))

  expect_s4_class(res, "Comparison")
})

test_that("compare_rmse handles negative RMSE values correctly", {
  ref_stats <- data.frame(
    variable = c("A", "B", "C"),
    rRMSE = c("-10,0", "-20,0", "-30,0")
  )
  new_stats <- data.frame(
    variable = c("A", "B", "C"),
    rRMSE = c("-9,0",  "-25,0", "-31,0")
  )

  result <- compare_rmse(ref_stats, new_stats)

  expect_equal(result@critical, "B")
  expect_equal(result@warning, "C")
  expect_equal(result@improved, "A")
})


test_that("compare_rmse treats negative values symmetrically for improvements", {
  ref_stats <- data.frame(
    variable = c("X", "Y"),
    rRMSE = c("-55,0", "-42,0")
  )
  new_stats <- data.frame(
    variable = c("X", "Y"),
    rRMSE = c("-50,0", "-40,0")
  )

  result <- compare_rmse(ref_stats, new_stats)

  expect_equal(result@critical, character(0))
  expect_equal(result@warning, character(0))
  expect_equal(result@improved, c("X", "Y"))
})
