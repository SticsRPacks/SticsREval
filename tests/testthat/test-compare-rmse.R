library(testthat)

test_that("compare_rmse returns expected columns", {

  ref_stats <- tibble(
    variable = c("a", "b"),
    rRMSE = c("1,0", "2,0")
  )

  new_stats <- tibble(
    variable = c("a", "b"),
    rRMSE = c("2,0", "4,0")
  )

  res <- compare_rmse("vig", ref_stats, new_stats)

  expect_named(
    res,
    c("species", "variable", "rmse_new", "rmse_ref", "ratio")
  )
})

test_that("compare_rmse computes correct ratio", {

  ref_stats <- tibble(
    variable = "a",
    rRMSE = "2,0"
  )

  new_stats <- tibble(
    variable = "a",
    rRMSE = "4,0"
  )

  res <- compare_rmse("vig", ref_stats, new_stats)

  expect_equal(res$ratio, 2)
})

test_that("compare_rmse converts comma decimals to numeric", {

  ref_stats <- tibble(
    variable = "a",
    rRMSE = "1,5"
  )

  new_stats <- tibble(
    variable = "a",
    rRMSE = "3,0"
  )

  res <- compare_rmse("vig", ref_stats, new_stats)

  expect_type(res$rmse_new, "double")
  expect_type(res$rmse_ref, "double")
  expect_equal(res$rmse_new, 3.0)
  expect_equal(res$rmse_ref, 1.5)
})

test_that("compare_rmse removes rows with NA", {

  ref_stats <- tibble(
    variable = c("a", NA),
    rRMSE = c("1,0", "2,0")
  )

  new_stats <- tibble(
    variable = c("a", "b"),
    rRMSE = c("2,0", NA)
  )

  res <- compare_rmse("vig", ref_stats, new_stats)

  expect_equal(nrow(res), 1)
  expect_equal(res$variable, "a")
})

test_that("compare_rmse performs left join on variable", {

  ref_stats <- tibble(
    variable = "a",
    rRMSE = "1,0"
  )

  new_stats <- tibble(
    variable = c("a", "b"),
    rRMSE = c("2,0", "3,0")
  )

  res <- compare_rmse("vig", ref_stats, new_stats)

  expect_equal(res$variable, "a")
})
