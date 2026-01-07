library(testthat)

test_that("is_critical detects critical ratios", {

  ratio <- c(1.1, 1.2, 1.3)
  pct <- 0.2

  res <- is_critical(ratio, percentage = pct)

  expect_equal(res, c(FALSE, TRUE, TRUE))
})

test_that("is_critical is TRUE at threshold", {

  pct <- 0.2
  ratio <- 1 + pct

  expect_true(is_critical(ratio, percentage = pct))
})

test_that("is_critical returns FALSE for NA", {

  ratio <- c(NA, 1.3)
  pct <- 0.2

  res <- is_critical(ratio, percentage = pct)

  expect_equal(res, c(FALSE, TRUE))
})

test_that("is_warning detects warning ratios", {

  pct <- 0.2
  ratio <- c(1.01, 1.1, 1.21)

  res <- is_warning(ratio, percentage = pct)

  expect_equal(res, c(TRUE, TRUE, FALSE))
})

test_that("is_warning is FALSE at bounds", {

  pct <- 0.2

  expect_false(is_warning(1, percentage = pct))
  expect_false(is_warning(1 + pct, percentage = pct))
})

test_that("is_warning returns FALSE for NA", {

  ratio <- c(NA, 1.1)
  pct <- 0.2

  res <- is_warning(ratio, percentage = pct)

  expect_equal(res, c(FALSE, TRUE))
})

test_that("is_improved detects improved ratios", {

  ratio <- c(0.8, 1, 1.1)

  res <- is_improved(ratio)

  expect_equal(res, c(TRUE, TRUE, FALSE))
})

test_that("is_improved returns FALSE for NA", {

  ratio <- c(NA, 0.9)

  res <- is_improved(ratio)

  expect_equal(res, c(FALSE, TRUE))
})

test_that("status functions are vectorized", {

  ratio <- c(0.9, 1.05, 1.3)
  pct <- 0.2

  expect_length(is_critical(ratio, pct), length(ratio))
  expect_length(is_warning(ratio, pct), length(ratio))
  expect_length(is_improved(ratio), length(ratio))
})

test_that("status functions are mutually exclusive", {

  ratio <- c(0.8, 1, 1.05, 1.25)
  pct <- 0.2

  crit <- is_critical(ratio, pct)
  warn <- is_warning(ratio, pct)
  impr <- is_improved(ratio)

  expect_true(all(!(crit & warn)))
  expect_true(all(!(crit & impr)))
  expect_true(all(!(warn & impr)))
})

test_that("each ratio falls into exactly one category", {

  ratio <- c(0.9, 1, 1.1, 1.3)
  pct <- 0.2

  total <- is_critical(ratio, pct) +
    is_warning(ratio, pct) +
    is_improved(ratio)

  expect_equal(total, rep(1, length(ratio)))
})
