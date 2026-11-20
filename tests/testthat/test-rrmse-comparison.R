# ---- Fixtures ----

make_stats <- function() {
  data.frame(
    group = c(
      "reference", "reference", "reference",
      "evaluated", "evaluated", "evaluated"
    ),
    situation = c("usm1", "usm1", "usm2", "usm1", "usm1", "usm2"),
    variable = c("LAI", "MASEC", "LAI", "LAI", "MASEC", "LAI"),
    rRMSE = c(0.10, 0.20, 0.15, 0.12, 0.18, 0.30),
    n_obs = c(20, 20, 20, 20, 20, 20),
    stringsAsFactors = FALSE
  )
}

make_comparison <- function(
  percentage = 10, species = "wheat",
  stats = make_stats()
) {
  rrmse_comparison(
    stats = stats,
    percentage = percentage,
    species = species
  )
}

# ---- rrmse_comparison ----

test_that("computes a comparison from stats", {
  cmp <- make_comparison()
  expect_gt(nrow(cmp), 0)
})

test_that("data is sorted descending by ratio", {
  ratios <- make_comparison()$rrmse_ratio
  expect_identical(ratios, sort(ratios, decreasing = TRUE))
})

test_that("computes ratio correctly", {
  stats <- data.frame(
    group = c("reference", "evaluated"),
    situation = c("usm1", "usm1"),
    variable = c("LAI", "LAI"),
    rRMSE = c(0.10, 0.15),
    n_obs = c(20, 20),
    stringsAsFactors = FALSE
  )
  cmp <- rrmse_comparison(stats, percentage = 10, species = "wheat")
  expect_identical(cmp$rrmse_ratio, 50)
})

test_that("filters out non-finite and NA rows", {
  stats <- data.frame(
    group = c(
      "reference", "reference", "reference",
      "evaluated", "evaluated", "evaluated"
    ),
    situation = c("usm1", "usm2", "usm3", "usm1", "usm2", "usm3"),
    variable = c("LAI", "LAI", NA_character_, "LAI", "LAI", NA_character_),
    rRMSE = c(0.10, Inf, 0.10, 0.12, 0.20, 0.15),
    n_obs = c(20, 20, 20, 20, 20, 20),
    stringsAsFactors = FALSE
  )
  cmp <- rrmse_comparison(stats, percentage = 10, species = "wheat")
  # only usm1/LAI survives
  expect_identical(nrow(cmp), 1L)
})

test_that("is empty when no situation matches", {
  # ref and eval with no matching situations
  stats <- data.frame(
    group = c("reference", "evaluated"),
    situation = c("usm1", "usm2"),
    variable = c("LAI", "LAI"),
    rRMSE = c(0.10, 0.12),
    n_obs = c(20, 20),
    stringsAsFactors = FALSE
  )
  cmp <- rrmse_comparison(stats, percentage = 10, species = "wheat")
  expect_identical(nrow(cmp), 0L)
})

test_that("returns expected columns", {
  expected_cols <- c(
    "situation", "variable", "rrmse_eval", "rrmse_ref", "rrmse_ratio",
    "n_obs", "species", "status"
  )
  expect_named(make_comparison(), expected_cols)
})

test_that("has no species column when species is not given", {
  cmp <- rrmse_comparison(make_stats(), percentage = 10)
  expect_false("species" %in% names(cmp))
})

# ---- add_rrmse_status ----

test_that("add_rrmse_status works on precomputed data", {
  data <- data.frame(
    species = "wheat",
    situation = c("usm1", "usm2"),
    variable = c("LAI", "MASEC"),
    rrmse_eval = c(0.12, 0.11),
    rrmse_ref = c(0.10, 0.10),
    rrmse_ratio = c(10.0, 20.0),
    n_obs = 20,
    stringsAsFactors = FALSE
  )
  cmp <- add_rrmse_status(data, percentage = 15)
  expect_identical(cmp$rrmse_ratio, c(20.0, 10.0))
  expect_identical(cmp$status, c("Critical", "Warning"))
})

# ---- rrmse_status_vars ----

test_that("Critical variables are at or above percentage threshold", {
  cmp <- make_comparison(percentage = 10)
  # LAI usm2: +100%, LAI usm1: +20% — both >= 10%
  expect_true("LAI" %in% rrmse_status_vars(cmp, "Critical"))
})

test_that("Critical does not include variables with n_obs < 10", {
  stats <- data.frame(
    group = c("reference", "evaluated"),
    situation = c("usm1", "usm1"),
    variable = c("LAI", "LAI"),
    rRMSE = c(0.15, 0.30),
    n_obs = c(9, 9),
    stringsAsFactors = FALSE
  )
  cmp <- rrmse_comparison(stats, percentage = 10, species = "wheat")
  expect_false("LAI" %in% rrmse_status_vars(cmp, "Critical"))
})

test_that("Warning variables are between 0 and percentage", {
  stats <- data.frame(
    group = c("reference", "evaluated"),
    situation = c("usm1", "usm1"),
    variable = c("LAI", "LAI"),
    rRMSE = c(0.10, 0.105),
    n_obs = c(20, 20),
    stringsAsFactors = FALSE
  )
  cmp <- rrmse_comparison(stats, percentage = 10, species = "wheat")
  # ratio = 5%, below threshold but above 0
  expect_true("LAI" %in% rrmse_status_vars(cmp, "Warning"))
})

test_that("Warning does not include variables with n_obs < 10", {
  stats <- data.frame(
    group = c("reference", "evaluated"),
    situation = c("usm1", "usm1"),
    variable = c("LAI", "LAI"),
    rRMSE = c(0.10, 0.105),
    n_obs = c(9, 9),
    stringsAsFactors = FALSE
  )
  cmp <- rrmse_comparison(stats, percentage = 10, species = "wheat")
  expect_false("LAI" %in% rrmse_status_vars(cmp, "Warning"))
})

test_that("Improved variables have a ratio <= 0", {
  cmp <- make_comparison(percentage = 10)
  # MASEC usm1: -10%
  expect_true("MASEC" %in% rrmse_status_vars(cmp, "Improved"))
})

test_that("Improved does not include variables with n_obs < 10", {
  stats <- data.frame(
    group = c("reference", "evaluated"),
    situation = c("usm1", "usm1"),
    variable = c("LAI", "LAI"),
    rRMSE = c(0.105, 0.10),
    n_obs = c(9, 9),
    stringsAsFactors = FALSE
  )
  cmp <- rrmse_comparison(stats, percentage = 10, species = "wheat")
  expect_false("LAI" %in% rrmse_status_vars(cmp, "Improved"))
})

test_that("variables with n_obs < 10 get the 'Not tested' status", {
  stats <- data.frame(
    group = c("reference", "reference", "evaluated", "evaluated"),
    situation = c("usm1", "usm1", "usm1", "usm1"),
    variable = c("LAI", "MASEC", "LAI", "MASEC"),
    rRMSE = c(0.10, 0.10, 0.30, 0.30),
    n_obs = c(9, 10, 9, 10),
    stringsAsFactors = FALSE
  )
  cmp <- rrmse_comparison(stats, percentage = 10, species = "wheat")
  expect_identical(cmp$status[cmp$variable == "LAI"], "Not tested")
  expect_identical(cmp$status[cmp$variable == "MASEC"], "Critical")
  expect_identical(rrmse_status_vars(cmp, "Not tested"), "LAI")
  expect_identical(rrmse_status_vars(cmp, "Critical"), "MASEC")
})

# ---- log_rrmse_comparison ----

test_that("log_rrmse_comparison lists variables by status", {
  cmp <- make_comparison(percentage = 10)
  out <- paste(
    cli::cli_fmt(log_rrmse_comparison(cmp, percentage = 10)),
    collapse = "\n"
  )
  # LAI usm1 (+20%) and usm2 (+100%) are critical, MASEC (-10%) improved
  expect_match(out, "wheat")
  expect_match(out, "2 critical")
  expect_match(out, "0 warning")
  expect_match(out, "1 improved")
  expect_match(out, "MASEC")
})

test_that("log_rrmse_comparison prints nothing for an empty comparison", {
  cmp <- make_comparison()[0, ]
  expect_silent(log_rrmse_comparison(cmp, percentage = 10))
})

# ---- deteriorated_usm_comparison ----

test_that("deteriorated_usm_comparison does not apply the n_obs criterion", {
  stats <- data.frame(
    group = c("reference", "evaluated"),
    situation = c("usm1", "usm1"),
    variable = c("LAI", "LAI"),
    rRMSE = c(0.10, 0.30),
    n_obs = c(3, 3),
    stringsAsFactors = FALSE
  )
  cmp <- deteriorated_usm_comparison(stats, percentage = 10, species = "wheat")
  expect_identical(cmp$status, "Critical")
})

test_that("deteriorated_usm_comparison keeps only rows with ratio > 0", {
  cmp <- deteriorated_usm_comparison(
    make_stats(), percentage = 10, species = "wheat"
  )
  expect_true(all(cmp$rrmse_ratio > 0))
  # MASEC ratio is -10%, should be excluded
  expect_false("MASEC" %in% cmp$variable)
})

test_that("deteriorated_usm_comparison is sorted descending by ratio", {
  cmp <- deteriorated_usm_comparison(
    make_stats(), percentage = 10, species = "wheat"
  )
  expect_identical(cmp$rrmse_ratio, sort(cmp$rrmse_ratio, decreasing = TRUE))
})

test_that("deteriorated_usm_comparison is empty when all ratios improved", {
  stats <- data.frame(
    group = c("reference", "evaluated"),
    situation = c("usm1", "usm1"),
    variable = c("LAI", "LAI"),
    rRMSE = c(0.20, 0.10),
    n_obs = c(20, 20),
    stringsAsFactors = FALSE
  )
  cmp <- deteriorated_usm_comparison(stats, percentage = 10, species = "wheat")
  expect_identical(nrow(cmp), 0L)
})
