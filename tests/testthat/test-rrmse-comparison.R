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
  RRmseComparison$new(
    percentage = percentage,
    species = species,
    stats = stats
  )
}

# ---- initialize ----

test_that("initializes from species + ref_stats + eval_stats", {
  cmp <- make_comparison()
  expect_false(cmp$is_empty)
})

test_that("initializes from precomputed data", {
  data <- data.frame(
    species = "wheat",
    situation = "usm1",
    variable = "LAI",
    rrmse_eval = 0.12,
    rrmse_ref = 0.10,
    rrmse_ratio = 20.0,
    stringsAsFactors = FALSE
  )
  cmp <- RRmseComparison$new(percentage = 10, data = data)
  expect_equal(cmp$get_data()$rrmse_ratio, 20.0)
})

test_that("errors when neither data nor stats provided", { # nolint: nonportable_path_linter
  expect_error(
    RRmseComparison$new(percentage = 10),
    "`data`, or `stats` must be defined" # nolint: nonportable_path_linter
  )
})

test_that("data is sorted descending by ratio", {
  cmp <- make_comparison()
  ratios <- cmp$get_data()$rrmse_ratio
  expect_identical(ratios, sort(ratios, decreasing = TRUE))
})

# ---- ratio computation ----

test_that("computes ratio correctly", {
  stats <- data.frame(
    group = c("reference", "evaluated"),
    situation = c("usm1", "usm1"),
    variable = c("LAI", "LAI"),
    rRMSE = c(0.10, 0.15),
    n_obs = c(20, 20),
    stringsAsFactors = FALSE
  )
  cmp <- RRmseComparison$new(percentage = 10, species = "wheat", stats = stats)
  expect_identical(cmp$get_data()$rrmse_ratio, 50)
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
  cmp <- RRmseComparison$new(percentage = 10, species = "wheat", stats = stats)
  # only usm1/LAI survives
  expect_identical(nrow(cmp$get_data()), 1L)
})

# ---- active bindings ----

test_that("critical_vars returns variables at or above percentage threshold", {
  cmp <- make_comparison(percentage = 10)
  # LAI usm2: +100%, LAI usm1: +20% — both >= 10%
  expect_true("LAI" %in% cmp$critical_vars)
})

test_that("critical_vars does not return variables with n_obs < 10", {
  stats <- data.frame(
    group = c("reference", "evaluated"),
    situation = c("usm1", "usm1"),
    variable = c("LAI", "LAI"),
    rRMSE = c(0.15, 0.30),
    n_obs = c(9, 9),
    stringsAsFactors = FALSE
  )
  cmp <- RRmseComparison$new(percentage = 10, species = "wheat", stats = stats)
  expect_false("LAI" %in% cmp$critical_vars)
})

test_that("warning_vars returns variables between 0 and percentage", {
  stats <- data.frame(
    group = c("reference", "evaluated"),
    situation = c("usm1", "usm1"),
    variable = c("LAI", "LAI"),
    rRMSE = c(0.10, 0.105),
    n_obs = c(20, 20),
    stringsAsFactors = FALSE
  )
  cmp <- RRmseComparison$new(percentage = 10, species = "wheat", stats = stats)
  # ratio = 5%, below threshold but above 0
  expect_true("LAI" %in% cmp$warning_vars)
})

test_that("warning_vars does not return variables with n_obs < 10", {
  stats <- data.frame(
    group = c("reference", "evaluated"),
    situation = c("usm1", "usm1"),
    variable = c("LAI", "LAI"),
    rRMSE = c(0.10, 0.105),
    n_obs = c(9, 9),
    stringsAsFactors = FALSE
  )
  cmp <- RRmseComparison$new(percentage = 10, species = "wheat", stats = stats)
  expect_false("LAI" %in% cmp$warning_vars)
})

test_that("improved_vars returns variables with ratio <= 0", {
  cmp <- make_comparison(percentage = 10)
  # MASEC usm1: -10%
  expect_true("MASEC" %in% cmp$improved_vars)
})

test_that("improved_vars does not return variables with n_obs < 10", {
  stats <- data.frame(
    group = c("reference", "evaluated"),
    situation = c("usm1", "usm1"),
    variable = c("LAI", "LAI"),
    rRMSE = c(0.105, 0.10),
    n_obs = c(9, 9),
    stringsAsFactors = FALSE
  )
  cmp <- RRmseComparison$new(percentage = 10, species = "wheat", stats = stats)
  expect_false("LAI" %in% cmp$improved_vars)
})

test_that("is_empty returns TRUE when no rows", {
  # ref and eval with no matching situations
  stats <- data.frame(
    group = c("reference", "evaluated"),
    situation = c("usm1", "usm2"),
    variable = c("LAI", "LAI"),
    rRMSE = c(0.10, 0.12),
    n_obs = c(20, 20),
    stringsAsFactors = FALSE
  )
  cmp <- RRmseComparison$new(percentage = 10, species = "wheat", stats = stats)
  expect_true(cmp$is_empty)
})

test_that("is_empty returns FALSE when rows exist", {
  cmp <- make_comparison()
  expect_false(cmp$is_empty)
})

# ---- get_data ----

test_that("get_data returns expected columns", {
  cmp <- make_comparison()
  expected_cols <- c(
    "situation", "variable", "rrmse_eval", "rrmse_ref", "rrmse_ratio",
    "n_obs", "species", "status"
  )
  expect_named(cmp$get_data(), expected_cols)
})

# ---- DeterioratedUSMComparison ----

test_that(
  "DeterioratedUSMComparison keeps only deteriorated rows (ratio > 0)",
  {
    cmp <- DeterioratedUSMComparison$new(
      species = "wheat",
      stats = make_stats(),
      percentage = 10
    )
    expect_true(all(cmp$get_data()$rrmse_ratio > 0))
  }
)

test_that("DeterioratedUSMComparison excludes improved variables", {
  cmp <- DeterioratedUSMComparison$new(
    species = "wheat",
    stats = make_stats(),
    percentage = 10
  )
  # MASEC ratio is -10%, should be excluded
  expect_false("MASEC" %in% cmp$get_data()$variable)
})

test_that("DeterioratedUSMComparison data is sorted descending by ratio", {
  cmp <- DeterioratedUSMComparison$new(
    species = "wheat",
    stats = make_stats(),
    percentage = 10
  )
  ratios <- cmp$get_data()$rrmse_ratio
  expect_identical(ratios, sort(ratios, decreasing = TRUE))
})

test_that("DeterioratedUSMComparison is empty when all ratios are improved", {
  stats <- data.frame(
    group = c("reference", "evaluated"),
    situation = c("usm1", "usm1"),
    variable = c("LAI", "LAI"),
    rRMSE = c(0.20, 0.10),
    n_obs = c(20, 20),
    stringsAsFactors = FALSE
  )
  cmp <- DeterioratedUSMComparison$new(
    species = "wheat", stats = stats, percentage = 10
  )
  expect_true(cmp$is_empty)
})
