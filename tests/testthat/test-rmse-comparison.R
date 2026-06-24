# ---- Fixtures ----

make_ref_stats <- function() {
  data.frame(
    situation = c("usm1", "usm1", "usm2"),
    variable = c("LAI", "MASEC", "LAI"),
    rRMSE = c(0.10, 0.20, 0.15),
    stringsAsFactors = FALSE
  )
}

make_eval_stats <- function() {
  data.frame(
    situation = c("usm1", "usm1", "usm2"),
    variable = c("LAI", "MASEC", "LAI"),
    rRMSE = c(0.12, 0.18, 0.30),
    stringsAsFactors = FALSE
  )
}

make_comparison <- function(
  percentage = 10, species = "wheat",
  ref = make_ref_stats(), eval = make_eval_stats()
) {
  RRmseComparison$new(
    percentage = percentage,
    species = species,
    ref_stats = ref,
    eval_stats = eval
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
    rrmse_new = 0.12,
    rrmse_ref = 0.10,
    ratio = 20.0,
    stringsAsFactors = FALSE
  )
  cmp <- RRmseComparison$new(percentage = 10, data = data)
  expect_equal(cmp$get_data()$ratio, 20.0)
})

test_that("errors when neither data nor species/ref/eval provided", { # nolint: nonportable_path_linter
  expect_error(
    RRmseComparison$new(percentage = 10),
    "`data`, or `ref_stats` \\+ `eval_stats` must be defined" # nolint: nonportable_path_linter
  )
})

test_that("data is sorted descending by ratio", {
  cmp <- make_comparison()
  ratios <- cmp$get_data()$ratio
  expect_identical(ratios, sort(ratios, decreasing = TRUE))
})

# ---- ratio computation ----

test_that("computes ratio correctly", {
  ref <- data.frame(
    situation = "usm1", variable = "LAI", rRMSE = 0.10,
    stringsAsFactors = FALSE
  )
  eval <- data.frame(
    situation = "usm1", variable = "LAI", rRMSE = 0.15,
    stringsAsFactors = FALSE
  )

  cmp <- RRmseComparison$new(
    percentage = 10, species = "wheat",
    ref_stats = ref, eval_stats = eval
  )
  expect_identical(cmp$get_data()$ratio, 50)
})

test_that("filters out non-finite and NA rows", {
  ref <- data.frame(
    situation = c("usm1", "usm2", "usm3"),
    variable = c("LAI", "LAI", NA_character_),
    rRMSE = c(0.10, Inf, 0.10),
    stringsAsFactors = FALSE
  )
  eval <- data.frame(
    situation = c("usm1", "usm2", "usm3"),
    variable = c("LAI", "LAI", NA_character_),
    rRMSE = c(0.12, 0.20, 0.15),
    stringsAsFactors = FALSE
  )
  cmp <- RRmseComparison$new(
    percentage = 10, species = "wheat",
    ref_stats = ref, eval_stats = eval
  )
  # only usm1/LAI survives
  expect_identical(nrow(cmp$get_data()), 1L)
})

# ---- active bindings ----

test_that("critical_vars returns variables at or above percentage threshold", {
  cmp <- make_comparison(percentage = 10)
  # LAI usm2: +100%, LAI usm1: +20% — both >= 10%
  expect_true("LAI" %in% cmp$critical_vars)
})

test_that("warning_vars returns variables between 0 and percentage", {
  ref <- data.frame(
    situation = "usm1", variable = "LAI", rRMSE = 0.10,
    stringsAsFactors = FALSE
  )
  eval <- data.frame(
    situation = "usm1", variable = "LAI", rRMSE = 0.105,
    stringsAsFactors = FALSE
  )
  cmp <- RRmseComparison$new(
    percentage = 10, species = "wheat",
    ref_stats = ref, eval_stats = eval
  )
  # ratio = 5%, below threshold but above 0
  expect_true("LAI" %in% cmp$warning_vars)
})

test_that("improved_vars returns variables with ratio <= 0", {
  cmp <- make_comparison(percentage = 10)
  # MASEC usm1: -10%
  expect_true("MASEC" %in% cmp$improved_vars)
})

test_that("is_empty returns TRUE when no rows", {
  # ref and eval with no matching situations
  ref <- data.frame(
    situation = "usm1", variable = "LAI", rRMSE = 0.10,
    stringsAsFactors = FALSE
  )
  eval <- data.frame(
    situation = "usm2", variable = "LAI", rRMSE = 0.12,
    stringsAsFactors = FALSE
  )
  cmp <- RRmseComparison$new(
    percentage = 10, species = "wheat",
    ref_stats = ref, eval_stats = eval
  )
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
    "situation", "variable", "rrmse_new", "rrmse_ref", "ratio", "species"
  )
  expect_named(cmp$get_data(), expected_cols)
})

# ---- DeterioratedUSMComparison ----

test_that(
  "DeterioratedUSMComparison keeps only deteriorated rows (ratio > 0)",
  {
    cmp <- DeterioratedUSMComparison$new(
      species = "wheat",
      ref_stats = make_ref_stats(),
      eval_stats = make_eval_stats(),
      percentage = 10
    )
    expect_true(all(cmp$get_data()$ratio > 0))
  }
)

test_that("DeterioratedUSMComparison excludes improved variables", {
  cmp <- DeterioratedUSMComparison$new(
    species = "wheat",
    ref_stats = make_ref_stats(),
    eval_stats = make_eval_stats(),
    percentage = 10
  )
  # MASEC ratio is -10%, should be excluded
  expect_false("MASEC" %in% cmp$get_data()$variable)
})

test_that("DeterioratedUSMComparison data is sorted descending by ratio", {
  cmp <- DeterioratedUSMComparison$new(
    species = "wheat",
    ref_stats = make_ref_stats(),
    eval_stats = make_eval_stats(),
    percentage = 10
  )
  ratios <- cmp$get_data()$ratio
  expect_identical(ratios, sort(ratios, decreasing = TRUE))
})

test_that("DeterioratedUSMComparison is empty when all ratios are improved", {
  ref <- data.frame(
    situation = "usm1", variable = "LAI", rRMSE = 0.20,
    stringsAsFactors = FALSE
  )
  eval <- data.frame(
    situation = "usm1", variable = "LAI", rRMSE = 0.10,
    stringsAsFactors = FALSE
  )

  cmp <- DeterioratedUSMComparison$new(
    species = "wheat", ref_stats = ref,
    eval_stats = eval, percentage = 10
  )
  expect_true(cmp$is_empty)
})
