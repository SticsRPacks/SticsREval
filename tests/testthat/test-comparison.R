# ===========================================================================
# Helpers
# ===========================================================================

make_stats <- function(
  situations = c("usm1", "usm2"),
  variables = c("LAI", "MASEC"),
  rrmse = c(0.3, 0.5, 0.4, 0.6)
) {
  expand.grid(
    situation = situations,
    variable = variables,
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(rRMSE = rrmse)
}

make_comparison_df <- function() {
  data.frame(
    species = "wheat",
    situation = c("usm1", "usm2", "usm3", "usm4"),
    variable = c("LAI", "MASEC", "ZRAC", "SLAi"),
    rmse_new = c(0.6, 0.8, 0.3, 0.5),
    rmse_ref = c(0.5, 1.0, 0.4, 0.5),
    ratio = c(20.0, -20.0, -25.0, 5.0),
    stringsAsFactors = FALSE
  )
}

# ===========================================================================
# Tests: is_critical / is_warning / is_improved # nolint
# ===========================================================================

test_that("is_critical returns TRUE when ratio >= percentage", {
  expect_true(is_critical(20, 10))
  expect_true(is_critical(10, 10))
})

test_that("is_critical returns FALSE when ratio < percentage", {
  expect_false(is_critical(9.9, 10))
  expect_false(is_critical(-5, 10))
})

test_that("is_critical returns FALSE for NA values", {
  expect_false(is_critical(NA, 10))
})

test_that("is_critical works on vectors", {
  result <- is_critical(c(15, 5, NA, -1), 10)
  expect_identical(result, c(TRUE, FALSE, FALSE, FALSE))
})

test_that("is_warning returns TRUE when 0 < ratio < percentage", {
  expect_true(is_warning(5, 10))
  expect_true(is_warning(0.1, 10))
})

test_that("is_warning returns FALSE when ratio >= percentage", {
  expect_false(is_warning(10, 10))
  expect_false(is_warning(15, 10))
})

test_that("is_warning returns FALSE when ratio <= 0", {
  expect_false(is_warning(0, 10))
  expect_false(is_warning(-5, 10))
})

test_that("is_warning returns FALSE for NA values", {
  expect_false(is_warning(NA, 10))
})

test_that("is_improved returns TRUE when ratio <= 0", {
  expect_true(is_improved(0))
  expect_true(is_improved(-5))
})

test_that("is_improved returns FALSE when ratio > 0", {
  expect_false(is_improved(0.1))
  expect_false(is_improved(10))
})

test_that("is_improved returns FALSE for NA values", {
  expect_false(is_improved(NA))
})

# ===========================================================================
# Tests: get_crit_vars / get_warn_vars / get_improved_vars # nolint
# ===========================================================================

test_that("get_crit_vars returns variables with ratio >= percentage", {
  df <- make_comparison_df()
  result <- get_crit_vars(df, 10)
  expect_identical(result, "LAI")
})

test_that("get_crit_vars returns empty vector when none critical", {
  df <- make_comparison_df()
  result <- get_crit_vars(df, 50)
  expect_length(result, 0)
})

test_that("get_warn_vars returns variables with 0 < ratio < percentage", {
  df <- make_comparison_df()
  result <- get_warn_vars(df, 10)
  expect_identical(result, "SLAi")
})

test_that("get_warn_vars returns empty vector when none warning", {
  df <- make_comparison_df()
  result <- get_warn_vars(df, 3)
  expect_length(result, 0)
})

test_that("get_improved_vars returns variables with ratio <= 0", {
  df <- make_comparison_df()
  result <- get_improved_vars(df)
  expect_setequal(result, c("MASEC", "ZRAC"))
})

test_that("get_improved_vars returns empty vector when none improved", {
  df <- make_comparison_df() |>
    dplyr::mutate(ratio = c(10, 20, 30, 40))
  result <- get_improved_vars(df)
  expect_length(result, 0)
})

# ===========================================================================
# Tests: compare_rmse
# ===========================================================================

test_that("compare_rmse returns a data frame with expected columns", {
  ref <- make_stats(rrmse = c(0.3, 0.5, 0.4, 0.6))
  new <- make_stats(rrmse = c(0.4, 0.6, 0.3, 0.7))

  result <- compare_rmse("wheat", ref, new)

  expect_s3_class(result, "data.frame")
  expect_true(all(
    c("species", "situation", "variable", "rmse_new", "rmse_ref", "ratio")
    %in% names(result)
  ))
})

test_that("compare_rmse computes ratio correctly", {
  ref <- data.frame(
    situation = "usm1",
    variable = "LAI",
    rRMSE = 0.5,
    stringsAsFactors = FALSE
  )
  new <- data.frame(
    situation = "usm1",
    variable = "LAI",
    rRMSE = 0.6,
    stringsAsFactors = FALSE
  )

  result <- compare_rmse("wheat", ref, new)
  expected_ratio <- round((0.6 - 0.5) / 0.5 * 100, 2)
  expect_identical(result$ratio, expected_ratio)
})

test_that("compare_rmse filters out non-finite ratios", {
  ref <- data.frame(
    situation = "usm1",
    variable = "LAI",
    rRMSE = 0,
    stringsAsFactors = FALSE
  )
  new <- data.frame(
    situation = "usm1",
    variable = "LAI",
    rRMSE = 0.5,
    stringsAsFactors = FALSE
  )

  result <- compare_rmse("wheat", ref, new)
  expect_identical(nrow(result), 0)
})

test_that("compare_rmse filters out rows with NA variable", {
  ref <- data.frame(
    situation = "usm1",
    variable = NA_character_,
    rRMSE = 0.5,
    stringsAsFactors = FALSE
  )
  new <- data.frame(
    situation = "usm1",
    variable = NA_character_,
    rRMSE = 0.6,
    stringsAsFactors = FALSE
  )

  result <- compare_rmse("wheat", ref, new)
  expect_identical(nrow(result), 0)
})

test_that("compare_rmse adds species column", {
  ref <- make_stats(
    situations = "usm1",
    variables = "LAI",
    rrmse = 0.5
  )
  new <- make_stats(
    situations = "usm1",
    variables = "LAI",
    rrmse = 0.6
  )

  result <- compare_rmse("wheat", ref, new)
  expect_true(all(result$species == "wheat"))
})

test_that("compare_rmse rounds ratio to 2 decimal places", {
  ref <- data.frame(
    situation = "usm1", variable = "LAI", rRMSE = 0.3, stringsAsFactors = FALSE
  )
  new <- data.frame(
    situation = "usm1", variable = "LAI", rRMSE = 0.4, stringsAsFactors = FALSE
  )

  result <- compare_rmse("wheat", ref, new)
  expect_identical(result$ratio, round(result$ratio, 2))
})

# ===========================================================================
# Tests: get_deteriorated_rmse_per_usm
# ===========================================================================

test_that(
  "get_deteriorated_rmse_per_usm returns NULL when no stats",
  {
    stub(
      get_deteriorated_rmse_per_usm,
      "get_rmse_per_usm",
      mock(NULL)
    )

    result <- get_deteriorated_rmse_per_usm(
      "/ws", "wheat", data.frame(), 10
    )
    expect_null(result)
  }
)

test_that(
  "get_deteriorated_rmse_per_usm returns only warning/critical rows", # nolint: nonportable_path_linter
  {
    ref <- data.frame(
      situation = c("usm1", "usm2", "usm3"),
      variable = c("LAI", "LAI", "LAI"),
      rRMSE = c(0.5, 0.5, 0.5),
      stringsAsFactors = FALSE
    )
    new <- data.frame(
      situation = c("usm1", "usm2", "usm3"),
      variable = c("LAI", "LAI", "LAI"),
      rRMSE = c(0.6, 0.4, 1.0),
      stringsAsFactors = FALSE
    )
    stub(get_deteriorated_rmse_per_usm, "get_rmse_per_usm", mock(new))

    result <- get_deteriorated_rmse_per_usm(
      "/ws", "wheat", ref, 10
    )

    expect_true(all(result$ratio > 0))
  }
)

test_that(
  "get_deteriorated_rmse_per_usm returns rows sorted by desc ratio",
  {
    ref <- data.frame(
      situation = c("usm1", "usm2"),
      variable = c("LAI", "LAI"),
      rRMSE = c(0.5, 0.5),
      stringsAsFactors = FALSE
    )
    new <- data.frame(
      situation = c("usm1", "usm2"),
      variable = c("LAI", "LAI"),
      rRMSE = c(0.6, 1.0),
      stringsAsFactors = FALSE
    )
    stub(get_deteriorated_rmse_per_usm, "get_rmse_per_usm", mock(new))

    result <- get_deteriorated_rmse_per_usm(
      "/ws", "wheat", ref, 5
    )

    expect_gte(result$ratio[1], result$ratio[2])
  }
)

# ===========================================================================
# Tests: gen_species_comparison
# ===========================================================================

test_that(
  "gen_species_comparison skips species when ref_stats is NULL",
  {
    mock_save <- mock(NULL)

    stub(gen_species_comparison, "read_ref_stats", mock(NULL))
    stub(gen_species_comparison, "get_stats", mock(NULL))
    stub(gen_species_comparison, "save_species_comparison", mock_save)

    gen_species_comparison("/ws", "wheat", "/ref", 10)
    expect_called(mock_save, 0)
  }
)

test_that(
  "gen_species_comparison skips species when stats is NULL",
  {
    mock_save <- mock(NULL)
    ref <- make_stats()

    stub(gen_species_comparison, "read_ref_stats", mock(ref))
    stub(gen_species_comparison, "get_stats", mock(NULL))
    stub(gen_species_comparison, "save_species_comparison", mock_save)

    gen_species_comparison("/ws", "wheat", "/ref", 10)
    expect_called(mock_save, 0)
  }
)

test_that(
  "gen_species_comparison saves comparison for each valid species",
  {
    mock_save <- mock(NULL, cycle = TRUE)
    ref <- make_stats()
    new <- make_stats(rrmse = c(0.4, 0.6, 0.3, 0.7))

    stub(gen_species_comparison, "read_ref_stats", mock(ref, cycle = TRUE))
    stub(gen_species_comparison, "get_stats", mock(new, cycle = TRUE))
    stub(
      gen_species_comparison,
      "save_species_comparison",
      mock_save
    )
    stub(gen_species_comparison, "log_comparison", mock(NULL, cycle = TRUE))

    gen_species_comparison(
      "/ws", c("wheat", "maize"), "/ref", 10
    )
    expect_called(mock_save, 2)
  }
)

# ===========================================================================
# Tests: gen_deteriorated_usm
# ===========================================================================

test_that(
  "gen_deteriorated_usm skips species when ref_stats is NULL",
  {
    mock_save <- mock(NULL)

    stub(gen_deteriorated_usm, "read_ref_rmse_per_usm", mock(NULL))
    stub(
      gen_deteriorated_usm,
      "get_deteriorated_rmse_per_usm",
      mock(NULL)
    )
    stub(gen_deteriorated_usm, "save_deteriorated_usm", mock_save)

    gen_deteriorated_usm("/ws", "wheat", "/ref", 10)
    expect_called(mock_save, 0)
  }
)

test_that(
  "gen_deteriorated_usm saves deteriorated USM for each valid species",
  {
    mock_save <- mock(NULL, cycle = TRUE)
    ref <- make_stats()
    det <- data.frame(situation = "usm1", ratio = 25, stringsAsFactors = FALSE)

    stub(
      gen_deteriorated_usm,
      "read_ref_rmse_per_usm",
      mock(ref, cycle = TRUE)
    )
    stub(
      gen_deteriorated_usm,
      "get_deteriorated_rmse_per_usm",
      mock(det, cycle = TRUE)
    )
    stub(gen_deteriorated_usm, "save_deteriorated_usm", mock_save)

    gen_deteriorated_usm(
      "/ws", c("wheat", "maize"), "/ref", 10
    )
    expect_called(mock_save, 2)
  }
)

# ===========================================================================
# Tests: display_comparisons_info
# ===========================================================================

test_that(
  "display_comparisons_info warns and returns when no comparisons",
  {
    mock_warn <- mock(NULL)

    stub(display_comparisons_info, "get_species", mock("wheat"))
    stub(
      display_comparisons_info,
      "get_species_comparison",
      mock(NULL)
    )
    stub(display_comparisons_info, "logger::log_warn", mock_warn)

    display_comparisons_info("/ws", 10)
    expect_called(mock_warn, 1)
  }
)

test_that(
  "display_comparisons_info stops when critical variables found",
  {
    df <- make_comparison_df()

    stub(display_comparisons_info, "get_species", mock("wheat"))
    stub(
      display_comparisons_info,
      "get_species_comparison",
      mock(df)
    )
    stub(display_comparisons_info, "logger::log_info", mock(NULL, cycle = TRUE))
    stub(display_comparisons_info, "logger::log_warn", mock(NULL, cycle = TRUE))
    stub(
      display_comparisons_info,
      "logger::log_error",
      mock(NULL, cycle = TRUE)
    )

    expect_error(display_comparisons_info("/ws", 10))
  }
)

test_that(
  "display_comparisons_info does not stop when only warnings",
  {
    df <- make_comparison_df() |>
      dplyr::mutate(ratio = c(5, -5, -5, 3))

    stub(display_comparisons_info, "get_species", mock("wheat"))
    stub(
      display_comparisons_info,
      "get_species_comparison",
      mock(df)
    )
    stub(display_comparisons_info, "logger::log_info", mock(NULL, cycle = TRUE))
    stub(display_comparisons_info, "logger::log_warn", mock(NULL, cycle = TRUE))

    expect_no_error(display_comparisons_info("/ws", 10))
  }
)

test_that(
  "display_comparisons_info does not stop when all variables ok",
  {
    df <- make_comparison_df() |>
      dplyr::mutate(ratio = c(-5, -10, -3, -1))

    stub(display_comparisons_info, "get_species", mock("wheat"))
    stub(
      display_comparisons_info,
      "get_species_comparison",
      mock(df)
    )
    stub(display_comparisons_info, "logger::log_info", mock(NULL, cycle = TRUE))

    expect_no_error(display_comparisons_info("/ws", 10))
  }
)
