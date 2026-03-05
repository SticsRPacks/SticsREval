# ===========================================================================
# Tests: prepare_species_workspace
# ===========================================================================

test_that("prepare_species_workspace creates species directory", {
  base <- file.path(tempdir(), basename(tempfile()))
  dir.create(base)

  stub(prepare_species_workspace, "get_species_usm", function(...) "usm1")

  prepare_species_workspace(base, "wheat")

  expect_true(dir.exists(file.path(base, "wheat")))
})

test_that("prepare_species_workspace skips species with no USMs", {
  base <- file.path(tempdir(), basename(tempfile()))
  dir.create(base)

  stub(prepare_species_workspace, "get_species_usm", function(...) character(0))

  prepare_species_workspace(base, "wheat")

  expect_false(dir.exists(file.path(base, "wheat")))
})

test_that("prepare_species_workspace processes multiple species", {
  base <- file.path(tempdir(), basename(tempfile()))
  dir.create(base)

  stub(prepare_species_workspace, "get_species_usm", function(...) "usm1")

  prepare_species_workspace(base, c("wheat", "maize"))

  expect_true(dir.exists(file.path(base, "wheat")))
  expect_true(dir.exists(file.path(base, "maize")))
})

test_that("prepare_species_workspace skips only species without USMs", {
  base <- file.path(tempdir(), basename(tempfile()))
  dir.create(base)

  stub(prepare_species_workspace, "get_species_usm", function(ws, spec) {
    if (spec == "wheat") "usm1" else character(0)
  })

  prepare_species_workspace(base, c("wheat", "maize"))

  expect_true(dir.exists(file.path(base, "wheat")))
  expect_false(dir.exists(file.path(base, "maize")))
})

test_that(
  "prepare_species_workspace throws an error when directory cannot be created",
  {
    stub(prepare_species_workspace, "get_species_usm", function(...) "usm1")
    invalid_path <- if (.Platform$OS.type == "windows") {
      "C:/invalid:path/test" # nolint: nonportable_path_linter
    } else {
      "/proc/invalid_root_path" # nolint: nonportable_path_linter
    }

    expect_error(
      suppressWarnings(
        prepare_species_workspace(invalid_path, "wheat")
      ),
      regexp = "Error while creating"
    )
  }
)

# ===========================================================================
# Tests: evaluate_all_species
# ===========================================================================

test_that("evaluate_all_species calls prepare_species_workspace", {
  mock_prepare <- mock(NULL)

  stub(evaluate_all_species, "get_species", mock("wheat"))
  stub(evaluate_all_species, "prepare_species_workspace", mock_prepare)
  stub(evaluate_all_species, "gen_species_stats", mock(NULL))
  stub(evaluate_all_species, "gen_deteriorated_usm", mock(NULL))
  stub(evaluate_all_species, "gen_species_comparison", mock(NULL))

  evaluate_all_species("/ws", "/ref", 20, FALSE, NA)

  expect_called(mock_prepare, 1)
})

test_that("evaluate_all_species calls gen_species_stats", {
  mock_stats <- mock(NULL)

  stub(evaluate_all_species, "get_species", mock("wheat"))
  stub(evaluate_all_species, "prepare_species_workspace", mock(NULL))
  stub(evaluate_all_species, "gen_species_stats", mock_stats)
  stub(evaluate_all_species, "gen_deteriorated_usm", mock(NULL))
  stub(evaluate_all_species, "gen_species_comparison", mock(NULL))

  evaluate_all_species("/ws", "/ref", 20, FALSE, NA)

  expect_called(mock_stats, 1)
})

test_that("evaluate_all_species calls gen_deteriorated_usm with correct args", {
  mock_det <- mock(NULL)

  stub(evaluate_all_species, "get_species", mock("wheat"))
  stub(evaluate_all_species, "prepare_species_workspace", mock(NULL))
  stub(evaluate_all_species, "gen_species_stats", mock(NULL))
  stub(evaluate_all_species, "gen_deteriorated_usm", mock_det)
  stub(evaluate_all_species, "gen_species_comparison", mock(NULL))

  evaluate_all_species("/ws", "/ref", 20, FALSE, NA)

  args <- mock_args(mock_det)[[1]]
  expect_identical(args[[2]], "wheat")
  expect_identical(args[[3]], "/ref")
  expect_identical(args[[4]], 20)
})

test_that(
  "evaluate_all_species calls gen_species_comparison with correct args",
  {
    mock_comp <- mock(NULL)

    stub(evaluate_all_species, "get_species", mock("wheat"))
    stub(evaluate_all_species, "prepare_species_workspace", mock(NULL))
    stub(evaluate_all_species, "gen_species_stats", mock(NULL))
    stub(evaluate_all_species, "gen_deteriorated_usm", mock(NULL))
    stub(evaluate_all_species, "gen_species_comparison", mock_comp)

    evaluate_all_species("/ws", "/ref", 20, FALSE, NA)

    args <- mock_args(mock_comp)[[1]]
    expect_identical(args[[2]], "wheat")
    expect_identical(args[[3]], "/ref")
    expect_identical(args[[4]], 20)
  }
)

test_that(
  "evaluate_all_species passes parallel and cores to gen_species_stats",
  {
    mock_stats <- mock(NULL)

    stub(evaluate_all_species, "get_species", mock("wheat"))
    stub(evaluate_all_species, "prepare_species_workspace", mock(NULL))
    stub(evaluate_all_species, "gen_species_stats", mock_stats)
    stub(evaluate_all_species, "gen_deteriorated_usm", mock(NULL))
    stub(evaluate_all_species, "gen_species_comparison", mock(NULL))

    evaluate_all_species("/ws", "/ref", 20, TRUE, 4)

    args <- mock_args(mock_stats)[[1]]
    expect_true(args[[3]])
    expect_identical(args[[4]], 4)
  }
)

# ===========================================================================
# Helpers
# ===========================================================================

make_eval_config <- function(overrides = list()) {
  cfg <- list(
    eval_workspace = "/ws",
    reference_data_dir = "/ref",
    percentage = 20,
    parallel = FALSE,
    cores = NA,
    init_workspace = FALSE,
    workspace = "/original_ws",
    metadata_file = "/meta.csv",
    stics_exe = "/stics",
    run_simulations = FALSE
  )
  for (nm in names(overrides)) cfg[[nm]] <- overrides[[nm]]
  cfg
}

# ===========================================================================
# Tests: evaluate
# ===========================================================================

test_that("evaluate calls validate_eval_configuration", {
  mock_validate <- mock(NULL)

  stub(evaluate, "validate_eval_configuration", mock_validate)
  stub(evaluate, "evaluate_all_species", mock(NULL))
  stub(evaluate, "display_comparisons_info", mock(NULL))

  evaluate(make_eval_config())

  expect_called(mock_validate, 1)
})

test_that("evaluate calls evaluate_all_species when init_workspace = FALSE", {
  mock_eval <- mock(NULL)

  stub(evaluate, "validate_eval_configuration", mock(NULL))
  stub(evaluate, "evaluate_all_species", mock_eval)
  stub(evaluate, "display_comparisons_info", mock(NULL))

  evaluate(make_eval_config(list(init_workspace = FALSE)))

  expect_called(mock_eval, 1)
})

test_that("evaluate calls init_eval_workspace when init_workspace = TRUE", {
  mock_init <- mock(NULL)

  stub(evaluate, "validate_eval_configuration", mock(NULL))
  stub(evaluate, "init_eval_workspace", mock_init)
  stub(evaluate, "evaluate_all_species", mock(NULL))
  stub(evaluate, "display_comparisons_info", mock(NULL))

  evaluate(make_eval_config(list(init_workspace = TRUE)))

  expect_called(mock_init, 1)
})

test_that(
  "evaluate does not call init_eval_workspace when init_workspace = FALSE",
  {
    mock_init <- mock(NULL)

    stub(evaluate, "validate_eval_configuration", mock(NULL))
    stub(evaluate, "init_eval_workspace", mock_init)
    stub(evaluate, "evaluate_all_species", mock(NULL))
    stub(evaluate, "display_comparisons_info", mock(NULL))

    evaluate(make_eval_config(list(init_workspace = FALSE)))

    expect_called(mock_init, 0)
  }
)

test_that(
  "evaluate calls display_comparisons_info with eval_workspace and percentage",
  {
    mock_display <- mock(NULL)

    stub(evaluate, "validate_eval_configuration", mock(NULL))
    stub(evaluate, "evaluate_all_species", mock(NULL))
    stub(evaluate, "display_comparisons_info", mock_display)

    evaluate(make_eval_config())

    args <- mock_args(mock_display)[[1]]
    expect_identical(args[[1]], "/ws")
    expect_identical(args[[2]], 20)
  }
)

test_that(
  "evaluate logs error and does not rethrow when evaluate_all_species fails",
  {
    stub(evaluate, "validate_eval_configuration", mock(NULL))
    stub(
      evaluate,
      "evaluate_all_species",
      function(...) stop("boom", call. = FALSE)
    )
    stub(evaluate, "display_comparisons_info", mock(NULL))
    stub(evaluate, "logger::log_error", mock(NULL))

    expect_no_error(evaluate(make_eval_config()))
  }
)

test_that(
  "evaluate does not call display_comparisons_info
  when evaluate_all_species fails",
  {
    mock_display <- mock(NULL)

    stub(evaluate, "validate_eval_configuration", mock(NULL))
    stub(
      evaluate,
      "evaluate_all_species",
      function(...) stop("boom", call. = FALSE)
    )
    stub(evaluate, "display_comparisons_info", mock_display)
    stub(evaluate, "logger::log_error", mock(NULL))

    evaluate(make_eval_config())

    expect_called(mock_display, 0)
  }
)
