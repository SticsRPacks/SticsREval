# ===========================================================================
# Tests: prepare_species_workspace
# ===========================================================================
test_that("prepare_species_workspace creates species directory", {
  base <- file.path(tempdir(), basename(tempfile()))
  dir.create(base)
  stub(prepare_species_workspace, "get_species", function(...) "wheat")
  stub(prepare_species_workspace, "get_species_usm", function(...) "usm1")
  prepare_species_workspace(base, species = "wheat")
  expect_true(dir.exists(file.path(base, "wheat")))
})

test_that("prepare_species_workspace skips species with no USMs", {
  base <- file.path(tempdir(), basename(tempfile()))
  dir.create(base)
  stub(prepare_species_workspace, "get_species", function(...) "wheat")
  stub(prepare_species_workspace, "get_species_usm", function(...) character(0))
  prepare_species_workspace(base, species = "wheat")
  expect_false(dir.exists(file.path(base, "wheat")))
})

test_that("prepare_species_workspace processes multiple species", {
  base <- file.path(tempdir(), basename(tempfile()))
  dir.create(base)
  stub(
    prepare_species_workspace, "get_species", function(...) c("wheat", "maize")
  )
  stub(prepare_species_workspace, "get_species_usm", function(...) "usm1")
  prepare_species_workspace(base, species = c("wheat", "maize"))
  expect_true(dir.exists(file.path(base, "wheat")))
  expect_true(dir.exists(file.path(base, "maize")))
})

test_that("prepare_species_workspace skips only species without USMs", {
  base <- file.path(tempdir(), basename(tempfile()))
  dir.create(base)
  stub(
    prepare_species_workspace, "get_species", function(...) c("wheat", "maize")
  )
  stub(prepare_species_workspace, "get_species_usm", function(ws, spec, ...) {
    if (spec == "wheat") "usm1" else character(0)
  })
  prepare_species_workspace(base, species = c("wheat", "maize"))
  expect_true(dir.exists(file.path(base, "wheat")))
  expect_false(dir.exists(file.path(base, "maize")))
})

test_that(
  "prepare_species_workspace with species = NULL processes all workspace
  species",
  {
    base <- file.path(tempdir(), basename(tempfile()))
    dir.create(base)
    stub(
      prepare_species_workspace,
      "get_species",
      function(...) c("wheat", "maize")
    )
    stub(prepare_species_workspace, "get_species_usm", function(...) "usm1")
    prepare_species_workspace(base)
    expect_true(dir.exists(file.path(base, "wheat")))
    expect_true(dir.exists(file.path(base, "maize")))
  }
)

test_that(
  "prepare_species_workspace filters out species not in species filter",
  {
    base <- file.path(tempdir(), basename(tempfile()))
    dir.create(base)
    stub(
      prepare_species_workspace,
      "get_species",
      function(...) c("wheat", "maize", "soy")
    )
    stub(prepare_species_workspace, "get_species_usm", function(...) "usm1")
    prepare_species_workspace(base, species = "wheat")
    expect_true(dir.exists(file.path(base, "wheat")))
    expect_false(dir.exists(file.path(base, "maize")))
    expect_false(dir.exists(file.path(base, "soy")))
  }
)

test_that(
  "prepare_species_workspace throws an error when directory cannot be created",
  {
    stub(prepare_species_workspace, "get_species", function(...) "wheat")
    stub(prepare_species_workspace, "get_species_usm", function(...) "usm1")
    invalid_path <- if (.Platform$OS.type == "windows") {
      "C:/invalid:path/test" # nolint: nonportable_path_linter
    } else {
      "/proc/invalid_root_path" # nolint: nonportable_path_linter
    }
    expect_error(
      suppressWarnings(
        prepare_species_workspace(invalid_path, species = "wheat")
      ),
      regexp = "Error while creating"
    )
  }
)

# ===========================================================================
# Tests: evaluate_species
# ===========================================================================
test_that("evaluate_species calls gen_species_stats", {
  mock_stats <- mock(NULL)
  stub(evaluate_species, "gen_species_stats", mock_stats)
  stub(evaluate_species, "gen_deteriorated_usm", mock(NULL))
  stub(evaluate_species, "gen_species_comparison", mock(NULL))
  evaluate_species("/ws", "wheat", "/ref", 20, FALSE, NA)
  expect_called(mock_stats, 1)
})
test_that("evaluate_species calls gen_deteriorated_usm with correct args", {
  mock_det <- mock(NULL)
  stub(evaluate_species, "gen_species_stats", mock(NULL))
  stub(evaluate_species, "gen_deteriorated_usm", mock_det)
  stub(evaluate_species, "gen_species_comparison", mock(NULL))
  evaluate_species("/ws", "wheat", "/ref", 20, FALSE, NA)
  args <- mock_args(mock_det)[[1]]
  expect_identical(args[[2]], "wheat")
  expect_identical(args[[3]], "/ref")
  expect_identical(args[[4]], 20)
})
test_that(
  "evaluate_species calls gen_species_comparison with correct args",
  {
    mock_comp <- mock(NULL)
    stub(evaluate_species, "gen_species_stats", mock(NULL))
    stub(evaluate_species, "gen_deteriorated_usm", mock(NULL))
    stub(evaluate_species, "gen_species_comparison", mock_comp)
    evaluate_species("/ws", "wheat", "/ref", 20, FALSE, NA)
    args <- mock_args(mock_comp)[[1]]
    expect_identical(args[[2]], "wheat")
    expect_identical(args[[3]], "/ref")
    expect_identical(args[[4]], 20)
  }
)
test_that(
  "evaluate_species passes parallel and cores to gen_species_stats",
  {
    mock_stats <- mock(NULL)
    stub(evaluate_species, "gen_species_stats", mock_stats)
    stub(evaluate_species, "gen_deteriorated_usm", mock(NULL))
    stub(evaluate_species, "gen_species_comparison", mock(NULL))
    evaluate_species("/ws", "wheat", "/ref", 20, TRUE, 4)
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
  stub(evaluate, "get_species", mock("wheat"))
  stub(evaluate, "prepare_species_workspace", mock(NULL))
  stub(evaluate, "evaluate_species", mock(NULL))
  stub(evaluate, "display_comparisons_info", mock(NULL))
  evaluate(make_eval_config())
  expect_called(mock_validate, 1)
})

test_that("evaluate calls evaluate_species when init_workspace = FALSE", {
  mock_eval <- mock(NULL)
  stub(evaluate, "validate_eval_configuration", mock(NULL))
  stub(evaluate, "get_species", mock("wheat"))
  stub(evaluate, "prepare_species_workspace", mock(NULL))
  stub(evaluate, "evaluate_species", mock_eval)
  stub(evaluate, "display_comparisons_info", mock(NULL))
  evaluate(make_eval_config(list(init_workspace = FALSE)))
  expect_called(mock_eval, 1)
})

test_that("evaluate calls init_eval_workspace when init_workspace = TRUE", {
  mock_init <- mock(NULL)
  stub(evaluate, "validate_eval_configuration", mock(NULL))
  stub(evaluate, "init_eval_workspace", mock_init)
  stub(evaluate, "get_species", mock("wheat"))
  stub(evaluate, "prepare_species_workspace", mock(NULL))
  stub(evaluate, "evaluate_species", mock(NULL))
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
    stub(evaluate, "get_species", mock("wheat"))
    stub(evaluate, "prepare_species_workspace", mock(NULL))
    stub(evaluate, "evaluate_species", mock(NULL))
    stub(evaluate, "display_comparisons_info", mock(NULL))
    evaluate(make_eval_config(list(init_workspace = FALSE)))
    expect_called(mock_init, 0)
  }
)

test_that(
  "evaluate calls prepare_species_workspace with eval_workspace, species and
  usms",
  {
    mock_prepare <- mock(NULL)
    stub(evaluate, "validate_eval_configuration", mock(NULL))
    stub(evaluate, "get_species", mock("wheat"))
    stub(evaluate, "prepare_species_workspace", mock_prepare)
    stub(evaluate, "evaluate_species", mock(NULL))
    stub(evaluate, "display_comparisons_info", mock(NULL))
    evaluate(make_eval_config())
    expect_called(mock_prepare, 1)
    args <- mock_args(mock_prepare)[[1]]
    expect_identical(args[[1]], "/ws")
    expect_null(args[[2]])
    expect_null(args[[3]])
  }
)

test_that(
  "evaluate calls display_comparisons_info with eval_workspace, species and
  percentage",
  {
    mock_display <- mock(NULL)
    stub(evaluate, "validate_eval_configuration", mock(NULL))
    stub(evaluate, "get_species", mock("wheat"))
    stub(evaluate, "prepare_species_workspace", mock(NULL))
    stub(evaluate, "evaluate_species", mock(NULL))
    stub(evaluate, "display_comparisons_info", mock_display)
    evaluate(make_eval_config())
    args <- mock_args(mock_display)[[1]]
    expect_identical(args[[1]], "/ws")
    expect_identical(args[[2]], "wheat")
    expect_identical(args[[3]], 20)
  }
)

test_that("evaluate filters species by config$species when provided", {
  mock_eval <- mock(NULL)
  stub(evaluate, "validate_eval_configuration", mock(NULL))
  stub(evaluate, "get_species", mock(c("wheat", "maize")))
  stub(evaluate, "prepare_species_workspace", mock(NULL))
  stub(evaluate, "evaluate_species", mock_eval)
  stub(evaluate, "display_comparisons_info", mock(NULL))
  evaluate(make_eval_config(list(species = "wheat")))
  args <- mock_args(mock_eval)[[1]]
  expect_identical(args[[2]], "wheat")
})

test_that("evaluate filters species by config$usms when provided", {
  mock_eval <- mock(NULL)
  stub(evaluate, "validate_eval_configuration", mock(NULL))
  stub(evaluate, "get_species", mock(c("wheat", "maize")))
  stub(evaluate, "prepare_species_workspace", mock(NULL))
  stub(evaluate, "get_species_usm", function(ws, sp, usms) {
    if (sp == "wheat") "usm1" else character(0)
  })
  stub(evaluate, "evaluate_species", mock_eval)
  stub(evaluate, "display_comparisons_info", mock(NULL))
  evaluate(make_eval_config(list(usms = "usm1")))
  args <- mock_args(mock_eval)[[1]]
  expect_identical(args[[2]], "wheat")
})

test_that(
  "evaluate returns early and does not call evaluate_species when no
  species remain",
  {
    mock_eval <- mock(NULL)
    stub(evaluate, "validate_eval_configuration", mock(NULL))
    stub(evaluate, "get_species", mock(c("wheat", "maize")))
    stub(evaluate, "prepare_species_workspace", mock(NULL))
    stub(evaluate, "evaluate_species", mock_eval)
    stub(evaluate, "display_comparisons_info", mock(NULL))
    evaluate(make_eval_config(list(species = "soy")))  # soy not in workspace
    expect_called(mock_eval, 0)
  }
)

test_that(
  "evaluate logs error and does not rethrow when evaluate_species fails",
  {
    stub(evaluate, "validate_eval_configuration", mock(NULL))
    stub(evaluate, "get_species", mock("wheat"))
    stub(evaluate, "prepare_species_workspace", mock(NULL))
    stub(
      evaluate,
      "evaluate_species",
      function(...) stop("boom", call. = FALSE)
    )
    stub(evaluate, "display_comparisons_info", mock(NULL))
    stub(evaluate, "logger::log_error", mock(NULL))
    expect_no_error(evaluate(make_eval_config()))
  }
)

test_that(
  "evaluate does not call display_comparisons_info when evaluate_species fails",
  {
    mock_display <- mock(NULL)
    stub(evaluate, "validate_eval_configuration", mock(NULL))
    stub(evaluate, "get_species", mock("wheat"))
    stub(evaluate, "prepare_species_workspace", mock(NULL))
    stub(
      evaluate,
      "evaluate_species",
      function(...) stop("boom", call. = FALSE)
    )
    stub(evaluate, "display_comparisons_info", mock_display)
    stub(evaluate, "logger::log_error", mock(NULL))
    evaluate(make_eval_config())
    expect_called(mock_display, 0)
  }
)
