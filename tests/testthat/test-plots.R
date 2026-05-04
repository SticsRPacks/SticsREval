fake_config <- function() {
  config <- list(
    output_dir = tempdir(),
    percentage = 5,
    reference_version = "v1",
    parallel = FALSE,
    cores = 1,
    eval_workspace = "dummy"
  )

  config$validate_export <- function() config
  config$validate_plots <- function() config

  config
}

fake_workspace <- function(species = "wheat") {
  structure(list(
    get_species = function() species,

    get_species_comparison = function(spec, pct) {
      if (spec == "empty") return(NULL)

      list(
        critical_vars = "var1",
        warning_vars = "var2",
        plot_comparison = function(...) NULL
      )
    },

    get_sim = function(...) data.frame(x = 1),
    get_obs = function(...) data.frame(x = 2),

    with_version = function(...) fake_workspace(species)
  ), class = "fake_workspace")
}

fake_backend <- function() {
  list(
    run = function(n, f) {
      lapply(seq_len(n), f)
      invisible(NULL)
    }
  )
}

test_that("gen_plots calls scatter when deteriorated vars exist", {

  config <- fake_config()
  workspace <- fake_workspace("wheat")
  backend <- fake_backend()

  called <- FALSE

  gen_plots(
    config,
    workspace = workspace,
    backend = backend,
    scatter_fn = function(...) {
      called <<- TRUE
    },
    comparison_fn = function(...) NULL,
    logger_info = function(...) NULL
  )

  expect_true(called)
})

test_that("gen_plots skips species when comparison is NULL", {

  config <- fake_config()
  workspace <- fake_workspace("empty")
  backend <- fake_backend()

  called <- FALSE

  gen_plots(
    config,
    workspace = workspace,
    backend = backend,
    scatter_fn = function(...) {
      called <<- TRUE
    },
    comparison_fn = function(...) NULL,
    logger_info = function(...) NULL
  )

  expect_false(called)
})

test_that("gen_plots does not call scatter when ref_sim is NULL", {

  config <- fake_config()

  workspace <- structure(list(
    get_species = function() "wheat",

    get_species_comparison = function(...) list(
      critical_vars = "var1",
      warning_vars = "var2",
      plot_comparison = function(...) NULL
    ),

    get_sim = function(...) NULL,  # <- important
    get_obs = function(...) data.frame(x = 2),

    with_version = function(...) workspace
  ), class = "fake_workspace")

  backend <- fake_backend()

  called <- FALSE

  gen_plots(
    config,
    workspace = workspace,
    backend = backend,
    scatter_fn = function(...) {
      called <<- TRUE
    },
    comparison_fn = function(...) NULL,
    logger_info = function(...) NULL
  )

  expect_false(called)
})

test_that("gen_plots skips scatter when no deteriorated vars", {

  config <- fake_config()

  workspace <- structure(list(
    get_species = function() "wheat",

    get_species_comparison = function(...) list(
      critical_vars = character(0),
      warning_vars = character(0),
      plot_comparison = function(...) NULL
    ),

    get_sim = function(...) data.frame(x = 1),
    get_obs = function(...) data.frame(x = 2),

    with_version = function(...) workspace
  ), class = "fake_workspace")

  backend <- fake_backend()

  called <- FALSE

  gen_plots(
    config,
    workspace = workspace,
    backend = backend,
    scatter_fn = function(...) {
      called <<- TRUE
    },
    comparison_fn = function(...) NULL,
    logger_info = function(...) NULL
  )

  expect_false(called)
})

test_that("gen_plots calls comparison function", {

  config <- fake_config()
  workspace <- fake_workspace("wheat")
  backend <- fake_backend()

  called <- FALSE

  gen_plots(
    config,
    workspace = workspace,
    backend = backend,
    scatter_fn = function(...) NULL,
    comparison_fn = function(...) {
      called <<- TRUE
    },
    logger_info = function(...) NULL
  )

  expect_true(called)
})