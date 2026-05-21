# ---- Helpers ----

format_duration <- function(...) "1s"
format_species <- function(x) paste(x, collapse = ",")

replace_private <- function(obj, name, fn) {
  env <- obj$.__enclos_env__$private
  unlockBinding(name, env)
  env[[name]] <- fn
}

make_fake_logger <- function() {
  env <- new.env()
  env$info_calls <- list()
  env$error_calls <- list()

  list(
    info = function(...) {
      env$info_calls <- append(env$info_calls, list(list(...)))
    },
    error = function(...) {
      env$error_calls <- append(env$error_calls, list(list(...)))
    },
    debug = function(...) {},
    warn = function(...) {},
    .env = env
  )
}

make_fake_summary <- function() {
  env <- new.env()
  env$called <- FALSE

  list(
    new = function(...) {
      list(
        display = function() {
          env$called <- TRUE
        }
      )
    },
    .env = env
  )
}

fake_backend <- list(
  run = function(n, fun) lapply(seq_len(n), fun)
)

make_fake_workspace <- function() {
  list(
    init = function(...) {},
    get_species = function() c("wheat", "barley"),
    get_species_usm = function(sp, usms) "usm1",
    get_sim = function(...) data.frame(),
    get_obs = function(...) data.frame(),
    save_stats = function(...) {},
    save_rmse_per_usm = function(...) {},
    with_version = function(...) make_fake_workspace(),
    get_rmse_per_usm = function(...) data.frame(),
    get_stats = function(...) data.frame(),
    save_deteriorated_usm = function(...) {},
    save_species_comparison = function(...) {}
  )
}

make_base_cfg <- function(...) {
  defaults <- list(
    validate_eval = function() {},
    init_workspace = FALSE,
    species = NULL,
    usms = NULL,
    var2exclude = NULL,
    reference_version = NULL,
    percentage = 5,
    parallel = FALSE,
    cores = 1,
    eval_workspace = "ws"
  )
  utils::modifyList(defaults, list(...))
}

make_eval <- function(
  cfg,
  workspace = make_fake_workspace(),
  logger = make_fake_logger(),
  summary = make_fake_summary()
) {
  Evaluation$new(cfg,
    workspace = workspace,
    backend = fake_backend,
    logger = logger,
    summary_class = summary
  )
}

# ---- Tests ----

test_that("run executes full workflow", {
  summary <- make_fake_summary()
  eval <- make_eval(
    make_base_cfg(reference_version = "v1"),
    summary = summary
  )
  replace_private(eval, "evaluate_global", function() {})
  replace_private(eval, "evaluate_species", function(...) {})

  eval$run()

  expect_true(summary$.env$called)
})

test_that("get_species_to_evaluate filters by species and usms", {
  workspace <- make_fake_workspace()
  workspace$get_species <- function() c("wheat", "barley", "corn")

  workspace$get_species_usm <- function(sp, usms) {
    if (sp %in% c("wheat", "corn")) return("usm1")
    character(0)
  }

  eval <- make_eval(
    make_base_cfg(species = c("wheat", "corn"), usms = "usm1"),
    workspace = workspace
  )

  species <- eval$.__enclos_env__$private$get_species_to_evaluate()

  expect_identical(species, c("wheat", "corn"))
})

test_that("run logs and rethrows error", {
  logger <- make_fake_logger()
  workspace <- make_fake_workspace()
  workspace$get_sim <- function(...) stop("fail", call. = FALSE)

  eval <- make_eval(make_base_cfg(), workspace = workspace, logger = logger)

  expect_error(eval$run(), "fail")
  expect_gt(length(logger$.env$error_calls), 0)
})

test_that("evaluate_species skips comparison if no reference_version", {
  eval <- make_eval(make_base_cfg())
  replace_private(eval, "gen_species_stats", function(...) {})
  called <- new.env()
  called$flag <- FALSE

  replace_private(
    eval,
    "gen_species_deteriorated_usm",
    function(...) {
      called$flag <- TRUE
    }
  )

  eval$.__enclos_env__$private$evaluate_species("wheat")

  expect_false(called$flag)
})
