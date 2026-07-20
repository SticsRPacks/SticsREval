test_that("BalanceClosureTest calls config$validate_balance_closure()", {
  called <- new.env()
  called$flag <- FALSE
  config <- make_base_cfg(usms_workspace = tempdir())
  config$validate_balance_closure <- function() {
    called$flag <- TRUE
  }
  BalanceClosureTest$new(config)$run()
  expect_true(called$flag)
})
test_that("BalanceClosureTest handles no USMs to test", {
  empty_dir <- file.path(tempdir(), "empty_test_dir")
  dir.create(empty_dir, showWarnings = FALSE)

  config <- make_base_cfg(usms_workspace = empty_dir, usms = "nonexistent_usm")
  logs <- character(0)

  log_env <- make_log_capture()
  on.exit(logger::log_appender(logger::appender_console), add = TRUE)

  BalanceClosureTest$new(config)$run()

  expect_match(
    log_env$logs,
    "No USMs to test for balance closure.",
    all = FALSE
  )
})
test_that("BalanceClosureTest logs balance closure issues", {
  temp_dir <- file.path(tempdir(), "usms_test_dir")
  dir.create(temp_dir, showWarnings = FALSE)

  usm_path <- file.path(temp_dir, "usm1")
  dir.create(usm_path, showWarnings = FALSE)

  sim_data <- data.frame(
    Date = as.Date(c("2024-01-01", "2024-01-02")),
    init_H2O_balance = c(100, 100),
    final_H2O_balance = c(90, 80)
  )

  config <- make_base_cfg(
    usms_workspace = temp_dir,
    usms = "usm1",
    stics_exe = file.path("path", "to", "stics.exe")
  )
  log_env <- make_log_capture()
  on.exit(logger::log_appender(logger::appender_console), add = TRUE)

  bc_test <- BalanceClosureTest$new(config)
  replace_private(bc_test, "loader", list(
    run_simulations = function(usms, var) {
      list(usm1 = sim_data)
    }
  ))

  expect_error(
    bc_test$run(),
    "Balance closure test failed for some USMs. Check logs for details."
  )

  expect_match(
    log_env$logs,
    paste(
      "Balance closure issue for USM usm1: H2O_balance difference = -20",
      "(initial = 100, final = 80)"
    ),
    all = FALSE,
    fixed = TRUE
  )
})
test_that("BalanceClosureTest handles USMs with missing or NA fields", {
  temp_dir <- file.path(tempdir(), "usms_test_dir")
  dir.create(temp_dir, showWarnings = FALSE)

  usm_path <- file.path(temp_dir, "usm1")
  dir.create(usm_path, showWarnings = FALSE)

  sim_data <- data.frame(
    Date = as.Date(c("2024-01-01", "2024-01-02")),
    init_H2O_balance = c(NA, NA),
    final_H2O_balance = c(NA, NA)
  )

  config <- make_base_cfg(
    usms_workspace = temp_dir,
    usms = "usm1",
    stics_exe = file.path("path", "to", "stics.exe")
  )
  log_env <- make_log_capture()
  on.exit(logger::log_appender(logger::appender_console), add = TRUE)

  bc_test <- BalanceClosureTest$new(config)
  replace_private(bc_test, "loader", list(
    run_simulations = function(usms, var) {
      list(usm1 = sim_data)
    }
  ))
  expect_no_error(bc_test$run())
})

test_that("BalanceClosureTest runs without errors when all USMs pass", {
  temp_dir <- file.path(tempdir(), "usms_test_dir")
  dir.create(temp_dir, showWarnings = FALSE)

  usm_path <- file.path(temp_dir, "usm1")
  dir.create(usm_path, showWarnings = FALSE)

  sim_data <- data.frame(
    Date = as.Date(c("2024-01-01", "2024-01-02")),
    init_H2O_balance = c(100, 100),
    final_H2O_balance = c(100, 100)
  )

  config <- make_base_cfg(
    usms_workspace = temp_dir,
    usms = "usm1",
    stics_exe = file.path("path", "to", "stics.exe")
  )

  bc_test <- BalanceClosureTest$new(config)
  replace_private(bc_test, "loader", list(
    run_simulations = function(usms, var) {
      list(usm1 = sim_data)
    }
  ))
  expect_no_error(bc_test$run())
})
