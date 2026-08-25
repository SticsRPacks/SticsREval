write_sim_rds <- function(sim_list) {
  path <- withr::local_tempfile(fileext = ".rds", .local_envir = parent.frame())
  saveRDS(sim_list, path)
  path
}

test_that("BalanceClosureTest calls config$validate_balance_closure()", {
  called <- new.env()
  called$flag <- FALSE
  config <- make_base_cfg(sim_rds = write_sim_rds(list()))
  config$validate_balance_closure <- function() {
    called$flag <- TRUE
  }
  BalanceClosureTest$new(config)$run()
  expect_true(called$flag)
})
test_that("BalanceClosureTest handles no USMs to test", {
  sim_data <- data.frame(
    Date = as.Date("2024-01-01"),
    init_H2O_balance = 100,
    final_H2O_balance = 100
  )
  config <- make_base_cfg(
    sim_rds = write_sim_rds(list(usm1 = sim_data)),
    usms = "nonexistent_usm"
  )
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
  sim_data <- data.frame(
    Date = as.Date(c("2024-01-01", "2024-01-02")),
    init_H2O_balance = c(100, 100),
    final_H2O_balance = c(90, 80)
  )

  config <- make_base_cfg(
    sim_rds = write_sim_rds(list(usm1 = sim_data))
  )
  log_env <- make_log_capture()
  on.exit(logger::log_appender(logger::appender_console), add = TRUE)

  bc_test <- BalanceClosureTest$new(config)

  expect_error(
    bc_test$run(),
    "Balance closure test failed for some USMs. Check logs for details."
  )

  expect_match(
    log_env$logs,
    paste(
      "Balance closure issue for USM usm1: H2O_balance rounded",
      "difference = -20 (initial = 100, final = 80)"
    ),
    all = FALSE,
    fixed = TRUE
  )
})
test_that("BalanceClosureTest handles USMs with missing or NA fields", {
  sim_data <- data.frame(
    Date = as.Date(c("2024-01-01", "2024-01-02")),
    init_H2O_balance = c(NA, NA),
    final_H2O_balance = c(NA, NA)
  )

  config <- make_base_cfg(
    sim_rds = write_sim_rds(list(usm1 = sim_data))
  )
  log_env <- make_log_capture()
  on.exit(logger::log_appender(logger::appender_console), add = TRUE)

  bc_test <- BalanceClosureTest$new(config)
  expect_no_error(bc_test$run())
})

test_that("BalanceClosureTest runs without errors when all USMs pass", {
  sim_data <- data.frame(
    Date = as.Date(c("2024-01-01", "2024-01-02")),
    init_H2O_balance = c(100, 100),
    final_H2O_balance = c(100, 100)
  )

  config <- make_base_cfg(
    sim_rds = write_sim_rds(list(usm1 = sim_data))
  )

  bc_test <- BalanceClosureTest$new(config)
  expect_no_error(bc_test$run())
})

test_that("BalanceClosureTest restricts to USMs listed in config$usms", {
  sim_data_ok <- data.frame(
    Date = as.Date(c("2024-01-01", "2024-01-02")),
    init_H2O_balance = c(100, 100),
    final_H2O_balance = c(100, 100)
  )
  sim_data_bad <- data.frame(
    Date = as.Date(c("2024-01-01", "2024-01-02")),
    init_H2O_balance = c(100, 100),
    final_H2O_balance = c(90, 80)
  )

  config <- make_base_cfg(
    sim_rds = write_sim_rds(list(usm1 = sim_data_ok, usm2 = sim_data_bad)),
    usms = "usm1"
  )

  bc_test <- BalanceClosureTest$new(config)
  expect_no_error(bc_test$run())
})

test_that(
  "balance_closure_test() builds a Configuration and runs
  BalanceClosureTest",
  {
    sim_data <- data.frame(
      Date = as.Date(c("2024-01-01", "2024-01-02")),
      init_H2O_balance = c(100, 100),
      final_H2O_balance = c(100, 100)
    )

    expect_no_error(
      balance_closure_test(
        sim_rds = write_sim_rds(list(usm1 = sim_data)),
        usms = "usm1"
      )
    )
  }
)

test_that(
  "balance_closure_test() forwards parallel and cores to the Configuration",
  {
    sim_data <- data.frame(
      Date = as.Date(c("2024-01-01", "2024-01-02")),
      init_H2O_balance = c(100, 100),
      final_H2O_balance = c(100, 100)
    )

    result <- balance_closure_test(
      sim_rds = write_sim_rds(list(usm1 = sim_data)),
      parallel = TRUE,
      cores = 2L
    )

    config <- result$.__enclos_env__$private$config
    expect_true(config$parallel)
    expect_identical(config$cores, 2L)
  }
)
