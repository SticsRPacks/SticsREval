write_sim_rds <- function(sim_list) {
  path <- withr::local_tempfile(fileext = ".rds", .local_envir = parent.frame())
  saveRDS(sim_list, path)
  path
}

test_that("BalanceClosureTest handles no USMs to test", {
  sim_data <- data.frame(
    Date = as.Date("2024-01-01"),
    init_H2O_balance = 100,
    final_H2O_balance = 100
  )

  init_logger(1L)
  log_env <- make_log_capture()
  on.exit(logger::log_appender(logger::appender_console), add = TRUE)

  BalanceClosureTest$new(
    sim_rds = write_sim_rds(list(usm1 = sim_data)),
    usms = "nonexistent_usm"
  )$run()

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

  init_logger(1L)
  log_env <- make_log_capture()
  on.exit(logger::log_appender(logger::appender_console), add = TRUE)

  bc_test <- BalanceClosureTest$new(
    sim_rds = write_sim_rds(list(usm1 = sim_data))
  )

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

  log_env <- make_log_capture()
  on.exit(logger::log_appender(logger::appender_console), add = TRUE)

  bc_test <- BalanceClosureTest$new(
    sim_rds = write_sim_rds(list(usm1 = sim_data))
  )
  expect_no_error(bc_test$run())
})

test_that("BalanceClosureTest runs without errors when all USMs pass", {
  sim_data <- data.frame(
    Date = as.Date(c("2024-01-01", "2024-01-02")),
    init_H2O_balance = c(100, 100),
    final_H2O_balance = c(100, 100)
  )

  bc_test <- BalanceClosureTest$new(
    sim_rds = write_sim_rds(list(usm1 = sim_data))
  )
  expect_no_error(bc_test$run())
})

test_that("BalanceClosureTest restricts to USMs listed in usms", {
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

  bc_test <- BalanceClosureTest$new(
    sim_rds = write_sim_rds(list(usm1 = sim_data_ok, usm2 = sim_data_bad)),
    usms = "usm1"
  )
  expect_no_error(bc_test$run())
})

test_that("BalanceClosureTest exports balance error details to output_dir", {
  sim_data <- data.frame(
    Date = as.Date(c("2024-01-01", "2024-01-02")),
    init_H2O_balance = c(100, 100),
    final_H2O_balance = c(90, 80)
  )
  output_dir <- withr::local_tempdir()

  bc_test <- BalanceClosureTest$new(
    sim_rds = write_sim_rds(list(usm1 = sim_data)),
    output_dir = output_dir
  )
  expect_error(bc_test$run())

  expect_true(file.exists(file.path(output_dir, "balance_errors_details.csv")))
})

test_that("balance_closure_test() runs the balance closure test end to end", {
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
})

test_that("balance_closure_test() validates its arguments", {
  sim_data <- data.frame(
    Date = as.Date(c("2024-01-01", "2024-01-02")),
    init_H2O_balance = c(100, 100),
    final_H2O_balance = c(100, 100)
  )

  expect_error(
    balance_closure_test(
      sim_rds = write_sim_rds(list(usm1 = sim_data)),
      parallel = TRUE,
      cores = NA
    ),
    "cores"
  )
})

test_that("balance_closure_test() stops when sim_rds does not exist", {
  expect_error(
    balance_closure_test(sim_rds = file.path("nonexistent", "sim.rds")),
    "not found"
  )
})
