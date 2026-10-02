fake_evaluation <- function(success, skip_reason = NULL) {
  list(
    success = success,
    skip_reason = skip_reason,
    export = function() NULL
  )
}

test_that("evaluation_status distinguishes not evaluated from failed", {
  expect_identical(evaluation_status(fake_evaluation(TRUE)), "success")
  expect_identical(evaluation_status(fake_evaluation(FALSE)), "failed")
  expect_identical(
    evaluation_status(fake_evaluation(FALSE, "no reference data")),
    "not evaluated"
  )
})

test_that("export_evaluations writes the status and skip reason", {
  output_dir <- withr::local_tempdir()
  evaluations <- list(
    "Global evaluation" = fake_evaluation(FALSE, "no reference data"),
    "Species evaluation" = fake_evaluation(TRUE),
    "USM evaluation" = fake_evaluation(FALSE)
  )

  export_evaluations(evaluations, output_dir)

  status <- read.csv(file.path(output_dir, "csv", "evaluation_status.csv"))
  expect_identical(status$status, c("not evaluated", "success", "failed"))
  expect_identical(status$reason, c("no reference data", NA, NA))
  expect_identical(status$success, c(NA, TRUE, FALSE))
})

test_that("report_evaluation_status shows the skip reason", {
  evaluations <- list(
    "Global evaluation" = fake_evaluation(FALSE, "no reference data")
  )

  expect_message(
    report_evaluation_status(evaluations),
    "not evaluated (no reference data)",
    fixed = TRUE
  )
})
