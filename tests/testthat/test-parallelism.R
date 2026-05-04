test_that("ParallelBackend runs sequentially when parallel = FALSE", {

  backend <- ParallelBackend$new(parallel = FALSE)

  result <- backend$run(3, function(i) i * 2)

  expect_equal(result, list(2, 4, 6))
})

test_that("ParallelBackend uses injected runner", {

  called <- FALSE

  fake_runner <- function(n, fun, private) {
    called <<- TRUE
    lapply(seq_len(n), fun)
  }

  backend <- ParallelBackend$new(
    parallel = TRUE,
    runner = fake_runner
  )

  result <- backend$run(2, function(i) i)

  expect_true(called)
  expect_equal(result, list(1, 2))
})