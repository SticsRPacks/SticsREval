# ===========================================================================
# Tests: get_cores
# ===========================================================================

test_that("get_cores returns availableCores when no cores_nb argument", {
  stub(get_cores, "future::availableCores", function() 8)
  expect_identical(get_cores(), 8)
})

test_that("get_cores returns the fake cores_nb when provided", {
  expect_identical(get_cores(cores_nb = 4), 4)
})

test_that(
  "get_cores ignores other dot arguments and still uses availableCores",
  {
    stub(get_cores, "future::availableCores", function() 6)
    expect_identical(get_cores(other_arg = 99), 6)
  }
)

# ===========================================================================
# Tests: get_cores_nb
# ===========================================================================

test_that("get_cores_nb returns 1 when parallel = FALSE", {
  expect_identical(get_cores_nb(parallel = FALSE), 1)
})

test_that(
  "get_cores_nb returns 1 when parallel = FALSE regardless of required_nb",
  {
    expect_identical(get_cores_nb(parallel = FALSE, required_nb = 8), 1)
  }
)

test_that("get_cores_nb keeps one core free when machine has >= 2 cores", {
  result <- get_cores_nb(parallel = TRUE, cores_nb = 4)
  expect_identical(result, 3)
})

test_that("get_cores_nb does not subtract core when machine has 1 core", {
  result <- get_cores_nb(parallel = TRUE, cores_nb = 1)
  expect_identical(result, 1)
})

test_that("get_cores_nb returns available cores when required_nb is NA", {
  result <- get_cores_nb(parallel = TRUE, required_nb = NA, cores_nb = 4)
  expect_identical(result, 3)  # 4 - 1 free
})

test_that(
  "get_cores_nb returns available cores when required_nb exceeds available",
  {
    result <- get_cores_nb(parallel = TRUE, required_nb = 10, cores_nb = 4)
    expect_identical(result, 3)
  }
)

test_that(
  "get_cores_nb returns required_nb when it is within available cores",
  {
    result <- get_cores_nb(parallel = TRUE, required_nb = 2, cores_nb = 6)
    expect_identical(result, 2)
  }
)

test_that("get_cores_nb returns required_nb equal to available cores", {
  result <- get_cores_nb(parallel = TRUE, required_nb = 3, cores_nb = 4)
  expect_identical(result, 3)
})

# ===========================================================================
# Tests: setup_parallel_backend
# ===========================================================================

test_that("setup_parallel_backend returns a list with map and cleanup", {
  result <- setup_parallel_backend(n_tasks = 3, parallel = FALSE, cores = NA)
  expect_type(result, "list")
  expect_type(result$map, "closure")
  expect_type(result$cleanup, "closure")
})

test_that("setup_parallel_backend sequential map applies fun to each element", {
  backend <- setup_parallel_backend(n_tasks = 3, parallel = FALSE, cores = NA)
  result  <- backend$map(1:3, function(i) i * 2)
  expect_identical(result, list(2, 4, 6))
})

test_that(
  "setup_parallel_backend sequential cleanup does nothing without error",
  {
    backend <- setup_parallel_backend(n_tasks = 3, parallel = FALSE, cores = NA)
    expect_no_error(backend$cleanup())
  }
)

test_that("setup_parallel_backend parallel returns list with map and cleanup", {
  mock_plan        <- mock(NULL, cycle = TRUE)
  mock_cores_nb    <- mock(2)
  mock_future_lapply <- mock(list(1, 2), cycle = TRUE)

  stub(setup_parallel_backend, "future::plan",               mock_plan)
  stub(setup_parallel_backend, "get_cores_nb",               mock_cores_nb)
  stub(
    setup_parallel_backend,
    "future.apply::future_lapply",
    mock_future_lapply
  )

  result <- setup_parallel_backend(n_tasks = 2, parallel = TRUE, cores = 2)

  expect_type(result, "list")
  expect_type(result$map, "closure")
  expect_type(result$cleanup, "closure")
})

test_that(
  "setup_parallel_backend parallel calls future::plan with multisession",
  {
    mock_plan     <- mock(NULL, cycle = TRUE)
    mock_cores_nb <- mock(2)

    stub(setup_parallel_backend, "future::plan",    mock_plan)
    stub(setup_parallel_backend, "get_cores_nb",    mock_cores_nb)

    setup_parallel_backend(n_tasks = 2, parallel = TRUE, cores = 2)

    expect_called(mock_plan, 1)
    args <- mock_args(mock_plan)[[1]]
    expect_true(
      inherits(args[[1]], "multisession") ||
        identical(args[[1]], future::multisession)
    )
  }
)

test_that("setup_parallel_backend parallel limits workers to n_tasks", {
  mock_plan     <- mock(NULL, cycle = TRUE)
  mock_cores_nb <- mock(8)   # 8 cores disponibles

  stub(setup_parallel_backend, "future::plan",    mock_plan)
  stub(setup_parallel_backend, "get_cores_nb",    mock_cores_nb)

  setup_parallel_backend(n_tasks = 2, parallel = TRUE, cores = NA)

  args <- mock_args(mock_plan)[[1]]
  expect_identical(args$workers, 2)
})

# ===========================================================================
# Tests: parallelizable_loop
# ===========================================================================

test_that("parallelizable_loop applies fun to each task sequentially", {
  result <- parallelizable_loop(
    n_tasks  = 3,
    parallel = FALSE,
    cores    = NA,
    fun      = function(i) i ^ 2
  )
  expect_identical(result, list(1, 4, 9))
})

test_that("parallelizable_loop returns a list of length n_tasks", {
  result <- parallelizable_loop(
    n_tasks  = 5,
    parallel = FALSE,
    cores    = NA,
    fun      = function(i) i
  )
  expect_length(result, 5)
})

test_that("parallelizable_loop calls cleanup on exit", {
  cleanup_called <- FALSE

  cleanup_env <- new.env(parent = emptyenv())
  assign("cleanup_called", FALSE, envir = cleanup_env)

  mock_backend <- list(
    map     = function(x, fun) lapply(x, fun),
    cleanup = function() {
      assign("cleanup_called", TRUE, envir = cleanup_env)
    }
  )
  stub(
    parallelizable_loop,
    "setup_parallel_backend",
    function(...) mock_backend
  )

  parallelizable_loop(3, FALSE, NA, function(i) i)

  expect_true(cleanup_env$cleanup_called)
})

test_that("parallelizable_loop calls cleanup even if fun throws an error", {
  cleanup_called <- FALSE

  cleanup_env <- new.env(parent = emptyenv())
  assign("cleanup_called", FALSE, envir = cleanup_env)

  mock_backend <- list(
    map     = function(x, fun) lapply(x, fun),
    cleanup = function() {
      assign("cleanup_called", TRUE, envir = cleanup_env)
    }
  )
  stub(
    parallelizable_loop,
    "setup_parallel_backend",
    function(...) mock_backend
  )

  try(
    parallelizable_loop(1, FALSE, NA, function(i) stop("oops", call. = FALSE)),
    silent = TRUE
  )

  expect_true(cleanup_env$cleanup_called)
})

test_that("parallelizable_loop passes correct task indices to fun", {
  indices_env <- new.env(parent = emptyenv())
  assign("indices", NULL, envir = indices_env)

  parallelizable_loop(
    n_tasks = 4,
    parallel = FALSE,
    cores = NA,
    fun = function(i) {
      assign("indices", c(indices_env$indices, i), envir = indices_env)
      i
    }
  )
  expect_identical(indices_env$indices, 1:4)
})
