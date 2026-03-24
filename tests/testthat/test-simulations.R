# ===========================================================================
# Tests: run_simulations
# ===========================================================================

test_that(
  "run_simulations calls stics_wrapper_options with correct arguments",
  {
    mock_options <- mock("fake_options")
    mock_wrapper <- mock(list(sim_list = list()))

    stub(run_simulations, "SticsOnR::stics_wrapper_options", mock_options)
    stub(run_simulations, "SticsOnR::stics_wrapper",         mock_wrapper)

    run_simulations(
      stics_exe = file.path("path", "to", "stics"),
      workspace = file.path("path", "to", "workspace"),
      usm_names = c("usm1", "usm2"),
      successive = list(c("usm1", "usm2")),
      verbose = FALSE
    )

    expect_called(mock_options, 1)
    args <- mock_args(mock_options)[[1]]
    expect_identical(args$stics_exe, file.path("path", "to", "stics"))
    expect_identical(args$workspace, file.path("path", "to", "workspace"))
    expect_identical(args$successive, list(c("usm1", "usm2")))
    expect_false(args$verbose)
    expect_false(args$parallel)
    expect_identical(args$cores, NA)
  }
)

test_that("run_simulations passes usm_names as situation to stics_wrapper", {
  mock_options <- mock("fake_options")
  mock_wrapper <- mock(list(sim_list = list()))

  stub(run_simulations, "SticsOnR::stics_wrapper_options", mock_options)
  stub(run_simulations, "SticsOnR::stics_wrapper", mock_wrapper)

  run_simulations(
    stics_exe = file.path("path", "to", "stics"),
    workspace = file.path("path", "to", "workspace"),
    usm_names = c("usm1", "usm2"),
    successive = NULL,
    verbose = FALSE
  )

  expect_called(mock_wrapper, 1)
  args <- mock_args(mock_wrapper)[[1]]
  expect_identical(args$situation, c("usm1", "usm2"))
})

test_that("run_simulations returns sim_list from stics_wrapper result", {
  fake_sim_list <- list(usm1 = data.frame(x = 1), usm2 = data.frame(x = 2))

  stub(run_simulations, "SticsOnR::stics_wrapper_options", mock(NULL))
  stub(
    run_simulations,
    "SticsOnR::stics_wrapper",
    mock(list(sim_list = fake_sim_list))
  )

  result <- run_simulations(
    stics_exe = file.path("path", "to", "stics"),
    workspace = file.path("path", "to", "workspace"),
    usm_names = c("usm1", "usm2"),
    successive = NULL,
    verbose = FALSE
  )

  expect_identical(result, fake_sim_list)
})

test_that(
  "run_simulations passes parallel and cores to stics_wrapper_options",
  {
    mock_options <- mock("fake_options")
    stub(run_simulations, "SticsOnR::stics_wrapper_options", mock_options)
    stub(
      run_simulations,
      "SticsOnR::stics_wrapper",
      mock(list(sim_list = list()))
    )

    run_simulations(
      stics_exe = file.path("path", "to", "stics"),
      workspace = file.path("path", "to", "workspace"),
      usm_names = "usm1",
      successive = NULL,
      verbose = FALSE,
      parallel = TRUE,
      cores = 4
    )

    args <- mock_args(mock_options)[[1]]
    expect_true(args$parallel)
    expect_identical(args$cores, 4)
  }
)

test_that(
  "run_simulations passes verbose to time_display in stics_wrapper_options",
  {
    mock_options <- mock("fake_options")
    stub(run_simulations, "SticsOnR::stics_wrapper_options", mock_options)
    stub(
      run_simulations,
      "SticsOnR::stics_wrapper",
      mock(list(sim_list = list()))
    )

    run_simulations(
      stics_exe = file.path("file", "to", "stics"),
      workspace = file.path("path", "to", "workspace"),
      usm_names = "usm1",
      successive = NULL,
      verbose = TRUE
    )

    args <- mock_args(mock_options)[[1]]
    expect_true(args$time_display)
  }
)
