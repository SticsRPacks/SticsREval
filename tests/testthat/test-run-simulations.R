# ---- get_usms_to_simulate ----

test_that("get_usms_to_simulate returns all USMs when usms_files is NULL", {
  ws_dir <- withr::local_tempdir()
  dir.create(file.path(ws_dir, "usm1"))
  dir.create(file.path(ws_dir, "usm2"))
  file.create(file.path(ws_dir, "not_a_usm.txt"))

  expect_setequal(get_usms_to_simulate(ws_dir, NULL), c("usm1", "usm2"))
})

test_that("get_usms_to_simulate restricts to the USMs listed in usms_files", {
  ws_dir <- withr::local_tempdir()
  dir.create(file.path(ws_dir, "usm1"))
  dir.create(file.path(ws_dir, "usm2"))

  list_file <- withr::local_tempfile()
  writeLines("usm1", list_file)

  expect_identical(get_usms_to_simulate(ws_dir, list_file), "usm1")
})

test_that("get_usms_to_simulate errors when a listed USM is missing", {
  ws_dir <- withr::local_tempdir()
  dir.create(file.path(ws_dir, "usm1"))

  list_file <- withr::local_tempfile()
  writeLines(c("usm1", "usm2"), list_file)

  expect_error(
    get_usms_to_simulate(ws_dir, list_file),
    "The following USMs are not found in the workspace: usm2"
  )
})

# ---- get_obs_files ----

test_that("get_obs_files returns the observations from SticsRFiles::get_obs", {
  fake_obs <- list(usm1 = data.frame(Date = 1, LAI = 1))

  seen <- new.env()
  mockery::stub(
    get_obs_files, "SticsRFiles::get_obs",
    function(workspace, usm, verbose, parallel, cores) {
      seen$workspace <- workspace
      seen$usm <- usm
      fake_obs
    }
  )

  result <- get_obs_files("ws", c("usm1", "usm2"), FALSE, NA)

  expect_identical(result, fake_obs)
  expect_identical(seen$workspace, "ws")
  expect_identical(seen$usm, c("usm1", "usm2"))
})

# ---- get_var_from_obs ----

test_that("get_var_from_obs excludes metadata columns from var names", {
  fake_obs <- list(
    usm1 = data.frame(
      Date = 1, situation = "usm1", species = "wheat", version = "v",
      Plant = 1, LAI = 1, MASEC = 1,
      stringsAsFactors = FALSE
    ),
    usm2 = data.frame(
      Date = 1, situation = "usm2", species = "wheat", version = "v",
      Plant = 1, HR_1 = 1,
      stringsAsFactors = FALSE
    )
  )

  result <- get_var_from_obs(fake_obs)
  expect_setequal(result, c("LAI", "MASEC", "HR_1"))
})

# ---- run_simulations ----

make_run_simulations_fixture <- function(.local_envir = parent.frame()) {
  ws_dir <- withr::local_tempdir(.local_envir = .local_envir)
  dir.create(file.path(ws_dir, "usm1"))
  dir.create(file.path(ws_dir, "usm2"))

  stics_exe <- withr::local_tempfile(.local_envir = .local_envir)
  file.create(stics_exe)

  metadata_file <- withr::local_tempfile(
    fileext = ".csv", .local_envir = .local_envir
  )
  writeLines("usm;rotation;rotation_order", metadata_file)

  list(
    ws_dir = ws_dir,
    stics_exe = stics_exe,
    metadata_file = metadata_file,
    output_dir = withr::local_tempdir(.local_envir = .local_envir)
  )
}

test_that("run_simulations runs simulations and saves sim and obs as RDS", {
  fx <- make_run_simulations_fixture()

  sim_data <- list(
    usm1 = data.frame(Date = as.Date("2024-01-01"), LAI = 1),
    usm2 = data.frame(Date = as.Date("2024-01-01"), LAI = 2)
  )
  obs_data <- list(
    usm1 = data.frame(Date = as.Date("2024-01-01"), LAI = 0.9),
    usm2 = data.frame(Date = as.Date("2024-01-01"), LAI = 1.9)
  )

  seen <- new.env()
  fake_workspace <- list(
    run_simulations = function(usms, var) {
      seen$usms <- usms
      seen$var <- var
      sim_data
    }
  )

  mockery::stub(
    run_simulations, "USMSWorkspace$new",
    function(config) fake_workspace
  )
  mockery::stub(
    run_simulations, "get_obs_files",
    function(...) obs_data
  )
  mockery::stub(
    run_simulations, "get_var_from_obs",
    function(...) "LAI"
  )

  result <- run_simulations(
    stics_exe = fx$stics_exe,
    usms_workspace = fx$ws_dir,
    metadata_file = fx$metadata_file,
    output_dir = fx$output_dir
  )

  expect_setequal(seen$usms, c("usm1", "usm2"))
  expect_identical(seen$var, "LAI")
  expect_identical(result, list(sim = sim_data, obs = obs_data))

  sim_file <- file.path(fx$output_dir, "simulations.rds")
  expect_true(file.exists(sim_file))
  expect_identical(readRDS(sim_file), sim_data)

  obs_file <- file.path(fx$output_dir, "observations.rds")
  expect_true(file.exists(obs_file))
  expect_identical(readRDS(obs_file), obs_data)
})

test_that(
  "run_simulations uses the explicit vars argument, bypassing
  get_var_from_obs",
  {
    fx <- make_run_simulations_fixture()

    sim_data <- list(
      usm1 = data.frame(Date = as.Date("2024-01-01"), init_H2O_balance = 1)
    )
    obs_data <- list(
      usm1 = data.frame(Date = as.Date("2024-01-01"), LAI = 0.9)
    )

    seen <- new.env()
    seen$get_var_from_obs_called <- FALSE
    fake_workspace <- list(
      run_simulations = function(usms, var) {
        seen$var <- var
        sim_data
      }
    )

    mockery::stub(
      run_simulations, "USMSWorkspace$new",
      function(config) fake_workspace
    )
    mockery::stub(
      run_simulations, "get_obs_files",
      function(...) obs_data
    )
    mockery::stub(
      run_simulations, "get_var_from_obs",
      function(...) {
        seen$get_var_from_obs_called <- TRUE
        "LAI"
      }
    )

    run_simulations(
      stics_exe = fx$stics_exe,
      usms_workspace = fx$ws_dir,
      metadata_file = fx$metadata_file,
      output_dir = fx$output_dir,
      vars = c("init_H2O_balance", "final_H2O_balance")
    )

    expect_identical(seen$var, c("init_H2O_balance", "final_H2O_balance"))
    expect_false(seen$get_var_from_obs_called)
  }
)

test_that("run_simulations restricts to USMs listed in usms_files", {
  fx <- make_run_simulations_fixture()

  list_file <- withr::local_tempfile()
  writeLines("usm1", list_file)

  seen <- new.env()
  fake_workspace <- list(
    run_simulations = function(usms, var) {
      seen$usms <- usms
      list(usm1 = data.frame(Date = as.Date("2024-01-01"), LAI = 1))
    }
  )

  mockery::stub(
    run_simulations, "USMSWorkspace$new",
    function(config) fake_workspace
  )
  mockery::stub(
    run_simulations, "get_obs_files",
    function(...) list(usm1 = data.frame(Date = as.Date("2024-01-01")))
  )
  mockery::stub(
    run_simulations, "get_var_from_obs",
    function(...) "LAI"
  )

  run_simulations(
    stics_exe = fx$stics_exe,
    usms_workspace = fx$ws_dir,
    metadata_file = fx$metadata_file,
    output_dir = fx$output_dir,
    usms_files = list_file
  )

  expect_identical(seen$usms, "usm1")
})

test_that("run_simulations errors when stics_exe does not exist on disk", {
  fx <- make_run_simulations_fixture()

  expect_error(
    run_simulations(
      stics_exe = file.path("nonexistent", "stics.exe"),
      usms_workspace = fx$ws_dir,
      metadata_file = fx$metadata_file,
      output_dir = fx$output_dir
    ),
    "stics_exe not found"
  )
})
