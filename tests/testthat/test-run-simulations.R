# ---- get_usms_to_simulate ----

test_that("get_usms_to_simulate returns all USMs when wanted_usms is NULL", {
  ws_dir <- withr::local_tempdir()
  dir.create(file.path(ws_dir, "usm1"))
  dir.create(file.path(ws_dir, "usm2"))
  file.create(file.path(ws_dir, "not_a_usm.txt"))

  expect_setequal(get_usms_to_simulate(ws_dir, NULL), c("usm1", "usm2"))
})

test_that("get_usms_to_simulate restricts to the wanted USMs", {
  ws_dir <- withr::local_tempdir()
  dir.create(file.path(ws_dir, "usm1"))
  dir.create(file.path(ws_dir, "usm2"))

  expect_identical(get_usms_to_simulate(ws_dir, c("usm1", "usm1")), "usm1")
})

test_that("get_usms_to_simulate warns and skips missing wanted USMs", {
  ws_dir <- withr::local_tempdir()
  dir.create(file.path(ws_dir, "usm1"))

  logger::log_threshold(logger::WARN)
  on.exit(logger::log_threshold(logger::FATAL), add = TRUE)
  log_env <- make_log_capture()

  expect_identical(get_usms_to_simulate(ws_dir, c("usm1", "usm2")), "usm1")
  expect_match(log_env$logs, "not found in .*: usm2", all = FALSE)
})

test_that("get_usms_to_simulate errors when no wanted USM is found", {
  ws_dir <- withr::local_tempdir()
  dir.create(file.path(ws_dir, "usm1"))

  expect_error(
    get_usms_to_simulate(ws_dir, "usm2"),
    "None of the requested USMs are found in"
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
  mockery::stub(
    run_simulations, "run_stics_simulations",
    function(usms, var, ...) {
      seen$usms <- usms
      seen$var <- var
      sim_data
    }
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
    mockery::stub(
      run_simulations, "run_stics_simulations",
      function(usms, var, ...) {
        seen$var <- var
        sim_data
      }
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

stub_restricted_run <- function(seen) {
  mockery::stub(
    run_simulations, "run_stics_simulations",
    function(usms, ...) {
      seen$usms <- usms
      list(usm1 = data.frame(Date = as.Date("2024-01-01"), LAI = 1))
    }
  )
  mockery::stub(
    run_simulations, "get_obs_files",
    function(...) list(usm1 = data.frame(Date = as.Date("2024-01-01")))
  )
  mockery::stub(
    run_simulations, "get_var_from_obs",
    function(...) "LAI"
  )
  run_simulations
}

test_that("run_simulations restricts to the USMs given in usms", {
  fx <- make_run_simulations_fixture()
  seen <- new.env()
  run_sims <- stub_restricted_run(seen)

  run_sims(
    stics_exe = fx$stics_exe,
    usms_workspace = fx$ws_dir,
    metadata_file = fx$metadata_file,
    output_dir = fx$output_dir,
    usms = "usm1"
  )

  expect_identical(seen$usms, "usm1")
})

test_that("run_simulations still accepts the deprecated usms_files", {
  fx <- make_run_simulations_fixture()
  seen <- new.env()
  run_sims <- stub_restricted_run(seen)

  list_file <- withr::local_tempfile()
  writeLines("usm1", list_file)

  lifecycle::expect_deprecated(
    run_sims(
      stics_exe = fx$stics_exe,
      usms_workspace = fx$ws_dir,
      metadata_file = fx$metadata_file,
      output_dir = fx$output_dir,
      usms_files = list_file
    )
  )

  expect_identical(seen$usms, "usm1")
})

test_that("run_simulations errors when both usms and usms_files are given", {
  fx <- make_run_simulations_fixture()
  list_file <- withr::local_tempfile()
  writeLines("usm1", list_file)

  expect_error(
    suppressWarnings(run_simulations(
      stics_exe = fx$stics_exe,
      usms_workspace = fx$ws_dir,
      metadata_file = fx$metadata_file,
      output_dir = fx$output_dir,
      usms = "usm1",
      usms_files = list_file
    )),
    "`usms` and `usms_files` can't both be supplied"
  )
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

# ---- get_rotation_list ----

write_metadata <- function(content, .local_envir = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = .local_envir)
  writeLines(content, path)
  path
}

test_that("get_rotation_list errors when metadata file does not exist", {
  expect_error(
    get_rotation_list(file.path("nonexistent", "path.csv")),
    "Metadata file not found"
  )
})

test_that("get_rotation_list errors on missing required columns", {
  path <- write_metadata("usm;other_col\nusm1;val")
  expect_error(get_rotation_list(path), "Missing columns in metadata file")
})

test_that("get_rotation_list errors when rotation_order is non-numeric", {
  path <- write_metadata(c(
    "usm;rotation;rotation_order",
    "usm1;rot1;1",
    "usm2;rot1;abc",
    "usm3;rot2;"
  ))
  expect_error(
    get_rotation_list(path),
    "Column must be numeric: rotation_order \\(invalid values for USMs usm2\\)"
  )
})

test_that("get_rotation_list returns empty list when no rows", {
  path <- write_metadata("usm;rotation;rotation_order")
  expect_identical(get_rotation_list(path), list())
})

test_that("get_rotation_list excludes rows where rotation is NA or '0'", {
  path <- write_metadata(c(
    "usm;rotation;rotation_order",
    "usm1;rot1;1",
    "usm2;0;2",
    "usm3;;3"
  ))

  usms_in_rotations <- unlist(get_rotation_list(path))
  expect_false("usm2" %in% usms_in_rotations)
  expect_false("usm3" %in% usms_in_rotations)
})

test_that("get_rotation_list groups USMs by rotation in order", {
  path <- write_metadata(c(
    "usm;rotation;rotation_order",
    "usm2;rot1;2",
    "usm1;rot1;1",
    "usm3;rot2;1"
  ))

  result <- get_rotation_list(path)
  expect_length(result, 2)
  expect_identical(result[[1]], c("usm1", "usm2"))
  expect_identical(result[[2]], "usm3")
})

test_that("get_rotation_list handles single-USM rotations", {
  path <- write_metadata(c(
    "usm;rotation;rotation_order",
    "usm1;rot1;1"
  ))
  expect_identical(get_rotation_list(path), list("usm1"))
})

test_that("get_rotation_list keeps only rotations fully in usm_names", {
  path <- write_metadata(c(
    "usm;rotation;rotation_order",
    "usm1;rot1;1",
    "usm2;rot1;2",
    "usm3;rot2;1"
  ))
  expect_identical(
    get_rotation_list(path, usm_names = c("usm1", "usm3")),
    list("usm3")
  )
})

# ---- run_stics_simulations ----

run_stics_sims <- function(usms = "usm1", var = "LAI") {
  run_stics_simulations(
    usms_workspace = "ws", stics_exe = "stics", metadata_file = "meta.csv",
    usms = usms, var = var, parallel = FALSE, cores = NA
  )
}

test_that("run_stics_simulations returns sim_list from the Stics wrapper", {
  fake_sim <- list(usm1 = data.frame(Date = 1, LAI = 1))
  seen <- new.env()
  local_mocked_bindings(get_rotation_list = function(...) list("usm1"))
  local_mocked_bindings(
    stics_wrapper_options = function(...) {
      seen$options <- list(...)
      list()
    },
    stics_wrapper = function(...) list(error = FALSE, sim_list = fake_sim),
    .package = "SticsOnR"
  )

  expect_identical(run_stics_sims(), fake_sim)
  expect_identical(seen$options$successive, list("usm1"))
})

test_that("run_stics_simulations catches and rethrows Stics wrapper errors", {
  local_mocked_bindings(get_rotation_list = function(...) list())
  local_mocked_bindings(
    stics_wrapper_options = function(...) list(),
    stics_wrapper = function(...) stop("boom", call. = FALSE),
    .package = "SticsOnR"
  )
  logger::log_threshold(logger::ERROR)
  on.exit(logger::log_threshold(logger::FATAL), add = TRUE)
  logs <- make_log_capture()

  expect_error(run_stics_sims(), "Error while running simulations: boom")
  expect_true(any(grepl("Stics wrapper failed.*boom", logs$logs)))
})

test_that("run_stics_simulations errors when the wrapper reports an error", {
  local_mocked_bindings(get_rotation_list = function(...) list())
  local_mocked_bindings(
    stics_wrapper_options = function(...) list(),
    stics_wrapper = function(...) list(error = TRUE, sim_list = list()),
    .package = "SticsOnR"
  )

  expect_error(run_stics_sims(), "Error while running simulations")
})
