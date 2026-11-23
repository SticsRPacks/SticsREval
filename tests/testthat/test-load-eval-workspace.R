# ---- Helpers ----

# EvalWorkspace mock recording what is saved, in call order. The species/USM
# association saved by save_species_usm() is served back by
# get_species_situations().
make_fake_workspace <- function() {
  env <- new.env()
  env$calls <- character(0)
  env$saved <- list()

  record <- function(name) {
    function(data, species_situations) {
      env$calls <- c(env$calls, name)
      env$saved[[name]] <- data
    }
  }

  list(
    save_species_usm = function(usms_species) {
      env$calls <- c(env$calls, "species_usm")
      env$species_usm <- usms_species
    },
    get_species_situations = function(species = NULL) env$species_usm,
    save_obs = record("obs"),
    save_sim = record("sim"),
    save_ref_sim = record("ref_sim"),
    remove_init_obs = function() env$calls <- c(env$calls, "remove_init_obs"),
    .env = env
  )
}

make_fake_backend <- function() {
  list(run = function(n, fun) lapply(seq_len(n), fun))
}

# Text workspace with one directory per USM, and sim/obs/ref_sim RDS files
# holding one data frame per USM.
make_load_fixture <- function(
  usms = c("usm1", "usm2"), .local_envir = parent.frame()
) {
  ws_dir <- withr::local_tempdir(.local_envir = .local_envir)
  for (usm in usms) dir.create(file.path(ws_dir, usm))

  write_rds <- function(value) {
    per_usm <- stats::setNames(
      lapply(usms, function(u) data.frame(Date = 1, LAI = value)), usms
    )
    path <- withr::local_tempfile(fileext = ".rds", .local_envir = .local_envir)
    saveRDS(per_usm, path)
    path
  }

  list(
    ws_dir = ws_dir,
    sim_rds = write_rds(1),
    obs_rds = write_rds(2),
    ref_sim_rds = write_rds(3)
  )
}

# Stubs SticsRFiles: every USM is a wheat USM.
local_wheat_species <- function(.local_envir = parent.frame()) {
  local_mocked_bindings(
    extract_species_from_usms = function(usms_workspace, usms, backend) {
      data.frame(species = "wheat", situation = usms, stringsAsFactors = FALSE)
    },
    .env = .local_envir
  )
}

# ---- load_eval_workspace ----

test_that("load_eval_workspace loads all the USMs of the workspace", {
  fx <- make_load_fixture()
  local_wheat_species()
  workspace <- make_fake_workspace()

  result <- load_eval_workspace(
    usms_workspace = fx$ws_dir,
    sim_rds = fx$sim_rds,
    obs_rds = fx$obs_rds,
    ref_sim_rds = fx$ref_sim_rds,
    workspace = workspace,
    backend = make_fake_backend()
  )

  env <- workspace$.env
  expect_identical(result, workspace)
  expect_setequal(env$species_usm$situation, c("usm1", "usm2"))
  expect_identical(
    env$calls,
    c("species_usm", "obs", "sim", "ref_sim", "remove_init_obs")
  )
  expect_setequal(names(env$saved$sim), c("usm1", "usm2"))
  expect_identical(env$saved$obs$usm1$LAI, 2)
  expect_identical(env$saved$ref_sim$usm1$LAI, 3)
})

test_that("load_eval_workspace skips reference simulations when not given", {
  fx <- make_load_fixture()
  local_wheat_species()
  workspace <- make_fake_workspace()

  load_eval_workspace(
    usms_workspace = fx$ws_dir,
    sim_rds = fx$sim_rds,
    obs_rds = fx$obs_rds,
    workspace = workspace,
    backend = make_fake_backend()
  )

  expect_identical(
    workspace$.env$calls,
    c("species_usm", "obs", "sim", "remove_init_obs")
  )
})

test_that("load_eval_workspace restricts to the requested USMs", {
  fx <- make_load_fixture()
  local_wheat_species()
  workspace <- make_fake_workspace()
  logger::log_threshold(logger::WARN)
  on.exit(logger::log_threshold(logger::FATAL), add = TRUE)
  logs <- make_log_capture()

  load_eval_workspace(
    usms_workspace = fx$ws_dir,
    sim_rds = fx$sim_rds,
    obs_rds = fx$obs_rds,
    usms = c("usm1", "usm3"),
    workspace = workspace,
    backend = make_fake_backend()
  )

  env <- workspace$.env
  expect_identical(env$species_usm$situation, "usm1")
  expect_named(env$saved$sim, "usm1")
  expect_named(env$saved$obs, "usm1")
  expect_match(logs$logs, "not found in .*: usm3", all = FALSE)
})

test_that("load_eval_workspace errors when no requested USM is found", {
  fx <- make_load_fixture()
  local_wheat_species()

  expect_error(
    load_eval_workspace(
      usms_workspace = fx$ws_dir,
      sim_rds = fx$sim_rds,
      obs_rds = fx$obs_rds,
      usms = "usm3",
      workspace = make_fake_workspace(),
      backend = make_fake_backend()
    ),
    "None of the requested USMs are found in"
  )
})

# ---- extract_species_from_usms ----

test_that("extract_species_from_usms reads the species of each USM", {
  species <- c(usm1 = "wheat", usm2 = "maize")
  local_mocked_bindings(
    get_plant_txt = function(workspace) {
      list(codeplante = species[[basename(workspace)]])
    },
    .package = "SticsRFiles"
  )

  result <- extract_species_from_usms(
    "ws", c("usm1", "usm2"), make_fake_backend()
  )

  expect_identical(result$situation, c("usm1", "usm2"))
  expect_identical(result$species, c("wheat", "maize"))
})
