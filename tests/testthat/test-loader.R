# ---- Helpers ----

replace_private <- function(obj, name, fn) {
  env <- obj$.__enclos_env__$private
  unlockBinding(name, env)
  env[[name]] <- fn
}


make_fake_workspace <- function() {

  env <- new.env()

  list(
    add_evaluated_version = function(v) env$version_added <- v,

    save_sim = function(...) env$sim_saved <- TRUE,
    save_obs = function(...) env$obs_saved <- TRUE,

    set_version = function(v) env$version <- v,
    get_version = function() env$version,

    get_sim_saved = function() env$sim_saved,
    get_obs_saved = function() env$obs_saved,

    .env = env
  )
}

make_fake_backend <- function() {
  list(
    run      = function(n, fun) lapply(seq_len(n), fun),
    parallel = FALSE,
    cores    = 1
  )
}

make_loader <- function(
  workspace      = make_fake_workspace(),
  backend        = make_fake_backend(),
  usms_workspace = withr::local_tempdir(),
  metadata_file  = tempfile(fileext = ".csv"),
  stics_exe      = "/stics",
  run_simulations = FALSE
) {
  WorkspaceLoader$new(
    workspace       = workspace,
    backend         = backend,
    usms_workspace  = usms_workspace,
    metadata_file   = metadata_file,
    stics_exe       = stics_exe,
    run_simulations = run_simulations
  )
}

write_metadata <- function(path, content) {
  writeLines(content, path)
}

access_private <- function(loader, name) {
  loader$.__enclos_env__$private[[name]]
}

call_private <- function(loader, name, ...) {
  loader$.__enclos_env__$private[[name]](...)
}

# ---- get_rotation_list ----

test_that("get_rotation_list errors when metadata file does not exist", {
  loader <- make_loader(metadata_file = "/nonexistent/path.csv")
  expect_error(
    call_private(loader, "get_rotation_list"),
    "Metadata file not found"
  )
})

test_that("get_rotation_list errors on missing required columns", {
  path <- withr::local_tempfile(fileext = ".csv")
  write_metadata(path, "usm;other_col\nusm1;val")

  loader <- make_loader(metadata_file = path)
  expect_error(
    call_private(loader, "get_rotation_list"),
    "Missing columns in metadata file"
  )
})

test_that("get_rotation_list errors when rotation_order is non-numeric", {
  path <- withr::local_tempfile(fileext = ".csv")
  write_metadata(path, "usm;rotation;rotation_order\nusm1;rot1;abc")

  loader <- make_loader(metadata_file = path)
  expect_error(
    call_private(loader, "get_rotation_list"),
    "Column must be numeric: rotation_order"
  )
})

test_that("get_rotation_list returns empty list when no rows", {
  path <- withr::local_tempfile(fileext = ".csv")
  write_metadata(path, "usm;rotation;rotation_order")

  loader <- make_loader(metadata_file = path)
  result <- call_private(loader, "get_rotation_list")
  expect_equal(result, list())
})

test_that("get_rotation_list excludes rows where rotation is NA or '0'", {
  path <- withr::local_tempfile(fileext = ".csv")
  write_metadata(path, c(
    "usm;rotation;rotation_order",
    "usm1;rot1;1",
    "usm2;0;2",
    "usm3;;3"
  ))

  loader <- make_loader(metadata_file = path)
  result <- call_private(loader, "get_rotation_list")

  usms_in_rotations <- unlist(result)
  expect_false("usm2" %in% usms_in_rotations)
  expect_false("usm3" %in% usms_in_rotations)
})

test_that("get_rotation_list groups USMs by rotation in order", {
  path <- withr::local_tempfile(fileext = ".csv")
  write_metadata(path, c(
    "usm;rotation;rotation_order",
    "usm2;rot1;2",
    "usm1;rot1;1",
    "usm3;rot2;1"
  ))

  loader <- make_loader(metadata_file = path)
  result <- call_private(loader, "get_rotation_list")

  expect_length(result, 2)
  expect_equal(result[[1]], c("usm1", "usm2"))  # rot1, ordered by rotation_order
  expect_equal(result[[2]], c("usm3"))           # rot2
})

test_that("get_rotation_list handles single-USM rotations", {
  path <- withr::local_tempfile(fileext = ".csv")
  write_metadata(path, c(
    "usm;rotation;rotation_order",
    "usm1;rot1;1"
  ))

  loader <- make_loader(metadata_file = path)
  result <- call_private(loader, "get_rotation_list")

  expect_length(result, 1)
  expect_equal(result[[1]], "usm1")
})

# ---- load ----

test_that("load sets workspace version to stics version", {

  ws_dir <- withr::local_tempdir()
  dir.create(file.path(ws_dir, "usm1"))

  meta_path <- withr::local_tempfile(fileext = ".csv")
  write_metadata(meta_path, "usm;rotation;rotation_order")

  workspace <- make_fake_workspace()

  loader <- make_loader(
    workspace      = workspace,
    usms_workspace = ws_dir,
    metadata_file  = meta_path
  )

  replace_private(loader, "extract_species_from_usms",
    function(...) data.frame(usm = "usm1", species = "wheat")
  )
  replace_private(loader, "load_stics_version", function() "v1")
  replace_private(loader, "load_sim", function(...) invisible(NULL))
  replace_private(loader, "load_obs", function(...) invisible(NULL))

  loader$load()

  expect_equal(workspace$get_version(), "v1")
})

test_that("load discovers USMs from usms_workspace subdirectories", {
  ws_dir <- withr::local_tempdir()
  dir.create(file.path(ws_dir, "usm1"))
  dir.create(file.path(ws_dir, "usm2"))

  meta_path <- withr::local_tempfile(fileext = ".csv")
  write_metadata(meta_path, "usm;rotation;rotation_order")

  seen_usms <- NULL
  loader    <- make_loader(
    usms_workspace = ws_dir,
    metadata_file  = meta_path
  )

  replace_private(loader, "extract_species_from_usms", function(usms) {
    seen_usms <<- usms
    data.frame(usm = usms, species = "wheat")
  })
  replace_private(loader, "load_stics_version", function() "v1")
  replace_private(loader, "load_sim",           function(...) invisible(NULL))
  replace_private(loader, "load_obs",           function(...) invisible(NULL))

  loader$load()

  expect_setequal(seen_usms, c("usm1", "usm2"))
})
