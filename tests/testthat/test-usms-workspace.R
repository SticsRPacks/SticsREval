# ---- Helpers ----

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

    save_species_usm = function(...) env$species_usm_saved <- TRUE,

    .env = env
  )
}

make_fake_backend <- function() {
  list(
    run = function(n, fun) lapply(seq_len(n), fun),
    parallel = FALSE,
    cores = 1
  )
}

make_loader <- function(
  workspace = make_fake_workspace(),
  backend = make_fake_backend(),
  usms_workspace = "usms_ws",
  metadata_file = NULL,
  ...
) {
  USMSWorkspace$new(
    usms_workspace = usms_workspace,
    metadata_file = metadata_file,
    workspace = workspace,
    backend = backend,
    ...
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
  loader <- make_loader(
    metadata_file = file.path("nonexistent", "path.csv")
  )
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
  expect_identical(result, list())
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
  expect_identical(result[[1]], c("usm1", "usm2"))
  expect_identical(result[[2]], "usm3")
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
  expect_identical(result[[1]], "usm1")
})

# ---- load ----

test_that("load discovers USMs from usms_workspace subdirectories", {
  ws_dir <- withr::local_tempdir()
  dir.create(file.path(ws_dir, "usm1"))
  dir.create(file.path(ws_dir, "usm2"))

  meta_path <- withr::local_tempfile(fileext = ".csv")
  write_metadata(meta_path, "usm;rotation;rotation_order")

  workspace <- make_fake_workspace()
  workspace$remove_init_obs <- function() {}
  loader <- make_loader(
    usms_workspace = ws_dir,
    metadata_file = meta_path,
    workspace = workspace
  )

  seen_usms <- new.env()
  seen_usms$value <- NULL

  replace_private(loader, "extract_species_from_usms", function(usms) {
    seen_usms$value <- usms
    data.frame(usm = usms, species = "wheat", stringsAsFactors = FALSE)
  })

  replace_private(loader, "load_sim", function(...) invisible(NULL))
  replace_private(loader, "load_obs", function(...) invisible(NULL))
  replace_private(loader, "load_ref_sim", function(...) invisible(NULL))

  loader$load()

  expect_setequal(seen_usms$value, c("usm1", "usm2"))
})
