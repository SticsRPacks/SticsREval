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
  ...
) {
  USMSWorkspace$new(
    usms_workspace = usms_workspace,
    workspace = workspace,
    backend = backend,
    ...
  )
}

# ---- load ----

test_that("load discovers USMs from usms_workspace subdirectories", {
  ws_dir <- withr::local_tempdir()
  dir.create(file.path(ws_dir, "usm1"))
  dir.create(file.path(ws_dir, "usm2"))

  workspace <- make_fake_workspace()
  workspace$remove_init_obs <- function() {}
  loader <- make_loader(
    usms_workspace = ws_dir,
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
