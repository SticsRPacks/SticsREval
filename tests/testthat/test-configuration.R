# ---- Helpers ----

make_cfg <- function(...) {
  defaults <- list(
    stics_exe       = NULL,
    usms_workspace  = NULL,
    metadata_file   = NULL,
    run_simulations = TRUE,
    verbose         = 0,      # suppress logger output in tests
    parallel        = FALSE,
    cores           = NA,
    reference_version = NULL,
    percentage      = 5,
    eval_workspace  = NULL,
    init_workspace  = FALSE,  # avoid triggering workspace checks by default
    output_dir      = NULL,
    species         = NULL,
    usms            = NULL,
    var2exclude     = NULL
  )
  do.call(Configuration$new, utils::modifyList(defaults, list(...)))
}

# ---- initialize ----

test_that("stores all fields correctly", {
  cfg <- make_cfg(
    stics_exe        = "/stics",
    usms_workspace   = "/usms",
    metadata_file    = NULL,
    run_simulations  = FALSE,
    parallel         = TRUE,
    cores            = 4,
    reference_version = "v1",
    percentage       = 10,
    eval_workspace   = "/ws",
    init_workspace   = FALSE,
    output_dir       = "/out",
    species          = c("wheat", "barley"),
    usms             = c("usm1"),
    var2exclude      = c("LAI")
  )

  expect_equal(cfg$stics_exe,         "/stics")
  expect_equal(cfg$usms_workspace,    "/usms")
  expect_false(cfg$run_simulations)
  expect_true(cfg$parallel)
  expect_equal(cfg$cores,             4)
  expect_equal(cfg$reference_version, "v1")
  expect_equal(cfg$percentage,        10)
  expect_equal(cfg$eval_workspace,    "/ws")
  expect_equal(cfg$output_dir,        "/out")
  expect_equal(cfg$species,           c("wheat", "barley"))
  expect_equal(cfg$usms,              c("usm1"))
  expect_equal(cfg$var2exclude,       c("LAI"))
})

test_that("default values are set correctly", {
  cfg <- make_cfg()

  expect_true(cfg$run_simulations)
  expect_false(cfg$parallel)
  expect_true(is.na(cfg$cores))
  expect_null(cfg$reference_version)
  expect_equal(cfg$percentage, 5)
  expect_null(cfg$output_dir)
  expect_null(cfg$species)
  expect_null(cfg$usms)
  expect_null(cfg$var2exclude)
})

# ---- validate_eval ----

test_that("validate_eval passes when init_workspace is FALSE and eval_workspace is set", {
  cfg <- make_cfg(eval_workspace = "/ws", init_workspace = FALSE)
  expect_invisible(cfg$validate_eval())
})

test_that("validate_eval errors when eval_workspace is NULL", {
  cfg <- make_cfg(eval_workspace = NULL, init_workspace = FALSE)
  expect_error(cfg$validate_eval(), "Eval workspace path must be defined")
})

test_that("validate_eval errors when init_workspace TRUE and stics_exe is NULL", {
  cfg <- make_cfg(
    init_workspace = TRUE,
    eval_workspace = "/ws",
    stics_exe      = NULL,
    usms_workspace = "/usms",
    metadata_file  = NULL
  )
  expect_error(cfg$validate_eval(), "Stics executable path must be defined")
})

test_that("validate_eval errors when init_workspace TRUE and usms_workspace is NULL", {
  cfg <- make_cfg(
    init_workspace = TRUE,
    eval_workspace = "/ws",
    stics_exe      = "/stics",
    usms_workspace = NULL,
    metadata_file  = NULL
  )
  expect_error(cfg$validate_eval(), "USMs workspace path must be defined")
})

test_that("validate_eval errors when metadata_file is NULL", {
  cfg <- make_cfg(
    init_workspace = TRUE,
    eval_workspace = "/ws",
    stics_exe      = "/stics",
    usms_workspace = "/usms",
    metadata_file  = NULL
  )
  expect_error(cfg$validate_eval(), "Metadata file must be a valid path")
})

test_that("validate_eval errors when metadata_file path does not exist", {
  cfg <- make_cfg(
    init_workspace = TRUE,
    eval_workspace = "/ws",
    stics_exe      = "/stics",
    usms_workspace = "/usms",
    metadata_file  = "/nonexistent/path.csv"
  )
  expect_error(cfg$validate_eval(), "Metadata file must be a valid path")
})

test_that("validate_eval passes when init_workspace TRUE and all paths valid", {
  dir  <- withr::local_tempdir()
  meta <- file.path(dir, "meta.csv")
  file.create(meta)

  cfg <- make_cfg(
    init_workspace = TRUE,
    eval_workspace = dir,
    stics_exe      = "/stics",
    usms_workspace = "/usms",
    metadata_file  = meta
  )
  expect_invisible(cfg$validate_eval())
})

test_that("validate_eval errors when reference_version not in workspace", {
  dir <- withr::local_tempdir()
  arrow::write_parquet(
    data.frame(stics_version = "v1", last_evaluated = TRUE),
    sink = file.path(dir, "metadata.parquet")
  )

  cfg <- make_cfg(
    eval_workspace    = dir,
    init_workspace    = FALSE,
    reference_version = "v99"
  )
  expect_error(cfg$validate_eval(), "Reference version is not present")
})

test_that("validate_eval passes when reference_version is in workspace", {
  dir <- withr::local_tempdir()
  arrow::write_parquet(
    data.frame(stics_version = "v1", last_evaluated = TRUE),
    sink = file.path(dir, "metadata.parquet")
  )

  cfg <- make_cfg(
    eval_workspace    = dir,
    init_workspace    = FALSE,
    reference_version = "v1"
  )
  expect_invisible(cfg$validate_eval())
})

# ---- validate_export ----

test_that("validate_export errors when output_dir is NULL", {
  cfg <- make_cfg(eval_workspace = "/ws", output_dir = NULL)
  expect_error(cfg$validate_export(), "Output dir path must be defined")
})

test_that("validate_export errors when eval_workspace is NULL", {
  dir <- withr::local_tempdir()
  cfg <- make_cfg(output_dir = dir, eval_workspace = NULL)
  expect_error(cfg$validate_export(), "Eval workspace path must be defined")
})

test_that("validate_export passes when output_dir exists", {
  dir <- withr::local_tempdir()
  cfg <- make_cfg(output_dir = dir, eval_workspace = "/ws")
  expect_invisible(cfg$validate_export())
})

test_that("validate_export creates output_dir if it does not exist", {
  dir     <- withr::local_tempdir()
  new_dir <- file.path(dir, "new_output")

  cfg <- make_cfg(output_dir = new_dir, eval_workspace = "/ws")
  cfg$validate_export()

  expect_true(dir.exists(new_dir))
})