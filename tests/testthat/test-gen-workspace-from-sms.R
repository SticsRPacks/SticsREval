# ---- Helpers ----

write_typo_usms <- function(sms_path, rows) {
  csv_lines <- c(
    "usm;source",
    vapply(rows, function(r) paste(r$usm, r$source, sep = ";"), character(1))
  )
  writeLines(csv_lines, file.path(sms_path, "typo_usms.csv"))
}

write_sms_fixture <- function(sms_path, stics_path) {
  dir.create(file.path(sms_path, "Obs"))
  writeLines(
    "ian;mo;jo;jul;INN;LAI;MASEC", file.path(sms_path, "Obs", "usm1.obs")
  )

  dir.create(file.path(sms_path, "Soil"))
  file.create(file.path(sms_path, "Soil", "sols.xml"))

  dir.create(file.path(sms_path, "Tec"))
  file.create(file.path(sms_path, "Tec", "usm1_tec.xml"))

  dir.create(file.path(sms_path, "USMs"))
  file.create(file.path(sms_path, "USMs", "usms.xml"))
  file.create(file.path(sms_path, "USMs", "ini_usm1.xml"))

  dir.create(file.path(sms_path, "Climate"))
  file.create(file.path(sms_path, "Climate", "station.2023"))

  model_dir <- file.path(stics_path, "input_files", "model")
  dir.create(model_dir, recursive = TRUE)
  file.create(file.path(
    model_dir,
    c("prof.mod", "rap.mod", "param_gen.xml", "param_newform.xml")
  ))

  plant_dir <- file.path(stics_path, "input_files", "plant")
  dir.create(plant_dir, recursive = TRUE)
  file.create(file.path(plant_dir, "wheat_plt.xml"))
}

# ---- get_header_fields ----

test_that("get_header_fields splits and trims the first line only", {
  f <- withr::local_tempfile()
  writeLines(c(" a ; b; c ", "1;2;3"), f)
  expect_identical(get_header_fields(f), c("a", "b", "c"))
})

# ---- gen_varmod_from_obs ----

test_that("gen_varmod_from_obs writes unique vars, excludes filtered ones", {
  sms_path <- withr::local_tempdir()
  obs_dir <- file.path(sms_path, "Obs")
  dir.create(obs_dir)
  writeLines("ian;mo;jo;jul;INN;LAI;MASEC", file.path(obs_dir, "usm1.obs"))
  writeLines("ian;mo;jo;jul;INN;LAI;HR_1", file.path(obs_dir, "usm2.obs"))

  out_path <- withr::local_tempdir()
  gen_varmod_from_obs(sms_path, out_path)

  out_file <- file.path(out_path, "var.mod")
  expect_true(file.exists(out_file))
  expect_setequal(readLines(out_file), c("LAI", "MASEC", "HR_1"))
})

test_that("gen_varmod_from_obs overwrites an existing var.mod file", {
  sms_path <- withr::local_tempdir()
  obs_dir <- file.path(sms_path, "Obs")
  dir.create(obs_dir)
  writeLines("LAI", file.path(obs_dir, "usm1.obs"))

  out_path <- withr::local_tempdir()
  out_file <- file.path(out_path, "var.mod")
  writeLines("OLD_VAR", out_file)

  gen_varmod_from_obs(sms_path, out_path)

  expect_identical(readLines(out_file), "LAI")
})

# ---- get_sms_usms_list ----

test_that("get_sms_usms_list returns rows with source == 'sms'", {
  sms_path <- withr::local_tempdir()
  write_typo_usms(sms_path, list(
    list(usm = "usm1", source = "sms"),
    list(usm = "usm2", source = "calibration")
  ))

  result <- get_sms_usms_list(sms_path)
  expect_identical(result$usm, "usm1")
})

test_that("get_sms_usms_list errors when the filter file can't be loaded", {
  sms_path <- withr::local_tempdir()
  expect_error(
    suppressWarnings(get_sms_usms_list(sms_path)),
    "Filter file could not be loaded"
  )
})

# ---- extract_sms_data ----

test_that("extract_sms_data copies all required files to the destination dir", {
  sms_path <- withr::local_tempdir()
  stics_path <- withr::local_tempdir()
  destination_dir <- withr::local_tempdir()

  write_sms_fixture(sms_path, stics_path)

  extract_sms_data(sms_path, stics_path, destination_dir)

  expect_true(file.exists(file.path(destination_dir, "usm1.obs")))
  expect_true(file.exists(file.path(destination_dir, "sols.xml")))
  expect_true(file.exists(file.path(destination_dir, "usms.xml")))
  expect_true(file.exists(file.path(destination_dir, "ini_usm1.xml")))
  expect_true(file.exists(file.path(destination_dir, "station.2023")))
  expect_true(file.exists(file.path(destination_dir, "prof.mod")))
  expect_true(file.exists(file.path(destination_dir, "rap.mod")))
  expect_true(file.exists(file.path(destination_dir, "param_gen.xml")))
  expect_true(file.exists(file.path(destination_dir, "param_newform.xml")))
  expect_true(file.exists(file.path(destination_dir, "plant", "wheat_plt.xml")))
})

# ---- set_intercrop_code_shape ----

test_that("set_intercrop_code_shape no-ops without usm_df or plantfile2 col", {
  expect_null(set_intercrop_code_shape("ws", NULL))
  expect_null(set_intercrop_code_shape(
    "ws", data.frame(usm = "usm1", stringsAsFactors = FALSE)
  ))
})

test_that("set_intercrop_code_shape no-ops without intercrop plantfile2", {
  usm_df <- data.frame(
    usm = c("usm1", "usm2"),
    plantfile2 = c(NA, "null"),
    stringsAsFactors = FALSE
  )
  expect_null(set_intercrop_code_shape("ws", usm_df))
})

test_that("set_intercrop_code_shape sets code_shape for existing plant files", {
  ws <- withr::local_tempdir()
  usm_dir <- file.path(ws, "usm1")
  dir.create(usm_dir)
  file.create(file.path(usm_dir, "ficplt1.txt"))
  file.create(file.path(usm_dir, "ficplt2.txt"))

  usm_df <- data.frame(
    usm = c("usm1", "usm2"),
    plantfile2 = c("ble_intercrop.xml", "null"),
    stringsAsFactors = FALSE
  )

  calls <- new.env()
  calls$files <- character(0)
  mockery::stub(
    set_intercrop_code_shape, "SticsRFiles::set_plant_txt",
    function(file, param, value, append, variety) {
      calls$files <- c(calls$files, file)
    }
  )

  set_intercrop_code_shape(ws, usm_df)

  expect_setequal(
    basename(calls$files),
    c("ficplt1.txt", "ficplt2.txt")
  )
})

test_that("set_intercrop_code_shape skips plant files that don't exist", {
  ws <- withr::local_tempdir()
  dir.create(file.path(ws, "usm1"))

  usm_df <- data.frame(
    usm = "usm1",
    plantfile2 = "ble_intercrop.xml",
    stringsAsFactors = FALSE
  )

  called <- new.env()
  called$flag <- FALSE
  mockery::stub(
    set_intercrop_code_shape, "SticsRFiles::set_plant_txt",
    function(...) called$flag <- TRUE
  )

  set_intercrop_code_shape(ws, usm_df)

  expect_false(called$flag)
})

# ---- read_usms_files ----

test_that("read_usms_files reads unique, trimmed, non-empty USM names", {
  f <- withr::local_tempfile()
  writeLines(c(" usm1 ", "usm2", "", "usm1"), f)
  expect_identical(read_usms_files(f), c("usm1", "usm2"))
})

test_that("read_usms_files reads from multiple files", {
  f1 <- withr::local_tempfile()
  writeLines("usm1", f1)
  f2 <- withr::local_tempfile()
  writeLines("usm2", f2)

  expect_setequal(
    read_usms_files(c(f1, f2)),
    c("usm1", "usm2")
  )
})

# ---- filter_usms_by_list ----

test_that("filter_usms_by_list keeps wanted USMs, warns about missing ones", {
  usm_df <- data.frame(
    usm = c("usm1", "usm2", "usm3"),
    source = "sms",
    stringsAsFactors = FALSE
  )
  list_file <- withr::local_tempfile()
  writeLines(c("usm1", "usm4"), list_file)

  logger::log_threshold(logger::WARN)
  on.exit(logger::log_threshold(logger::FATAL), add = TRUE)
  log_env <- make_log_capture()
  on.exit(logger::log_appender(logger::appender_console), add = TRUE)

  result <- filter_usms_by_list(usm_df, list_file)

  expect_identical(result$usm, "usm1")
  expect_match(log_env$logs, "usm4", all = FALSE)
})

# ---- gen_workspace_from_sms ----

test_that("gen_workspace_from_sms generates a text workspace from SMS data", {
  sms_path <- withr::local_tempdir()
  stics_path <- withr::local_tempdir()
  output_dir <- file.path(withr::local_tempdir(), "workspace")

  write_sms_fixture(sms_path, stics_path)
  write_typo_usms(sms_path, list(
    list(usm = "usm1", source = "sms"),
    list(usm = "usm2", source = "sms")
  ))
  file.create(file.path(
    sms_path, "typo_usms_FR_14_12_2017_pour_tri_evaluation_officielle.csv"
  ))

  seen <- new.env()
  mockery::stub(
    gen_workspace_from_sms, "SticsRFiles::gen_usms_xml2txt",
    function(workspace, out_dir, verbose, usm, parallel, cores) {
      seen$out_dir <- out_dir
      seen$usm <- usm
      dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    }
  )

  gen_workspace_from_sms(sms_path, stics_path, output_dir)

  expect_setequal(seen$usm, c("usm1", "usm2"))
  expect_identical(seen$out_dir, output_dir)
  expect_true(file.exists(file.path(output_dir, "typo_usms.csv")))
  expect_true(file.exists(file.path(
    output_dir, "typo_usms_FR_14_12_2017_pour_tri_evaluation_officielle.csv"
  )))
})

test_that("gen_workspace_from_sms restricts USMs using usms_files", {
  sms_path <- withr::local_tempdir()
  stics_path <- withr::local_tempdir()
  output_dir <- file.path(withr::local_tempdir(), "workspace")

  write_sms_fixture(sms_path, stics_path)
  write_typo_usms(sms_path, list(
    list(usm = "usm1", source = "sms"),
    list(usm = "usm2", source = "sms")
  ))
  file.create(file.path(
    sms_path, "typo_usms_FR_14_12_2017_pour_tri_evaluation_officielle.csv"
  ))

  list_file <- withr::local_tempfile()
  writeLines("usm1", list_file)

  seen <- new.env()
  mockery::stub(
    gen_workspace_from_sms, "SticsRFiles::gen_usms_xml2txt",
    function(workspace, out_dir, verbose, usm, parallel, cores) {
      seen$usm <- usm
      dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    }
  )

  gen_workspace_from_sms(
    sms_path, stics_path, output_dir, usms_files = list_file
  )

  expect_identical(seen$usm, "usm1")
})

test_that("gen_workspace_from_sms copies intercrop_links.xml when present", {
  sms_path <- withr::local_tempdir()
  stics_path <- withr::local_tempdir()
  output_dir <- file.path(withr::local_tempdir(), "workspace")

  write_sms_fixture(sms_path, stics_path)
  write_typo_usms(sms_path, list(list(usm = "usm1", source = "sms")))
  file.create(file.path(
    sms_path, "typo_usms_FR_14_12_2017_pour_tri_evaluation_officielle.csv"
  ))
  file.create(file.path(sms_path, "USMs", "intercrop_links.xml"))

  mockery::stub(
    gen_workspace_from_sms, "SticsRFiles::gen_usms_xml2txt",
    function(workspace, out_dir, verbose, usm, parallel, cores) {
      dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    }
  )

  gen_workspace_from_sms(sms_path, stics_path, output_dir)

  expect_true(file.exists(file.path(output_dir, "intercrop_links.xml")))
})
