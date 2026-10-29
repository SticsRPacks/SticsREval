# ---- Helpers ----

idestics_url <- "https://idestics.test"

json_response <- function(body, status = 200L) {
  httr2::response_json(status_code = status, body = body)
}

# JSON body sent by a mocked request, as a string
req_json_body <- function(req) {
  as.character(do.call(
    jsonlite::toJSON,
    c(list(req$body$data), req$body$params)
  ))
}

rotation_row <- function(mus_name, mus_id = 1L, wsp_id = 7L, rotation = "0",
                         rotation_order = 1L, wsp_statut = "actif") {
  list(
    wsp_statut = wsp_statut,
    mus_id = mus_id,
    mus_name = mus_name,
    rotation = rotation,
    rotation_order = rotation_order,
    wsp_id = wsp_id
  )
}

write_usms_xml <- function(workspace, usms) {
  usm_nodes <- vapply(usms, function(usm) {
    paste0(
      '<usm nom="', usm, '"><plante dominance="1"><fobs>', usm,
      ".obs</fobs></plante></usm>"
    )
  }, character(1))
  writeLines(
    c("<usms>", usm_nodes, "</usms>"),
    file.path(workspace, "usms.xml")
  )
}

write_stics_fixture <- function(stics_path) {
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

# ---- resolve_idestics_credential ----

test_that("resolve_idestics_credential reads the environment variable", {
  withr::local_envvar(IDESTICS_TEST = "from_env")
  prompted <- new.env()
  prompted$flag <- FALSE

  value <- resolve_idestics_credential(
    "IDESTICS_TEST", "username",
    function() prompted$flag <- TRUE
  )

  expect_identical(value, "from_env")
  expect_false(prompted$flag)
})

test_that("resolve_idestics_credential errors when unset and non interactive", {
  withr::local_envvar(IDESTICS_TEST = NA)
  mockery::stub(resolve_idestics_credential, "interactive", function() FALSE)

  expect_error(
    resolve_idestics_credential("IDESTICS_TEST", "username", function() "x"),
    "No IDEStics username provided: set the IDESTICS_TEST environment"
  )
})

test_that("resolve_idestics_credential prompts in an interactive session", {
  withr::local_envvar(IDESTICS_TEST = NA)
  mockery::stub(resolve_idestics_credential, "interactive", function() TRUE)

  value <- resolve_idestics_credential(
    "IDESTICS_TEST", "username", function() "typed"
  )

  expect_identical(value, "typed")
})

test_that("resolve_idestics_credential errors on an empty or cancelled input", {
  withr::local_envvar(IDESTICS_TEST = NA)
  mockery::stub(resolve_idestics_credential, "interactive", function() TRUE)

  expect_error(
    resolve_idestics_credential("IDESTICS_TEST", "password", function() ""),
    "No IDEStics password entered"
  )
  # askpass::askpass() returns NULL when the prompt is cancelled
  expect_error(
    resolve_idestics_credential("IDESTICS_TEST", "password", function() NULL),
    "No IDEStics password entered"
  )
})

test_that("username and password are read from their environment vars", {
  withr::local_envvar(
    IDESTICS_USERNAME = "user",
    IDESTICS_PASSWORD = "secret"
  )

  expect_identical(resolve_idestics_username(), "user")
  expect_identical(resolve_idestics_password("user"), "secret")
})

# ---- idestics_error_body ----

test_that("idestics_error_body extracts the error field of the response", {
  resp <- json_response(list(error = "db down"), 500L)
  expect_identical(idestics_error_body(resp), "IDEStics API: db down")
})

test_that("idestics_error_body returns nothing without a usable error field", {
  expect_identical(
    idestics_error_body(json_response(list(detail = "x"), 500L)),
    character()
  )
  expect_identical(
    idestics_error_body(httr2::response(500L, body = charToRaw("oops"))),
    character()
  )
})

# ---- idestics_request ----

test_that("idestics_request builds a JSON POST request on the API path", {
  req <- idestics_request(idestics_url, c("api", "login"), list(a = 1))

  expect_identical(req$url, "https://idestics.test/api/login")
  expect_identical(req$method, "POST")
  expect_identical(req_json_body(req), '{"a":1}')
  expect_null(httr2::req_get_headers(req)$Authorization)
})

test_that("idestics_request adds the bearer token when given", {
  req <- idestics_request(idestics_url, "api", list(), token = "tok")

  expect_identical(
    httr2::req_get_headers(req, "reveal")$Authorization,
    "Bearer tok"
  )
})

test_that("idestics_request adds the API error message to HTTP errors", {
  httr2::local_mocked_responses(function(req) {
    json_response(list(error = "db down"), 500L)
  })

  expect_error(
    httr2::req_perform(idestics_request(idestics_url, "api", list())),
    "IDEStics API: db down",
    class = "httr2_http_500"
  )
})

# ---- auth ----

test_that("auth sends the credentials and returns the token", {
  seen <- new.env()
  httr2::local_mocked_responses(function(req) {
    seen$url <- req$url
    seen$body <- req_json_body(req)
    json_response(list(token = "tok"))
  })

  expect_identical(auth(idestics_url, "user", "secret"), "tok")
  expect_identical(seen$url, "https://idestics.test/api/login")
  expect_identical(seen$body, '{"username":"user","password":"secret"}')
})

test_that("auth reports invalid credentials on a 401 response", {
  httr2::local_mocked_responses(function(req) {
    json_response(list(error = "bad credentials"), 401L)
  })

  expect_error(
    auth(idestics_url, "user", "wrong"),
    "Invalid IDEStics username or password"
  )
})

test_that("auth errors when the login response contains no token", {
  httr2::local_mocked_responses(function(req) json_response(list(foo = 1)))

  expect_error(
    auth(idestics_url, "user", "secret"),
    "login response contains no token"
  )
})

test_that("auth keeps the API error message on other HTTP errors", {
  httr2::local_mocked_responses(function(req) {
    json_response(list(error = "db down"), 500L)
  })

  expect_error(
    auth(idestics_url, "user", "secret"),
    "IDEStics API: db down",
    class = "httr2_http_500"
  )
})

# ---- get_metadata ----

test_that("get_metadata returns the active rotations of the USMs", {
  httr2::local_mocked_responses(function(req) {
    json_response(list(
      rotation_row("usm1", mus_id = 1L, wsp_id = 7L, rotation = "0"),
      rotation_row(
        "usm2", mus_id = 2L, wsp_id = 7L, rotation = "3", rotation_order = 2L
      ),
      rotation_row("usm3", mus_id = 3L, wsp_statut = "inactif")
    ))
  })

  metadata <- get_metadata(idestics_url, "tok", c("usm1", "usm2"))

  expect_identical(metadata$usm, c("usm1", "usm2"))
  expect_identical(metadata$rotation, c("0", "7_3"))
  expect_identical(metadata$rotation_order, c(1L, 2L))
  expect_identical(metadata$idestics_usm_id, c(1L, 2L))
  expect_identical(metadata$idestics_usm_name, c("7_usm1", "7_usm2"))
})

test_that("get_metadata sends a single USM as a JSON array", {
  seen <- new.env()
  httr2::local_mocked_responses(function(req) {
    seen$url <- req$url
    seen$body <- req_json_body(req)
    json_response(list(rotation_row("usm1")))
  })

  get_metadata(idestics_url, "tok", "usm1")

  expect_identical(seen$url, "https://idestics.test/api/extraction/rotations")
  expect_identical(seen$body, '{"usm_list":["usm1"],"format":"json"}')
})

test_that("get_metadata errors when no USM is returned", {
  httr2::local_mocked_responses(function(req) json_response(list()))

  expect_error(
    get_metadata(idestics_url, "tok", "usm1"),
    "None of the requested USMs are found in IDEStics"
  )
})

test_that("get_metadata errors when no returned USM is active", {
  httr2::local_mocked_responses(function(req) {
    json_response(list(rotation_row("usm1", wsp_statut = "inactif")))
  })

  expect_error(
    get_metadata(idestics_url, "tok", "usm1"),
    "None of the requested USMs are found in active IDEStics workspaces"
  )
})

test_that("get_metadata warns about the USMs missing from IDEStics", {
  httr2::local_mocked_responses(function(req) {
    json_response(list(rotation_row("usm1")))
  })
  logger::log_threshold(logger::WARN)
  on.exit(logger::log_threshold(logger::FATAL), add = TRUE)
  log_env <- make_log_capture()
  on.exit(logger::log_appender(logger::appender_console), add = TRUE)

  metadata <- get_metadata(idestics_url, "tok", c("usm1", "usm2"))

  expect_identical(metadata$usm, "usm1")
  expect_match(log_env$logs, "usm2", all = FALSE)
})

# ---- get_usms ----

test_that("get_usms extracts the ZIP archive returned by the API", {
  skip_if(!nzchar(Sys.which("zip")), "zip command not available")
  src <- withr::local_tempdir()
  writeLines("<usms/>", file.path(src, "usms.xml"))
  zip_file <- withr::local_tempfile(fileext = ".zip")
  withr::with_dir(src, utils::zip(zip_file, "usms.xml", flags = "-q"))

  seen <- new.env()
  httr2::local_mocked_responses(function(req) {
    seen$url <- req$url
    seen$body <- req_json_body(req)
    httr2::response(
      200L,
      headers = list(`Content-Type` = "application/zip"), # nolint: nonportable_path_linter
      body = readBin(zip_file, "raw", file.size(zip_file))
    )
  })

  output_dir <- withr::local_tempdir()
  get_usms(idestics_url, "tok", 1L, output_dir)

  expect_true(file.exists(file.path(output_dir, "usms.xml")))
  expect_identical(seen$url, "https://idestics.test/api/extraction/sync-usms")
  expect_identical(
    seen$body,
    '{"mus_ids":[1],"format":"javastics","mode":"strict"}'
  )
})

test_that("get_usms errors when the API returns no ZIP archive", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      200L,
      headers = list(`Content-Type` = "text/html"), # nolint: nonportable_path_linter
      body = charToRaw("<html></html>")
    )
  })

  expect_error(
    get_usms(idestics_url, "tok", 1L, withr::local_tempdir()),
    "returned no ZIP archive .*content type: text/html"
  )
})

# ---- rename_idestics_usms ----

test_that("rename_idestics_usms renames the USMs and their obs files", {
  ws <- withr::local_tempdir()
  write_usms_xml(ws, c("7_usm+1", "other"))
  file.create(file.path(ws, c("7_usm+1.obs", "7_usm+1a.obs", "other.obs")))

  metadata <- data.frame(
    usm = "usm+1",
    idestics_usm_name = "7_usm+1",
    stringsAsFactors = FALSE
  )

  rename_idestics_usms(ws, metadata)

  doc <- xml2::read_xml(file.path(ws, "usms.xml"))
  expect_identical(
    xml2::xml_attr(xml2::xml_find_all(doc, "//usm"), "nom"),
    c("usm+1", "other")
  )
  expect_identical(
    xml2::xml_text(xml2::xml_find_all(doc, "//fobs")),
    c("usm+1.obs", "other.obs")
  )
  expect_setequal(
    list.files(ws, pattern = "\\.obs$"),
    c("usm+1.obs", "usm+1a.obs", "other.obs")
  )
})

test_that("rename_idestics_usms errors on duplicated USM names", {
  metadata <- data.frame(
    usm = c("usm1", "usm1"),
    idestics_usm_name = c("7_usm1", "8_usm1"),
    stringsAsFactors = FALSE
  )

  expect_error(
    rename_idestics_usms(withr::local_tempdir(), metadata),
    "USM names are not unique across active IDEStics workspaces: usm1"
  )
})

# ---- extract_idestics_data ----

test_that("extract_idestics_data retrieves the USMs and completes them", {
  stics_path <- withr::local_tempdir()
  write_stics_fixture(stics_path)
  output_dir <- withr::local_tempdir()

  metadata <- data.frame(
    usm = "usm1",
    rotation = "0",
    rotation_order = 1L,
    idestics_usm_id = 1L,
    idestics_usm_name = "7_usm1",
    stringsAsFactors = FALSE
  )
  seen <- new.env()
  mockery::stub(
    extract_idestics_data, "auth",
    function(url, username, password) {
      seen$credentials <- c(username, password)
      "tok"
    }
  )
  mockery::stub(
    extract_idestics_data, "get_metadata",
    function(url, token, usms) {
      seen$usms <- usms
      metadata
    }
  )
  mockery::stub(
    extract_idestics_data, "get_usms",
    function(url, token, ids, output_dir) {
      seen$token <- token
      seen$ids <- ids
    }
  )
  mockery::stub(
    extract_idestics_data, "rename_idestics_usms",
    function(...) NULL
  )

  extract_idestics_data(
    idestics_url, "user", "secret", "usm1", stics_path, output_dir
  )

  expect_identical(seen$credentials, c("user", "secret"))
  expect_identical(seen$usms, "usm1")
  expect_identical(seen$token, "tok")
  expect_identical(seen$ids, 1L)
  expect_true(all(file.exists(file.path(
    output_dir,
    c("prof.mod", "rap.mod", "param_gen.xml", "param_newform.xml")
  ))))
  expect_true(file.exists(file.path(output_dir, "plant", "wheat_plt.xml")))
  written <- read_csv(file.path(output_dir, "metadata.csv"), delimiter = ";")
  expect_identical(written$usm, "usm1")
})

# ---- gen_varmod_from_obs_idestics ----

test_that("gen_varmod_from_obs_idestics writes unique vars from obs files", {
  ws <- withr::local_tempdir()
  writeLines("ian;mo;jo;jul;INN;LAI;MASEC", file.path(ws, "usm1.obs"))
  writeLines("ian;mo;jo;jul;LAI;HR_1", file.path(ws, "usm2.obs"))
  writeLines("OLD_VAR", file.path(ws, "var.mod"))

  gen_varmod_from_obs_idestics(ws)

  expect_setequal(
    readLines(file.path(ws, "var.mod")),
    c("LAI", "MASEC", "HR_1")
  )
})

# ---- gen_workspace_from_idestics ----

test_that("gen_workspace_from_idestics generates a text workspace", {
  withr::local_envvar(
    IDESTICS_USERNAME = "user",
    IDESTICS_PASSWORD = "secret"
  )
  stics_path <- withr::local_tempdir()
  output_dir <- file.path(withr::local_tempdir(), "workspace")

  seen <- new.env()
  mockery::stub(
    gen_workspace_from_idestics, "extract_idestics_data",
    function(url, username, password, usms, stics_path, output_dir) {
      seen$credentials <- c(username, password)
      writeLines("ian;mo;jo;jul;LAI", file.path(output_dir, "usm1.obs"))
      writeLines("usm;rotation", file.path(output_dir, "metadata.csv"))
    }
  )
  mockery::stub(
    gen_workspace_from_idestics, "SticsRFiles::gen_usms_xml2txt",
    function(workspace, out_dir, verbose, usm, parallel, cores) {
      seen$usm <- usm
      seen$var_mod <- readLines(file.path(workspace, "var.mod"))
    }
  )

  gen_workspace_from_idestics(
    idestics_url, stics_path, output_dir, usms = "usm1", verbose = 0L
  )

  expect_identical(seen$credentials, c("user", "secret"))
  expect_identical(seen$usm, "usm1")
  expect_identical(seen$var_mod, "LAI")
  expect_true(file.exists(file.path(output_dir, "metadata.csv")))
})

test_that("gen_workspace_from_idestics errors on a missing stics_path", {
  stics_path <- file.path(withr::local_tempdir(), "missing")

  expect_error(
    gen_workspace_from_idestics(
      idestics_url, stics_path, withr::local_tempdir(),
      usms = "usm1", verbose = 0L
    ),
    "no such directory"
  )
})

test_that("gen_workspace_from_idestics asks credentials before extracting", {
  called <- new.env()
  called$flag <- FALSE
  mockery::stub(
    gen_workspace_from_idestics, "resolve_idestics_username",
    function() stop("No IDEStics username provided", call. = FALSE)
  )
  mockery::stub(
    gen_workspace_from_idestics, "extract_idestics_data",
    function(...) called$flag <- TRUE
  )

  expect_error(
    gen_workspace_from_idestics(
      idestics_url, withr::local_tempdir(), withr::local_tempdir(),
      usms = "usm1", verbose = 0L
    ),
    "No IDEStics username provided"
  )
  expect_false(called$flag)
})
