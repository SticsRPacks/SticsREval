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

# Stub of httr2::req_perform_parallel() for requests performed with `paths`:
# httr2 mocked responses ignore them, so the response bodies would never be
# written to disk. Copies the `body_files` to `paths` and records the
# requests.
mock_downloads <- function(body_files, content_type, seen = new.env()) {
  function(reqs, paths, ...) {
    seen$urls <- vapply(reqs, function(req) req$url, character(1))
    seen$bodies <- vapply(reqs, req_json_body, character(1))
    seen$paths <- paths
    file.copy(unlist(body_files), paths)
    lapply(paths, function(p) {
      httr2::response(200L, headers = list(`Content-Type` = content_type))
    })
  }
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

test_that("idestics_request sends no body when none is given", {
  req <- idestics_request(idestics_url, "api", method = "GET")

  expect_identical(req$method, "GET")
  expect_null(req$body)
})

test_that("idestics_request retries on network failures", {
  req <- idestics_request(idestics_url, "api", list())

  expect_identical(req$policies$retry_max_tries, 3)
  expect_true(req$policies$retry_on_failure)
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

  metadata <- get_metadata(idestics_url, "tok", c("usm1", "usm2"))

  expect_identical(metadata$usm, "usm1")
  expect_match(log_env$logs, "usm2", all = FALSE)
})

test_that("get_metadata errors on duplicated USM names", {
  httr2::local_mocked_responses(function(req) {
    json_response(list(
      rotation_row("usm1", mus_id = 1L, wsp_id = 7L),
      rotation_row("usm1", mus_id = 2L, wsp_id = 8L),
      rotation_row("usm2", mus_id = 3L, wsp_id = 8L)
    ))
  })

  expect_error(
    get_metadata(idestics_url, "tok", c("usm1", "usm2")),
    "USM names are not unique across active IDEStics workspaces: usm1"
  )
})

test_that("get_metadata ignores duplicated names of inactive workspaces", {
  httr2::local_mocked_responses(function(req) {
    json_response(list(
      rotation_row("usm1", mus_id = 1L, wsp_id = 7L),
      rotation_row("usm1", mus_id = 2L, wsp_id = 8L, wsp_statut = "inactif")
    ))
  })

  metadata <- get_metadata(idestics_url, "tok", "usm1")

  expect_identical(metadata$idestics_usm_id, 1L)
})

# ---- get_usms ----

test_that("get_usms extracts the ZIP archive returned by the API", {
  skip_if(!nzchar(Sys.which("zip")), "zip command not available")
  src <- withr::local_tempdir()
  writeLines("<usms/>", file.path(src, "usms.xml"))
  zip_file <- withr::local_tempfile(fileext = ".zip")
  withr::with_dir(src, utils::zip(zip_file, "usms.xml", flags = "-q"))

  seen <- new.env()
  mockery::stub(
    get_usms, "httr2::req_perform_parallel",
    mock_downloads(zip_file, "application/zip", seen) # nolint: nonportable_path_linter
  )

  output_dir <- withr::local_tempdir()
  get_usms(idestics_url, "tok", 1L, 42L, output_dir)

  expect_true(file.exists(file.path(output_dir, "usms.xml")))
  # The downloaded archive is removed after extraction
  expect_false(file.exists(seen$paths))
  expect_identical(seen$urls, "https://idestics.test/api/extraction/sync-usms")
  expect_identical(
    seen$bodies,
    '{"mus_ids":[1],"format":"javastics","mode":"strict","siv_id":42}'
  )
})

test_that("get_usms requests the USMs by batches and merges the workspaces", {
  skip_if(!nzchar(Sys.which("zip")), "zip command not available")
  make_zip <- function(usms, soil) {
    src <- withr::local_tempdir(.local_envir = parent.frame(2))
    write_usms_xml(src, usms)
    writeLines(
      c("<sols>", paste0('<sol nom="', soil, '"/>'), "</sols>"),
      file.path(src, "sols.xml")
    )
    file.create(file.path(src, paste0(usms, ".obs")))
    zip_file <- withr::local_tempfile(
      fileext = ".zip", .local_envir = parent.frame(2)
    )
    withr::with_dir(src, utils::zip(zip_file, list.files(), flags = "-q"))
    zip_file
  }
  batch_zips <- list(
    make_zip(c("u1", "u2"), "s1"),
    make_zip("u3", "s1")
  )

  seen <- new.env()
  mockery::stub(
    get_usms, "httr2::req_perform_parallel",
    mock_downloads(batch_zips, "application/zip", seen) # nolint: nonportable_path_linter
  )

  output_dir <- withr::local_tempdir()
  get_usms(idestics_url, "tok", 1:3, 42L, output_dir, batch_size = 2L)

  expect_identical(
    seen$bodies,
    c(
      '{"mus_ids":[1,2],"format":"javastics","mode":"strict","siv_id":42}',
      '{"mus_ids":[3],"format":"javastics","mode":"strict","siv_id":42}'
    )
  )
  usms_doc <- xml2::read_xml(file.path(output_dir, "usms.xml"))
  expect_identical(
    xml2::xml_attr(xml2::xml_find_all(usms_doc, "//usm"), "nom"),
    c("u1", "u2", "u3")
  )
  sols_doc <- xml2::read_xml(file.path(output_dir, "sols.xml"))
  expect_identical(
    xml2::xml_attr(xml2::xml_find_all(sols_doc, "//sol"), "nom"),
    "s1"
  )
  expect_true(all(file.exists(
    file.path(output_dir, c("u1.obs", "u2.obs", "u3.obs"))
  )))
})

test_that("get_usms errors when the API returns no ZIP archive", {
  html_file <- withr::local_tempfile(lines = "<html></html>")
  seen <- new.env()
  mockery::stub(
    get_usms, "httr2::req_perform_parallel",
    mock_downloads(html_file, "text/html", seen) # nolint: nonportable_path_linter
  )

  expect_error(
    get_usms(idestics_url, "tok", 1L, 42L, withr::local_tempdir()),
    "returned no ZIP archive .*content type: text/html"
  )
  expect_false(file.exists(seen$paths))
})

test_that("get_usms errors on an empty response", {
  empty_file <- withr::local_tempfile()
  file.create(empty_file)
  mockery::stub(
    get_usms, "httr2::req_perform_parallel",
    mock_downloads(empty_file, "application/zip") # nolint: nonportable_path_linter
  )

  expect_error(
    get_usms(idestics_url, "tok", 1L, 42L, withr::local_tempdir()),
    "returned no ZIP archive"
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

# ---- get_simulator_version_id ----

simulator_version_row <- function(siv_id, version, active = TRUE) {
  list(siv_id = siv_id, version = version, active = active)
}

test_that("get_simulator_version_id returns the ID of the active version", {
  seen <- new.env()
  httr2::local_mocked_responses(function(req) {
    seen$url <- req$url
    seen$method <- req$method
    seen$body <- req$body
    json_response(list(
      simulator_version_row(41L, "10.2.0", active = FALSE),
      simulator_version_row(42L, "10.2.0"),
      simulator_version_row(43L, "11.0.0")
    ))
  })

  expect_identical(
    get_simulator_version_id(idestics_url, "tok", "10.2.0"),
    42L
  )
  expect_identical(seen$url, "https://idestics.test/api/simulator-versions")
  expect_identical(seen$method, "GET")
  expect_null(seen$body)
})

test_that("get_simulator_version_id errors on several active versions", {
  httr2::local_mocked_responses(function(req) {
    json_response(list(
      simulator_version_row(41L, "10.2.0"),
      simulator_version_row(42L, "10.2.0")
    ))
  })

  expect_error(
    get_simulator_version_id(idestics_url, "tok", "10.2.0"),
    "several active simulator versions for STICS version 10.2.0: 41, 42"
  )
})

test_that("get_simulator_version_id errors on an unknown version", {
  httr2::local_mocked_responses(function(req) {
    json_response(list(
      simulator_version_row(41L, "10.2.0", active = FALSE),
      simulator_version_row(43L, "11.0.0")
    ))
  })

  expect_error(
    get_simulator_version_id(idestics_url, "tok", "10.2.0"),
    "no simulator version ID for STICS version 10.2.0"
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
    extract_idestics_data, "get_simulator_version_id",
    function(url, token, stics_version) {
      seen$stics_version <- stics_version
      42L
    }
  )
  mockery::stub(
    extract_idestics_data, "get_usms",
    function(url, token, ids, simulator_id, output_dir, ...) {
      seen$token <- token
      seen$ids <- ids
      seen$simulator_id <- simulator_id
    }
  )
  mockery::stub(
    extract_idestics_data, "rename_idestics_usms",
    function(...) NULL
  )

  extract_idestics_data(
    idestics_url, "user", "secret", "10.2.0", "usm1", stics_path, output_dir
  )

  expect_identical(seen$credentials, c("user", "secret"))
  expect_identical(seen$usms, "usm1")
  expect_identical(seen$token, "tok")
  expect_identical(seen$stics_version, "10.2.0")
  expect_identical(seen$ids, 1L)
  expect_identical(seen$simulator_id, 42L)
  expect_true(all(file.exists(file.path(
    output_dir,
    c("prof.mod", "rap.mod", "param_gen.xml", "param_newform.xml")
  ))))
  expect_true(file.exists(file.path(output_dir, "plant", "wheat_plt.xml")))
  written <- read_csv(file.path(output_dir, "metadata.csv"), delimiter = ";")
  expect_identical(written$usm, "usm1")
})

test_that("extract_idestics_data returns the metadata", {
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
  mockery::stub(extract_idestics_data, "auth", function(...) "tok")
  mockery::stub(extract_idestics_data, "get_metadata", function(...) metadata)
  mockery::stub(
    extract_idestics_data, "get_simulator_version_id",
    function(...) 42L
  )
  mockery::stub(extract_idestics_data, "get_usms", function(...) NULL)
  mockery::stub(
    extract_idestics_data, "rename_idestics_usms",
    function(...) NULL
  )

  expect_identical(
    extract_idestics_data(
      idestics_url, "user", "secret", "10.2.0", "usm1", stics_path,
      output_dir
    ),
    metadata
  )
})

# ---- get_usms_plantfile2 ----

test_that("get_usms_plantfile2 reads the plant file of the second plant", {
  ws <- withr::local_tempdir()
  writeLines(
    c(
      "<usms>",
      '<usm nom="sole"><plante dominance="1"><fplt>wheat_plt.xml</fplt>',
      '</plante><plante dominance="2"><fplt>null</fplt></plante></usm>',
      '<usm nom="inter"><plante dominance="1"><fplt>wheat_plt.xml</fplt>',
      '</plante><plante dominance="2"><fplt>pea_plt.xml</fplt></plante></usm>',
      '<usm nom="single"><plante dominance="1"><fplt>wheat_plt.xml</fplt>',
      "</plante></usm>",
      "</usms>"
    ),
    file.path(ws, "usms.xml")
  )

  expect_identical(
    get_usms_plantfile2(ws),
    data.frame(
      usm = c("sole", "inter", "single"),
      plantfile2 = c("null", "pea_plt.xml", NA),
      stringsAsFactors = FALSE
    )
  )
})

# ---- check_idestics_args ----

test_that("check_idestics_args accepts valid arguments", {
  expect_no_error(check_idestics_args("11.0.0", 300L, 10L))
  expect_no_error(check_idestics_args("11.0.0", Inf, 1))
})

test_that("check_idestics_args rejects an invalid stics_version", {
  for (stics_version in list(NULL, NA_character_, "", c("10.2.0", "11.0.0"),
                             11)) {
    expect_error(
      check_idestics_args(stics_version, 300L, 10L),
      "`stics_version` must be a single non-empty string"
    )
  }
})

test_that("check_idestics_args rejects an invalid batch_size", {
  for (batch_size in list(0L, -1L, 2.5, NA_real_, "300", c(1L, 2L))) {
    expect_error(
      check_idestics_args("11.0.0", batch_size, 10L),
      "`batch_size` must be a positive whole number or Inf"
    )
  }
})

test_that("check_idestics_args rejects an invalid max_active", {
  for (max_active in list(0L, 2.5, Inf, NA_real_, "10")) {
    expect_error(
      check_idestics_args("11.0.0", 300L, max_active),
      "`max_active` must be a positive whole number"
    )
  }
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
    function(url, username, password, stics_version, usms, stics_path,
             output_dir, ...) {
      seen$credentials <- c(username, password)
      seen$stics_version <- stics_version
      write_usms_xml(output_dir, "usm1")
      writeLines("ian;mo;jo;jul;LAI", file.path(output_dir, "usm1.obs"))
      writeLines("usm;rotation", file.path(output_dir, "metadata.csv"))
      data.frame(usm = "usm1", stringsAsFactors = FALSE)
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
    idestics_url, stics_path, output_dir, "10.2.0",
    usms = "usm1", verbose = 0L
  )

  expect_identical(seen$credentials, c("user", "secret"))
  expect_identical(seen$stics_version, "10.2.0")
  expect_identical(seen$usm, "usm1")
  expect_identical(seen$var_mod, "LAI")
  expect_true(file.exists(file.path(output_dir, "metadata.csv")))
})

test_that("gen_workspace_from_idestics only converts the USMs found", {
  withr::local_envvar(
    IDESTICS_USERNAME = "user",
    IDESTICS_PASSWORD = "secret"
  )
  output_dir <- file.path(withr::local_tempdir(), "workspace")

  seen <- new.env()
  mockery::stub(
    gen_workspace_from_idestics, "extract_idestics_data",
    function(url, username, password, stics_version, usms, stics_path,
             output_dir, ...) {
      write_usms_xml(output_dir, c("usm3", "usm1"))
      writeLines("usm;rotation", file.path(output_dir, "metadata.csv"))
      data.frame(usm = c("usm3", "usm1"), stringsAsFactors = FALSE)
    }
  )
  mockery::stub(
    gen_workspace_from_idestics, "SticsRFiles::gen_usms_xml2txt",
    function(workspace, out_dir, verbose, usm, parallel, cores) {
      seen$usm <- usm
    }
  )

  gen_workspace_from_idestics(
    idestics_url, withr::local_tempdir(), output_dir, "10.2.0",
    usms = c("usm1", "usm2", "usm3"), verbose = 0L
  )

  # Requested order kept, missing usm2 dropped
  expect_identical(seen$usm, c("usm1", "usm3"))
})

test_that("gen_workspace_from_idestics forces the intercrop code_shape", {
  withr::local_envvar(
    IDESTICS_USERNAME = "user",
    IDESTICS_PASSWORD = "secret"
  )
  output_dir <- file.path(withr::local_tempdir(), "workspace")
  plantfile2 <- data.frame(
    usm = "usm1", plantfile2 = "pea_plt.xml", stringsAsFactors = FALSE
  )

  seen <- new.env()
  mockery::stub(
    gen_workspace_from_idestics, "extract_idestics_data",
    function(...) data.frame(usm = "usm1", stringsAsFactors = FALSE)
  )
  mockery::stub(
    gen_workspace_from_idestics, "gen_varmod_from_obs_idestics",
    function(...) NULL
  )
  mockery::stub(
    gen_workspace_from_idestics, "SticsRFiles::gen_usms_xml2txt",
    function(...) NULL
  )
  mockery::stub(
    gen_workspace_from_idestics, "get_usms_plantfile2",
    function(workspace) plantfile2
  )
  mockery::stub(
    gen_workspace_from_idestics, "set_intercrop_code_shape",
    function(workspace, usm_df) {
      seen$workspace <- workspace
      seen$usm_df <- usm_df
    }
  )

  gen_workspace_from_idestics(
    idestics_url, withr::local_tempdir(), output_dir, "10.2.0",
    usms = "usm1", verbose = 0L
  )

  expect_identical(seen$workspace, output_dir)
  expect_identical(seen$usm_df, plantfile2)
})

test_that("gen_workspace_from_idestics can skip forcing code_shape", {
  withr::local_envvar(
    IDESTICS_USERNAME = "user",
    IDESTICS_PASSWORD = "secret"
  )
  called <- new.env()
  called$flag <- FALSE
  mockery::stub(
    gen_workspace_from_idestics, "extract_idestics_data",
    function(...) data.frame(usm = "usm1", stringsAsFactors = FALSE)
  )
  mockery::stub(
    gen_workspace_from_idestics, "gen_varmod_from_obs_idestics",
    function(...) NULL
  )
  mockery::stub(
    gen_workspace_from_idestics, "SticsRFiles::gen_usms_xml2txt",
    function(...) NULL
  )
  mockery::stub(
    gen_workspace_from_idestics, "set_intercrop_code_shape",
    function(...) called$flag <- TRUE
  )

  gen_workspace_from_idestics(
    idestics_url, withr::local_tempdir(),
    file.path(withr::local_tempdir(), "workspace"), "10.2.0",
    usms = "usm1", force_code_shape = FALSE, verbose = 0L
  )

  expect_false(called$flag)
})

test_that("gen_workspace_from_idestics errors on a missing stics_path", {
  stics_path <- file.path(withr::local_tempdir(), "missing")

  expect_error(
    gen_workspace_from_idestics(
      idestics_url, stics_path, withr::local_tempdir(), "10.2.0",
      usms = "usm1", verbose = 0L
    ),
    "no such directory"
  )
})

test_that("gen_workspace_from_idestics checks its arguments first", {
  called <- new.env()
  called$flag <- FALSE
  mockery::stub(
    gen_workspace_from_idestics, "resolve_idestics_username",
    function() called$flag <- TRUE
  )

  expect_error(
    gen_workspace_from_idestics(
      idestics_url, withr::local_tempdir(), withr::local_tempdir(), "11.0.0",
      usms = "usm1", batch_size = 0L, verbose = 0L
    ),
    "`batch_size` must be a positive whole number"
  )
  expect_false(called$flag)
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
      idestics_url, withr::local_tempdir(), withr::local_tempdir(), "10.2.0",
      usms = "usm1", verbose = 0L
    ),
    "No IDEStics username provided"
  )
  expect_false(called$flag)
})
