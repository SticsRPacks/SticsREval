metadata_file_name <- "metadata.csv"

# Resolve an IDEStics credential without it ever being written in a script:
# environment variable first, then interactive prompt.
resolve_idestics_credential <- function(env_var, label, prompt_fun) {
  value <- Sys.getenv(env_var)
  if (nzchar(value)) {
    return(value)
  }
  if (!interactive()) {
    stop(
      "No IDEStics ", label, " provided: set the ", env_var, " environment ",
      "variable or run in an interactive session.",
      call. = FALSE
    )
  }
  value <- prompt_fun()
  if (is.null(value) || !nzchar(value)) {
    stop("No IDEStics ", label, " entered.", call. = FALSE)
  }
  value
}

resolve_idestics_username <- function() {
  resolve_idestics_credential(
    "IDESTICS_USERNAME", "username",
    function() trimws(readline("IDEStics username: "))
  )
}

resolve_idestics_password <- function(username) {
  resolve_idestics_credential(
    "IDESTICS_PASSWORD", "password",
    function() {
      askpass::askpass(paste0("IDEStics password for ", username, ": "))
    }
  )
}

# Append the "error" field of the API JSON response to httr2 HTTP errors
idestics_error_body <- function(resp) {
  msg <- tryCatch(
    httr2::resp_body_json(resp)$error,
    error = function(e) NULL
  )
  if (is.character(msg) && length(msg) == 1 && nzchar(msg)) {
    paste0("IDEStics API: ", msg)
  } else {
    character()
  }
}

# Build a POST request to the IDEStics API. Transient failures (network
# errors, 429 and 503 responses) are retried.
idestics_request <- function(url, path, body, token = NULL) {
  req <- httr2::request(url) |>
    httr2::req_url_path_append(path) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(body) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_error(body = idestics_error_body)
  if (!is.null(token)) {
    req <- httr2::req_auth_bearer_token(req, token)
  }
  req
}

auth <- function(url, username, password) {
  logger::log_info("Authenticating to IDestics API...")
  resp <- tryCatch(
    idestics_request(
      url,
      c("api", "login"),
      list(username = username, password = password)
    ) |>
      httr2::req_perform(),
    httr2_http_401 = function(e) {
      stop("Invalid IDEStics username or password.", call. = FALSE)
    }
  )

  token <- httr2::resp_body_json(resp)$token
  if (!is.character(token) || length(token) != 1 || !nzchar(token)) {
    stop("IDEStics API login response contains no token.", call. = FALSE)
  }
  token
}

get_metadata <- function(url, token, usms) {
  logger::log_info("Retrieving metadata from IDestics API...")
  logger::log_debug("Retrieving rotations and USMs ids from IDEstics API...")
  resp <- idestics_request(
    url,
    c("api", "extraction", "rotations"),
    # as.list() keeps a single USM as a JSON array instead of a scalar
    list(usm_list = as.list(usms), format = "json"),
    token = token
  ) |>
    httr2::req_perform()

  resp <- httr2::resp_body_json(resp)
  if (length(resp) == 0) {
    stop("None of the requested USMs are found in IDEStics.", call. = FALSE)
  }
  metadata <- resp |>
    dplyr::bind_rows() |>
    dplyr::filter(.data$wsp_statut == "actif") |>
    dplyr::transmute(
      usm = .data$mus_name,
      rotation = dplyr::if_else(
        .data$rotation == "0",
        "0",
        paste0(.data$wsp_id, "_", .data$rotation)
      ),
      rotation_order = .data$rotation_order,
      idestics_usm_id = .data$mus_id,
      idestics_usm_name = paste0(.data$wsp_id, "_", .data$mus_name)
    )
  # Warn about the requested USMs missing from active workspaces, and stop if
  # none of them are found
  select_available_usms(metadata$usm, usms, "active IDEStics workspaces")
  metadata
}

get_usms <- function(url, token, ids, output_dir) {
  logger::log_info("Retrieving USMs from IDestics API...")
  resp <- idestics_request(
    url,
    c("api", "extraction", "sync-usms"),
    list(mus_ids = as.list(ids), format = "javastics", mode = "strict"),
    token = token
  ) |>
    httr2::req_perform()

  # A ZIP archive starts with the "PK" signature: anything else (e.g. an HTML
  # error page from a proxy) can't be extracted
  zip_content <- httr2::resp_body_raw(resp)
  if (length(zip_content) < 2 ||
        !identical(zip_content[1:2], charToRaw("PK"))) {
    stop(
      "IDEStics API returned no ZIP archive for the USMs extraction ",
      "(content type: ", httr2::resp_content_type(resp), ").",
      call. = FALSE
    )
  }

  # Save the response body to a temporary ZIP file
  zip_file <- tempfile(fileext = ".zip")
  writeBin(zip_content, zip_file)
  utils::unzip(zip_file, exdir = output_dir)
}

rename_idestics_usms <- function(workspace, metadata) {
  if (anyDuplicated(metadata$usm)) {
    stop(
      "USM names are not unique across active IDEStics workspaces: ",
      toString(unique(metadata$usm[duplicated(metadata$usm)])),
      call. = FALSE
    )
  }
  mapping <- stats::setNames(metadata$usm, metadata$idestics_usm_name)

  usms_xml <- file.path(workspace, "usms.xml")
  doc <- xml2::read_xml(usms_xml)
  for (node in xml2::xml_find_all(doc, "//usm")) {
    old <- xml2::xml_attr(node, "nom")
    if (old %in% names(mapping)) {
      xml2::xml_set_attr(node, "nom", mapping[[old]])
      for (fobs in xml2::xml_find_all(node, ".//fobs")) {
        xml2::xml_text(fobs) <- sub(old, mapping[[old]], xml2::xml_text(fobs),
                                    fixed = TRUE)
      }
    }
  }
  xml2::write_xml(doc, usms_xml)

  # Build the candidate obs file names explicitly rather than with a regex:
  # USM names may contain regex metacharacters such as "+" or ".".
  for (old in names(mapping)) {
    suffixes <- c("", "a", "p")
    obs <- file.path(workspace, paste0(old, suffixes, ".obs"))
    obs_new_path <- file.path(
      workspace,
      paste0(mapping[[old]], suffixes, ".obs")
    )
    found <- file.exists(obs)
    file.rename(obs[found], obs_new_path[found])
  }
}


extract_idestics_data <- function(
  url, username, password, usms, stics_path, output_dir
) {
  token <- auth(url, username, password)
  metadata <- get_metadata(url, token, usms)

  ids <- unique(metadata$idestics_usm_id)
  # Extracting USMs data from IDEStics
  get_usms(url, token, ids, output_dir)
  rename_idestics_usms(output_dir, metadata)

  stics_input_files_path <- file.path(stics_path, "input_files")

  # Copy inputs files from STICS repository
  model_path <- file.path(
    stics_input_files_path,
    "model",
    c("prof.mod", "rap.mod", "param_gen.xml", "param_newform.xml")
  )
  file.copy(from = model_path, to = output_dir, overwrite = TRUE)
  # Copy plant files from STICS repository
  plant_path <- list.files(
    file.path(stics_input_files_path, "plant"),
    full.names = TRUE
  )
  if (!dir.exists(file.path(output_dir, "plant"))) {
    dir.create(file.path(output_dir, "plant"))
  }
  file.copy(
    from = plant_path,
    to = file.path(output_dir, "plant"),
    overwrite = TRUE
  )
  # Write metadata to CSV
  safe_write_csv(
    metadata,
    file.path(output_dir, metadata_file_name),
    delimiter = ";"
  )
}

gen_varmod_from_obs_idestics <- function(out_path) {
  out_file <- file.path(out_path, "var.mod")
  if (file.exists(out_file)) invisible(file.remove(out_file))

  # Getting obs files list
  obs_files <- list.files(
    out_path,
    pattern = "\\.obs$",
    full.names = TRUE
  )

  # Generating var.mod file
  # Added a temporary hack to filter INN from beeing written to var.mod
  var_filt <- c("ian", "mo", "jo", "jul", "INN")
  all_vars <- unique(unlist(lapply(obs_files, get_header_fields)))
  var_names <- setdiff(all_vars, var_filt)
  cat(var_names, file = out_file, sep = "\n")
}

#' Generate a Stics workspace from IDEStics data, for evaluation.
#'
#' @description
#' Retrieves the rotations and USMs data from the IDEStics API, completes them
#' with the model and plant files of the Stics repository, and generates the
#' corresponding text workspace. The rotations are written to
#' \code{metadata.csv} in \code{output_dir}.
#'
#' The IDEStics credentials are read from the \code{IDESTICS_USERNAME} and
#' \code{IDESTICS_PASSWORD} environment variables if they are set (e.g. in the
#' user-level \code{~/.Renviron}). Otherwise, they are prompted for in an
#' interactive session, the password being masked.
#'
#' @param idestics_url URL of the IDEStics server
#' @param stics_path path to Stics repository
#' @param output_dir path to the Stics workspace to generate
#' @param usms character vector of USM names to generate. Use
#'  \code{\link{read_usms_files}} to read them from USM list files.
#' @param parallel Boolean. Is the computation to be done in parallel ?
#' @param cores Number of cores to use for parallel computation
#' @param force_code_shape Boolean. Currently unused, kept for consistency
#'  with \code{\link{gen_workspace_from_sms}}.
#' @param verbose Integer. Logging verbosity level: 0 = warnings and errors
#'  only, 1 = info, 2 = debug
#' @param usms_files Deprecated. Use
#'  \code{usms = read_usms_files(usms_files)} instead.
#'
#' @returns NULL (invisibly)
#'
#' @export
gen_workspace_from_idestics <- function(
  idestics_url,
  stics_path,
  output_dir,
  usms,
  parallel = FALSE,
  cores = NA,
  force_code_shape = TRUE,
  verbose = 1L,
  usms_files = lifecycle::deprecated()
) {
  init_logger(verbose)
  usms <- resolve_usms_files(usms, usms_files, "gen_workspace_from_idestics")
  logger::log_info("Generating IDestics workspace...")

  if (!dir.exists(stics_path)) {
    stop(stics_path, ": no such directory", call. = FALSE)
  }

  logger::log_info("Selected ", length(usms), " USMs")

  # Ask for the credentials before any long-running step
  username <- resolve_idestics_username()
  password <- resolve_idestics_password(username)

  workspace_tmp <- tempfile()
  dir.create(workspace_tmp)

  # Create subdirectory for the temporary extracted XML workspace
  workspace_tmp_actual <- file.path(workspace_tmp, "workspace")
  dir.create(workspace_tmp_actual)

  on.exit(
    {
      unlink(workspace_tmp, recursive = TRUE)
      logger::log_info("Temporary workspace ", workspace_tmp, " deleted.")
    },
    add = TRUE
  )

  extract_idestics_data(
    idestics_url, username, password, usms, stics_path, workspace_tmp_actual
  )

  gen_varmod_from_obs_idestics(workspace_tmp_actual)
  logger::log_info(
    "Generating text workspace using ",
    workspace_tmp_actual,
    " files"
  )

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  SticsRFiles::gen_usms_xml2txt(
    workspace = workspace_tmp_actual,
    out_dir = output_dir,
    verbose = is_debug(),
    usm = usms,
    parallel = parallel,
    cores = cores
  )
  file.copy(
    file.path(workspace_tmp_actual, metadata_file_name),
    file.path(output_dir, metadata_file_name),
    overwrite = TRUE
  )
  invisible(NULL)
}
