rotation_file_name <- "rotations.csv"

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
    function() askpass::askpass(paste0("IDEStics password for ", username, ": "))
  )
}

auth <- function(url, username, password) {
  logger::log_info("Authenticating to IDestics API...")
  resp <- httr2::request(url) |>
    httr2::req_url_path_append("api", "login") |>
    httr2::req_method("POST") |>
    httr2::req_body_json(list(username = username, password = password)) |>
    httr2::req_perform()

  resp <- httr2::resp_body_json(resp)

  resp$token
}

rotations <- function(url, token, usms) {
  logger::log_info("Retrieving rotations from IDestics API...")
  resp <- httr2::request(url) |>
    httr2::req_url_path_append("api", "extraction", "rotations") |>
    httr2::req_method("POST") |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_body_json(list(
      usm_list = usms,
      format = "json"
    )) |>
    httr2::req_perform()

  resp <- httr2::resp_body_json(resp)
  resp |>
    dplyr::bind_rows() |>
    dplyr::filter(.data$wsp_statut == "actif") |>
    dplyr::transmute(
      usm_id = .data$mus_id,
      usm = .data$mus_name,
      rotation = dplyr::if_else(
        .data$rotation == "0",
        "0",
        paste0(.data$wsp_id, "_", .data$rotation)
      ),
      rotation_order = .data$rotation_order,
      idestics_usm = paste0(.data$wsp_id, "_", .data$mus_name)
    )
}

get_usms <- function(url, token, ids, output_dir) {
  logger::log_info("Retrieving USMs from IDestics API...")
  resp <- httr2::request(url) |>
    httr2::req_url_path_append("api", "extraction", "sync-usms") |>
    httr2::req_method("POST") |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_body_json(list(
      mus_ids = ids,
      format = "javastics",
      mode = "strict"
    )) |>
    httr2::req_perform()

  # Save the response body to a temporary ZIP file
  zip_file <- tempfile(fileext = ".zip")
  writeBin(httr2::resp_body_raw(resp), zip_file)
  utils::unzip(zip_file, exdir = output_dir)
}

rename_idestics_usms <- function(workspace, rotations_df) {
  if (anyDuplicated(rotations_df$usm)) {
    stop(
      "USM names are not unique across active IDEStics workspaces: ",
      toString(unique(rotations_df$usm[duplicated(rotations_df$usm)])),
      call. = FALSE
    )
  }
  mapping <- stats::setNames(rotations_df$usm, rotations_df$idestics_usm)

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
  rotations_df <- rotations(url, token, usms)

  ids <- unique(rotations_df$usm_id)
  # Extracting USMs data from IDEStics
  get_usms(url, token, ids, output_dir)
  rename_idestics_usms(output_dir, rotations_df)

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
  # Write rotations to CSV
  safe_write_csv(
    rotations_df,
    file.path(output_dir, rotation_file_name),
    delimiter = ";"
  )
  rotations_df
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

  #on.exit(
  #  {
  #    unlink(workspace_tmp, recursive = TRUE)
  #    logger::log_info("Temporary workspace ", workspace_tmp, " deleted.")
  #  },
  #  add = TRUE
  #)

  metadata <- extract_idestics_data(
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
    file.path(workspace_tmp_actual, rotation_file_name),
    file.path(output_dir, rotation_file_name),
    overwrite = TRUE
  )
  invisible(NULL)
}
