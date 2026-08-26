get_header_fields <- function(f) {
  trimws(unlist(strsplit(readLines(f, 1), split = ";", fixed = TRUE)))
}

#' Generate var.mod file from observation file
#'
#' @description
#' The variable list is extracted from the header of all
#' the observation files present in the SMS workspace
#'
#' @param sms_path path to the SMS repository
#' @param out_path directory where the var.mod file should
#'  be written
#'
#' @keywords internal
gen_varmod_from_obs <- function(sms_path, out_path) {
  out_file <- file.path(out_path, "var.mod")
  if (file.exists(out_file)) invisible(file.remove(out_file))

  # Getting obs files list
  obs_files <- list.files(
    file.path(sms_path, "Obs"),
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

#' Get USM list from SMS for evaluation and calibration.
#'
#' @description
#' Returns the list of USMs from the SMS repository, including those
#' used for calibration.
#'
#' @param sms_path path to the SMS repository
#'
#' @returns a data frame containing USM information
#'
#' @keywords internal
#'
#' @importFrom rlang .data
get_sms_usms_list <- function(sms_path) {
  filter_file_path <- file.path(sms_path, "typo_usms.csv")
  filter_file <- try(read.csv(filter_file_path, sep = ";"), TRUE)
  if (is(filter_file, "try-error")) {
    stop(
      "Filter file could not be loaded: ", filter_file_path,
      call. = FALSE
    )
  }
  dplyr::filter(filter_file, .data$source == "sms")
}

#' Extract all necessary files from SMS and copy it to a destination directory.
#'
#' @param sms_path path to the SMS repository
#' @param stics_path path to Stics repository
#' @param destination_dir path where the files must be copied
#'
#' @keywords internal
extract_sms_data <- function(sms_path, stics_path, destination_dir) {
  logger::log_info("Copying XML files from SMS workspace to ", destination_dir)
  obs_path <- list.files(file.path(sms_path, "Obs"), full.names = TRUE)
  soil_path <- file.path(sms_path, "Soil", "sols.xml")
  tec_path <- list.files(file.path(sms_path, "Tec"), full.names = TRUE)
  ini_path <- list.files(file.path(sms_path, "USMs"), full.names = TRUE)
  usms_path <- file.path(sms_path, "USMs", "usms.xml")
  clim_path <- list.files(file.path(sms_path, "Climate"), full.names = TRUE)

  stics_input_files_path <- file.path(stics_path, "input_files")
  model_path <- file.path(
    stics_input_files_path,
    "model",
    c("prof.mod", "rap.mod", "param_gen.xml", "param_newform.xml")
  )
  files_path <- c(
    obs_path,
    soil_path,
    tec_path,
    ini_path,
    usms_path,
    clim_path,
    model_path
  )
  file.copy(from = files_path, to = destination_dir, overwrite = TRUE)
  plant_path <- list.files(
    file.path(stics_input_files_path, "plant"),
    full.names = TRUE
  )
  if (!dir.exists(file.path(destination_dir, "plant"))) {
    dir.create(file.path(destination_dir, "plant"))
  }
  file.copy(
    from = plant_path,
    to = file.path(destination_dir, "plant"),
    overwrite = TRUE
  )
}

#' Set intercrop code shape parameter.
#'
#' @description
#' Sets the code_shape parameter to 2 for intercrop plant files (ficplt1.txt
#' and ficplt2.txt). This function only processes USMs that have a plantfile2
#' specified and not set to "null".
#'
#' @param workspace path to the workspace containing USM folders
#' @param usm_df data frame containing USM information with a plantfile2 column
#'
#' @returns NULL (invisibly). Modifies plant files as a side effect.
#'
#' @keywords internal
set_intercrop_code_shape <- function(workspace, usm_df) {
  if (is.null(usm_df) || !"plantfile2" %in% names(usm_df)) {
    return(invisible(NULL))
  }

  intercrop_usms <- usm_df$usm[
    !is.na(usm_df$plantfile2) & usm_df$plantfile2 != "null"
  ]
  if (length(intercrop_usms) == 0) {
    return(invisible(NULL))
  }

  lapply(intercrop_usms, function(usm_name) {
    usm_dir <- file.path(workspace, usm_name)
    plant1 <- file.path(usm_dir, "ficplt1.txt")
    plant2 <- file.path(usm_dir, "ficplt2.txt")

    if (file.exists(plant1)) {
      SticsRFiles::set_plant_txt(
        file = plant1,
        param = "code_shape",
        value = 2,
        append = FALSE,
        variety = NULL
      )
    }

    if (file.exists(plant2)) {
      SticsRFiles::set_plant_txt(
        file = plant2,
        param = "code_shape",
        value = 2,
        append = FALSE,
        variety = NULL
      )
    }
  })

  invisible(NULL)
}

#' Read one or more USM list files.
#'
#' @param usms_files character vector of one or more paths to text
#'  files, each containing one USM name per line (e.g. the output of
#'  \code{get_usms_files.R})
#'
#' @returns a character vector of unique USM names
#'
#' @export
read_usms_files <- function(usms_files) {
  usms <- unique(unlist(
    lapply(usms_files, function(f) trimws(readLines(f)))
  ))
  usms[nzchar(usms)]
}

#' Restrict a USM data frame to the USMs listed in one or more USM list files.
#'
#' @param usm_df data frame containing USM information, as returned by
#'  \code{get_sms_usms_list}
#' @param usms_files character vector of one or more paths to text
#'  files, each containing one USM name per line
#'
#' @returns the filtered data frame
#'
#' @keywords internal
filter_usms_by_list <- function(usm_df, usms_files) {
  wanted_usms <- read_usms_files(usms_files)
  missing_usms <- setdiff(wanted_usms, usm_df$usm)
  if (length(missing_usms) > 0) {
    logger::log_warn(
      "USMs listed in ",
      toString(usms_files),
      " but not found in the SMS repository: ",
      toString(missing_usms)
    )
  }
  usm_df[usm_df$usm %in% wanted_usms, ]
}

#' Generate a Stics workspace from SMS data, for evaluation and calibration.
#'
#' @param sms_path path to the SMS repository
#' @param stics_path path to Stics repository
#' @param output_dir path to the Stics workspace to generate
#' @param usms_files character vector of one or more paths to text
#'  files listing the USMs (one per line) to generate. If NULL (default),
#'  all evaluation and calibration USMs are generated.
#' @param parallel Boolean. Is the computation to be done in parallel ?
#' @param cores Number of cores to use for parallel computation
#'
#' @returns NULL (invisibly)
#'
#' @export
gen_workspace_from_sms <- function(
  sms_path,
  stics_path,
  output_dir,
  usms_files = NULL,
  parallel = FALSE,
  cores = NA
) {
  logger::log_info("Generating SMS workspace...")

  if (!dir.exists(sms_path)) {
    stop(sms_path, ": no such directory", call. = FALSE)
  }
  if (!dir.exists(stics_path)) {
    stop(stics_path, ": no such directory", call. = FALSE)
  }
  usm_df <- get_sms_usms_list(sms_path)

  if (!is.null(usms_files)) {
    usm_df <- filter_usms_by_list(usm_df, usms_files)
  }

  usms <- usm_df$usm
  logger::log_info("Found ", length(usms), " USMs")

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

  extract_sms_data(sms_path, stics_path, workspace_tmp_actual)
  gen_varmod_from_obs(sms_path, workspace_tmp_actual)
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
    verbose = FALSE,
    usm = usms,
    parallel = parallel,
    cores = cores
  )

  set_intercrop_code_shape(output_dir, usm_df)

  typo_file_path <- file.path(sms_path, "typo_usms.csv")
  typo_grass_file_path <- file.path(
    sms_path,
    "typo_usms_FR_14_12_2017_pour_tri_evaluation_officielle.csv"
  )
  file.copy(from = typo_file_path, to = output_dir, overwrite = TRUE)
  file.copy(
    from = typo_grass_file_path,
    to = output_dir,
    overwrite = TRUE
  )

  intercrop_links_src <- file.path(sms_path, "USMs", "intercrop_links.xml")
  if (file.exists(intercrop_links_src)) {
    file.copy(
      from = intercrop_links_src,
      to = file.path(output_dir, "intercrop_links.xml"),
      overwrite = TRUE
    )
  }

  invisible(NULL)
}
