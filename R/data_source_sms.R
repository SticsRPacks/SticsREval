get_sms_usms_list <- function(sms_path) {
  filter_file_path <- file.path(sms_path, "typo_usms.csv")
  filter_file <- try(read_csv(filter_file_path), TRUE)
  if (is(filter_file, "try-error")) {
    stop(paste0("Filter file could not be loaded: ", filter_file_path))
  }
  filter_file %>%
    dplyr::filter(source == "sms", UsedForCalibration == 0)
}

#' Getting the filtered USM list using the typo file.
#'
#' @description
#' This function is specific to SMS data source.
#' The selected USMs have the "sms" source and are not used for calibration.
#'
#' @param sms_path path to the SMS repository
#'
#' @returns a list of usm names
#'
#' @examples
#'   get_sms_usms_list("/path/to/sms")
get_sms_usms_names <- function(sms_path) {
  usm_list <- get_sms_usms_list(sms_path)
  logger::log_debug("Found ", usm_list$usm, " USMs in ", sms_path)
  usm_list$usm
}

get_sms_rotations <- function(sms_path) {
  filter_file <- get_sms_usms_list(sms_path)
  get_rotation_list(filter_file)
}

#' Extract all necessary files from SMS and copy it to a destination directory.
#'
#' @param sms_path path to the SMS repository
#' @param stics_path path to Stics repository
#' @param destination_dir path where the files must be copied
#'
extract_sms_data <- function(sms_path, stics_path, destination_dir) {
  logger::log_debug("Copying XML files from SMS workspace to ", destination_dir)
  obs_path <- list.files(file.path(sms_path, "Obs"), full.names = TRUE)
  soil_path <- file.path(sms_path, "Soil","sols.xml")
  tec_path <- list.files(file.path(sms_path, "Tec"), full.names = TRUE)
  ini_path <- list.files(file.path(sms_path, "USMs"), full.names = TRUE)
  usms_path <- file.path(sms_path, "USMs","usms.xml")
  clim_path <- list.files(file.path(sms_path, "Climate"), full.names = TRUE)

  stics_input_files_path <- file.path(stics_path, "input_files")
  model_path <- file.path(
    stics_input_files_path,
    "model",
    c("var.mod", "prof.mod", "rap.mod", "param_gen.xml", "param_newform.xml")
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
  file.copy(from = files_path, to = destination_dir)
  plant_path <- list.files(
    file.path(stics_input_files_path, "plant"),
    full.names = TRUE
  )
  if (!dir.exists(file.path(destination_dir, "plant"))) {
    dir.create(file.path(destination_dir,"plant"))
  }
  file.copy(from = plant_path, to = file.path(destination_dir, "plant"))
}

#' Generate a Stics workspace from SMS data.
#'
#' @param sms_path path to the SMS repository
#' @param stics_path path to Stics repository
#' @param workspace path to the Stics workspace
#'
#' @returns a DataSource object containing the USM names list
gen_sms_workspace <- function(
  sms_path,
  stics_path,
  workspace
) {
  logger::log_info("Generating SMS workspace...")
  usms <- get_sms_usms_names(sms_path)
  rotations <- get_sms_rotations(sms_path)
  workspace_tmp <- tempfile()
  dir.create(workspace_tmp)
  extract_sms_data(sms_path, stics_path, workspace_tmp)
  logger::log_debug("Generating text workspace using ", workspace_tmp, " files")
  SticsRFiles::gen_usms_xml2txt(
    workspace = workspace_tmp,
    out_dir = workspace,
    verbose = is_debug(),
    usm = usms,
    parallel = TRUE
  )
  unlink(workspace_tmp, recursive = TRUE)

  new(
    "DataSource",
    usms = usms,
    rotations = rotations
  )
}
