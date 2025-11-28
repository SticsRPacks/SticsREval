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
get_sms_usms_list <- function(sms_path) {
  library(dplyr)
  filter_file_path <- file.path(sms_path, "typo_usms.csv")
  filter_file <- try(read.csv2(
    filter_file_path,
    header = TRUE,
    na.strings = c(NA, "NaN", "OK", "rejection M=0"),
    sep = ";",
    stringsAsFactors = FALSE
  ), TRUE)
  if (is(filter_file, "try-error")) {
    stop(paste0("Filter file could not be loaded: ", filter_file_path))
  }
  usm_list <- filter_file %>%
    dplyr::filter(source == "sms", UsedForCalibration == 0)
  usm_list$usm
}

#' Extract all necessary files from SMS and copy it to a destination directory.
#'
#' @param sms_path path to the SMS repository
#' @param stics_path path to Stics repository
#' @param destination_dir path where the files must be copied
#'
extract_sms_data <- function(sms_path, stics_path, destination_dir) {
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
  cp_status <- file.copy(from = files_path, to = destination_dir)
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
  usms <- get_sms_usms_list(sms_path)
  workspace_tmp <- tempfile()
  dir.create(workspace_tmp)
  extract_sms_data(sms_path, stics_path, workspace_tmp)
  SticsRFiles::gen_usms_xml2txt(
    workspace = workspace_tmp,
    out_dir = workspace,
    verbose = FALSE,
    usm = usms,
    parallel = TRUE
  )
  unlink(workspace_tmp, recursive = TRUE)

  new(
    "DataSource",
    usms = usms
  )
}
