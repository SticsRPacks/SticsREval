#' @importFrom rlang .data
get_sms_usms_list <- function(sms_path) {
  filter_file_path <- file.path(sms_path, "typo_usms.csv")
  filter_file <- try(read_csv(filter_file_path), TRUE)
  if (is(filter_file, "try-error")) {
    stop(paste0("Filter file could not be loaded: ", filter_file_path))
  }
  filter_file %>%
    dplyr::filter(.data$source == "sms", .data$UsedForCalibration == 0)
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
  logger::log_info("Found ", length(usm_list$usm), " USMs in ", sms_path)
  usm_list$usm
}

#' Extract all necessary files from SMS and copy it to a destination directory.
#'
#' @param sms_path path to the SMS repository
#' @param stics_path path to Stics repository
#' @param destination_dir path where the files must be copied
#'
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
    dir.create(file.path(destination_dir, "plant"))
  }
  file.copy(from = plant_path, to = file.path(destination_dir, "plant"))
}

#' Generate a Stics workspace from SMS data.
#'
#' @param sms_path path to the SMS repository
#' @param stics_path path to Stics repository
#' @param workspace path to the Stics workspace
#' @param parallel Boolean. Is the computation to be done in parallel ?
#' @param cores Number of cores to use for parallel computation
#'
#' @returns a DataSource object containing the USM names list
#'
#' @export
gen_workspace_from_sms <- function(
  sms_path,
  stics_path,
  workspace,
  parallel = FALSE,
  cores = NA
) {
  logger::log_info("Generating SMS workspace...")
  usms <- get_sms_usms_names(sms_path)
  workspace_tmp <- tempfile()
  dir.create(workspace_tmp)
  on.exit({
    unlink(workspace_tmp, recursive = TRUE)
    logger::log_info("Temporary workspace ", workspace_tmp, " deleted.")
  }, add = TRUE)
  extract_sms_data(sms_path, stics_path, workspace_tmp)
  logger::log_info("Generating text workspace using ", workspace_tmp, " files")
  SticsRFiles::gen_usms_xml2txt(
    workspace = workspace_tmp,
    out_dir = workspace,
    verbose = is_debug(),
    usm = usms,
    parallel = parallel,
    cores = cores
  )
}
