#' Getting the filtered USM list using the typo file.
#'
#' This function is specific to SMS data source
#'
#' @param sms_path path to the SMS repository
#'
#' @returns a list of usm with their associated species
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

extract_sms_data <- function(sms_path, stics_path, destination_dir) {
  obs_path <- list.files(file.path(sms_path, "Obs"), full.names = TRUE)
  soil_path <- file.path(sms_path, "Soil","sols.xml")
  tec_path <- list.files(file.path(sms_path, "Tec"), full.names = TRUE)
  ini_path <- list.files(file.path(sms_path, "USMs"), full.names = TRUE)
  usms_path <- file.path(sms_path, "USMs","usms.xml")
  clim_path <- list.files(file.path(sms_path, "Climate"), full.names = TRUE)
  model_path <- file.path(
    stics_path,
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
  plant_path <- list.files(file.path(stics_path, "plant"), full.names = TRUE)
  if (!dir.exists(file.path(destination_dir, "plant"))) {
    dir.create(file.path(destination_dir,"plant"))
  }
  file.copy(from = plant_path, to = file.path(destination_dir, "plant"))
}

gen_sms_workspace <- function(
  sms_path,
  stics_path,
  workspace
) {
  # getting usms list
  usms_path <- file.path(sms_path, "USMs", "usms.xml")
  all_usms <- SticsRFiles::get_usms_list(file = usms_path)

  usms <- get_sms_usms_list(sms_path)
  usms_exist <- all_usms %in% usms
  if (!any(usms_exist))
    stop("All usms are missing !")
  sel_usms <- usms

  usmsdoc <- SticsRFiles:::xmldocument(usms_path)
  workspace_tmp <- tempfile()
  dir.create(workspace_tmp)
  extract_sms_data(sms_path, stics_path, workspace_tmp)
  SticsRFiles::gen_usms_xml2txt(
    workspace = workspace_tmp,
    out_dir = workspace,
    verbose = FALSE,
    usm = sel_usms,
    parallel = TRUE
  )
  unlink(workspace_tmp, recursive = TRUE)
  SticsRFiles:::delete(usmsdoc)

  new(
    "DataSource",
    usms = usms
  )
}
