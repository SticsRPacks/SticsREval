#' Getting the filtered USM list using the typo file.
#'
#' This function is specific to SMS data source
#'
#' @param workspace path to the SMS workspace
#'
#' @returns a list of usm with their associated species
#'
#' @examples
#'   get_usms_list("/path/to/workspace")
get_usms_list <- function(workspace) {
  library(dplyr)
  filter_file_path <- file.path(workspace, "typo_usms.csv")
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
    dplyr::filter(source == "sms", UsedForCalibration == 0) %>%
    dplyr::rename(species = plant1, situation = usm) %>%
    dplyr::select(c(situation, species))
  usm_list
}

gen_workspace <- function(
  sms_path,
  stics_path,
  workspace
) {
  workspace_tmp <- file.path(workspace, "tmp")
  if (!dir.exists(workspace_tmp)) {
    dir.create(workspace_tmp, recursive = TRUE)
  }

  # getting usms list
  usms_path <- file.path(sms_path, "USMs", "usms.xml")
  all_usms <- SticsRFiles::get_usms_list(file = usms_path)

  usms <- get_usms_list(sms_path)
  usms_exist <- all_usms %in% usms$situation
  if (!any(usms_exist))
    stop("All usms are missing !")
  sel_usms <- usms$situation

  usmsdoc <- SticsRFiles:::xmldocument(usms_path)

  # Obs
  obs_path <- file.path(
    sms_path,
    "Obs",
    get_names_from_usms(usmsdoc@content, "fobs", usms_exist)
  )
  plant_path <- file.path(
    stics_path,
    "plant",
    get_names_from_usms(usmsdoc@content, "fplt", usms_exist)
  )
  # *.mod files
  mod_path <- file.path(
    stics_path,
    "model",
    c("var.mod", "prof.mod", "rap.mod")
  )
  # general parameters
  param_gen_path <- file.path(
    stics_path,
    "model",
    c("param_gen.xml", "param_newform.xml")
  )
  # Soil:
  soil_path <- file.path(sms_path, "Soil","sols.xml")
  rewrite_sols_file(
    usms_file = usms_path,
    sols_file = soil_path,
    out_dir = workspace_tmp,
    usms = sel_usms
  )
  tec_path <- file.path(
    sms_path,
    "Tec",
    get_names_from_usms(usmsdoc@content, "ftec", usms_exist)
  )
  ini_path <- file.path(
    sms_path,
    "USMs",
    get_names_from_usms(usmsdoc@content, "finit", usms_exist)
  )
  usms_path <- file.path(sms_path, "USMs","usms.xml")
  rewrite_usms_file(
    usms_file = usms_path,
    out_dir = workspace_tmp,
    usms = sel_usms
  )
  clim_path <- file.path(
    sms_path,
    "Climate",
    get_names_from_usms(usmsdoc@content, "fclim", usms_exist)
  )
  sta_path <- file.path(
    sms_path,
    "Climate",
    get_names_from_usms(usmsdoc@content, "fstation", usms_exist)
  )
  files_path <- c(
    obs_path,
    tec_path,
    ini_path,
    clim_path,
    sta_path,
    usms_path,
    soil_path
  )

  cp_status <- file.copy(from = files_path, to = workspace_tmp)
  if (!dir.exists(file.path(workspace_tmp, "plant"))) {
    dir.create(file.path(workspace_tmp,"plant"))
  }
  file.copy(from = plant_path, to = file.path(workspace_tmp, "plant"))

  file.copy(from = param_gen_path, to = workspace_tmp)
  file.copy(from = mod_path, to = workspace_tmp)
  SticsRFiles::gen_usms_xml2txt(
    workspace = workspace_tmp,
    out_dir = workspace,
    verbose = FALSE,
    usm = sel_usms
  )
  unlink(workspace_tmp, recursive = TRUE)
  SticsRFiles:::delete(usmsdoc)

  usms
}


get_names_from_usms <- function(xml_doc, par_name, usms_exist = NULL) {

  node_names <- c("fobs", "fplt", "ftec", "finit", "flai", "fclim",
                  "nomsol", "fstation")
  files_nb <- c(2, 2, 2, 1, 2, 2, 1, 1)

  idx <- which(node_names %in% par_name )

  if (length(idx) < 1)
    stop("unknown node name: ", par_name)

  if (grepl(pattern = "^fclim", x = par_name))
    par_name <- "*[contains(name(),'fclim')]"

  xpath <- paste0("//", par_name)

  file_names <- unlist(lapply(
    XML::getNodeSet(doc = xml_doc,
                    path = xpath),
    XML::xmlValue
  ))


  file_names_matrix <- matrix(file_names, ncol = files_nb[idx], byrow = TRUE)

  if (!is.null(usms_exist))
    file_names_matrix <- file_names_matrix[usms_exist, ]

  file_names <- file_names_matrix[file_names_matrix != "null"]

  return(unique(file_names))

}

rewrite_usms_file <- function(usms_file, out_dir, usms) {
  usms_doc <- SticsRFiles:::xmldocument(file = usms_file)

  all_usms <- SticsRFiles::get_usms_list(file = usms_file)

  usms_to_remove <- setdiff(all_usms, usms)

  if(is.null(usms_to_remove)) return()
  nodes_to_remove <- unlist(lapply(usms_to_remove, function(x) {
    xpath = paste0('//usm[@nom="', x, '"]')
    SticsRFiles:::get_nodes(usms_doc, xpath)
  }
  ))

  XML::removeNodes(nodes_to_remove)
  SticsRFiles:::save_xml_doc(usms_doc, file.path(out_dir, "usms.xml"))
}

rewrite_sols_file <- function(usms_file, sols_file, out_dir, usms) {
  sols_doc <- SticsRFiles:::xmldocument(file = sols_file)
  all_sols <- SticsRFiles::get_soils_list(sols_file)
  sols <- unique(SticsRFiles::get_param_xml(
    file = usms_file,
    param = "nomsol",
    select = "usm",
    select_value = usms
  )$usms.xml$nomsol)
  sols_to_remove <- setdiff(all_sols, sols)
  if(is.null(sols_to_remove)) return()

  nodes_to_remove <- unlist(lapply(sols_to_remove, function(x) {
    xpath = paste0('//sol[@nom="', x, '"]')
    SticsRFiles:::get_nodes(sols_doc, xpath)
  }
  ))
  XML::removeNodes(nodes_to_remove)
  SticsRFiles:::save_xml_doc(sols_doc, file.path(out_dir, "sols.xml"))
}


