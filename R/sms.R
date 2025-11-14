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
