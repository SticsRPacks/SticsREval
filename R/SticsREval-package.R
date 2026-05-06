#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom R6 R6Class
#' @importFrom arrow open_dataset write_dataset write_parquet
#' @importFrom dplyr filter select mutate collect distinct pull arrange
#' @importFrom dplyr group_by summarise left_join inner_join across all_of
#' @importFrom dplyr any_of if_else desc bind_rows case_when
#' @importFrom rlang .data %||% abort last_trace
#' @importFrom methods is
#' @importFrom utils getS3method
#' @importFrom future availableCores plan multisession sequential
#' @importFrom future.apply future_lapply
#' @importFrom ggplot2 ggplot aes labs geom_point scale_color_manual
#' @importFrom ggplot2 geom_abline theme ggtitle
#' @importFrom ggrepel geom_text_repel
#' @importFrom logger log_info log_warn log_error log_debug
#' @importFrom SticsRFiles get_plant_txt get_sim get_obs
#' @importFrom SticsOnR get_version_number stics_wrapper_options stics_wrapper
#' @importFrom readr write_delim read_delim locale
## usethis namespace: end
NULL
