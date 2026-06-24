prepare_output_dir <- function(output_dir) {
  o_dir <- file.path(output_dir)
  if (!dir.exists(o_dir) && !dir.create(o_dir, recursive = TRUE)) {
    stop(
      "Can't create output directory ",
      o_dir,
      call. = FALSE
    )
  }
}

#' Export evaluation statistics to CSV files
#'
#' This function exports different evaluation metrics from the evaluation
#' workspace into CSV files. It retrieves data from the workspace and writes
#' the following datasets when available:
#'
#' - Species statistics (`species_stats.csv`)
#' - Global statistics (`global_stats.csv`)
#' - rRMSE per USM (`rRMSE_per_usm.csv`)
#' - List of deteriorated USMs (`Deteriorated_USM.csv`)
#'
#' The data are retrieved from the evaluation workspace using dedicated helper
#' functions. Files are written safely using `safe_write_csv()`.
#'
#' @param config List. Configuration object created by `make_config()`,
#'    containing all parameters required for the export.
#'
#' @return NULL. This function is called for its side effects (writing files).
#'
#' @details
#' The function:
#' \itemize{
#'   \item Validates the configuration for export.
#'   \item Creates the output directory if needed.
#'   \item Exports species statistics if available.
#'   \item Exports global statistics if available.
#'   \item Exports rRMSE per USM if available.
#'   \item Exports deteriorated USM information if available.
#' }
#'
#' @examples
#' \dontrun{
#' config <- make_config(
#'  output_dir = "results/"
#' )
#' export_stats_to_csv(config)
#' }
#'
#' @export
export_stats_to_csv <- function(config) {
  start_time <- Sys.time()
  config$validate_export()
  prepare_output_dir(config$output_dir)
  eval_workspace <- EvalWorkspace$new(config$eval_workspace)
  species <- eval_workspace$get_species()
  logger::log_info("Exporting stats data")
  stats <- eval_workspace$get_stats(species, TRUE)
  if (!is.null(stats)) {
    safe_write_csv(stats, file.path(config$output_dir, "species_stats.csv"))
  }
  global_stats <- eval_workspace$get_global_stats(collect = TRUE)
  if (!is.null(global_stats)) {
    safe_write_csv(
      global_stats,
      file.path(config$output_dir, "global_stats.csv")
    )
  }
  rrmse_per_usm <- eval_workspace$get_rrmse_per_usm(species, TRUE)
  if (!is.null(rrmse_per_usm)) {
    safe_write_csv(
      rrmse_per_usm, file.path(config$output_dir, "rRMSE_per_usm.csv")
    )
  }
  deteriorated_usms <- eval_workspace$get_deteriorated_usm(
    species, config$percentage
  )
  if (!is.null(deteriorated_usms) && !is.null(deteriorated_usms$get_data())) {
    safe_write_csv(
      deteriorated_usms$get_data(),
      file.path(config$output_dir, "Deteriorated_USM.csv")
    )
  }
  logger::log_info(paste0("Stats export time: ", format_duration(start_time)))
}
