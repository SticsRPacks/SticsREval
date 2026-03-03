prepare_species_output_dir <- function(output_dir, species) {
  o_dir <- file.path(output_dir, species)
  if (!dir.exists(o_dir) && !dir.create(o_dir, recursive = TRUE)) {
    stop(paste0(
      "Can't create output directory ", o_dir, " for species {species}"
    ))
  }
  o_dir
}

#' Export evaluation statistics to CSV files
#'
#' This function exports different evaluation metrics for each species found
#' in the evaluation workspace into CSV files. For every species, it creates
#' an output directory (if needed) and writes the following datasets when
#' available:
#'
#' - Global statistics (`Criteres_stats.csv`)
#' - RMSE per USM (`RMSE_per_usm.csv`)
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
#' For each species:
#' \itemize{
#'   \item Creates a species-specific output directory.
#'   \item Exports statistics if available.
#'   \item Exports RMSE per USM if available.
#'   \item Exports deteriorated USM information if available.
#' }
#'
#' @examples
#' \dontrun{
#' export_stats_to_csv("results/", eval_workspace = "workspace/")
#' }
#'
#' @export
export_stats_to_csv <- function(config) {
  start_time <- Sys.time()
  validate_export_config(config)
  species <- get_species(config$eval_workspace)
  for (spec in species) {
    logger::log_info("Exporting stats data for species {spec}")
    o_dir <- prepare_species_output_dir(config$output_dir, spec)
    stats <- get_stats(config$eval_workspace, spec, TRUE)
    if (!is.null(stats)) {
      safe_write_csv(stats, file.path(o_dir, "Criteres_stats.csv"))
    }
    rmse_per_usm <- get_rmse_per_usm(config$eval_workspace, spec, TRUE)
    if (!is.null(rmse_per_usm)) {
      safe_write_csv(
        rmse_per_usm, file.path(o_dir, "RMSE_per_usm.csv")
      )
    }
    deteriorated_usm <- get_deteriorated_usm(config$eval_workspace, spec, TRUE)
    if (!is.null(deteriorated_usm)) {
      safe_write_csv(
        deteriorated_usm, file.path(o_dir, "Deteriorated_USM.csv")
      )
    }
  }
  logger::log_info(paste0("Stats export time: ", format_duration(start_time)))
}

#' Export simulation data per species to Parquet files
#'
#' This function exports simulation datasets for each species found in the
#' evaluation workspace into Parquet files. For every species, it creates
#' an output directory (if needed) and writes the corresponding simulation
#' data as a Parquet file.
#'
#' Simulation data are retrieved using `get_by_species()` and written using
#' `arrow::write_parquet()`.
#'
#' @param config List. Configuration object created by `make_config()`,
#'    containing all parameters required for the export.
#'
#' @return NULL. This function is called for its side effects (writing files).
#'
#' @details
#' For each species:
#' \itemize{
#'   \item Creates a species-specific output directory.
#'   \item Retrieves simulation data.
#'   \item Writes the data to a `Simulations.parquet` file.
#' }
#'
#' @examples
#' \dontrun{
#' export_species_sim("results/", eval_workspace = "workspace/")
#' }
#'
#' @export

export_species_sim <- function(config) {
  start_time <- Sys.time()
  validate_export_config(config)
  species <- get_species(config$eval_workspace)
  for (spec in species) {
    logger::log_info("Exporting simulations data for species {spec}")
    o_dir <- prepare_species_output_dir(config$output_dir, spec)
    sim <- get_by_species(config$eval_workspace, spec, "sim")
    arrow::write_parquet(
      x = sim,
      sink = file.path(o_dir, "Simulations.parquet")
    )
  }
  logger::log_info(paste0(
    "Simulation export time: ", format_duration(start_time)
  ))
}