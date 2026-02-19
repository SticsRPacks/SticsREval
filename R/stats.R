read_ref_stats <- function(
  species,
  reference_data_dir
) {
  reference_dir <- file.path(reference_data_dir, species)
  reference_file <- file.path(reference_dir, "Criteres_stats.csv")
  if (!length(reference_file) || !file.exists(reference_file)) {
    return(NULL)
  }
  read_csv(reference_file)
}

save_stats <- function(
  stats,
  output_dir
) {
  safe_write_csv(stats, file.path(output_dir, "Criteres_stats.csv"))
}

read_ref_rmse_per_usm <- function(species, reference_data_dir) {
  reference_dir <- file.path(reference_data_dir, species)
  reference_file <- file.path(reference_dir, "RMSE_per_usm.csv")
  if (!length(reference_file) || !file.exists(reference_file)) {
    return(NULL)
  }
  read_csv(reference_file)
}

save_rmse_per_usm <- function(rmse_per_usm, output_dir) {
  safe_write_csv(rmse_per_usm, file.path(output_dir, "RMSE_per_usm.csv"))
}

save_deteriorated_usm <- function(deteriorated, output_dir, percentage) {
  safe_write_csv(
    deteriorated,
    file.path(output_dir, "Deteriorated_RMSE_per_usm.csv")
  )
}

gen_species_stats <- function(species, sim, obs, save, output_dir) {
  logger::log_info("Generating statistics for ", species)
  stats <- run_with_log_control(
    # Calling summary() directly does not work in a future context
    CroPlotR:::summary.cropr_simulation(sim, obs = obs)
  )
  if (save) {
    save_stats(stats, output_dir)
  }
  stats
}

gen_species_rmse_per_usm <- function(species, sim, obs, save, output_dir) {
  logger::log_info("Generating RMSE per USM for species ", species)
  rmse_per_usm <- run_with_log_control(
    # Calling summary() directly does not work in a future context
    CroPlotR:::summary.cropr_simulation(
      sim,
      obs = obs,
      all_situations = FALSE,
      stats = "rRMSE"
    )
  )
  if (save) {
    save_rmse_per_usm(rmse_per_usm, output_dir)
  }
  rmse_per_usm
}