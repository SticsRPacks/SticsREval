read_ref_stats <- function(species, reference_data_dir) {
  reference_dir <- file.path(reference_data_dir, species)
  reference_file <- file.path(reference_dir, "Criteres_stats.csv")
  if (!length(reference_file) || !file.exists(reference_file)) {
    logger::log_warn(
      "No reference stats found for species {species} in {reference_dir}"
    )
    return(NULL)
  }
  read_csv(reference_file)
}

read_ref_rmse_per_usm <- function(species, reference_data_dir) {
  reference_dir <- file.path(reference_data_dir, species)
  reference_file <- file.path(reference_dir, "RMSE_per_usm.csv")
  if (!length(reference_file) || !file.exists(reference_file)) {
    logger::log_warn(
      "No reference RMSE per USM found for species {species} in {reference_dir}"
    )
    return(NULL)
  }
  read_csv(reference_file)
}

#' @importFrom utils getS3method
gen_species_stats <- function(
  eval_workspace, species, parallel, cores, usms = NULL
) {
  results <- parallelizable_loop(
    length(species),
    parallel,
    cores,
    function(i) {
      spec <- species[i]
      logger::log_debug(
        "Splitting simulations and observations data for species {spec}"
      )
      splited_sim <- CroPlotR::split_df2sim(
        get_by_species(eval_workspace, spec, "sim", collect = TRUE, usms = usms)
      )
      splited_obs <- CroPlotR::split_df2sim(
        get_by_species(eval_workspace, spec, "obs", collect = TRUE, usms = usms)
      )

      logger::log_info("Generating statistics for ", spec)
      loadNamespace("CroPlotR")
      stats <- run_with_log_control(
        getS3method("summary", class(splited_sim))(
          splited_sim,
          obs = splited_obs
        )
      )
      logger::log_info("Generating RMSE per USM for species ", spec)
      rmse_per_usm <- run_with_log_control(
        getS3method("summary", class(splited_sim))(
          splited_sim,
          obs = splited_obs,
          all_situations = FALSE,
          stats = "rRMSE"
        )
      )
      list(
        species = spec,
        stats = stats,
        rmse_per_usm = rmse_per_usm
      )
    }
  )
  for (res in results) {
    logger::log_debug("Saving statistics for ", res$species)
    save_stats(eval_workspace, res$species, res$stats)
    logger::log_debug("Saving RMSE per USM for species {res$species}")
    save_rmse_per_usm(eval_workspace, res$species, res$rmse_per_usm)
    logger::log_debug("RMSE per USM for species {res$species} saved.")
  }
}
