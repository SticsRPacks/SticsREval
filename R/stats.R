#' @importFrom utils getS3method
gen_species_stats <- function(
  eval_workspace, species, parallel, cores, usms = NULL, var2exclude = NULL
) {
  evaluated_version <- get_stics_version(eval_workspace)
  results <- parallelizable_loop(
    length(species),
    parallel,
    cores,
    function(i) {
      spec <- species[i]
      logger::log_debug(
        "Splitting simulations and observations data for species {spec}"
      )
      exclude <- c("version", "species")
      if (!is.null(var2exclude)) {
        exclude <- c(exclude, var2exclude)
      }
      splited_sim <- CroPlotR::split_df2sim(
        get_sim(
          eval_workspace, evaluated_version, spec,
          usms = usms, var2exclude = exclude
        )
      )
      splited_obs <- CroPlotR::split_df2sim(
        get_obs(
          eval_workspace, evaluated_version, spec,
          usms = usms, var2exclude = exclude
        )
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
    save_stats(eval_workspace, evaluated_version, res$species, res$stats)
    logger::log_debug("Saving RMSE per USM for species {res$species}")
    save_rmse_per_usm(
      eval_workspace, evaluated_version, res$species, res$rmse_per_usm
    )
    logger::log_debug("RMSE per USM for species {res$species} saved.")
  }
}
