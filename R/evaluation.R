library(CroPlotR)
library(plotly)
library(dplyr)
library(htmltools)

#' Running evaluation over a USM list
#'
#' @description
#' At first, statistical criteria are computed using the CroPlotR package.
#' Then, if a reference data directory is specified, the reference RMSE is
#' compared to the computed RMSE.
#'
#'
#' @param species the species corresponding to the simulations and observations
#' @param sim a list of simulations
#' @param obs a list of observations
#' @param workspace path to the simulation and observation data
#' @param reference_data_dir Path to the directory which contains the reference
#' data to use for comparison
#' @param verbose Logical value for displaying or not information while running
#'
#' @returns a list containing the Comparison objects for the species
evaluate_species <- function(
  species,
  sim,
  obs,
  config
) {
  eval_res <- list(
    species = species,
    comparison = NULL,
    stats = NULL,
    ref_stats = NULL,
    sim = sim,
    obs = obs
  )
  eval_res$stats <- summary(sim, obs = obs)
  eval_res$ref_stats <- read_ref_stats(config, species)
  if (!is.null(eval_res$ref_stats)) {
    logger::log_debug("Comparing RMSE for species ", species)
    eval_res$comparison <- compare_rmse(
      species,
      eval_res$ref_stats,
      eval_res$stats
    )
  }
  eval_res
}

evaluate_all_species <- function(species, sorted_usms, sim, obs, config) {
  backend <- setup_parallel_backend(config, length(species))
  on.exit(backend$cleanup(), add = TRUE)
  `%do_par_or_not%` <- backend$do
  eval_results <- foreach::foreach(
    i = seq_along(species),
    .packages = c("dplyr", "CroPlotR")
  ) %do_par_or_not% {
    spec <- species[i]
    logger::log_info("Starting evaluation of species ", spec)
    selected_usms <- dplyr::filter(sorted_usms, species == spec)$usm
    common_usms <- selected_usms[
      selected_usms %in% names(sim) &
        selected_usms %in% names(obs)
    ]
    if (!length(common_usms)) {
      logger::warning("No common USM for species ", spec, ".")
      return(NULL)
    }
    selected_sim <- sim[common_usms]
    selected_obs <- obs[common_usms]
    if (length(selected_sim) == 0 || length(selected_obs) == 0) {
      logger::warning(
        "No simulation or observation data found for species ",
        spec
      )
      return(NULL)
    }
    evaluate_species(
      spec,
      selected_sim,
      selected_obs,
      config
    )
  }
  eval_results
}

display_comparison <- function(species, comparison) {
  logger::log_info(
    "-----------------------------------------------------------------"
  )
  logger::log_info("Species: ", species)
  total <- length(comparison$critical) +
    length(comparison$warning) +
    length(comparison$improved)
  logger::log_info("Total number of variables: ", total)
  logger::log_info(
    length(comparison$critical),
    " deteriorated variables (>5%): "
  )
  if (length(comparison$critical) > 0) {
    logger::log_info(paste(comparison$critical, collapse = ", "))
  }
  logger::log_info(
    length(comparison$warning),
    " deteriorated variables (>0%, <=5%): "
  )
  if (length(comparison$warning) > 0) {
    logger::log_info(paste(comparison$warning, collapse = ", "))
  }
  logger::log_info(length(comparison$improved), " improved variables (<0%): ")
  if (length(comparison$improved) > 0) {
    logger::log_info(paste(comparison$improved, collapse = ", "))
  }
  logger::log_info(
    "-----------------------------------------------------------------"
  )
}

export_evaluation_result <- function(config, eval_result) {
  species_output_dir <- file.path(config$output_dir, eval_result$species)
  if (!is.null(config$exports) && !file.exists(species_output_dir)) {
    logger::log_info("Exporting ", eval_result$species, " evaluation results")
    dir.create(species_output_dir)
  }
  if ("sim" %in% config$exports) {
    logger::log_debug("Exporting ", eval_result$species, " simulation data")
    save_sim(config, eval_result$species, eval_result$sim)
  }
  if ("stats" %in% config$exports) {
    logger::log_debug("Exporting ", eval_result$species, " statistics")
    save_stats(config, eval_result$species, eval_result$stats)
  }
  comparison <- eval_result$comparison
  if ("plots" %in% config$exports && !is.null(comparison)) {
    logger::log_debug("Generating ", eval_result$species, " scatter plots")
    ref_sim <- read_ref_sim(config, eval_result$species)
    deteriorated <- c(comparison$critical, comparison$warning)
    gen_scatter_plot(
      eval_result$sim,
      ref_sim,
      eval_result$obs,
      deteriorated,
      species_output_dir
    )
    if (!is.null(eval_result$ref_stats)) {
      logger::log_debug("Generating ", eval_result$species, " comparison plot")
      gen_comparison_plot(
        eval_result$stats,
        eval_result$ref_stats,
        species_output_dir
      )
    }
  }
  if (!is.null(comparison)) {
    display_comparison(eval_result$species, comparison)
  }
}

sort_usm_by_species <- function(config, usms) {
  logger::log_debug("Sorting USMs by species...")
  backend <- setup_parallel_backend(config, length(usms))
  on.exit(backend$cleanup(), add = TRUE)
  `%do_par_or_not%` <- backend$do

  result <- foreach::foreach(
    i = seq_along(usms)
  ) %do_par_or_not% {
    usm <- usms[i]
    species <- SticsRFiles::get_plant_txt(
      workspace = file.path(config$workspace, usm)
    )
    list(
      species = species$codeplante,
      usm = usm
    )
  }
  sorted <- dplyr::bind_rows(result)
  logger::log_debug("Found ", length(unique(sorted$species)), " species")
  sorted
}

#' @title Running a complete evaluation process of STICS model
#'
#' @param config List containing any information needed for the evaluation
#'  process. See [make_config()] for the complete list of parameters.
#'
#' @export
evaluate <- function(config) {
  init_logger(config$verbose)
  start_time <- Sys.time()
  logger::log_info("Starting evaluation...")
  ds <- get_data_source_from_config(config)
  usms <- usms(ds)
  sim <- load_workspace_sim(config, usms, rotations(ds))
  obs <- load_workspace_obs(config, usms)
  sorted_usms <- sort_usm_by_species(config, usms)
  species <- unique(sorted_usms$species)
  eval_results <- evaluate_all_species(species, sorted_usms, sim, obs, config)
  invisible(lapply(eval_results, function(res) {
    if (!is.null(res)) export_evaluation_result(config, res)
  }))
  criticals <- vapply(eval_results, function(res) {
    if (is.null(res) || is.null(res$comparison)) return(0L)
    length(res$comparison$critical)
  }, integer(1))
  end_time <- Sys.time()
  time_taken <- round(end_time - start_time, 2)
  logger::log_info("Evaluation time: ", time_taken, " s")
  if (sum(criticals) > 0) {
    logger::log_error("Found at least one critical deteriorated variable")
    stop()
  }
}
