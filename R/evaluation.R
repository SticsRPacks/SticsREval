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
#'
#' @returns a list containing the Comparison objects for the species
evaluate_species <- function(
  species,
  sim,
  obs,
  reference_data_dir = get_config_env()$reference_data_dir
) {
  eval_res <- list(
    species = species,
    comparison = NULL,
    stats = NULL,
    sim = sim,
    obs = obs
  )
  eval_res$stats <- run_with_log_control(
    # Calling summary() directly does not work in a future context
    CroPlotR:::summary.cropr_simulation(sim, obs = obs)
  )
  ref_stats <- read_ref_stats(species, reference_data_dir)
  if (!is.null(ref_stats)) {
    logger::log_debug("Comparing RMSE for species ", species)
    eval_res$comparison <- compare_rmse(
      species,
      ref_stats,
      eval_res$stats
    )
  }
  eval_res
}

evaluate_all_species <- function(species, sorted_usms, sim, obs) {
  config <- get_config_env()
  backend <- setup_parallel_backend(length(species))
  on.exit(backend$cleanup(), add = TRUE)
  eval_results <- backend$map(seq_along(species), function(i) {
    logger::log_appender(logger::appender_stdout)
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
      config$reference_data_dir
    )
  })
  # Remove NULL values
  eval_results[!vapply(eval_results, is.null, logical(1))]
}

export_evaluation_result <- function(eval_result, config = get_config_env()) {
  species_output_dir <- file.path(config$output_dir, eval_result$species)
  if (!is.null(config$exports) && !file.exists(species_output_dir)) {
    logger::log_info("Exporting ", eval_result$species, " evaluation results")
    dir.create(species_output_dir)
  }
  if ("sim" %in% config$exports) {
    logger::log_debug("Exporting ", eval_result$species, " simulation data")
    save_sim(eval_result$species, eval_result$sim)
  }
  if ("stats" %in% config$exports) {
    logger::log_debug("Exporting ", eval_result$species, " statistics")
    save_stats(eval_result$species, eval_result$stats)
  }
  comparison <- eval_result$comparison
  if (!is.null(comparison)) {
    log_comparison(comparison)
    if ("plots" %in% config$exports) {
      gen_plots_file(
        eval_result$species,
        species_output_dir,
        eval_result$comparison,
        eval_result$sim,
        eval_result$obs
      )
      logger::log_debug(eval_result$species, " plots file generated")
    }
  }
}

sort_usm_by_species <- function(usms, config = get_config_env()) {
  logger::log_debug("Sorting USMs by species...")
  backend <- setup_parallel_backend(length(usms))
  on.exit(backend$cleanup(), add = TRUE)
  result <- backend$map(seq_along(usms), function(i) {
    logger::log_appender(logger::appender_stdout)
    usm <- usms[i]
    species <- SticsRFiles::get_plant_txt(
      workspace = file.path(config$workspace, usm)
    )
    list(
      species = species$codeplante,
      usm = usm
    )
  })
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
  init_config_env(config)
  init_logger(config$verbose)
  start_time <- Sys.time()
  logger::log_info("Starting evaluation...")
  ds <- get_data_source_from_config()
  usms <- usms(ds)
  sim <- load_workspace_sim(usms, rotations(ds))
  obs <- load_workspace_obs(usms)
  sorted_usms <- sort_usm_by_species(usms)
  species <- unique(sorted_usms$species)
  eval_results <- evaluate_all_species(species, sorted_usms, sim, obs)
  # Sorting eval results by species
  eval_results <- eval_results[order(sapply(eval_results, `[[`, "species"))]
  comparisons <- lapply(eval_results, function(res) {
    export_evaluation_result(res)
    res$comparison
  })
  log_comparison_table(comparisons)
  criticals <- vapply(comparisons, function(res) {
    if (is.null(res)) return(0L)
    length(get_crit_vars(res))
  }, integer(1))
  warnings <- vapply(comparisons, function(res) {
    if (is.null(res)) return(0L)
    length(get_warn_vars(res))
  }, integer(1))
  end_time <- Sys.time()
  time_taken <- round(end_time - start_time, 2)
  logger::log_info("Evaluation time: ", time_taken, " s")
  if (sum(warnings) > 0) {
    logger::log_warn("Found at least one deteriorated variable")
  }
  if (sum(criticals) > 0) {
    logger::log_error("Found at least one critical deteriorated variable")
    stop()
  }
}
