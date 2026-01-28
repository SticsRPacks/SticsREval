library(CroPlotR)
library(plotly)
library(dplyr)
library(htmltools)

.local_eval_env <- new.env(parent = emptyenv())

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
  reference_data_dir
) {
  eval_res <- list(
    species = species,
    comparison = NULL,
    stats = NULL
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

evaluate_all_species <- function(
  species,
  sorted_usms,
  reference_data_dir,
  parallel,
  cores
) {
  eval_results <- parallelizable_loop(
    length(species),
    parallel,
    cores,
    function(i, env) {
      spec <- species[i]
      logger::log_info("Starting evaluation of species ", spec)
      selected_usms <- dplyr::filter(sorted_usms, species == spec)$usm
      sim_situations <- get_all_sim_situations(env$data_dir)
      obs_situations <- get_all_obs_situations(env$data_dir)
      common_usms <- selected_usms[
        selected_usms %in% sim_situations &
          selected_usms %in% obs_situations
      ]
      if (!length(common_usms)) {
        logger::warning("No common USM for species ", spec, ".")
        return(NULL)
      }
      selected_sim <- get_sim_by_situations(env$data_dir, common_usms)
      save_species_sim(
        env$data_dir,
        selected_sim,
        spec
      )
      selected_obs <- get_obs_by_situations(env$data_dir, common_usms)
      save_species_obs(
        env$data_dir,
        selected_obs,
        spec
      )
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
        reference_data_dir
      )
    }
  )
  remove_null_values(eval_results)
}

export_evaluation_results <- function(
  eval_results,
  exports,
  output_dir,
  reference_data_dir,
  percentage,
  parallel,
  cores
) {
  parallelizable_loop(
    length(eval_results),
    parallel,
    cores,
    function(i, env) {
      eval_result <- eval_results[[i]]
      species_output_dir <- file.path(output_dir, eval_result$species)
      if (!is.null(exports) && !file.exists(species_output_dir)) {
        logger::log_info(
          "Exporting ", eval_result$species, " evaluation results"
        )
        dir.create(species_output_dir)
      }
      if ("sim" %in% exports) {
        logger::log_debug("Exporting ", eval_result$species, " simulation data")
        export_species_sim_ds_to_csv(
          env$data_dir,
          eval_result$species,
          species_output_dir
        )
      }
      if ("stats" %in% exports) {
        logger::log_debug("Exporting ", eval_result$species, " statistics")
        save_stats(eval_result$species, eval_result$stats, output_dir)
      }
      comparison <- eval_result$comparison
      if (!is.null(comparison)) {
        log_comparison(comparison, percentage)
        if ("plots" %in% exports) {
          sim <- get_species_sim(env$data_dir, eval_result$species)
          obs <- get_species_obs(env$data_dir, eval_result$species)
          ref_sim <- read_ref_sim(eval_result$species, reference_data_dir)
          gen_plots_file(
            eval_result$species,
            species_output_dir,
            eval_result$comparison,
            sim,
            obs,
            ref_sim,
            percentage
          )
          logger::log_debug(eval_result$species, " plots file generated")
          rm(ref_sim)
          rm(sim)
          rm(obs)
          gc()
        }
      }
    }
  )
}

sort_usm_by_species <- function(usms, workspace, parallel, cores) {
  logger::log_debug("Sorting USMs by species...")
  result <- parallelizable_loop(
    length(usms),
    parallel,
    cores,
    function(i, env) {
      usm <- usms[i]
      species <- SticsRFiles::get_plant_txt(
        workspace = file.path(workspace, usm)
      )
      list(
        species = species$codeplante,
        usm = usm
      )
    }
  )
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
  data_dir <- init_tmp_data_dir()
  on.exit({
    clean_tmp_data_dir()
    end_time <- Sys.time()
    time_taken <- round(end_time - start_time, 2)
    logger::log_info("Evaluation time: ", time_taken, " s")
  }, add = TRUE)
  start_time <- Sys.time()
  usms <- list.dirs(config$workspace, full.names = FALSE, recursive = FALSE)
  rotations <- get_rotation_list(config$rotation_file)
  sim <- load_workspace_sim(
    usms,
    rotations,
    config$workspace,
    config$run_simulations,
    config$stics_exe,
    config$parallel,
    config$cores
  )
  save_sim(data_dir, sim)
  rm(sim)
  obs <- load_workspace_obs(
    usms,
    config$workspace,
    config$parallel,
    config$cores
  )
  save_obs(data_dir, obs)
  rm(obs)
  gc()
  sorted_usms <- sort_usm_by_species(
    usms,
    config$workspace,
    config$parallel,
    config$cores
  )
  species <- unique(sorted_usms$species)
  logger::log_info("Starting evaluation...")
  eval_results <- evaluate_all_species(
    species,
    sorted_usms,
    config$reference_data_dir,
    config$parallel,
    config$cores
  )
  # Sorting eval results by species
  eval_results <- eval_results[order(sapply(eval_results, `[[`, "species"))]
  export_evaluation_results(
    eval_results,
    config$exports,
    config$output_dir,
    config$reference_data_dir,
    config$percentage,
    config$parallel,
    config$cores
  )
  comparisons <- lapply(eval_results, function(res) {
    res$comparison
  })
  comparisons <- remove_null_values(comparisons)
  if (length(comparisons) == 0) {
    logger::log_info("No comparison done.")
    return()
  }
  log_comparison_table(comparisons)
  counts <- vapply(
    comparisons,
    function(res) {
      if (is.null(res)) return(c(criticals = 0L, warnings = 0L))
      c(
        criticals = length(get_crit_vars(res, config$percentage)),
        warnings  = length(get_warn_vars(res, config$percentage))
      )
    },
    integer(2)
  )
  if (sum(counts["warnings", ]) > 0) {
    logger::log_warn("Found at least one deteriorated variable")
  }
  if (sum(counts["criticals", ]) > 0) {
    logger::log_error("Found at least one critical deteriorated variable")
    stop()
  }
}
