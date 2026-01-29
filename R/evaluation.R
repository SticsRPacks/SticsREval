library(CroPlotR)
library(plotly)
library(dplyr)
library(htmltools)

.local_eval_env <- new.env(parent = emptyenv())

gen_species_stats <- function(species, sim, obs, save_stats, output_dir) {
  logger::log_info("Generating statistics for ", species)
  stats <- run_with_log_control(
    # Calling summary() directly does not work in a future context
    CroPlotR:::summary.cropr_simulation(sim, obs = obs)
  )
  if (save_stats) {
    safe_write_csv(stats, file.path(output_dir, "Criteres_Stats.csv"))
  }
  stats
}

gen_species_comparison <- function(species, stats, reference_data_dir) {
  ref_stats <- read_ref_stats(species, reference_data_dir)
  if (is.null(ref_stats)) {
    return(NULL)
  }
  logger::log_info("Comparing RMSE for species ", species)
  compare_rmse(
    species,
    ref_stats,
    stats
  )
}

evaluate_all_species <- function(
  species,
  sorted_usms,
  reference_data_dir,
  output_dir,
  exports,
  percentage,
  parallel,
  cores
) {
  comparisons <- parallelizable_loop(
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
      species_output_dir <- file.path(output_dir, spec)
      if (!is.null(exports)) {
        if (!dir.exists(species_output_dir) &&
            !dir.create(species_output_dir, recursive = TRUE)
        ) {
          stop("Error while creating ", spec, " output directory")
        }
        logger::log_info(
          "Exporting ", spec, " evaluation results in ", species_output_dir
        )
      }
      selected_sim <- get_sim_by_situations(env$data_dir, common_usms)
      selected_obs <- get_obs_by_situations(env$data_dir, common_usms)
      if (length(selected_sim) == 0 || length(selected_obs) == 0) {
        logger::warning(
          "No simulation or observation data found for species ",
          spec
        )
        return()
      }
      if ("sim" %in% exports) {
        safe_write_csv(
          CroPlotR::bind_rows(selected_sim),
          file.path(species_output_dir, "Simulations.csv")
        )
      }
      stats <- gen_species_stats(
        spec, selected_sim, selected_obs,
        "stats" %in% exports, species_output_dir
      )
      comparison <- gen_species_comparison(spec, stats, reference_data_dir)
      if ("plots" %in% exports && !is.null(comparison)) {
        logger::log_info("Generating comparison plot for species ", spec)
        gen_comparison_plot(species_output_dir, comparison, percentage)
        ref_sim <- read_ref_sim(spec, reference_data_dir)
        if (!is.null(ref_sim)) {
          logger::log_info("Generating scatter plots for species ", spec)
          deteriorated <- c(
            get_crit_vars(comparison, percentage),
            get_warn_vars(comparison, percentage)
          )
          gen_scatter_plot(
            species_output_dir,
            selected_sim,
            selected_obs,
            ref_sim,
            deteriorated
          )
        }
      }
      log_comparison(comparison, percentage)
      comparison
    }
  )
  remove_null_values(comparisons)
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

display_comparisons_info <- function(comparisons) {
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
  load_workspace_sim(
    data_dir,
    usms,
    rotations,
    config$workspace,
    config$run_simulations,
    config$stics_exe,
    config$parallel,
    config$cores
  )
  load_workspace_obs(
    data_dir,
    usms,
    config$workspace,
    config$parallel,
    config$cores
  )
  sorted_usms <- sort_usm_by_species(
    usms,
    config$workspace,
    config$parallel,
    config$cores
  )
  species <- sort(unique(sorted_usms$species))
  logger::log_info("Starting evaluation...")
  comparisons <- evaluate_all_species(
    species,
    sorted_usms,
    config$reference_data_dir,
    config$output_dir,
    config$exports,
    config$percentage,
    config$parallel,
    config$cores
  )
  comparisons <- remove_null_values(comparisons)
  display_comparisons_info(comparisons)
}
