.local_eval_env <- new.env(parent = emptyenv())

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

gen_deteriorated_usm_comparison <- function(
  species, rmse_per_usm, reference_data_dir, percentage
) {
  ref_stats <- read_ref_rmse_per_usm(species, reference_data_dir)
  if (is.null(ref_stats)) {
    return(NULL)
  }
  logger::log_info("Comparing RMSE per usm for species ", species)
  get_deteriorated_rmse_per_usm(
    species,
    ref_stats,
    rmse_per_usm,
    percentage
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
        logger::log_warn("No common USM for species ", spec, ".")
        return(NULL)
      }
      species_output_dir <- file.path(output_dir, spec)
      if (!dir.exists(species_output_dir) &&
          !dir.create(species_output_dir, recursive = TRUE)
      ) {
        stop("Error while creating ", spec, " output directory")
      }
      logger::log_info(
        "Exporting ", spec, " evaluation results in ", species_output_dir
      )
      selected_sim <- get_sim_by_situations(env$data_dir, common_usms)
      sim_count <- get_count(selected_sim)
      selected_obs <- get_obs_by_situations(env$data_dir, common_usms)
      obs_count <- get_count(selected_obs)
      if (sim_count == 0 || obs_count == 0) {
        logger::log_warn(
          "No simulation or observation data found for species ",
          spec
        )
        return()
      }
      if ("sim" %in% exports) {
        arrow::write_csv_arrow(
          selected_sim,
          sink = file.path(species_output_dir, "Simulations.csv"),
        )
      }
      collected_sim <- selected_sim %>%
        dplyr::collect() %>%
        CroPlotR::split_df2sim()
      collected_obs <- selected_obs %>%
        dplyr::collect() %>%
        CroPlotR::split_df2sim()
      stats <- gen_species_stats(
        spec, collected_sim, collected_obs,
        "stats" %in% exports, species_output_dir
      )
      rmse_per_usm <- gen_species_rmse_per_usm(
        spec,
        collected_sim,
        collected_obs,
        "rmse_per_usm" %in% exports, species_output_dir
      )
      rm(collected_sim, collected_obs)
      gc()
      deteriorated_usm <- gen_deteriorated_usm_comparison(
        spec, rmse_per_usm, reference_data_dir, percentage
      )
      if (!is.null(deteriorated_usm) && nrow(deteriorated_usm) > 0) {
        save_deteriorated_usm(deteriorated_usm, species_output_dir)
      }
      comparison <- gen_species_comparison(spec, stats, reference_data_dir)
      if (!is.null(comparison)) {
        log_comparison(comparison, percentage)
      }
      if ("plots" %in% exports && !is.null(comparison)) {
        if (!is.null(comparison)) {
          logger::log_info("Generating comparison plot for species ", spec)
          gen_comparison_plot(species_output_dir, comparison, percentage)
        }
        ref_sim <- read_ref_sim(spec, reference_data_dir)
        if (!is.null(ref_sim)) {
          logger::log_info("Generating scatter plots for species ", spec)
          deteriorated <- c(
            get_crit_vars(comparison, percentage),
            get_warn_vars(comparison, percentage)
          )
          collected_sim <- selected_sim %>%
            dplyr::collect() %>%
            CroPlotR::split_df2sim()
          collected_obs <- selected_obs %>%
            dplyr::collect() %>%
            CroPlotR::split_df2sim()
          gen_scatter_plot(
            species_output_dir,
            collected_sim,
            collected_obs,
            ref_sim,
            deteriorated
          )
          rm(collected_sim, collected_obs)
          gc()
        }
      }
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

display_comparisons_info <- function(comparisons, config) {
  if (length(comparisons) == 0) {
    logger::log_info("No comparison done.")
    return()
  }
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
    logger::log_info("Evaluation time: ", format_duration(start_time, end_time))
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
  display_comparisons_info(comparisons, config)
}
