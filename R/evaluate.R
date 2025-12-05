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
  output_dir,
  reference_data_dir,
  verbose
) {
  library(CroPlotR)
  stats <- summary(sim, obs = obs)
  filename <- paste0("Criteres_stats_", species, ".csv")
  if (!is.null(output_dir)) {
    safe_write_csv(stats, file.path(output_dir, filename))
  }
  reference_file <- file.path(reference_data_dir, filename)
  if (!length(reference_file) || !file.exists(reference_file)) {
    return(NULL)
  }
  ref_stats <- read_csv(reference_file)
  compare_rmse(
    species,
    ref_stats,
    stats
  )
}

#' @title Running a complete evaluation process of STICS model
#'
#' @param config List containing any information needed for the evaluation
#'  process. See [make_config()] for the complete list of parameters.
#'
#' @export
evaluate <- function(config) {
  start.time <- Sys.time()
  ds <- get_data_source_from_config(config)
  usms <- usms(ds)
  sim <- NULL
  if (config$run_simulations) {
    if (config$verbose) {
      message("Starting running simulations...")
    }
    sim <- run_simulations(
      stics_exe = config$stics_exe,
      workspace = config$workspace,
      usm_names = usms,
      successive = rotations(ds),
      verbose = config$verbose,
      parallel = config$parallel,
      cores = config$cores
    )
  }
  if (!is.null(config$output_dir) && !file.exists(config$output_dir)) {
    dir.create(config$output_dir, recursive = TRUE)
  }
  if (config$do_evaluation) {
    if (config$verbose) {
      message("Starting evaluation...")
    }
    if (is.null(sim)) {
      sim <- SticsRFiles::get_sim(
        workspace = config$workspace,
        usm = usms,
        verbose = config$verbose,
        parallel = config$parallel,
        cores = config$cores
      )
    }
    obs <- SticsRFiles::get_obs(
      workspace = config$workspace,
      usm = usms,
      verbose = config$verbose,
      parallel = config$parallel,
      cores = config$cores
    )
    sorted_usms <- sort_usm_by_species(
      config$workspace,
      usms,
      parallel = config$parallel,
      cores = config$cores
    )
    species <- unique(sorted_usms$species)
    if (config$parallel) {
      cl <- setup_parallelism(length(species), cores = config$cores)
      on.exit(stopCluster(cl))
      `%do_par_or_not%` <- foreach::`%dopar%`
    } else {
      `%do_par_or_not%` <- foreach::`%do%`
    }

    comparisons <- foreach::foreach(
      i = seq_along(species),
      .packages = c("dplyr", "CroPlotR")
    ) %do_par_or_not% {
      spec <- species[i]
      selected_usms <- dplyr::filter(sorted_usms, species == spec)$usm
      if (!length(selected_usms)) {
        return(NULL)
      }
      selected_sim <- sim[selected_usms]
      selected_obs <- obs[selected_usms]
      if (length(selected_sim) == 0 || length(selected_obs) == 0) {
        return(NULL)
      }
      evaluate_species(
        spec,
        selected_sim,
        selected_obs,
        config$output_dir,
        config$reference_data_dir,
        config$verbose
      )
    }
    total_critical <- 0
    for (c in comparisons) {
      if (!is.null(c)) {
        show(c)
        total_critical <- total_critical + critical_nb(c)
      }
    }
    end.time <- Sys.time()
    time.taken <- round(end.time - start.time, 2)
    print(time.taken)
    if (total_critical > 0) {
      stop("Found at least one critical deteriorated variable")
    }
  }
}
