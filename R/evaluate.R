gen_scatter_plot <- function(sim, obs, vars, output_dir, species) {

  library(CroPlotR)
  library(ggplot2)
  library(plotly)
  library(patchwork)
  library(dplyr)
  library(htmltools)

  plot_list <- list()
  for (var in vars) {
    plots <- plot(
      sim,
      obs = obs,
      type = "scatter",
      select_scat = "sim",
      var = var
    )
    plot_list[[length(plot_list) + 1]] <- ggplotly(plots[[1]])
  }
  page <- tagList(plot_list)
  htmltools::save_html(
    page,
    file = file.path(
      output_dir,
      paste0(species, "_scatter_interactive.html")
    )
  )
}

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
  reference_file <- file.path(reference_data_dir, filename)
  if (!length(reference_file) || !file.exists(reference_file)) {
    return(NULL)
  }
  ref_stats <- read_csv(reference_file)
  comparison <- compare_rmse(
    species,
    ref_stats,
    stats
  )
  if (critical_nb(comparison) > 0) {
    species_output_dir <- file.path(output_dir, species)
    dir.create(file.path(species_output_dir))
    #gen_comparison_plot(species, stats, ref_stats, species_output_dir)
    deteriorated <- c(comparison@critical, comparison@warning)
    gen_scatter_plot(sim, obs, deteriorated, species_output_dir, species)
    safe_write_csv(stats, file.path(species_output_dir, filename))
    file.copy(
      reference_file,
      file.path(species_output_dir, paste0("Ref_", filename))
    )
  }
  comparison
}

load_simulations <- function(config, usms, rotations) {
  if (config$run_simulations) {
    if (config$verbose) {
      message("Starting running simulations...")
    }
    return(
      run_simulations(
        stics_exe = config$stics_exe,
        workspace = config$workspace,
        usm_names = usms,
        successive = rotations,
        verbose = config$verbose,
        parallel = config$parallel,
        cores = config$cores
      )
    )
  }
  SticsRFiles::get_sim(
    workspace = config$workspace,
    usm = usms,
    verbose = config$verbose,
    parallel = config$parallel,
    cores = config$cores
  )
}

load_observations <- function(config, usms) {
  SticsRFiles::get_obs(
    workspace = config$workspace,
    usm = usms,
    verbose = config$verbose,
    parallel = config$parallel,
    cores = config$cores
  )
}

evaluate_all_species <- function(species, sorted_usms, sim, obs, config) {
  backend <- setup_parallel_backend(config, length(species))
  on.exit(backend$cleanup(), add = TRUE)
  `%do_par_or_not%` <- backend$do
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
  comparisons
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
  sim <- load_simulations(config, usms, rotations(ds))
  obs <- load_observations(config, usms)
  sorted_usms <- sort_usm_by_species(config, usms)
  species <- unique(sorted_usms$species)
  comparisons <- evaluate_all_species(species, sorted_usms, sim, obs, config)
  criticals <- sapply(comparisons, function(c) {
    show(c)
    critical_nb(c)
  })
  end.time <- Sys.time()
  time.taken <- round(end.time - start.time, 2)
  print(time.taken)
  if (sum(criticals) > 0) {
    stop("Found at least one critical deteriorated variable")
  }
}
