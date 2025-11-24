#' Running evaluation over a USM list
#'
#' @param usm_list a list of USM names
#' @param workspace path to the simulation and observation data
#' @param verbose Logical value for displaying or not information while running
#' @param reference_file Path to the file containing the statistical criterion
#'  to use in comparison
#' @param output_file Path where the statistical criterion of the evaluated
#'  version will be saved
#'
#' @returns a list containing
evaluate_usm_list <- function(
  species,
  usm_list,
  workspace,
  verbose,
  reference_file = NULL,
  output_file = NULL
) {
  sim <- SticsRFiles::get_sim(
    workspace = workspace,
    usm = usm_list,
    verbose = verbose
  )
  obs <- SticsRFiles::get_obs(
    workspace = workspace,
    usm = usm_list,
    verbose = verbose
  )
  if (length(sim) == 0 || length(obs) == 0) {
    return(NULL)
  }
  library(CroPlotR)
  stats <- summary(sim, obs = obs)
  if (!is.null(output_file)) {
    safe_write_csv(stats, output_file)
  }
  comparisons <- NULL
  if (!is.null(reference_file)) {
    ref_stats <- read_csv(reference_file)
    comparisons <- compare_rmse(
      species,
      ref_stats,
      stats
    )
  }
  comparisons
}

#' Running a complete evaluation process of STICS model
#'
#' @param stics_exe path to the STICS executable
#' @param workspace path to the simulation and observation data
#' @param reference_data_dir path to the reference data to use for comparison
#' @param output_dir path where output files will be saved
#' @param run_simulations Logical value for running simulation or not
#' @param do_evaluation Logical value for running evaluation or not
#' @param verbose Logical value for displaying or not information while running
#'
#' @export
evaluate <- function(
  stics_exe,
  workspace,
  data_source = "local",
  sms_path = NULL,
  stics_path = NULL,
  reference_data_dir = NULL,
  output_dir = NULL,
  run_simulations = TRUE,
  do_evaluation = TRUE,
  verbose = FALSE
) {
  start.time <- Sys.time()
  ds <- validate_configuration(
    workspace = workspace,
    data_source = data_source,
    stics_exe = stics_exe,
    stics_path = stics_path,
    sms_path = sms_path,
    run_simulations = run_simulations
  )
  usms <- usms(ds)
  if (run_simulations) {
    if (verbose) {
      message("Starting running simulations...")
    }
    run_simulations(stics_exe, workspace, usms, verbose)
  }
  if (!is.null(output_dir) && !file.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  if (do_evaluation) {
    if (verbose) {
      message("Starting evaluation...")
    }
    sorted_usms <- sort_usm_by_species(workspace, usms)
    species <- unique(sorted_usms$species)
    comparisons <- list()
    for (spec in species) {
      selected_usms <- dplyr::filter(sorted_usms, species == spec)$usm
      filename <- paste0("Criteres_stats_", spec, ".csv")
      reference_file <- NULL
      test_file <- file.path(reference_data_dir, filename)
      if (!is.null(reference_data_dir) && file.exists(test_file)) {
        reference_file <- test_file
      }
      if (length(selected_usms) > 0) {
        spec_comp <- evaluate_usm_list(
          spec,
          selected_usms,
          workspace,
          verbose,
          output_file = if (!is.null(output_dir)) file.path(output_dir, filename) else NULL,
          reference_file = reference_file
        )
        if (!is.null(spec_comp)) {
          comparisons <- append(comparisons, list(spec_comp))
        }
      }
    }
    for (c in comparisons) {
      show(c)
    }
    end.time <- Sys.time()
    time.taken <- round(end.time - start.time, 2)
    print(time.taken)
  }
}
