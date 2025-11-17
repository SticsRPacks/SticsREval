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
  reference_data_dir = NULL,
  output_dir = NULL,
  run_simulations = TRUE,
  do_evaluation = TRUE,
  verbose = FALSE
) {
  usm_list <- get_usms_list(workspace)
  if (run_simulations) {
    if (verbose) {
      message("Starting running simulations...")
    }
    run_simulations(stics_exe, workspace, usm_list$situation, verbose)
  }
  if (do_evaluation) {
    if (verbose) {
      message("Starting evaluation...")
    }
    species <- unique(usm_list$species)
    comparisons <- list()
    for (spec in species) {

      usms <- dplyr::filter(usm_list, species == spec)
      filename <- paste0("Criteres_stats_", spec, ".csv")
      reference_file <- NULL
      test_file <- file.path(reference_data_dir, filename)
      if (!is.null(reference_data_dir) && file.exists(test_file)) {
        reference_file <- test_file
      }
      if (length(usms$situation) > 0) {
        spec_comp <- evaluate_usm_list(
          usms$situation,
          workspace,
          verbose,
          output_file = if (!is.null(output_dir)) file.path(output_dir, filename) else NULL,
          reference_file = reference_file
        )
        if (!is.null(spec_comp)) {
          spec_comp <- c(spec_comp, species=spec)
          comparisons <- append(comparisons, list(spec_comp))
        }
      }
    }
    total_critical <- 0
    total_warning <- 0
    for (c in comparisons) {
      total_critical <- total_critical + length(c$critical)
      total_warning <- total_warning + length(c$warning)
      print_comparison_summary(c)
    }
    if (total_critical > 0) {
      stop("Found deteriorated variables.")
    }
  }
}
