#' Running simulations of STICS using a STICS executable
#'
#' @param stics_exe path to the STICS executable
#' @param workspace path to the simulation and observation data
#' @param usm_names a list of USM name
#' @param verbose a boolean used to run simulation in verbose mode
#'
#' @returns a list of crop simulation
#'
#' @examples
#'   run_simulations(
#'     "/path/to/stics/exe",
#'     "/path/to/workspace",
#'     c("usm1", "usm2", "usm3"),
#'     TRUE
#'   )
run_simulations <- function(stics_exe, workspace, usm_names, verbose) {
  options <- SticsOnR::stics_wrapper_options(
    stics_exe = stics_exe,
    workspace = workspace,
    parallel = TRUE,
    verbose = verbose,
    time_display = verbose
  )
  res <- SticsOnR::stics_wrapper(options, situation = usm_list)
  res$sim_list
}
