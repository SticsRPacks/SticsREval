#' Running simulations of STICS using a STICS executable
#'
#' @param stics_exe path to the STICS executable
#' @param workspace path to the simulation and observation data
#' @param usm_names a list of USM name
#' @param successive a list of vectors containing the names of the UMSs to
#'  consider as successive (e.g. list(c("usm1.1","usm1.2"),c("usm2.1","usm2.2"))
#'  defines 2 successions usm1.1->usm1.2 and usm2.1->usm2.2)
#' @param verbose a boolean used to run simulation in verbose mode
#' @param parallel Boolean. Is the computation to be done in parallel ?
#' @param cores Number of cores to use for parallel computation.
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
run_simulations <- function(
    stics_exe,
    workspace,
    usm_names,
    successive,
    verbose,
    parallel = FALSE,
    cores = NA
) {
  options <- SticsOnR::stics_wrapper_options(
    stics_exe = stics_exe,
    workspace = workspace,
    parallel = parallel,
    cores = cores,
    successive = successive,
    verbose = verbose,
    time_display = verbose
  )
  res <- SticsOnR::stics_wrapper(options, situation = usm_names)
  res$sim_list
}
