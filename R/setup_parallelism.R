#' Sets up a parallelism cluster
#'
#' @description This function creates a parallelism cluster in order
#' to use parallelism. It must be called before using a parallel loop
#' like foreach::foreach.
#'
#' @param inputs_number      Number of inputs
#' @param cores              Number of cores to use for parallel computation.
#'
#' @importFrom parallel clusterCall makeCluster
#' @importFrom doParallel registerDoParallel
#'
#' @keywords internal
#'
#' @noRd
#'
setup_parallelism <- function(inputs_number, cores = NA) {
  # Managing parallel model simulations
  # Managing cores number to use
  cores_nb <- get_cores_nb(parallel = TRUE, required_nb = cores)

  # Do not allow more cores than number of inputs: waste of time
  cores_nb <- min(cores_nb, inputs_number)

  # Launching the cluster
  cl <- makeCluster(cores_nb)

  # Registering cluster
  registerDoParallel(cl)
  clusterCall(cl, function(x) .libPaths(x), .libPaths())
  cl
}

#' @importFrom foreach %dopar% %do%
#' @importFrom parallel stopCluster
#'
#' @keywords internal
#'
#' @noRd
#'
setup_parallel_backend <- function(config, n_tasks) {
  if (!config$parallel) {
    return(list(do = foreach::`%do%`, cleanup = function(){}))
  }
  cl <- setup_parallelism(n_tasks, config$cores)
  return(list(do = foreach::`%dopar%`, cleanup = function() stopCluster(cl)))
}
