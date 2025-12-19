#' Getting the available number of cores for parallel calculations
#'
#' @param parallel Logical for performing parallel loop (TRUE) or not (FALSE)
#' @param required_nb Wanted number of cores
#' @param ... To pass additional arguments (i.e. cores_nb, fake machine cores)
#'
#' @return Available cores number tu use
#'
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#'
#' n_cores <- get_cores_nb()
#'
#' n_cores <- get_cores_nb(parallel = TRUE)
#'
#' n_cores <- get_cores_nb(parallel = TRUE, required_nb = 4)
#'
get_cores_nb <- function(parallel = FALSE, required_nb = NA, ...) {
  # For sequential execution
  if (!parallel) {
    return(1)
  }

  # Getting true (from the machine) or fake cores number,
  # forcing it through cores_nb argument in
  # three dots arguments (for testing purpose)
  cores_nb <- get_cores(...)

  # Keeping one free core left
  if (cores_nb >= 2) {
    cores_nb <- cores_nb - 1
  }

  # Limiting the required cores, if any
  if (
    base::is.na(required_nb) ||
    required_nb > cores_nb
  ) {
    return(cores_nb)
  }

  # Getting the right required cores number
  return(required_nb)
}


#' Detecting machine cores number
#'
#' @param ... To pass additional argument (for testing purpose)
#'
#' @return Total cores number
#'
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#'
#' get_cores()
#'
#' get_cores(cores_nb = 4)
#'
#' @importFrom parallel detectCores
#'
get_cores <- function(...) {
  # Getting additional args list with cores_nb in it !
  dot_args <- list(...)

  # Getting real cores number
  if (!("cores_nb" %in% names(dot_args))) {
    return(detectCores())
  }

  # Returning a fake number of cores gave as an input
  return(dot_args$cores_nb)
}

#' Sets up a parallelism cluster
#'
#' @description This function creates a parallelism cluster in order
#' to use parallelism. It must be called before using a parallel loop
#' like foreach::foreach.
#'
#' @param inputs_number      Number of inputs
#' @param cores              Number of cores to use for parallel computation.
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
  cl <- parallel::makeCluster(cores_nb, outfile = NULL)

  # Registering cluster
  doParallel::registerDoParallel(cl)
  parallel::clusterCall(cl, function(x) .libPaths(x), .libPaths())
  cl
}

#' @keywords internal
#'
#' @noRd
#'
setup_parallel_backend <- function(config, n_tasks) {
  if (!config$parallel) {
    return(
      list(
        do = foreach::`%do%`,
        cleanup = function() { }
      )
    )
  }
  cl <- setup_parallelism(n_tasks, config$cores)
  parallel::clusterExport(
    cl,
    varlist = c("init_logger", "is_debug", "config"),
    envir = environment()
  )
  parallel::clusterEvalQ(cl, {
    sink(stdout(), type = "output")
  })
  parallel::clusterEvalQ(cl, {
    library(logger)
    init_logger(verbose = config$verbose)
  })
  invisible(
    list(
      do = foreach::`%dopar%`,
      cleanup = function() parallel::stopCluster(cl)
    )
  )
}
