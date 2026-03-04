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
#' @examples
#'
#' \dontrun{
#'
#' n_cores <- get_cores_nb()
#'
#' n_cores <- get_cores_nb(parallel = TRUE)
#'
#' n_cores <- get_cores_nb(parallel = TRUE, required_nb = 4)
#' }
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
  required_nb
}


#' Detecting machine cores number
#'
#' @param ... To pass additional argument (for testing purpose)
#'
#' @return Total cores number
#'
#' @keywords internal
#'
#' @examples
#'
#' \dontrun{
#' get_cores()
#'
#' get_cores(cores_nb = 4)
#' }
#'
get_cores <- function(...) {
  # Getting additional args list with cores_nb in it !
  dot_args <- list(...)

  # Getting real cores number
  if (!("cores_nb" %in% names(dot_args))) {
    return(future::availableCores())
  }

  # Returning a fake number of cores gave as an input
  dot_args$cores_nb
}

setup_parallel_backend <- function(
  n_tasks,
  parallel,
  cores
) {
  if (!parallel) {
    return(
      list(
        map = function(x, fun) {
          base::lapply(
            x,
            function(i) fun(i)
          )
        },
        cleanup = function() {}
      )
    )
  }
  workers <- get_cores_nb(parallel = TRUE, required_nb = cores)
  workers <- min(workers, n_tasks)
  future::plan(
    future::multisession,
    workers = workers
  )
  invisible(
    list(
      map = function(x, fun) {
        future.apply::future_lapply(
          x,
          function(i) fun(i),
          future.seed = TRUE
        )
      },
      cleanup = function() future::plan(future::sequential)
    )
  )
}

parallelizable_loop <- function(n_tasks, parallel, cores, fun) {
  backend <- setup_parallel_backend(n_tasks, parallel, cores)
  on.exit(backend$cleanup(), add = TRUE)
  backend$map(
    seq_len(n_tasks),
    function(i) {
      logger::log_appender(logger::appender_stdout)
      fun(i)
    }
  )
}