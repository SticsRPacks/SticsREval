ParallelBackend <- R6::R6Class("ParallelBackend", # nolint: object_name_linter

  active = list(
    parallel = function() private$.parallel,
    cores = function() private$.cores
  ),

  private = list(
    .parallel = NULL,
    .cores = NULL,
    .runner = NULL,

    get_cores_nb = function(required_nb = NA) {
      if (!private$.parallel) return(1)

      cores_nb <- future::availableCores()

      if (cores_nb >= 2) cores_nb <- cores_nb - 1

      if (is.na(required_nb) || required_nb > cores_nb) {
        return(cores_nb)
      }

      required_nb
    }
  ),

  public = list(

    initialize = function(
      parallel = FALSE,
      cores = NA,
      runner = NULL
    ) {
      private$.parallel <- parallel
      private$.cores <- cores
      private$.runner <- runner %||% self$default_runner
    },

    run = function(n_tasks, fun) {
      private$.runner(n_tasks, fun, private)
    },

    default_runner = function(n_tasks, fun, private) {

      if (!private$.parallel) {
        return(lapply(seq_len(n_tasks), fun))
      }

      workers <- private$get_cores_nb(required_nb = private$.cores)
      workers <- min(workers, n_tasks)

      future::plan(future::multisession, workers = workers)
      on.exit(future::plan(future::sequential), add = TRUE)

      future.apply::future_lapply(
        seq_len(n_tasks),
        fun,
        future.seed = TRUE
      )
    }
  )
)
