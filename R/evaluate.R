#' Run All Evaluations
#'
#' Initializes the evaluation workspace, runs global and species evaluations,
#' exports results, displays a summary of results, and stops with an error if
#' any test failed.
#'
#' @param config A list containing the evaluation configuration. Must include at
#'   least the following element:
#'   \describe{
#'     \item{eval_workspace}{Character. Path to the directory used as evaluation
#'       workspace. Created recursively if it does not already exist.}
#'   }
#'
#' @return Invisibly \code{NULL}. Called for its side effects: workspace
#'   creation, evaluation runs, and console reporting.
#'
#' @details
#' The function proceeds in the following steps:
#' \enumerate{
#'   \item Creates \code{config$eval_workspace} if it does not exist, and stops
#'     with an error if creation fails.
#'   \item Loads the USMS workspace via \code{USMSWorkspace$new(config)$load()}.
#'   \item Instantiates and runs a \code{GlobalEvaluation} then a
#'     \code{SpeciesEvaluation}.
#'   \item Exports results to \code{config$output_dir} if defined.
#'   \item Prints their respective summaries.
#'   \item Displays a CLI report listing each evaluation as
#'     \strong{success} (green ✔) or \strong{failed} (red ✗).
#'   \item Stops with an error if at least one evaluation did not succeed.
#' }
#'
#' @export
evaluate <- function(config) {
  initialize_eval_workspace(config)

  evaluations <- create_evaluations(config)

  run_evaluations(evaluations)

  if (!is.null(config$output_dir)) {
    export_evaluations(evaluations, config$output_dir)
  }

  summarize_evaluations(evaluations)

  report_evaluation_status(evaluations)
}


initialize_eval_workspace <- function(config) {
  logger::log_info(
    "Initializing workspace {config$eval_workspace} for evaluation..."
  )

  if (!dir.exists(config$eval_workspace) &&
        !dir.create(config$eval_workspace, recursive = TRUE)) {
    stop("Can't create evaluation workspace", call. = FALSE)
  }

  USMSWorkspace$new(config = config)$load()
}


create_evaluations <- function(config) {
  list(
    "Global evaluation" = GlobalEvaluation$new(config = config),
    "Species evaluation" = SpeciesEvaluation$new(config = config),
    "USM evaluation" = USMEvaluation$new(config = config)
  )
}


run_evaluations <- function(evaluations) {
  lapply(evaluations, function(eval) eval$run())
}


export_evaluations <- function(evaluations, output_dir) {
  prepare_output_dir(output_dir)

  lapply(evaluations, function(eval) eval$export())
}


summarize_evaluations <- function(evaluations) {
  lapply(evaluations, function(eval) eval$summary())
}


report_evaluation_status <- function(evaluations) {
  ok <- paste(cli::col_green(cli::symbol$tick), cli::col_green("success"))
  nok <- paste(cli::col_red(cli::symbol$cross), cli::col_red("failed"))

  cli::cli_h1("Tests results")
  cli::cli_ul()

  for (name in names(evaluations)) {
    status <- if (evaluations[[name]]$success) ok else nok
    cli::cli_li("{name}: {status}")
  }

  cli::cli_end()

  if (!all(vapply(evaluations, function(x) x$success, logical(1)))) {
    stop("At least one test failed, see details above.", call. = FALSE)
  }
}
