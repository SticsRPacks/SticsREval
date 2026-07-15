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
  logger::log_info(
    "Initializing workspace {config$eval_workspace}
    for evaluation..."
  )
  if (!dir.exists(config$eval_workspace) &&
      !dir.create(config$eval_workspace, recursive = TRUE)
  ) {
    stop("Can't create evaluation workspace", call. = FALSE)
  }
  USMSWorkspace$new(config = config)$load()
  global_eval <- GlobalEvaluation$new(config = config)
  global_eval$run()
  species_eval <- SpeciesEvaluation$new(config = config)
  species_eval$run()

  if (!is.null(config$output_dir)) {
    prepare_output_dir(config$output_dir)
    global_eval$export()
    species_eval$export()
  }

  global_eval$summary()
  species_eval$summary()

  ok  <- paste(cli::col_green(cli::symbol$tick), cli::col_green("success"))
  nok <- paste(cli::col_red(cli::symbol$cross),  cli::col_red("failed"))

  cli::cli_h1("Tests results")
  cli::cli_ul()
  cli::cli_li("Global evaluation: {if (global_eval$success) ok else nok}")
  cli::cli_li("Species evaluation: {if (species_eval$success) ok else nok}")
  cli::cli_end()
  if (!global_eval$success || !species_eval$success) {
    stop("At least one test failed, see details above.", call. = FALSE)
  }
}
