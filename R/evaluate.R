#' Run All Evaluations
#'
#' Loads simulation, observation and reference data from \code{sim_rds},
#' \code{obs_rds} and \code{ref_sim_rds} into a temporary evaluation
#' workspace, runs the global, per-species and per-USM evaluations,
#' exports results to \code{output_dir} (if defined), prints a summary,
#' and stops with an error if any evaluation failed. \code{evaluate()}
#' never runs STICS simulations itself — use \code{\link{run_simulations}}
#' to produce \code{sim_rds} / \code{obs_rds} beforehand.
#'
#' @param usms_workspace path to the Stics text workspace containing the
#'  USMs to evaluate. Used to determine the species associated with each
#'  USM
#' @param sim_rds path to an \code{.rds} file containing pre-computed
#'  simulation results for the evaluated version — see
#'  \code{\link{run_simulations}}
#' @param obs_rds path to an \code{.rds} file containing pre-computed
#'  observation data — see \code{\link{run_simulations}}
#' @param ref_sim_rds path to an \code{.rds} file containing pre-computed
#'  simulation results for the reference version. If NULL (default),
#'  evaluation runs against observations only, without regression
#'  comparison against a reference
#' @param output_dir directory where CSV exports and plots are written. If
#'  NULL (default), results are not exported
#' @param report Boolean. If \code{TRUE} and \code{output_dir} is set,
#'  renders an HTML dashboard (see \code{\link{render_report}}) summarizing
#'  the exported results once evaluation is done. Default \code{FALSE}. The
#'  per-species pages are rendered according to \code{parallel}/\code{cores}.
#' @param percentage threshold (%) above which a variable is flagged as
#'  deteriorated vs. the reference (default 5)
#' @param species optional character vector of species to evaluate. NULL
#'  (default) evaluates all available species
#' @param usms optional character vector of USMs to evaluate. NULL
#'  (default) evaluates all available USMs
#' @param var2exclude optional character vector of variables to exclude
#'  from evaluation
#' @param ratio_threshold numeric threshold (%) above which a single
#'  variable makes a USM fail the USM-level evaluation (default 50)
#' @param degraded_threshold numeric threshold (%) above which a variable
#'  is considered degraded for the USM-level evaluation (default 20)
#' @param max_degraded_vars maximum number of degraded variables (ratio >
#'  \code{degraded_threshold}) tolerated per USM before it is considered
#'  failed (default 3, i.e. fails starting at 4 degraded variables)
#' @param parallel Boolean. Is the computation to be done in parallel ?
#' @param cores Number of cores to use for parallel computation
#' @param verbose Integer. Logging verbosity level: 0 = silent, 1 = info,
#'  2 = debug
#'
#' @return Invisibly \code{NULL}. Called for its side effects: workspace
#'   creation, evaluation runs, and console reporting.
#'
#' @details
#' The function proceeds in the following steps:
#' \enumerate{
#'   \item Creates a temporary evaluation workspace, cleaned up on exit.
#'   \item Loads simulation/observation/reference data into it.
#'   \item Runs the global, per-species and per-USM evaluations.
#'   \item Exports results to \code{output_dir} if defined.
#'   \item Prints their respective summaries.
#'   \item Displays a CLI report listing each evaluation as
#'     \strong{success} (green ✔) or \strong{failed} (red ✗).
#'   \item Stops with an error if at least one evaluation did not succeed.
#' }
#'
#' @export
evaluate <- function(
  usms_workspace,
  sim_rds,
  obs_rds,
  ref_sim_rds = NULL,
  output_dir = NULL,
  report = FALSE,
  percentage = 5,
  species = NULL,
  usms = NULL,
  var2exclude = NULL,
  ratio_threshold = 50,
  degraded_threshold = 20,
  max_degraded_vars = 3,
  parallel = FALSE,
  cores = NA,
  verbose = 1L
) {
  init_logger(verbose)

  arg_values <- as.list(environment())
  schema <- list(
    fields = list(
      usms_workspace = field_spec(type = "character", nullable = FALSE),
      sim_rds = field_spec(
        type = "character", nullable = FALSE, validator = validate_rds_path
      ),
      obs_rds = field_spec(
        type = "character", nullable = FALSE, validator = validate_rds_path
      ),
      ref_sim_rds = field_spec(type = "character", validator = validate_rds_path), # nolint: line_length_linter
      output_dir = field_spec(type = "character"),
      report = field_spec(type = "logical", nullable = FALSE),
      percentage = field_spec(
        type = "numeric", nullable = FALSE, min = 0, max = 100
      ),
      species = field_spec(type = "character", validator = validate_nonempty_chr), # nolint: line_length_linter
      usms = field_spec(type = "character", validator = validate_nonempty_chr),
      var2exclude = field_spec(type = "character", validator = validate_nonempty_chr), # nolint: line_length_linter
      ratio_threshold = field_spec(type = "numeric", nullable = FALSE),
      degraded_threshold = field_spec(type = "numeric", nullable = FALSE),
      max_degraded_vars = field_spec(type = "numeric", nullable = FALSE),
      parallel = field_spec(type = "logical", nullable = FALSE),
      cores = field_spec(validator = validate_cores),
      verbose = field_spec(type = "integer", nullable = FALSE, min = 0L)
    ),
    cross_validators = list(
      list(
        desc = "If parallel = TRUE, cores must be an integer >= 1",
        check = check_parallel_cores
      )
    ),
    filesystem_checks = list(
      list(
        desc = "usms_workspace must point to an existing directory",
        check = check_path_exists("usms_workspace")
      ),
      list(
        desc = "sim_rds must point to an existing file",
        check = check_path_exists("sim_rds")
      ),
      list(
        desc = "obs_rds must point to an existing file",
        check = check_path_exists("obs_rds")
      ),
      list(
        desc = "ref_sim_rds must point to an existing file",
        check = check_path_exists("ref_sim_rds")
      )
    )
  )
  validate_schema(arg_values, schema)
  validate_filesystem(arg_values, schema)

  if (!is.null(output_dir) && !dir.exists(output_dir) && !dir.create(output_dir)) { # nolint: line_length_linter
    stop("Can't create ", output_dir, " directory", call. = FALSE)
  }

  eval_workspace <- tempfile(pattern = "eval_workspace_")
  on.exit(unlink(eval_workspace, recursive = TRUE), add = TRUE)

  initialize_eval_workspace(
    usms_workspace = usms_workspace,
    sim_rds = sim_rds,
    obs_rds = obs_rds,
    ref_sim_rds = ref_sim_rds,
    eval_workspace = eval_workspace,
    usms = usms,
    parallel = parallel,
    cores = cores
  )

  evaluations <- create_evaluations(
    eval_workspace = eval_workspace,
    species = species,
    usms = usms,
    var2exclude = var2exclude,
    percentage = percentage,
    output_dir = output_dir,
    ratio_threshold = ratio_threshold,
    degraded_threshold = degraded_threshold,
    max_degraded_vars = max_degraded_vars,
    parallel = parallel,
    cores = cores
  )

  run_evaluations(evaluations)

  if (!is.null(output_dir)) {
    export_evaluations(evaluations, output_dir)
    if (report) {
      render_report(output_dir, open = FALSE, parallel = parallel, cores = cores) # nolint: line_length_linter
    }
  }

  summarize_evaluations(evaluations)

  cat("\n")

  report_evaluation_status(evaluations)
}


initialize_eval_workspace <- function(
  usms_workspace, sim_rds, obs_rds, ref_sim_rds, eval_workspace, usms,
  parallel, cores
) {
  logger::log_info(
    "Initializing workspace {eval_workspace} for evaluation..."
  )

  if (!dir.exists(eval_workspace) &&
        !dir.create(eval_workspace, recursive = TRUE)) {
    stop("Can't create evaluation workspace", call. = FALSE)
  }

  USMSWorkspace$new(
    usms_workspace = usms_workspace,
    sim_rds = sim_rds,
    obs_rds = obs_rds,
    ref_sim_rds = ref_sim_rds,
    eval_workspace = eval_workspace,
    usms = usms,
    parallel = parallel,
    cores = cores
  )$load()
}


create_evaluations <- function(
  eval_workspace, species, usms, var2exclude, percentage, output_dir,
  ratio_threshold, degraded_threshold, max_degraded_vars, parallel, cores
) {
  species_evaluation <- SpeciesEvaluation$new(
    eval_workspace = eval_workspace,
    species = species,
    usms = usms,
    var2exclude = var2exclude,
    percentage = percentage,
    output_dir = output_dir,
    parallel = parallel,
    cores = cores
  )
  list(
    "Global evaluation" = GlobalEvaluation$new(
      eval_workspace = eval_workspace,
      usms = usms,
      var2exclude = var2exclude,
      percentage = percentage,
      output_dir = output_dir
    ),
    "Species evaluation" = species_evaluation,
    "USM evaluation" = USMEvaluation$new(
      eval_workspace = eval_workspace,
      species_filter = species,
      usms = usms,
      var2exclude = var2exclude,
      percentage = percentage,
      output_dir = output_dir,
      ratio_threshold = ratio_threshold,
      degraded_threshold = degraded_threshold,
      max_degraded_vars = max_degraded_vars,
      parallel = parallel,
      cores = cores,
      species_evaluation = species_evaluation
    )
  )
}


run_evaluations <- function(evaluations) {
  lapply(evaluations, function(eval) eval$run())
}


export_evaluations <- function(evaluations, output_dir) {
  prepare_output_dir(output_dir)

  lapply(evaluations, function(eval) eval$export())

  safe_write_csv(
    data.frame(
      evaluation = names(evaluations),
      success = vapply(evaluations, function(eval) eval$success, logical(1)),
      stringsAsFactors = FALSE
    ),
    csv_output_path(output_dir, "evaluation_status.csv")
  )
}


summarize_evaluations <- function(evaluations) {
  lapply(evaluations, function(eval) {
    cat("\n")
    eval$summary()
  })
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
  cli::cli_rule()

  if (!all(vapply(evaluations, function(x) x$success, logical(1)))) {
    stop("At least one test failed, see details above.", call. = FALSE)
  }
}
