library(CroPlotR)
library(plotly)
library(dplyr)
library(htmltools)

#' Running evaluation over a USM list
#'
#' @description
#' At first, statistical criteria are computed using the CroPlotR package.
#' Then, if a reference data directory is specified, the reference RMSE is
#' compared to the computed RMSE.
#'
#'
#' @param species the species corresponding to the simulations and observations
#' @param sim a list of simulations
#' @param obs a list of observations
#'
#' @returns a list containing the Comparison objects for the species
evaluate_species <- function(
  species,
  sim,
  obs,
  reference_data_dir = get_config_env()$reference_data_dir
) {
  eval_res <- list(
    species = species,
    comparison = NULL,
    stats = NULL,
    ref_stats = NULL,
    sim = sim,
    obs = obs
  )
  eval_res$stats <- run_with_log_control(
    # Calling summary() directly does not work in a future context
    CroPlotR:::summary.cropr_simulation(sim, obs = obs)
  )
  eval_res$ref_stats <- read_ref_stats(species, reference_data_dir)
  if (!is.null(eval_res$ref_stats)) {
    logger::log_debug("Comparing RMSE for species ", species)
    eval_res$comparison <- compare_rmse(
      eval_res$ref_stats,
      eval_res$stats
    )
  }
  eval_res
}

evaluate_all_species <- function(species, sorted_usms, sim, obs) {
  config <- get_config_env()
  backend <- setup_parallel_backend(length(species))
  on.exit(backend$cleanup(), add = TRUE)
  eval_results <- backend$map(seq_along(species), function(i) {
    logger::log_appender(logger::appender_stdout)
    spec <- species[i]
    logger::log_info("Starting evaluation of species ", spec)
    selected_usms <- dplyr::filter(sorted_usms, species == spec)$usm
    common_usms <- selected_usms[
      selected_usms %in% names(sim) &
        selected_usms %in% names(obs)
    ]
    if (!length(common_usms)) {
      logger::warning("No common USM for species ", spec, ".")
      return(NULL)
    }
    selected_sim <- sim[common_usms]
    selected_obs <- obs[common_usms]
    if (length(selected_sim) == 0 || length(selected_obs) == 0) {
      logger::warning(
        "No simulation or observation data found for species ",
        spec
      )
      return(NULL)
    }
    evaluate_species(
      spec,
      selected_sim,
      selected_obs,
      config$reference_data_dir
    )
  })
  eval_results
}

log_comparison <- function(species, comparison) {
  logger::log_info(
    "-----------------------------------------------------------------"
  )
  logger::log_info("Species: ", species)
  total <- length(comparison$critical) +
    length(comparison$warning) +
    length(comparison$improved)
  logger::log_info("Total number of variables: ", total)
  logger::log_info(
    length(comparison$critical),
    " deteriorated variables (>5%): "
  )
  if (length(comparison$critical) > 0) {
    logger::log_info(paste(comparison$critical, collapse = ", "))
  }
  logger::log_info(
    length(comparison$warning),
    " deteriorated variables (>0%, <=5%): "
  )
  if (length(comparison$warning) > 0) {
    logger::log_info(paste(comparison$warning, collapse = ", "))
  }
  logger::log_info(length(comparison$improved), " improved variables (<0%): ")
  if (length(comparison$improved) > 0) {
    logger::log_info(paste(comparison$improved, collapse = ", "))
  }
  logger::log_info(
    "-----------------------------------------------------------------"
  )
}

export_evaluation_result <- function(eval_result, config = get_config_env()) {
  species_output_dir <- file.path(config$output_dir, eval_result$species)
  if (!is.null(config$exports) && !file.exists(species_output_dir)) {
    logger::log_info("Exporting ", eval_result$species, " evaluation results")
    dir.create(species_output_dir)
  }
  if ("sim" %in% config$exports) {
    logger::log_debug("Exporting ", eval_result$species, " simulation data")
    save_sim(eval_result$species, eval_result$sim)
  }
  if ("stats" %in% config$exports) {
    logger::log_debug("Exporting ", eval_result$species, " statistics")
    save_stats(eval_result$species, eval_result$stats)
  }
  comparison <- eval_result$comparison
  if ("plots" %in% config$exports && !is.null(comparison)) {
    logger::log_debug("Generating ", eval_result$species, " scatter plots")
    ref_sim <- read_ref_sim(eval_result$species)
    deteriorated <- c(comparison$critical, comparison$warning)
    gen_scatter_plot(
      eval_result$sim,
      ref_sim,
      eval_result$obs,
      deteriorated,
      species_output_dir
    )
    logger::log_debug(eval_result$species, " scatter plots generated")
    if (!is.null(eval_result$ref_stats)) {
      logger::log_debug("Generating ", eval_result$species, " comparison plot")
      gen_comparison_plot(
        eval_result$stats,
        eval_result$ref_stats,
        species_output_dir
      )
      logger::log_debug(eval_result$species, " comparison plot generated")
    }
  }
  if (!is.null(comparison)) {
    log_comparison(eval_result$species, comparison)
  }
}

sort_usm_by_species <- function(usms, config = get_config_env()) {
  logger::log_debug("Sorting USMs by species...")
  backend <- setup_parallel_backend(length(usms))
  on.exit(backend$cleanup(), add = TRUE)
  result <- backend$map(seq_along(usms), function(i) {
    logger::log_appender(logger::appender_stdout)
    usm <- usms[i]
    species <- SticsRFiles::get_plant_txt(
      workspace = file.path(config$workspace, usm)
    )
    list(
      species = species$codeplante,
      usm = usm
    )
  })
  sorted <- dplyr::bind_rows(result)
  logger::log_debug("Found ", length(unique(sorted$species)), " species")
  sorted
}

#' @title Running a complete evaluation process of STICS model
#'
#' @param config List containing any information needed for the evaluation
#'  process. See [make_config()] for the complete list of parameters.
#'
#' @export
evaluate <- function(config) {
  init_config_env(config)
  init_logger(config$verbose)
  start_time <- Sys.time()
  logger::log_info("Starting evaluation...")
  ds <- get_data_source_from_config()
  usms <- usms(ds)
  sim <- load_workspace_sim(usms, rotations(ds))
  obs <- load_workspace_obs(usms)
  sorted_usms <- sort_usm_by_species(usms)
  species <- unique(sorted_usms$species)
  eval_results <- evaluate_all_species(species, sorted_usms, sim, obs)
  # Sorting eval results by species
  eval_results <- eval_results[order(sapply(eval_results, `[[`, "species"))]
  invisible(lapply(eval_results, function(res) {
    if (!is.null(res)) export_evaluation_result(res)
  }))
  log_comparison_table(eval_results)
  criticals <- vapply(eval_results, function(res) {
    if (is.null(res) || is.null(res$comparison)) return(0L)
    length(res$comparison$critical)
  }, integer(1))
  warnings <- vapply(eval_results, function(res) {
    if (is.null(res) || is.null(res$comparison)) return(0L)
    length(res$comparison$warning)
  }, integer(1))
  end_time <- Sys.time()
  time_taken <- round(end_time - start_time, 2)
  logger::log_info("Evaluation time: ", time_taken, " s")
  if (sum(warnings) > 0) {
    logger::log_warn("Found at least one deteriorated variable")
  }
  if (sum(criticals) > 0) {
    logger::log_error("Found at least one critical deteriorated variable")
    stop()
  }
}

get_vars <- function(comp, key) {
  if (is.null(comp[[key]])) character() else unlist(comp[[key]])
}

log_comparison_table <- function(eval_results) {
  df <- do.call(rbind, lapply(eval_results, function(x) {

    comp <- x$comparison

    critical <- get_vars(comp, "critical")
    warning  <- get_vars(comp, "warning")
    improved <- get_vars(comp, "improved")

    vars <- unique(c(critical, warning, improved))

    status <- rep(NA_character_, length(vars))
    status[vars %in% improved] <- "improved"
    status[vars %in% warning]  <- "warning"
    status[vars %in% critical] <- "critical"

    data.frame(
      species  = x$species,
      variable = vars,
      status   = status,
      stringsAsFactors = FALSE
    )
  }))
  df$status <- factor(
    df$status,
    levels = c("improved", "warning", "critical"),
    ordered = TRUE
  )
  df <- df[order(df$status, df$species, df$variable), ]
  for (line in capture.output(print(df, row.names = FALSE))) {
    logger::log_info(line)
  }
}