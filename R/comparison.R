#' Comparing relative RMSE of two STICS versions for a species
#'
#' @param species the species
#' @param ref_stats the reference statistical criterion
#' @param new_stats the new version statistical criterion
#'
#' @returns a list containing the variable for a species associated
#' to its RMSEs ratio
#' @importFrom rlang .data
compare_rmse <- function(species, ref_stats, new_stats) {
  new_stats |>
    dplyr::left_join(ref_stats, by = c("situation", "variable")) |>
    dplyr::mutate(
      rmse_new = as.numeric(.data$rRMSE.x),
      rmse_ref = as.numeric(.data$rRMSE.y)
    ) |>
    dplyr::filter(
      is.finite(.data$rmse_new),
      is.finite(.data$rmse_ref),
      !is.na(.data$variable),
      !is.na(.data$situation)
    ) |>
    dplyr::mutate(
      species = species,
      ratio = (abs(.data$rmse_new) - abs(.data$rmse_ref)) /
        abs(.data$rmse_ref) * 100
    ) |>
    dplyr::filter(
      is.finite(.data$ratio)
    ) |>
    dplyr::mutate(
      ratio = round(.data$ratio, 2)
    ) |>
    dplyr::select(
      .data$species,
      .data$situation,
      .data$variable,
      .data$rmse_new,
      .data$rmse_ref,
      .data$ratio
    )
}

get_deteriorated_rmse_per_usm <- function(
  eval_workspace,
  species,
  ref_stats,
  percentage
) {
  logger::log_debug("Reading RMSE per USM parquet file for species {species}")
  new_stats <- get_rmse_per_usm(eval_workspace, species)
  if (is.null(new_stats)) {
    return()
  }
  logger::log_debug("Generating RMSE per USM comparison for species {species}")
  compare_rmse(species, ref_stats, new_stats) |>
    dplyr::collect() |>
    dplyr::filter(is_warning(.data$ratio, percentage) |
        is_critical(.data$ratio, percentage)
    ) |>
    dplyr::arrange(dplyr::desc(.data$ratio))
}

is_critical <- function(ratio, percentage) {
  out <- ratio >= percentage
  out[is.na(out)] <- FALSE
  out
}

is_warning <- function(ratio, percentage) {
  out <- ratio < percentage & ratio > 0
  out[is.na(out)] <- FALSE
  out
}

is_improved <- function(ratio) {
  out <- ratio <= 0
  out[is.na(out)] <- FALSE
  out
}

#' @importFrom rlang .data
get_crit_vars <- function(comparison, percentage) {
  comparison |>
    dplyr::filter(is_critical(.data$ratio, percentage)) |>
    dplyr::pull(.data$variable)
}

#' @importFrom rlang .data
get_warn_vars <- function(comparison, percentage) {
  comparison |>
    dplyr::filter(is_warning(.data$ratio, percentage)) |>
    dplyr::pull(.data$variable)
}

#' @importFrom rlang .data
get_improved_vars <- function(comparison) {
  comparison |>
    dplyr::filter(is_improved(.data$ratio)) |>
    dplyr::pull(.data$variable)
}

log_comparison <- function(
  comparison,
  percentage
) {
  if (nrow(comparison) == 0) {
    return()
  }
  logger::log_info(
    "-----------------------------------------------------------------"
  )
  logger::log_info("Species: ", comparison$species[1])
  total <- nrow(comparison)
  logger::log_info("Total number of variables: ", total)
  crit_vars <- get_crit_vars(comparison, percentage)
  logger::log_info(
    length(crit_vars),
    " deteriorated variables (>={percentage}%): "
  )
  if (length(crit_vars) > 0) {
    logger::log_info(paste(crit_vars, collapse = ", "))
  }
  warn_vars <- get_warn_vars(comparison, percentage)
  logger::log_info(
    length(warn_vars),
    " deteriorated variables (>0%, <{percentage}%): "
  )
  if (length(warn_vars) > 0) {
    logger::log_info(paste(warn_vars, collapse = ", "))
  }
  improved_vars <- get_improved_vars(comparison)
  logger::log_info(length(improved_vars), " improved variables (<=0%): ")
  if (length(improved_vars) > 0) {
    logger::log_info(paste(improved_vars, collapse = ", "))
  }
  logger::log_info(
    "-----------------------------------------------------------------"
  )
}

gen_species_comparison <- function(
  eval_workspace, species, reference_data_dir, percentage
) {
  for (spec in species) {
    ref_stats <- read_ref_stats(spec, reference_data_dir)
    if (is.null(ref_stats)) {
      next
    }
    logger::log_debug("Reading stats file for species {spec}")
    stats <- get_stats(eval_workspace, spec)
    if (is.null(stats)) {
      next
    }
    logger::log_info("Comparing RMSE for species {spec}")
    comp <- compare_rmse(
      spec,
      ref_stats,
      stats
    )
    logger::log_debug("Saving RMSE comparison for species {spec}")
    save_species_comparison(eval_workspace, spec, comp)
    logger::log_debug("Species comparison saved for species {spec}")
    log_comparison(comp |> dplyr::collect(), percentage)
  }

}

gen_deteriorated_usm <- function(
  eval_workspace, species, reference_data_dir, percentage
) {
  for (spec in species) {
    logger::log_debug("Reading reference RMSE per USM for species {spec}")
    ref_stats <- read_ref_rmse_per_usm(spec, reference_data_dir)
    if (is.null(ref_stats)) {
      next
    }
    logger::log_info("Comparing RMSE per usm for species {spec}")
    deteriorated_usm <- get_deteriorated_rmse_per_usm(
      eval_workspace,
      spec,
      ref_stats,
      percentage
    )
    logger::log_debug("Saving deteriorated USM for species {spec}")
    save_deteriorated_usm(eval_workspace, spec, deteriorated_usm)
    logger::log_debug("Deteriorated USM saved for species {spec}")
  }
}

display_comparisons_info <- function(data_dir, percentage) {
  species <- get_species(data_dir)
  comparisons <- lapply(species, function(s) {
    get_species_comparison(data_dir, s, TRUE)
  })
  comparisons <- remove_null_values(comparisons)
  if (length(comparisons) == 0) {
    logger::log_warn("No comparison done.")
    return()
  }
  results <- lapply(
    comparisons,
    function(res) {
      if (is.null(res)) {
        return(list(
          criticals = character(0),
          warnings  = character(0),
          ok        = character(0)
        ))
      }

      crit_vars <- get_crit_vars(res, percentage)
      warn_vars <- get_warn_vars(res, percentage)

      crit_species <- unique(res$species[res$variable %in% crit_vars])
      warn_species <- unique(res$species[res$variable %in% warn_vars])

      all_species <- unique(res$species)

      ok_species <- setdiff(all_species, union(crit_species, warn_species))

      list(
        criticals = crit_species,
        warnings = warn_species,
        ok = ok_species
      )
    }
  )
  all_crit <- unique(unlist(lapply(results, `[[`, "criticals")))
  all_warn <- unique(unlist(lapply(results, `[[`, "warnings")))
  all_ok   <- unique(unlist(lapply(results, `[[`, "ok")))
  logger::log_info("==========================================================")
  logger::log_info("Summary:")
  logger::log_info("==========================================================")
  logger::log_info("The following species show at least one variable with:")
  logger::log_info(
    paste0("Major degradation (> ",
      percentage, "% rRMSE increase): ", format_species(all_crit)
    )
  )
  logger::log_info(
    paste0("Minor degradation (≤ ",
      percentage, "% rRMSE increase): ", format_species(all_warn)
    )
  )
  logger::log_info(
    paste0(
      "No degradation (rRMSE stable or improved): ", format_species(all_ok)
    )
  )
  logger::log_info("==========================================================")
  if (length(all_warn) > 0) {
    logger::log_warn("Found at least one deteriorated variable")
  }
  if (length(all_crit) > 0) {
    logger::log_error("Found at least one critical deteriorated variable")
    stop()
  }
}