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
  dplyr::left_join(new_stats, ref_stats, by = c("situation", "variable")) %>%
    dplyr::mutate(
      rmse_new = as.numeric(sub(",", ".", .data$rRMSE.x, fixed = TRUE)),
      rmse_ref = as.numeric(sub(",", ".", .data$rRMSE.y, fixed = TRUE))
    ) %>%
    dplyr::filter(
      is.finite(.data$rmse_new),
      is.finite(.data$rmse_ref),
      !is.na(.data$variable),
      !is.na(.data$situation)
    ) %>%
    dplyr::mutate(
      species = species,
      ratio = (abs(.data$rmse_new) - abs(.data$rmse_ref)) / abs(.data$rmse_ref) * 100
    ) %>%
    dplyr::filter(
      is.finite(.data$ratio)
    ) %>%
    dplyr::mutate(
      ratio = round(.data$ratio, 2)
    ) %>%
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
  species,
  ref_stats,
  new_stats,
  percentage
) {
  compare_rmse(species, ref_stats, new_stats) %>%
    dplyr::filter(is_warning(.data$ratio, percentage) |
        is_critical(.data$ratio, percentage)
    ) %>%
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
  comparison %>%
    dplyr::filter(is_critical(.data$ratio, percentage)) %>%
    dplyr::pull(.data$variable)
}

#' @importFrom rlang .data
get_warn_vars <- function(comparison, percentage) {
  comparison %>%
    dplyr::filter(is_warning(.data$ratio, percentage)) %>%
    dplyr::pull(.data$variable)
}

#' @importFrom rlang .data
get_improved_vars <- function(comparison) {
  comparison %>%
    dplyr::filter(is_improved(.data$ratio)) %>%
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

gen_species_comparison <- function(species, stats, reference_data_dir) {
  ref_stats <- read_ref_stats(species, reference_data_dir)
  if (is.null(ref_stats)) {
    return(NULL)
  }
  logger::log_info("Comparing RMSE for species ", species)
  compare_rmse(
    species,
    ref_stats,
    stats
  )
}

gen_deteriorated_usm_comparison <- function(
  species, rmse_per_usm, reference_data_dir, percentage
) {
  ref_stats <- read_ref_rmse_per_usm(species, reference_data_dir)
  if (is.null(ref_stats)) {
    return(NULL)
  }
  logger::log_info("Comparing RMSE per usm for species ", species)
  get_deteriorated_rmse_per_usm(
    species,
    ref_stats,
    rmse_per_usm,
    percentage
  )
}