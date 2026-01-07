#' Comparing relative RMSE of two STICS versions for a species
#'
#' @param species the species
#' @param ref_stats the reference statistical criterion
#' @param new_stats the new version statistical criterion
#'
#' @returns a list containing the variable for a species associated
#' to its RMSEs ratio
compare_rmse <- function(species, ref_stats, new_stats) {
  dplyr::left_join(new_stats, ref_stats, by = "variable") %>%
    dplyr::mutate(
      rmse_new = as.numeric(sub(",", ".", rRMSE.x, fixed = TRUE)),
      rmse_ref = as.numeric(sub(",", ".", rRMSE.y, fixed = TRUE))
    ) %>%
    dplyr::filter(
      !is.na(rmse_new),
      !is.na(rmse_ref),
      !is.na(variable)
    ) %>%
    dplyr::mutate(
      species = species,
      ratio = abs(rmse_new) / abs(rmse_ref)
    ) %>%
    dplyr::filter(
      !is.na(ratio)
    ) %>%
    dplyr::select(species, variable, rmse_new, rmse_ref, ratio)
}

is_critical <- function(ratio, percentage = get_config_env()$percentage) {
  out <- ratio >= 1 + percentage
  out[is.na(out)] <- FALSE
  out
}

is_warning <- function(ratio, percentage = get_config_env()$percentage) {
  out <- ratio < 1 + percentage & ratio > 1
  out[is.na(out)] <- FALSE
  out
}

is_improved <- function(ratio) {
  out <- ratio <= 1
  out[is.na(out)] <- FALSE
  out
}

get_crit_vars <- function(comparison) {
  comparison %>%
    dplyr::filter(is_critical(ratio)) %>%
    dplyr::pull(variable)
}

get_warn_vars <- function(comparison) {
  comparison %>%
    dplyr::filter(is_warning(ratio)) %>%
    dplyr::pull(variable)
}

get_improved_vars <- function(comparison) {
  comparison %>%
    dplyr::filter(is_improved(ratio)) %>%
    dplyr::pull(variable)
}

log_comparison <- function(
  comparison,
  percentage = get_config_env()$percentage
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
  crit_vars <- get_crit_vars(comparison)
  logger::log_info(
    length(crit_vars),
    " deteriorated variables (>={percentage * 100}%): "
  )
  if (length(crit_vars) > 0) {
    logger::log_info(paste(crit_vars, collapse = ", "))
  }
  warn_vars <- get_warn_vars(comparison)
  logger::log_info(
    length(warn_vars),
    " deteriorated variables (>0%, <{percentage * 100}%): "
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

log_comparison_table <- function(comparisons) {
  df <- dplyr::bind_rows(comparisons) %>%
    dplyr::arrange(ratio, species, species)
  for (line in capture.output(print(df, row.names = FALSE))) {
    logger::log_info(line)
  }
}
