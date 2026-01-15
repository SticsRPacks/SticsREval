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
  dplyr::left_join(new_stats, ref_stats, by = "variable") %>%
    dplyr::mutate(
      rmse_new = as.numeric(sub(",", ".", .data$rRMSE.x, fixed = TRUE)),
      rmse_ref = as.numeric(sub(",", ".", .data$rRMSE.y, fixed = TRUE))
    ) %>%
    dplyr::filter(
      !is.na(.data$rmse_new),
      !is.na(.data$rmse_ref),
      !is.na(.data$variable)
    ) %>%
    dplyr::mutate(
      species = species,
      ratio = abs(.data$rmse_new) / abs(.data$rmse_ref)
    ) %>%
    dplyr::filter(
      !is.na(.data$ratio)
    ) %>%
    dplyr::select(
      .data$species,
      .data$variable,
      .data$rmse_new,
      .data$rmse_ref,
      .data$ratio
    )
}

is_critical <- function(ratio, percentage) {
  out <- ratio >= 1 + percentage
  out[is.na(out)] <- FALSE
  out
}

is_warning <- function(ratio, percentage) {
  out <- ratio < 1 + percentage & ratio > 1
  out[is.na(out)] <- FALSE
  out
}

is_improved <- function(ratio) {
  out <- ratio <= 1
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
    " deteriorated variables (>={percentage * 100}%): "
  )
  if (length(crit_vars) > 0) {
    logger::log_info(paste(crit_vars, collapse = ", "))
  }
  warn_vars <- get_warn_vars(comparison, percentage)
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

#' @importFrom rlang .data
log_comparison_table <- function(comparisons) {
  df <- dplyr::bind_rows(comparisons) %>%
    dplyr::arrange(.data$ratio, .data$species)
  for (line in capture.output(print(df, row.names = FALSE))) {
    logger::log_info(line)
  }
}
