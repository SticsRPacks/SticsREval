#' Comparing relative RMSE of two STICS versions for a species
#'
#' @param species the species
#' @param ref_stats the reference statistical criterion
#' @param new_stats the new version statistical criterion
#'
#' @returns a list containing the variable for a species associated
#' to its RMSEs ratio
compare_rmse <- function(species, ref_stats, new_stats) {
  result <- dplyr::left_join(new_stats, ref_stats, by = "variable") %>%
    dplyr::mutate(
      rmse_new = as.numeric(sub(",", ".", rRMSE.x, fixed = TRUE)),
      rmse_ref = as.numeric(sub(",", ".", rRMSE.y, fixed = TRUE))
    ) %>%
    dplyr::filter(
      !is.na(rmse_new),
      !is.na(rmse_ref)
    ) %>%
    dplyr::mutate(
      ratio = abs(rmse_new) / abs(rmse_ref)
    )
  result <- result[!is.na(result$variable) & !is.na(result$ratio), ]
  invisible(
    lapply(seq_len(nrow(result)), function(i) {
      list(
        species = species,
        variable = result$variable[i],
        ratio = result$ratio[i]
      )
    })
  )
}

is_critical <- function(ratio, percentage = get_config_env()$percentage) {
  ratio >= 1 + percentage
}

is_warning <- function(ratio, percentage = get_config_env()$percentage) {
  ratio < 1 + percentage && ratio > 1
}

is_improved <- function(ratio) {
  ratio <= 1
}

get_crit_vars <- function(comparison) {
  crit_comp <- comparison[sapply(comparison, function(x) is_critical(x$ratio))]
  vapply(crit_comp, `[[`, "", "variable")
}

get_warn_vars <- function(comparison) {
  warn_comp <- comparison[sapply(comparison, function(x) is_warning(x$ratio))]
  vapply(warn_comp, `[[`, "", "variable")
}

get_improved_vars <- function(comparison) {
  imp_comp <- comparison[sapply(comparison, function(x) is_improved(x$ratio))]
  vapply(imp_comp, `[[`, "", "variable")
}

log_comparison <- function(
  comparison,
  percentage = get_config_env()$percentage
) {
  if (length(comparison) == 0) {
    return()
  }
  logger::log_info(
    "-----------------------------------------------------------------"
  )
  logger::log_info("Species: ", comparison[[1]]$species)
  total <- length(comparison)
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
  df <- do.call(rbind, lapply(comparisons, function(comp) {
    do.call(
      rbind,
      lapply(comp, function(x) as.data.frame(x, stringsAsFactors = FALSE))
    )
  }))
  df <- df[order(df$ratio, df$species, df$variable), ]
  for (line in capture.output(print(df, row.names = FALSE))) {
    logger::log_info(line)
  }
}
