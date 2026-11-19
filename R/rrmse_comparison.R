# Minimum number of observations a variable needs,
# at the global or species level, to be tested. Below it, the variable is
# reported with the "Not tested" status, and, for the per-USM test, its
# situations are not tested either for that species.
min_obs_threshold <- 10

has_enough_obs <- function(n_obs) {
  !is.na(n_obs) & n_obs >= min_obs_threshold
}

# Joins the evaluated and reference rRMSE of `stats` (a CroPlotR summary
# with a `group` column) and computes their ratio, in %.
compare_rrmse <- function(stats, species = NULL) {
  ref_stats <- dplyr::filter(stats, .data$group == "reference")
  new_stats <- dplyr::filter(stats, .data$group == "evaluated")

  res <- new_stats |>
    dplyr::left_join(ref_stats, by = c("situation", "variable")) |>
    dplyr::mutate(
      rrmse_eval = as.numeric(.data$rRMSE.x),
      rrmse_ref = as.numeric(.data$rRMSE.y),
      n_obs = as.numeric(.data$n_obs.y)
    ) |>
    dplyr::filter(
      is.finite(.data$rrmse_eval), is.finite(.data$rrmse_ref),
      !is.na(.data$variable), !is.na(.data$situation)
    ) |>
    dplyr::mutate(
      rrmse_ratio = round(
        (abs(.data$rrmse_eval) - abs(.data$rrmse_ref)) / abs(.data$rrmse_ref) * 100, # nolint: line_length_linter
        2
      )
    ) |>
    dplyr::filter(is.finite(.data$rrmse_ratio)) |>
    dplyr::select(
      "situation", "variable", "rrmse_eval",
      "rrmse_ref", "rrmse_ratio", "n_obs"
    )
  if (!is.null(species)) {
    res <- dplyr::mutate(res, species = species)
  }
  res
}

# Sorts a comparison by decreasing ratio and adds its `status` column
# (Critical/Warning/Improved/Not tested/Other). The status is exported (e.g.
# in species_rrmse_comparison.csv) so consumers (e.g. the dashboard) can tell
# critical/warning variables apart without duplicating this threshold logic.
# When `check_n_obs` is TRUE, variables without enough observations (see
# `min_obs_threshold`) are not tested.
add_rrmse_status <- function(data, percentage, check_n_obs = TRUE) {
  data |>
    dplyr::arrange(dplyr::desc(.data$rrmse_ratio)) |>
    dplyr::mutate(status = dplyr::case_when(
      check_n_obs & !has_enough_obs(.data$n_obs) ~ "Not tested",
      .data$rrmse_ratio >= percentage ~ "Critical",
      .data$rrmse_ratio > 0 & .data$rrmse_ratio < percentage ~ "Warning",
      .data$rrmse_ratio <= 0 ~ "Improved",
      TRUE ~ "Other"
    ))
}

# Per-variable rRMSE comparison between the evaluated and the reference
# versions. Returns a data frame with columns situation, variable,
# rrmse_eval, rrmse_ref, rrmse_ratio, n_obs, species (if given) and status,
# sorted by decreasing ratio.
rrmse_comparison <- function(stats, percentage, species = NULL) {
  add_rrmse_status(compare_rrmse(stats, species), percentage)
}

# Per-USM rRMSE comparison, restricted to deteriorated rows (ratio > 0).
# The per-USM test has no minimum number of observations (the species-level
# criterion is applied upstream, by USMEvaluation).
deteriorated_usm_comparison <- function(stats, percentage, species = NULL) {
  compare_rrmse(stats, species) |>
    add_rrmse_status(percentage, check_n_obs = FALSE) |>
    dplyr::filter(.data$rrmse_ratio > 0)
}

# Variables of a comparison with the given status.
rrmse_status_vars <- function(comparison, status) {
  comparison$variable[comparison$status == status]
}

log_rrmse_comparison <- function(comparison, percentage) {
  if (nrow(comparison) == 0) return(invisible(NULL))

  div_id <- NULL
  if ("species" %in% names(comparison)) {
    cli_species_rule(comparison$species[1])
    div_id <- cli_indent_start()
  }

  crit_vars <- rrmse_status_vars(comparison, "Critical")
  warn_vars <- rrmse_status_vars(comparison, "Warning")
  imp_vars <- rrmse_status_vars(comparison, "Improved")
  not_tested <- rrmse_status_vars(comparison, "Not tested")

  n_crit <- length(crit_vars)
  n_warn <- length(warn_vars)
  n_imp <- length(imp_vars)

  cli::cli_dl(c(
    "Total variables" = "{n_crit + n_warn + n_imp}"
  ))

  cli::cli_li("{.strong {n_crit} critical} (>= {percentage}%)")
  if (n_crit > 0) {
    cli::cli_text(cli::col_red("{toString(crit_vars)}"))
  }
  cli::cli_li("{.strong {n_warn} warning} (> 0%, < {percentage}%)")
  if (n_warn > 0) {
    cli::cli_text(cli::col_yellow("{toString(warn_vars)}"))
  }

  cli::cli_li("{.strong {n_imp} improved} (<= 0%)")
  if (n_imp > 0) {
    cli::cli_text(cli::col_green("{toString(imp_vars)}"))
  }

  if (length(not_tested) > 0) {
    cli::cli_li(
      "{.strong {length(not_tested)} not tested} (< {min_obs_threshold} obs)"
    )
    cli::cli_text(cli::col_grey("{toString(not_tested)}"))
  }

  if (!is.null(div_id)) cli_indent_end(div_id)

  invisible(NULL)
}
