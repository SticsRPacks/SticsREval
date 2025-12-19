read_ref_stats <- function(config, species) {
  reference_dir <- file.path(config$reference_data_dir, species)
  reference_file <- file.path(reference_dir, "Criteres_stats.csv")
  if (!length(reference_file) || !file.exists(reference_file)) {
    return(NULL)
  }
  read_csv(reference_file)
}

save_stats <- function(config, species, stats) {
  output_dir <- file.path(config$output_dir, species)
  safe_write_csv(stats, file.path(output_dir, "Criteres_stats.csv"))
}

copy_ref_stats <- function(config, species) {
  src_file <- file.path(
    config$reference_data_dir, species, "Criteres_stats.csv"
  )
  dest_dir <- file.path(
    config$output_dir, species
  )
  file.copy(src_file, dest_dir)
}

#' Comparing relative RMSE of two STICS versions
#'
#' @param species the species corresponding to the statistical criteria
#' @param ref_stats the reference statistical criterion
#' @param new_stats the new version statistical criterion
#'
#' @returns a list containing the critically deteriorated variables, the
#'  moderately deteriorated variables and the improved variables
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
      abs_ref = abs(rmse_ref),
      abs_new = abs(rmse_new),
      ratio = abs_new / abs_ref
    )

  critical_rows <- dplyr::filter(result, ratio > 1.05)
  warning_rows  <- dplyr::filter(result, ratio > 1 & ratio <= 1.05)
  improved_rows <- dplyr::filter(result, ratio <= 1)

  invisible(
    list(
      critical = critical_rows$variable,
      warning  = warning_rows$variable,
      improved = improved_rows$variable
    )
  )
}
