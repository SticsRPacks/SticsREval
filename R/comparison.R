#' Comparing relative RMSE of two STICS versions
#'
#' @param ref_stats the reference statistical criterion
#' @param new_stats the new version statistical criterion
#'
#' @returns a list containing the critically deteriorated variables, the
#'  moderately deteriorated variables and the improved variables
compare_rmse <- function(
    ref_stats,
    new_stats
) {
  result <- dplyr::left_join(
    new_stats,
    ref_stats,
    by = c("variable")
  ) %>%
    dplyr::mutate(
      rmse_new = as.numeric(sub(",", ".", rRMSE.x, fixed = TRUE)),
      rmse_ref = as.numeric(sub(",", ".", rRMSE.y, fixed = TRUE))
    ) %>%
    dplyr::filter(
      is.numeric(rmse_new),
      is.numeric(rmse_ref),
      !is.na(rmse_new),
      !is.na(rmse_ref)
    )

  critical_rows <- filter(result, rmse_new > rmse_ref * 1.05)
  warning_rows <- filter(result, rmse_new > rmse_ref & rmse_new <= rmse_ref * 1.05)
  improved_rows <- filter(result, rmse_new <= rmse_ref)
  list(
    critical = critical_rows$variable,
    warning = warning_rows$variable,
    improved = improved_rows$variable
  )
}

#' Displaying information about deteriorated and improved variables
#'
#' @param summary the summary of deteriorated and improved variables
print_comparison_summary <- function(summary) {
  message("-----------------------------------------------------------------")
  message("Species: ", summary$species)
  message(
    "Total number of variables: ",
    length(summary$critical) + length(summary$warning) + length(summary$improved)
  )
  message(length(summary$critical), " deteriorated variables (>5%): ")
  if (length(summary$critical) > 0) {
    message(paste(summary$critical, collapse = ", "))
  }
  message(length(summary$warning), " deteriorated variables (>0%, <=5%): ")
  if (length(summary$warning) > 0) {
    message(paste(summary$warning, collapse = ", "))
  }
  message(length(summary$improved), " improved variables (<0%): ")
  if (length(summary$improved) > 0) {
    message(paste(summary$improved, collapse = ", "))
  }
  message("-----------------------------------------------------------------")
}
