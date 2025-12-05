setGeneric("critical_nb", function(object) standardGeneric("critical_nb"))
setClass(
  "Comparison",
  slots = list(
    species = "character",
    critical = "character",
    warning = "character",
    improved = "character"
  )
)
setMethod(
  "critical_nb",
  "Comparison",
  function(object) length(object@critical)
)
setMethod(
  "show",
  "Comparison",
  function(object) {
    message("-----------------------------------------------------------------")
    message("Species: ", object@species)
    message(
      "Total number of variables: ",
      length(object@critical) + length(object@warning) + length(object@improved)
    )
    message(length(object@critical), " deteriorated variables (>5%): ")
    if (length(object@critical) > 0) {
      message(paste(object@critical, collapse = ", "))
    }
    message(length(object@warning), " deteriorated variables (>0%, <=5%): ")
    if (length(object@warning) > 0) {
      message(paste(object@warning, collapse = ", "))
    }
    message(length(object@improved), " improved variables (<0%): ")
    if (length(object@improved) > 0) {
      message(paste(object@improved, collapse = ", "))
    }
    message("-----------------------------------------------------------------")
  }
)

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

  invisible(new(
    "Comparison",
    species  = species,
    critical = critical_rows$variable,
    warning  = warning_rows$variable,
    improved = improved_rows$variable
  ))
}
