EvaluationSummary <- R6::R6Class("EvaluationSummary", # nolint: object_name_linter
  private = list(
    workspace = NULL,
    species = NULL,
    percentage = NULL
  ),
  public = list(
    initialize = function(workspace, species, percentage) {
      private$workspace <- workspace
      private$species <- species
      private$percentage <- percentage
    },
    display = function() {
      global_comparison <- private$workspace$get_global_comparison(
        private$percentage
      )
      if (!is.null(global_comparison)) {
        logger::log_info(strrep("=", 58))
        logger::log_info("Global comparison:")
        logger::log_info(strrep("=", 58))
        global_comparison$log()
      }
      comparisons <- lapply(
        private$species,
        private$workspace$get_species_comparison,
        private$percentage
      )
      comparisons <- Filter(Negate(is.null), comparisons)

      if (length(comparisons) == 0) {
        logger::log_warn("No comparison done.")
        return(invisible(self))
      }

      logger::log_info(strrep("=", 58))
      logger::log_info("Species comparisons:")
      logger::log_info(strrep("=", 58))

      all_crit <- unique(unlist(lapply(comparisons, function(c) {
        if (length(c$critical_vars) > 0) c$get_data()$species[1]
      })))
      all_warn <- unique(unlist(lapply(comparisons, function(c) {
        if (length(c$warning_vars) > 0) c$get_data()$species[1]
      })))
      all_warn <- setdiff(all_warn, all_crit)
      all_ok <- unique(unlist(lapply(comparisons, function(c) {
        if (length(c$critical_vars) == 0 && length(c$warning_vars) == 0) {
          c$get_data()$species[1]
        }
      })))

      for (comp in comparisons) comp$log()

      logger::log_info(strrep("=", 58))
      logger::log_info("Summary:")
      logger::log_info(strrep("=", 58))
      logger::log_info("The following species show at least one variable with:")
      logger::log_info(paste0(
        "Major degradation (> ", private$percentage, "% rRMSE increase): ",
        format_species(all_crit)
      ))
      logger::log_info(paste0(
        "Minor degradation (<= ", private$percentage, "% rRMSE increase): ",
        format_species(all_warn)
      ))
      logger::log_info(paste0(
        "No degradation (rRMSE stable or improved): ", format_species(all_ok)
      ))
      logger::log_info(strrep("=", 58))

      if (length(all_warn) > 0)
        logger::log_warn("Found at least one deteriorated variable")
      if (length(all_crit) > 0) {
        logger::log_error("Found at least one critical deteriorated variable")
        stop("Found at least one critical deteriorated variable", call. = FALSE)
      }
      invisible(self)
    }
  )
)
