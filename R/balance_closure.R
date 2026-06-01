#' @export
BalanceClosureTest <- R6::R6Class("BalanceClosureTest",  # nolint: object_name_linter
  private = list(
    config = NULL,

    check_balance_closure = function(balance, sim) {
      balance_fields <- c(
        paste0("init_", balance), paste0("final_", balance)
      )
      if (!all(balance_fields %in% names(sim))) {
        logger::log_debug(
          "USM ", usm, ": balance fields are missing, skipping..."
        )
        return(TRUE)
      }
      latest_sim <- sim[which.max(sim$Date), ]
      if (all(is.na(latest_sim[balance_fields]))) {
        logger::log_debug(
          "USM ", usm,
          ": balance fields are present but empty, skipping..."
        )
        return(TRUE)
      }
      init_balance <- round(latest_sim[[paste0("init_", balance)]])
      final_balance <- round(latest_sim[[paste0("final_", balance)]])
      if (
        !is.na(init_balance) && !is.na(final_balance) &&
          init_balance != final_balance
      ) {
        logger::log_warn(
          "USM ", usm, ": ", balance, " balance closure failed (init: ",
          init_balance, ", final: ", final_balance, ")"
        )
        return(FALSE)
      }
      logger::log_debug(
        "USM ", usm, ": ", balance, " balance closure passed (init: ",
        init_balance, ", final: ", final_balance, ")"
      )
      TRUE
    }
  ),
  public = list(
    initialize = function(config) {
      private$config <- config
    },
    run = function() {
      usms <- list.files(private$config$usms_workspace)
      if (!is.null(private$config$usms)) {
        usms <- intersect(usms, private$config$usms)
      }
      logger::log_info(
        "Running balance closure test on ",
        length(usms),
        " USMs..."
      )
      error_usms <- NULL
      backend <- ParallelBackend$new(
        parallel = private$config$parallel,
        cores = private$config$cores
      )
      loader <- WorkspaceLoader$new(
        workspace = private$config$usms_workspace,
        backend = backend,
        config = private$config
      )
      balances <- c(
        "H2O_balance", "plant_N_balance", "soil_mineral_N_balance",
        "soil_organic_N_balance", "soil_organic_C_balance"
      )
      logger::log_debug("Loading simulations data for balance closure test...")
      sim_list <- loader$run_simulations(
        usms = usms,
        rotations = NULL,
        var = c(paste0("init_", balances), paste0("final_", balances))
      )
      logger::log_debug("Loading simulations data for balance closure test...")
      for (usm in names(sim_list)) {
        sim <- sim_list[[usm]]
        for (balance in balances) {
          if (!private$check_balance_closure(balance, sim)) {
            error_usms <- c(error_usms, usm)
          }
        }
      }
      logger::log_info(
        "Balance closure test completed with ",
        length(unique(error_usms)),
        " USMs with balance closure issues."
      )
      if (length(unique(error_usms)) > 0) {
        logger::log_info(
          "USMs with balance closure issues: ",
          toString(unique(error_usms))
        )
      }
    }
  )
)