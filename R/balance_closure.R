#' @export
BalanceClosureTest <- R6::R6Class("BalanceClosureTest",  # nolint: object_name_linter
  private = list(
    config = NULL,

    check_usm_balances = function(sim, usm, balances) {

      latest_sim <- sim[order(sim$Date, decreasing = TRUE)[1], ]
      vapply(balances, function(balance) {

        fields <- c(paste0("init_", balance), paste0("final_", balance))

        if (!all(fields %in% names(latest_sim))) {
          logger::log_debug(
            "USM ",
            usm,
            " does not have the required fields for balance closure test: ",
            toString(fields)
          )
          return(TRUE)
        }

        if (all(is.na(latest_sim[fields]))) {
          logger::log_debug(
            "USM ",
            usm,
            " has NA values for balance closure test fields: ",
            toString(fields)
          )
          return(TRUE)
        }

        init  <- round(latest_sim[[fields[1]]])
        final <- round(latest_sim[[fields[2]]])

        check <- is.na(init) || is.na(final) || init == final
        if (!check) {
          logger::log_warn(
            "USM ",
            usm,
            " has balance closure issue for balance ",
            balance,
            ": init = ",
            init,
            ", final = ",
            final
          )
        }
        check

      }, logical(1)) |> all()
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
      errors <- Map(
        function(sim, usm) {
          !private$check_usm_balances(sim, usm, balances)
        },
        sim_list,
        names(sim_list)
      )
      errors <- unlist(errors)
      logger::log_info(
        "Balance closure test completed with ",
        length(which(errors)),
        " USMs with balance closure issues."
      )
      if (any(errors)) {
        logger::log_info(
          "USMs with balance closure issues: ",
          toString(unique(names(sim_list)[errors]))
        )
      }
    }
  )
)