#' Balance Closure Test
#'
#' This class implements a test to check the balance closure of water and
#' nitrogen in the simulations. It checks if the initial and final
#' balances of water and nitrogen are equal (or NA) for each USM.
#' If there are discrepancies, it logs a warning with the details of the issue.
#'
#' @name BalanceClosureTest
#' @docType class
#'
#' @examples
#' \dontrun{
#' config <- Configuration$new(
#'   stics_exe = "/path/to/stics",
#'   metadata_file = "metadata.csv",
#'   usms_workspace = "path/to/usms_workspace"
#' )
#' BalanceClosureTest$new(config)$run()
#' }
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
    #' @description
    #' Create a new BalanceClosureTest object.
    #' @param config A Configuration object containing the necessary parameters
    #' for the test.
    initialize = function(config) {
      private$config <- config
    },
    #' @description
    #' Run the balance closure test on the simulations.
    #' This method loads the simulations data for the specified USMs and checks
    #' the balance closure for each USM. It logs the results of the test,
    #' including any USMs that have balance closure issues.
    run = function() {
      private$config$validate_balance_closure()
      usms <- list.files(private$config$usms_workspace)
      if (!is.null(private$config$usms)) {
        usms <- intersect(usms, private$config$usms)
      }
      if (length(usms) == 0) {
        logger::log_info("No USMs to test for balance closure.")
        return(invisible(self))
      }
      logger::log_info(
        "Running balance closure test on ",
        length(usms),
        " USMs..."
      )
      logger::log_debug("Loading simulations data for balance closure test...")
      loader <- WorkspaceLoader$new(
        workspace = private$config$usms_workspace,
        backend = ParallelBackend$new(
          parallel = private$config$parallel,
          cores = private$config$cores
        ),
        config = private$config
      )
      balances <- c(
        "H2O_balance", "plant_N_balance", "soil_mineral_N_balance",
        "soil_organic_N_balance", "soil_organic_C_balance"
      )
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
