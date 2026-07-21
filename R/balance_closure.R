#' Balance Closure Test
#'
#' This class implements a test to check the balance closure of water and
#' nitrogen in the simulations. It checks if the initial and final
#' balances of water and nitrogen are equal (or NA) for each USM.
#' If there are discrepancies, it logs a warning with the details of the issue.
#' Detailed information about the balance closure issues is exported to a CSV
#' file in the specified output directory.
#'
#' @name BalanceClosureTest
#' @docType class
#'
#' @examples
#' \dontrun{
#' config <- Configuration$new(
#'   stics_exe = "/path/to/stics",
#'   metadata_file = "/path/to/metadata.csv",
#'   usms_workspace = "/path/to/usms_workspace",
#'   output_dir = "/path/to/output_dir",
#'   parallel = TRUE,
#'   cores = 4
#' )
#' BalanceClosureTest$new(config)$run()
#' }
#' @export
BalanceClosureTest <- R6::R6Class("BalanceClosureTest", # nolint: object_name_linter
  private = list(
    config = NULL,
    loader = NULL,

    format_usms = function(usms, n = 5) {
      if (length(usms) <= n) {
        return(toString(usms))
      }

      paste0(
        toString(usms[seq_len(n)]),
        " (+",
        length(usms) - n,
        " more)"
      )
    },

    get_balance_summary = function(errors, balances) {
      data.frame(
        Balance = balances,
        `Usms with error` = vapply(
          balances,
          function(balance) {
            sum(vapply(
              errors,
              function(x) balance %in% x$Balance,
              logical(1)
            ))
          },
          integer(1)
        ),
        `Example USMs` = vapply(
          balances,
          function(balance) {
            usms <- names(errors)[
              vapply(
                errors,
                function(x) balance %in% x$Balance,
                logical(1)
              )
            ]
            private$format_usms(usms)
          },
          character(1)
        ),
        check.names = FALSE
      )
    },

    get_balance_details = function(errors, balances) {
      details <- lapply(names(errors), function(usm) {
        usm_errors <- errors[[usm]]

        if (nrow(usm_errors) == 0) {
          return(NULL)
        }

        data.frame(
          Balance = usm_errors$Balance,
          USM = usm,
          Init = usm_errors$Init,
          Final = usm_errors$Final,
          Diff = usm_errors$Diff,
          stringsAsFactors = FALSE
        )
      })

      details <- Filter(Negate(is.null), details)

      if (length(details) == 0) {
        return(data.frame())
      }

      do.call(rbind, details)
    },

    check_usm_balances = function(usm, sim, balances) {
      latest_sim <- sim[order(sim$Date, decreasing = TRUE)[1], ]
      bad_balances <- data.frame(
        Balance = character(),
        Init = numeric(),
        Final = numeric(),
        Diff = numeric(),
        stringsAsFactors = FALSE
      )
      for (balance in balances) {
        fields <- c(
          paste0("init_", balance),
          paste0("final_", balance)
        )
        if (!all(fields %in% names(latest_sim))) {
          next
        }
        if (all(is.na(latest_sim[fields]))) {
          next
        }
        init <- latest_sim[[fields[1]]]
        final <- latest_sim[[fields[2]]]
        balance_diff <- round(final - init)

        if (!is.na(balance_diff) && abs(balance_diff) > 1) {
          logger::log_warn(
            paste0(
              "Balance closure issue for USM ", usm, ": ",
              balance, " rounded difference = ", balance_diff,
              " (initial = ", init, ", final = ", final, ")"
            )
          )

          bad_balances <- rbind(
            bad_balances,
            data.frame(
              Balance = balance,
              Init = init,
              Final = final,
              Diff = balance_diff,
              stringsAsFactors = FALSE
            )
          )
        }
      }

      bad_balances
    },
    export_details = function(balance_details) {
      if (is.null(private$config$output_dir)) {
        logger::log_info(
          "Output directory not specified. Skipping export of balance details."
        )
        return()
      }
      output_path <- file.path(
        private$config$output_dir,
        "balance_errors_details.csv"
      )
      if (!dir.exists(private$config$output_dir)) {
        dir.create(private$config$output_dir, recursive = TRUE)
      }
      safe_write_csv(
        balance_details,
        output_path
      )
      logger::log_info(
        "Detailed balance errors written to {output_path}"
      )
    }
  ),
  public = list(
    #' @description
    #' Create a new BalanceClosureTest object.
    #' @param config A Configuration object containing the necessary parameters
    #' for the test.
    initialize = function(config) {
      private$config <- config
      private$loader <- USMSWorkspace$new(
        workspace = private$config$usms_workspace,
        backend = ParallelBackend$new(
          parallel = private$config$parallel,
          cores = private$config$cores
        ),
        config = private$config
      )
    },

    #' @description
    #' Run the balance closure test on the simulations.
    run = function() {
      private$config$validate_balance_closure()
      usms <- list.files(private$config$usms_workspace)
      if (!is.null(private$config$usms)) {
        usms <- intersect(usms, private$config$usms)
      }
      if (length(usms) == 0) {
        logger::log_info(
          "No USMs to test for balance closure."
        )
        return(invisible(self))
      }
      logger::log_info(
        "Running balance closure test on ",
        length(usms),
        " USMs..."
      )

      balances <- c(
        "H2O_balance",
        "plant_N_balance",
        "soil_mineral_N_balance",
        "soil_organic_N_balance",
        "soil_organic_C_balance"
      )
      sim_list <- private$loader$run_simulations(
        usms = usms,
        var = c(paste0("init_", balances), paste0("final_", balances))
      )
      errors <- Map(
        function(usm, sim) {
          private$check_usm_balances(usm, sim, balances)
        },
        names(sim_list),
        sim_list
      )
      has_error <- vapply(errors, nrow, integer(1)) > 0
      n_errors <- sum(has_error)

      logger::log_info(
        "Balance closure test completed with ",
        n_errors,
        " USMs with balance closure issues."
      )
      if (any(has_error)) {
        balance_summary <- private$get_balance_summary(
          errors,
          balances
        )
        cli::cli_text(
          "{.strong Balance closure summary}"
        )
        cat(
          capture.output(
            print(balance_summary, row.names = FALSE)
          ),
          sep = "\n"
        )
        balance_details <- private$get_balance_details(
          errors,
          balances
        )
        private$export_details(balance_details)
      }
      ok <- paste(
        cli::col_green(cli::symbol$tick),
        cli::col_green("success")
      )
      nok <- paste(
        cli::col_red(cli::symbol$cross),
        cli::col_red("failed")
      )
      cli::cli_li(
        "Balance closure test: {if (any(has_error)) nok else ok}"
      )
      if (any(has_error)) {
        stop(
          "Balance closure test failed for some USMs. Check logs for details.",
          call. = FALSE
        )
      }
      invisible(self)
    }
  )
)
