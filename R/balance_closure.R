#' Run the Balance Closure Test
#'
#' Checks that the initial and final water and nitrogen balances are equal
#' (or NA) for each USM, using pre-computed simulation data from
#' \code{sim_rds} — see \code{\link{run_simulations}} to produce it. If
#' there are discrepancies, it logs a warning with the details of the issue
#' and stops with an error listing the affected USMs.
#'
#' @param sim_rds path to an \code{.rds} file containing pre-computed
#'  simulation data, including the balance variables — see
#'  \code{\link{run_simulations}}'s \code{vars} argument. The USMs to test
#'  are the names of this list, optionally restricted by \code{usms}
#' @param output_dir directory where balance error details
#'  (\code{balance_errors_details.csv}) will be written, if any. If NULL
#'  (default), details are not exported
#' @param usms character vector of USMs to restrict the test to. If NULL
#'  (default), all USMs found in \code{sim_rds} are tested
#' @param parallel Boolean. Is the computation to be done in parallel ?
#' @param cores Number of cores to use for parallel computation
#' @param verbose Integer. Logging verbosity level: 0 = silent, 1 = info,
#'  2 = debug
#'
#' @return Invisibly, the internal test object. Called for its side
#'  effects: logging a summary of the test, exporting detailed balance
#'  errors to `output_dir` (if defined), and stopping with an error if any
#'  USM failed the test.
#'
#' @examples
#' \dontrun{
#' balance_closure_test(
#'   sim_rds    = "/path/to/simulations.rds",
#'   output_dir = "/path/to/output_dir",
#'   parallel   = TRUE,
#'   cores      = 4
#' )
#' }
#' @export
balance_closure_test <- function(
  sim_rds,
  output_dir = NULL,
  usms = NULL,
  parallel = FALSE,
  cores = NA,
  verbose = 1L
) {
  init_logger(verbose)

  arg_values <- as.list(environment())
  schema <- list(
    fields = list(
      sim_rds = field_spec(
        type = "character", nullable = FALSE, validator = validate_rds_path
      ),
      output_dir = field_spec(type = "character"),
      usms = field_spec(type = "character", validator = validate_nonempty_chr),
      parallel = field_spec(type = "logical", nullable = FALSE),
      cores = field_spec(validator = validate_cores),
      verbose = field_spec(type = "integer", nullable = FALSE, min = 0L)
    ),
    cross_validators = list(
      list(
        desc = "If parallel = TRUE, cores must be an integer >= 1",
        check = check_parallel_cores
      )
    ),
    filesystem_checks = list(
      list(
        desc = "sim_rds must point to an existing file",
        check = check_path_exists("sim_rds")
      )
    )
  )
  validate_schema(arg_values, schema)
  validate_filesystem(arg_values, schema)

  BalanceClosureTest$new(sim_rds = sim_rds, output_dir = output_dir, usms = usms)$run() # nolint: line_length_linter
}

# Internal class implementing the balance closure test. Use
# balance_closure_test() instead of instantiating this class directly.
BalanceClosureTest <- R6::R6Class("BalanceClosureTest", # nolint: object_name_linter
  private = list(
    sim_rds = NULL,
    output_dir = NULL,
    usms = NULL,

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
      if (is.null(private$output_dir)) {
        logger::log_info(
          "Output directory not specified. Skipping export of balance details."
        )
        return()
      }
      output_path <- file.path(
        private$output_dir,
        "balance_errors_details.csv"
      )
      if (!dir.exists(private$output_dir)) {
        dir.create(private$output_dir, recursive = TRUE)
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
    initialize = function(sim_rds, output_dir = NULL, usms = NULL) {
      private$sim_rds <- sim_rds
      private$output_dir <- output_dir
      private$usms <- usms
    },

    run = function() {
      sim_list <- readRDS(private$sim_rds)
      usms <- names(sim_list)
      if (!is.null(private$usms)) {
        usms <- intersect(usms, private$usms)
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
      sim_list <- sim_list[usms]
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
        withr::with_options(
          list(width = 200),
          cat(
            capture.output(
              print(balance_summary, row.names = FALSE)
            ),
            sep = "\n"
          )
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
