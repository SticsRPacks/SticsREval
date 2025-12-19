is_debug <- function() {
  logger::log_threshold() >= logger::DEBUG
}

run_with_log_control <- function(expr) {
  if (is_debug()) {
    eval(expr)
  } else {
    suppressMessages(eval(expr))
  }
}

get_log_level <- function(verbose) {
  if (verbose <= 0) return("WARN")
  if (verbose == 1) return("INFO")
  "DEBUG"
}

init_logger <- function(verbose = 1) {
  log_level <- get_log_level(verbose)
  logger::log_layout(logger::layout_glue_colors)
  logger::log_threshold(log_level)

  logger::log_info("Logger initialisé (niveau = {log_level})")
}