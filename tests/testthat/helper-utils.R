make_base_cfg <- function(...) {
  defaults <- list(
    validate_eval = function() {},
    validate_balance_closure = function() {},
    species = NULL,
    usms = NULL,
    var2exclude = NULL,
    reference_version = NULL,
    percentage = 5,
    parallel = FALSE,
    cores = 1,
    eval_workspace = "ws",
    verbose = 2L
  )
  init_logger(defaults$verbose)
  utils::modifyList(defaults, list(...))
}

replace_private <- function(obj, name, fn) {
  env <- obj$.__enclos_env__$private
  unlockBinding(name, env)
  env[[name]] <- fn
}

make_log_capture <- function() {
  env <- new.env(parent = emptyenv())
  env$logs <- character(0)

  logger::log_appender(
    function(lines, ...) env$logs <- c(env$logs, lines),
    namespace = logger::log_namespaces()
  )

  env
}
