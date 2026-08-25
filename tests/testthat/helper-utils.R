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
