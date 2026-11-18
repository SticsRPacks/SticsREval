library(mockery) # nolint: unused_import_linter

withr::local_dir(tempdir(), .local_envir = teardown_env())

logger::log_threshold(logger::FATAL)
withr::defer(logger::log_threshold(logger::DEBUG), envir = teardown_env())

# Functions under test call init_logger(), which raises the threshold back to
# INFO: discard the log lines instead, so they never reach the test output.
silence_logs()
withr::defer(
  logger::log_appender(logger::appender_console),
  envir = teardown_env()
)
