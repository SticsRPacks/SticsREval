library(mockery) # nolint: unused_import_linter

withr::local_dir(tempdir(), .local_envir = teardown_env())

logger::log_threshold(logger::FATAL)
withr::defer(logger::log_threshold(logger::DEBUG), envir = teardown_env())
