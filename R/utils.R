remove_null_values <- function(l) {
  result <- l[!vapply(l, is.null, logical(1))]
  if (length(result) == 0) list() else result
}
format_duration <- function(start_time, end_time = Sys.time(), digits = 2) {
  elapsed <- as.numeric(end_time - start_time, units = "secs")

  h <- elapsed %/% 3600
  m <- (elapsed %% 3600) %/% 60
  s <- elapsed %% 60

  if (digits > 0) {
    s_str <- formatC(
      s, format = "f", digits = digits, flag = "0", width = 2 + digits + 1
    )
  } else {
    s_str <- sprintf("%02d", round(s))
  }

  sprintf("%02d:%02d:%s", h, m, s_str)
}

format_species <- function(x) {
  if (length(x) == 0) {
    return("None")
  }
  paste(x, collapse = ", ")
}

safe_write_csv <- function(data, path) {
  tryCatch({
    readr::write_delim(
      data,
      path,
      delim = ",",
      na = "NA"
    )
  },
  error = function(e) {
    logger::log_error(
      sprintf("❌ Unable to create '%s': %s", path, e$message)
    )
    stop(sprintf("Error: unable to create %s", path), call. = FALSE)
  })
}

read_csv <- function(filepath, delimiter = ",") {
  data <- readr::read_delim(
    filepath,
    delim = delimiter,
    na = c("NA", "NaN", ""),
    locale = readr::locale(
      decimal_mark = ".",
      date_format = "%Y-%m-%d"
    ),
    show_col_types = is_debug()
  )
  names(data) <- trimws(names(data))
  data
}