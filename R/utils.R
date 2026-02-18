remove_null_values <- function(l) l[!vapply(l, is.null, logical(1))]

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