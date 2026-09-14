#' @importFrom rlang .data
NULL

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
  toString(x)
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
      sprintf("ERROR: Unable to create '%s': %s", path, e$message)
    )
    stop(sprintf("Error: unable to create %s", path), call. = FALSE)
  })
}

csv_output_path <- function(output_dir, filename) {
  csv_dir <- file.path(output_dir, "csv")
  if (!dir.exists(csv_dir) && !dir.create(csv_dir, recursive = TRUE)) {
    stop("Can't create ", csv_dir, " directory", call. = FALSE)
  }
  file.path(csv_dir, filename)
}

read_csv <- function(filepath, delimiter = ",") {
  csv_data <- readr::read_delim(
    filepath,
    delim = delimiter,
    na = c("NA", "NaN", ""),
    locale = readr::locale(
      decimal_mark = ".",
      date_format = "%Y-%m-%d"
    ),
    show_col_types = is_debug()
  )
  names(csv_data) <- trimws(names(csv_data))
  csv_data
}

#' Generate one independent scatter plot HTML file per variable
#'
#' @description
#' Unlike a single combined page, each variable in \code{vars} gets its own
#' standalone HTML file, named \code{<var>_scatter_plot.html} in
#' \code{output_dir}, so it can be viewed, shared or embedded independently
#' of the others (e.g. one per tab/card in a dashboard).
#'
#' @keywords internal
gen_scatter_plot <- function(output_dir, sim, obs, ref_sim, vars) {
  plots <- plot(
    "New version" = sim,
    "Ref version" = ref_sim,
    obs = obs,
    type = "scatter",
    select_scat = "sim",
    var = vars
  )
  for (var in vars) {
    widget <- suppressWarnings(
      plotly::ggplotly(CroPlotR::extract_plot(plots, var = var)[[1]])
    )
    htmltools::save_html(
      htmltools::tagList(widget),
      file = file.path(output_dir, paste0(var, "_scatter_plot.html"))
    )
  }
  invisible(NULL)
}

prepare_output_dir <- function(output_dir) {
  o_dir <- file.path(output_dir)
  if (!dir.exists(o_dir) && !dir.create(o_dir, recursive = TRUE)) {
    stop(
      "Can't create output directory ",
      o_dir,
      call. = FALSE
    )
  }
}

truncate <- function(x, max_chars = 50) {
  if (nchar(x) > max_chars) {
    paste0(substr(x, 1, max_chars), "...")
  } else {
    x
  }
}

# Print a full-width `cli` rule, nested under an unindented rule/heading
# above it by drawing `prefix` line characters before the label instead of
# `cli::cli_rule()`'s usual 2 - the whole width is still solid rule, no
# leading blank margin.
cli_species_rule <- function(species, prefix = 4) {
  line_char <- cli::symbol$line
  left <- paste0(strrep(line_char, prefix), " Species: ", species, " ")
  fill <- max(0, cli::console_width() - nchar(left, type = "width"))
  cli::cli_verbatim(paste0(left, strrep(line_char, fill)))
}

# Print already-interpolated `text` verbatim, indented by `margin` spaces.
#
# Only for a status line with no `cli_li()` bullet anywhere before it in
# the current indented block (e.g. a lone "all passed" message) - such a
# line otherwise ends up flush left, because `cli::cli_text()`/
# `cli_alert_*()` strip leading whitespace and only pick up a container's
# left margin once a `cli_li()` has actually been drawn inside it.
# `cli_verbatim()` never reformats its input, so a manual prefix always
# renders correctly here - but for the same reason it does *not* do glue
# interpolation (`text` must already be a plain string, e.g. built with
# `sprintf()`), and if a `cli_li()` *has* already run in the block,
# `cli_verbatim()` picks up the container's margin on top of this manual
# one and ends up double-indented - use plain `cli::cli_text()` there
# instead, which by then indents correctly on its own.
cli_indent_text <- function(text, margin = 2) {
  cli::cli_verbatim(paste0(strrep(" ", margin), text))
}

# Open a `cli` container that indents `cli_dl()` and `cli_li()` calls
# printed inside it by two spaces, matching `cli_species_rule()`'s indent.
# `cli_text()`/`cli_alert_*()` calls placed *after* a `cli_li()` in the
# same block also come out correctly indented, for a status line with no
# `cli_li()` anywhere before it, use `cli_indent_text()` instead.
#
# A plain `cli_div()` with a "margin-left" theme renders fine for `cli_dl()`,
# but RStudio's console has its own rendering of `cli` bullets that ignores
# such a margin, so `cli_li()` items end up flush left. Using `cli`'s native
# list nesting instead (one `cli_ul()` inside another, with no item on the
# outer one) sidesteps that: it's the indent `cli_li()` itself understands,
# so it renders consistently everywhere.
#
# Close with `cli_indent_end()` on the returned ids. `.envir` must resolve
# to the caller's frame (not this function's own), since `cli`
# containers auto-close once that frame exits.
cli_indent_start <- function(.envir = parent.frame()) {
  list(
    outer = cli::cli_ul(.envir = .envir),
    inner = cli::cli_ul(.envir = .envir)
  )
}

# Close a container pair opened by `cli_indent_start()`.
cli_indent_end <- function(ids) {
  cli::cli_end(ids$inner)
  cli::cli_end(ids$outer)
}
