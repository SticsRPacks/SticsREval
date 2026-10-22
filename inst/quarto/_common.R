# Helpers shared by the dashboard pages (index.qmd, global.qmd and
# species.qmd), sourced from their setup chunk. Copied next to them by
# `write_project_files()` (see render_report.R) and removed once rendered;
# the leading `_` keeps Quarto from treating it as a page or a resource.

read_csv_safe <- function(path) {
  if (!file.exists(path)) return(NULL)
  csv_df <- utils::read.csv(
    path, stringsAsFactors = FALSE, check.names = FALSE
  )
  if (nrow(csv_df) == 0) NULL else csv_df
}

# Minimum number of observations a variable needs to be tested
# (`min_obs_threshold`, exported by `evaluate()` to `thresholds.csv`), or
# its historical value of 10 for outputs written before that export.
read_min_obs <- function(csv_dir) {
  thresholds <- read_csv_safe(file.path(csv_dir, "thresholds.csv"))
  if (is.null(thresholds$min_obs)) 10 else thresholds$min_obs[1]
}

dt <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(htmltools::tags$p(htmltools::em("No data.")))
  }
  DT::datatable(
    df,
    rownames = FALSE,
    filter = "top",
    options = list(pageLength = 10, scrollX = TRUE)
  )
}

# CSS class (see styles.css) of the badges of each status: variable
# statuses (Critical/Warning/Improved/Not tested), species statuses
# (Critical/Warning/OK) and USM evaluation results (Failed/Passed).
tag_class <- c(
  Critical = "tag-critical", Warning = "tag-warning", Improved = "tag-ok",
  OK = "tag-ok", `Not tested` = "tag-not-tested",
  Failed = "tag-critical", Passed = "tag-ok"
)

# Renders `items` (e.g. species or variable names) as colored pill badges,
# echoing the colored lists `cli` prints to the console for the same
# groupings, styled for HTML instead. If `href` is given, it maps `items`
# to the links of their badges (NA for no link).
tag_list <- function(items, status, href = NULL) {
  if (length(items) == 0) {
    return('<span class="tag-empty">None</span>')
  }
  tags <- sprintf('<span class="tag %s">%s</span>', tag_class[[status]], items)
  if (!is.null(href)) {
    links <- href(items)
    tags <- ifelse(
      is.na(links), tags,
      sprintf('<a href="%s" class="tag-link">%s</a>', links, tags)
    )
  }
  paste(tags, collapse = " ")
}

# Anchor of a variable's scatter plot on its species page (linked from the
# homepage heatmap and the species page badges). Explicit, since Pandoc's
# automatic ids (lowercased, some characters dropped) can't be predicted
# reliably from the variable name.
scatter_id <- function(var) {
  paste0("scatter-", gsub("[^A-Za-z0-9_-]", "-", var))
}

# Writes one block per status: the number of variables, what the status
# means, and their badges. `href` (optional) links the degraded variables.
# `status` already accounts for the minimum number of observations
# (variables without enough of them are "Not tested").
# Returns the number of "Not tested" variables, invisibly.
status_summary <- function(comparison, min_obs, href = NULL) {
  by_status <- split(comparison$variable, comparison$status)
  statuses <- c("Critical", "Warning", "Improved", "Not tested")
  details <- c(
    "major rRMSE increase", "minor rRMSE increase", "rRMSE stable or improved",
    sprintf("fewer than %d observations", min_obs)
  )

  for (i in seq_along(statuses)) {
    vars <- by_status[[statuses[i]]]
    if (statuses[i] == "Not tested" && length(vars) == 0) next
    degraded <- statuses[i] %in% c("Critical", "Warning")
    tags <- if (degraded && !is.null(href)) {
      tag_list(vars, statuses[i], href = href)
    } else {
      tag_list(vars, statuses[i])
    }
    cat(sprintf(
      "<p><strong>%d variable(s) %s</strong> (%s)<br>\n%s</p>\n",
      length(vars), tolower(statuses[i]), details[i], tags
    ))
  }

  invisible(length(by_status[["Not tested"]]))
}
