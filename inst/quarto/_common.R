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
