#' Render an HTML evaluation dashboard
#'
#' @description
#' Renders an HTML dashboard (\code{dashboard.html}) summarizing the
#' evaluation results written to \code{output_dir} by \code{\link{evaluate}}
#' (called with \code{output_dir} set). The dashboard shows the global stats
#' (\code{global_stats.csv}) and deteriorated USMs
#' (\code{Deteriorated_USM.csv}) from \code{output_dir/csv}, and links to one
#' page per species (\code{output_dir/plots/<species>/species.html}) with
#' that species' statistics, rRMSE comparison plot and scatter plots -
#' species pages are rendered separately so a browser only has to load one
#' species' plots at a time, instead of every species' plots at once on a
#' single page. Nothing is recomputed or replotted: only the files
#' \code{evaluate()} already wrote are read.
#'
#' None of the pages are standalone files: they link to the PNG/HTML plots
#' via relative paths (and Quarto writes its own JS/CSS assets to
#' \code{dashboard_files/} and \code{plots/<species>/species_files/}
#' subfolders), so keep \code{output_dir} intact when sharing or moving
#' results.
#'
#' Requires the Quarto CLI (\url{https://quarto.org}) to be installed, along
#' with the \pkg{quarto} and \pkg{DT} R packages.
#'
#' @param output_dir path to a directory previously populated by
#'  \code{\link{evaluate}} (i.e. \code{evaluate(..., output_dir = output_dir)}
#'  must have been run beforehand). The rendered dashboard is written there.
#' @param open Boolean. Open the rendered dashboard in a browser once done.
#'  Defaults to \code{TRUE} in interactive sessions.
#'
#' @return invisibly, the path to the rendered \code{dashboard.html} file
#'
#' @export
render_report <- function(output_dir, open = interactive()) {
  if (!dir.exists(output_dir)) {
    stop("output_dir does not exist: ", output_dir, call. = FALSE)
  }
  if (!file.exists(file.path(output_dir, "csv", "global_stats.csv"))) {
    stop(
      "No evaluation results found in ", output_dir, ". Run evaluate(...,",
      " output_dir = \"", output_dir, "\") first.",
      call. = FALSE
    )
  }

  rlang::check_installed(
    c("quarto", "DT"),
    reason = "to render the evaluation dashboard"
  )
  if (is.null(quarto::quarto_path())) {
    stop(
      "Quarto CLI not found. Install it from https://quarto.org before ",
      "calling render_report().",
      call. = FALSE
    )
  }

  render_species_pages(output_dir)

  template <- system.file(
    "quarto", "dashboard.qmd",
    package = "SticsREval"
  )
  qmd_path <- file.path(output_dir, "dashboard.qmd")
  file.copy(template, qmd_path, overwrite = TRUE)
  on.exit(unlink(qmd_path), add = TRUE)

  logger::log_info("Rendering evaluation dashboard...")
  quarto::quarto_render(input = qmd_path, quiet = !is_debug())

  html_path <- file.path(output_dir, "dashboard.html")
  logger::log_info("Dashboard written to {html_path}")

  if (open) {
    utils::browseURL(html_path)
  }

  invisible(html_path)
}

# Renders one `species.qmd` page per species found in
# `output_dir/csv/species_stats.csv`, colocated at
# `output_dir/plots/<species>/species.html` alongside that species' own
# plots (so its `img`/`iframe` src can stay a bare filename).
render_species_pages <- function(output_dir) {
  species_stats_path <- file.path(output_dir, "csv", "species_stats.csv")
  if (!file.exists(species_stats_path)) return(invisible(NULL))

  species_stats <- utils::read.csv(species_stats_path, stringsAsFactors = FALSE) # nolint: line_length_linter
  if (!"species" %in% names(species_stats)) return(invisible(NULL))
  species_list <- sort(unique(species_stats$species))

  template <- system.file("quarto", "species.qmd", package = "SticsREval")
  abs_output_dir <- normalizePath(output_dir)

  for (sp in species_list) {
    logger::log_info("Rendering evaluation page for species {sp}...")
    sp_dir <- file.path(output_dir, "plots", sp)
    dir.create(sp_dir, recursive = TRUE, showWarnings = FALSE)
    sp_qmd_path <- file.path(sp_dir, "species.qmd")
    file.copy(template, sp_qmd_path, overwrite = TRUE)
    on.exit(unlink(sp_qmd_path), add = TRUE)

    quarto::quarto_render(
      input = sp_qmd_path,
      execute_params = list(
        species = sp, output_dir = abs_output_dir, species_list = species_list
      ),
      quiet = !is_debug()
    )
  }

  invisible(NULL)
}
