#' Render an HTML evaluation site
#'
#' @description
#' Renders an HTML site summarizing the evaluation results written to
#' \code{output_dir} by \code{\link{evaluate}} (called with \code{output_dir}
#' set): a homepage (\code{index.html}) showing the evaluation summary
#' (pass/fail status, and per-species status/USM counts), a global page
#' (\code{global.html}) showing the degraded variables and global stats
#' (\code{global_stats.csv}) from \code{output_dir/csv}, and one page per
#' species (\code{output_dir/species/<species>.html}) with that species'
#' statistics, degraded variables, deteriorated/failed USMs, rRMSE
#' comparison plot and scatter plots - the plots themselves stay under
#' \code{output_dir/plots/<species>/} (where \code{\link{evaluate}} wrote
#' them), the species page just links to them. It's a single Quarto
#' website project (a generated \code{_quarto.yml}), so every page shares
#' the same navigation sidebar - but each species page is still rendered
#' and loaded separately, so a browser only has to load one species' plots
#' at a time, instead of every species' plots at once on a single page.
#' Nothing is recomputed or replotted: only the files \code{evaluate()}
#' already wrote are read.
#'
#' None of the pages are standalone files: they link to the PNG/HTML plots
#' via relative paths (and Quarto writes its own JS/CSS assets to a shared
#' \code{site_libs/} subfolder), so keep \code{output_dir} intact when
#' sharing or moving results.
#'
#' Requires the Quarto CLI (\url{https://quarto.org}) to be installed, along
#' with the \pkg{quarto} and \pkg{DT} R packages.
#'
#' @param output_dir path to a directory previously populated by
#'  \code{\link{evaluate}} (i.e. \code{evaluate(..., output_dir = output_dir)}
#'  must have been run beforehand). The rendered site is written there.
#' @param open Boolean. Open the rendered dashboard in a browser once done.
#'  Defaults to \code{TRUE} in interactive sessions.
#'
#' @return invisibly, the path to the rendered \code{index.html} file
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
    c("quarto", "DT", "yaml"),
    reason = "to render the evaluation dashboard"
  )
  if (is.null(quarto::quarto_path())) {
    stop(
      "Quarto CLI not found. Install it from https://quarto.org before ",
      "calling render_report().",
      call. = FALSE
    )
  }

  species_list <- get_species_list(output_dir)
  written <- write_project_files(output_dir, species_list)
  on.exit(
    {
      unlink(written)
      unlink(file.path(output_dir, ".quarto"), recursive = TRUE)
    },
    add = TRUE
  )

  logger::log_info("Rendering evaluation dashboard...")
  quarto::quarto_render(
    input = file.path(output_dir, "index.qmd"), quiet = !is_debug()
  )
  quarto::quarto_render(
    input = file.path(output_dir, "global.qmd"), quiet = !is_debug()
  )

  abs_output_dir <- normalizePath(output_dir)
  for (sp in species_list) {
    logger::log_info("Rendering evaluation page for species {sp}...")
    quarto::quarto_render(
      input = file.path(output_dir, "species", paste0(sp, ".qmd")),
      execute_params = list(species = sp, output_dir = abs_output_dir),
      quiet = !is_debug()
    )
  }

  html_path <- file.path(output_dir, "index.html")
  if (!file.exists(html_path)) {
    stop("Quarto render did not produce ", html_path, call. = FALSE)
  }
  logger::log_info("Dashboard written to {html_path}")

  if (open) {
    utils::browseURL(html_path)
  }

  invisible(html_path)
}

get_species_list <- function(output_dir) {
  species_stats_path <- file.path(output_dir, "csv", "species_stats.csv")
  if (!file.exists(species_stats_path)) return(character(0))

  species_stats <- utils::read.csv(species_stats_path, stringsAsFactors = FALSE) # nolint: line_length_linter
  if (!"species" %in% names(species_stats)) return(character(0))
  sort(unique(species_stats$species))
}

# Writes the Quarto project files needed to render `output_dir` as a single
# website: a generated `_quarto.yml` (so the "Species" sidebar section is
# only included when there's species data), a copy of `index.qmd` (the
# homepage - named that way, rather than e.g. "dashboard.qmd", so Quarto
# renders it straight to `index.html` instead of generating a redirect stub
# there pointing at whichever page it picks first), a copy of `global.qmd`
# (degraded variables and global stats, kept off the homepage), and one
# `species/<species>.qmd` per species (so it renders to
# `species/<species>.html`) - kept separate from `plots/<species>/` (where
# the species' own PNG/HTML plots were written by `evaluate()`) so report
# pages and raw plot assets don't mix; species.qmd links to its plots via a
# relative `../plots/<species>/` path instead of a bare filename. Templates
# are copied verbatim - the `species`/`output_dir` params they declare are
# filled in at render time via `execute_params`, not by rewriting the file.
# Returns every path written, for the caller to clean up after rendering.
write_project_files <- function(output_dir, species_list) {
  quarto_dir <- system.file("quarto", package = "SticsREval")

  yml_path <- write_quarto_yml(output_dir, species_list)
  index_path <- file.path(output_dir, "index.qmd")
  file.copy(
    file.path(quarto_dir, "index.qmd"), index_path, overwrite = TRUE
  )
  global_path <- file.path(output_dir, "global.qmd")
  file.copy(
    file.path(quarto_dir, "global.qmd"), global_path, overwrite = TRUE
  )
  written <- c(yml_path, index_path, global_path)

  if (length(species_list) > 0) {
    species_dir <- file.path(output_dir, "species")
    dir.create(species_dir, recursive = TRUE, showWarnings = FALSE)
    for (sp in species_list) {
      sp_qmd_path <- file.path(species_dir, paste0(sp, ".qmd"))
      file.copy(
        file.path(quarto_dir, "species.qmd"), sp_qmd_path, overwrite = TRUE
      )
      written <- c(written, sp_qmd_path)
    }
  }

  written
}

write_quarto_yml <- function(output_dir, species_list) {
  quarto_dir <- system.file("quarto", package = "SticsREval")

  yml_path <- file.path(output_dir, "_quarto.yml")

  quarto_content <- yaml::read_yaml(file.path(quarto_dir, "_quarto.yml"))

  if (length(species_list) > 0) {
    entries <- lapply(species_list, function(sp) {
      list(
        text = sp,
        href = file.path("species", paste0(sp, ".qmd"))
      )
    })
    species_section <- list(section = "Species", contents = entries)
    # `read_yaml()` collapses a sequence of plain strings (e.g. the static
    # `index.qmd`/`global.qmd` entries) into an atomic character vector
    # rather than a list, so it must go through `as.list()` before being
    # combined with `species_section` - otherwise `c()`/`list()` would nest
    # it as a single array-within-array entry instead of flattening it.
    quarto_content$website$sidebar$contents <- c(
      as.list(quarto_content$website$sidebar$contents),
      list(species_section)
    )
  }

  yml_content <- yaml::as.yaml(
    quarto_content, handlers = list(logical = yaml::verbatim_logical)
  )

  writeLines(yml_content, yml_path)
  yml_path
}
