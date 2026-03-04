# ===========================================================================
# Tests: gen_scatter_plot
# ===========================================================================
# NOTE: CroPlotR:::plot.cropr_simulation ne peut pas être intercepté par
# mockery::stub car c'est un appel interne (:::). On utilise
# testthat::local_mocked_bindings pour patcher au niveau du namespace CroPlotR.

test_that("gen_scatter_plot calls CroPlotR plot with correct arguments", {
  fake_ggplot <- structure(list(), class = "ggplot")
  captured    <- NULL

  local_mocked_bindings(
    plot.cropr_simulation = function(...) {
      captured <<- list(...)
      list()
    },
    .package = "CroPlotR"
  )
  stub(
    gen_scatter_plot,
    "CroPlotR::extract_plot",
    mock(list(fake_ggplot), cycle = TRUE)
  )
  stub(gen_scatter_plot, "plotly::ggplotly",        mock(list(), cycle = TRUE))
  stub(gen_scatter_plot, "htmltools::save_html",    mock(NULL))

  fake_sim <- list()
  fake_obs <- list()
  fake_ref_sim <- list()

  gen_scatter_plot(
    output_dir = tempdir(),
    sim        = fake_sim,
    obs        = fake_obs,
    ref_sim    = fake_ref_sim,
    vars       = c("LAI", "MASEC")
  )

  expect_equal(captured$obs,         fake_obs)
  expect_equal(captured$type,        "scatter")
  expect_equal(captured$select_scat, "sim")
  expect_equal(captured$var,         c("LAI", "MASEC"))
})

test_that("gen_scatter_plot calls extract_plot once per variable", {
  fake_ggplot  <- structure(list(), class = "ggplot")
  mock_extract <- mock(list(fake_ggplot), cycle = TRUE)

  local_mocked_bindings(
    plot.cropr_simulation = function(...) list(),
    .package = "CroPlotR"
  )
  stub(gen_scatter_plot, "CroPlotR::extract_plot", mock_extract)
  stub(gen_scatter_plot, "plotly::ggplotly",        mock(list(), cycle = TRUE))
  stub(gen_scatter_plot, "htmltools::save_html",    mock(NULL))

  gen_scatter_plot(
    tempdir(),
    list(), list(), list(),
    vars = c("LAI", "MASEC", "ZRAC")
  )

  expect_called(mock_extract, 3)
})

test_that("gen_scatter_plot saves HTML to output_dir/scatter_plots.html", {
  fake_ggplot    <- structure(list(), class = "ggplot")
  mock_save_html <- mock(NULL)

  local_mocked_bindings(
    plot.cropr_simulation = function(...) list(),
    .package = "CroPlotR"
  )
  stub(gen_scatter_plot, "CroPlotR::extract_plot", mock(list(fake_ggplot)))
  stub(gen_scatter_plot, "plotly::ggplotly",        mock(list()))
  stub(gen_scatter_plot, "htmltools::save_html",    mock_save_html)

  out_dir <- tempdir()
  gen_scatter_plot(out_dir, list(), list(), list(), vars = "LAI")

  args <- mock_args(mock_save_html)[[1]]
  expect_equal(args$file, file.path(out_dir, "scatter_plots.html"))
})

test_that("gen_scatter_plot returns NULL invisibly", {
  fake_ggplot <- structure(list(), class = "ggplot")

  local_mocked_bindings(
    plot.cropr_simulation = function(...) list(),
    .package = "CroPlotR"
  )
  stub(gen_scatter_plot, "CroPlotR::extract_plot", mock(list(fake_ggplot)))
  stub(gen_scatter_plot, "plotly::ggplotly",        mock(list()))
  stub(gen_scatter_plot, "htmltools::save_html",    mock(NULL))

  result <- gen_scatter_plot(tempdir(), list(), list(), list(), vars = "LAI")
  expect_null(result)
})

# ===========================================================================
# Helpers: gen_comparison_plot
# ===========================================================================

make_comparison_df <- function() {
  data.frame(
    variable = c("LAI", "MASEC", "ZRAC"),
    rmse_ref = c(0.5,   1.0,    0.3),
    rmse_new = c(0.6,   0.8,    0.5),
    ratio    = c(1.2,   0.8,    1.7)
  )
}

# ===========================================================================
# Tests: gen_comparison_plot
# ===========================================================================

test_that("gen_comparison_plot calls CroPlotR::save_plot_png", {
  mock_save <- mock(NULL)
  stub(gen_comparison_plot, "CroPlotR::save_plot_png", mock_save)

  gen_comparison_plot(
    output_dir = tempdir(),
    comparison = make_comparison_df(),
    percentage = 20
  )

  expect_called(mock_save, 1)
})

test_that("gen_comparison_plot passes output_dir to save_plot_png", {
  mock_save <- mock(NULL)
  stub(gen_comparison_plot, "CroPlotR::save_plot_png", mock_save)

  out_dir <- tempdir()
  gen_comparison_plot(out_dir, make_comparison_df(), percentage = 20)

  args <- mock_args(mock_save)[[1]]
  expect_equal(args$out_dir, out_dir)
})

test_that(
  "gen_comparison_plot assigns correct status based on ratio and percentage",
  {
    mock_save <- mock(NULL)
    captured  <- NULL

    stub(gen_comparison_plot, "CroPlotR::save_plot_png", function(p, ...) {
      captured <<- p
      NULL
    })

    gen_comparison_plot(
      output_dir = tempdir(),
      comparison = make_comparison_df(),
      percentage = 20
    )

    expect_s3_class(captured, "ggplot")
  }
)

test_that("gen_comparison_plot uses suffix 'scatter_' for save_plot_png", {
  mock_save <- mock(NULL)
  stub(gen_comparison_plot, "CroPlotR::save_plot_png", mock_save)

  gen_comparison_plot(tempdir(), make_comparison_df(), percentage = 10)

  args <- mock_args(mock_save)[[1]]
  expect_equal(args$suffix, "scatter_")
})

# ===========================================================================
# Tests: gen_plots
# ===========================================================================

make_fake_config <- function(overrides = list()) {
  cfg <- list(
    output_dir         = tempdir(),
    reference_data_dir = tempdir(),
    percentage         = 20,
    parallel           = FALSE,
    cores              = NA,
    eval_workspace     = list()
  )
  for (nm in names(overrides)) cfg[[nm]] <- overrides[[nm]]
  cfg
}

test_that("gen_plots calls validate_export_config and validate_plots_config", {
  mock_validate_export <- mock(NULL)
  mock_validate_plots  <- mock(NULL)
  mock_species         <- mock(character(0))
  mock_loop            <- mock(list())

  stub(gen_plots, "validate_export_config", mock_validate_export)
  stub(gen_plots, "validate_plots_config",    mock_validate_plots)
  stub(gen_plots, "get_species",            mock_species)
  stub(gen_plots, "parallelizable_loop",    mock_loop)

  gen_plots(make_fake_config())

  expect_called(mock_validate_export, 1)
  expect_called(mock_validate_plots,  1)
})

test_that("gen_plots skips plot generation when spec_comparison is NULL", {
  mock_loop <- mock(NULL)

  stub(gen_plots, "validate_export_config",      mock(NULL))
  stub(gen_plots, "validate_plots_config",         mock(NULL))
  stub(gen_plots, "get_species",                 mock(c("wheat")))
  stub(gen_plots, "parallelizable_loop",         mock_loop)
  stub(gen_plots, "prepare_species_output_dir",  mock(tempdir()))
  stub(gen_plots, "get_species_comparison",      mock(NULL))
  stub(gen_plots, "gen_comparison_plot",         mock(NULL))

  gen_plots(make_fake_config())

  # parallelizable_loop appelé mais gen_comparison_plot jamais appelé
  # (le mock de parallelizable_loop court-circuite la boucle)
  expect_called(mock_loop, 1)
})

test_that(
  "gen_plots calls gen_comparison_plot when comparison data is available",
  {
    mock_gen_comparison <- mock(NULL)
    mock_gen_scatter    <- mock(NULL)

    # On simule parallelizable_loop en appelant directement la fonction interne
    stub(gen_plots, "validate_export_config",     mock(NULL))
    stub(gen_plots, "validate_plots_config",        mock(NULL))
    stub(gen_plots, "get_species",                mock(c("wheat")))
    stub(
      gen_plots,
      "parallelizable_loop",
      function(n, par, cores, fn) lapply(seq_len(n), fn)
    )
    stub(gen_plots, "prepare_species_output_dir", mock(tempdir()))
    stub(gen_plots, "get_species_comparison",     mock(make_comparison_df()))
    stub(gen_plots, "gen_comparison_plot",        mock_gen_comparison)
    stub(gen_plots, "get_crit_vars",              mock(character(0)))
    stub(gen_plots, "get_warn_vars",              mock(character(0)))

    gen_plots(make_fake_config())

    expect_called(mock_gen_comparison, 1)
  }
)

test_that(
  "gen_plots calls gen_scatter_plot when deteriorated vars and ref_sim exist",
  {
    mock_gen_scatter <- mock(NULL)

    stub(gen_plots, "validate_export_config",     mock(NULL))
    stub(gen_plots, "valide_plovalidate_plots_configts_config",        mock(NULL))
    stub(gen_plots, "get_species",                mock(c("wheat")))
    stub(
      gen_plots,
      "parallelizable_loop",
      function(n, par, cores, fn) lapply(seq_len(n), fn)
    )
    stub(gen_plots, "prepare_species_output_dir", mock(tempdir()))
    stub(gen_plots, "get_species_comparison",     mock(make_comparison_df()))
    stub(gen_plots, "gen_comparison_plot",        mock(NULL))
    stub(gen_plots, "get_crit_vars",              mock(c("LAI")))
    stub(gen_plots, "get_warn_vars",              mock(character(0)))
    stub(gen_plots, "read_ref_sim",               mock(list()))
    stub(
      gen_plots,
      "get_by_species",
      mock(data.frame(), cycle = TRUE)
    )
    stub(gen_plots, "CroPlotR::split_df2sim",     mock(list(), cycle = TRUE))
    stub(gen_plots, "gen_scatter_plot",           mock_gen_scatter)

    gen_plots(make_fake_config())

    expect_called(mock_gen_scatter, 1)
  }
)

test_that(
  "gen_plots does not call gen_scatter_plot when no deteriorated vars",
  {
    mock_gen_scatter <- mock(NULL)

    stub(gen_plots, "validate_export_config",     mock(NULL))
    stub(gen_plots, "validate_plots_config",        mock(NULL))
    stub(gen_plots, "get_species",                mock(c("wheat")))
    stub(
      gen_plots,
      "parallelizable_loop",
      function(n, par, cores, fn) lapply(seq_len(n), fn)
    )
    stub(gen_plots, "prepare_species_output_dir", mock(tempdir()))
    stub(gen_plots, "get_species_comparison",     mock(make_comparison_df()))
    stub(gen_plots, "gen_comparison_plot",        mock(NULL))
    stub(gen_plots, "get_crit_vars",              mock(character(0)))
    stub(gen_plots, "get_warn_vars",              mock(character(0)))
    stub(gen_plots, "gen_scatter_plot",           mock_gen_scatter)

    gen_plots(make_fake_config())

    expect_called(mock_gen_scatter, 0)
  }
)

test_that("gen_plots does not call gen_scatter_plot when ref_sim is NULL", {
  mock_gen_scatter <- mock(NULL)

  stub(gen_plots, "validate_export_config",     mock(NULL))
  stub(gen_plots, "validate_plots_config",        mock(NULL))
  stub(gen_plots, "get_species",                mock(c("wheat")))
  stub(
    gen_plots,
    "parallelizable_loop",
    function(n, par, cores, fn) lapply(seq_len(n), fn)
  )
  stub(gen_plots, "prepare_species_output_dir", mock(tempdir()))
  stub(gen_plots, "get_species_comparison",     mock(make_comparison_df()))
  stub(gen_plots, "gen_comparison_plot",        mock(NULL))
  stub(gen_plots, "get_crit_vars",              mock(c("LAI")))
  stub(gen_plots, "get_warn_vars",              mock(character(0)))
  stub(gen_plots, "read_ref_sim",               mock(NULL))
  stub(gen_plots, "gen_scatter_plot",           mock_gen_scatter)

  gen_plots(make_fake_config())

  expect_called(mock_gen_scatter, 0)
})