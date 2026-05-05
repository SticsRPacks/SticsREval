#' @importFrom utils getS3method
gen_scatter_plot <- function(output_dir, sim, obs, ref_sim, vars) {
  loadNamespace("CroPlotR")
  plots <- getS3method("plot", class(sim))(
    "New version" = sim,
    "Ref version" = ref_sim,
    obs = obs,
    type = "scatter",
    select_scat = "sim",
    var = vars
  )
  page_list <- htmltools::tagList(
    lapply(vars, function(var) {
      suppressWarnings(
        plotly::ggplotly(
          CroPlotR::extract_plot(plots, var = var)[[1]]
        )
      )
    })
  )
  htmltools::save_html(
    page_list,
    file = file.path(
      output_dir,
      "scatter_plots.html"
    )
  )
  invisible(NULL)
}

#' Generate comparison and scatter plots for each species
#'
#' This function generates diagnostic plots for each species found in the
#' evaluation workspace. It produces comparison plots and, when possible,
#' scatter plots highlighting deteriorated variables based on a given threshold.
#'
#' For each species, the function:
#' \itemize{
#'   \item Retrieves comparison data between simulation and observation.
#'   \item Generates comparison plots.
#'   \item Identifies deteriorated variables using a threshold percentage.
#'   \item Optionally generates scatter plots if reference simulation data
#'      are available.
#' }
#'
#' Scatter plots are only generated when both reference simulation data and
#' deteriorated variables are available.
#'
#' @param config List. Configuration object created by `make_config()`,
#'    containing all parameters required for the plots generation
#' @param workspace EvalWorkspace. An instance of `EvalWorkspace` to access the
#'   evaluation data. Defaults to a new instance using the `eval_workspace`
#'  parameter from the configuration.
#' @param backend ParallelBackend. An instance of `ParallelBackend` to run
#'  parallel computations. Defaults to a new instance using the `parallel` and
#' `cores` parameters from the configuration.
#' @param scatter_fn Function. A function to generate scatter plots. It should
#' accept the following arguments: `output_dir`, `sim`, `obs`, `ref_sim`, and `vars`.
#' Defaults to `gen_scatter_plot()`.
#' @param comparison_fn Function. A function to generate comparison plots. It should
#' accept the following arguments: `comparison` and `output_dir`. Defaults to a
#'  function that calls the `plot_comparison()` method of the comparison object.
#' @param logger_info Function. A logging function to report progress. It should
#' accept a single string argument. Defaults to `logger::log_info()`.
#' @return NULL. This function is called for its side effects (writing plot
#' files).
#' 
#' @details
#' For each species:
#' \itemize{
#'   \item Creates a species-specific output directory.
#'   \item Generates comparison plots using `gen_comparison_plot()`.
#'   \item Identifies critical and warning variables using the given percentage.
#'   \item Generates scatter plots using `gen_scatter_plot()` if reference data
#'   and deteriorated variables are available.
#' }
#'
#' @examples
#' \dontrun{
#' config <- make_config(
#'  output_dir = "results/",
#'  reference_workspace = "ref_dir/"
#' )
#' gen_plots(config)
#' }
#'
#' @export
gen_plots <- function(
  config,
  workspace = EvalWorkspace$new(config$eval_workspace),
  backend = ParallelBackend$new(config$parallel, config$cores),
  scatter_fn = gen_scatter_plot,
  comparison_fn = function(x, dir) x$plot_comparison(dir),
  logger_info = logger::log_info
) {
  start_time <- Sys.time()

  config$validate_export()$validate_plots()

  species <- workspace$get_species()

  backend$run(
    length(species),
    function(i) {

      spec <- species[i]

      o_dir <- prepare_species_output_dir(config$output_dir, spec)

      spec_comparison <- workspace$get_species_comparison(
        spec,
        config$percentage
      )

      if (is.null(spec_comparison)) {
        logger_info(sprintf("Skipping plot generation for species %s", spec))
        return(NULL)
      }

      logger_info(sprintf(
        "Generating variable comparison plot for species %s", spec
      ))

      comparison_fn(spec_comparison, o_dir)

      deteriorated <- c(
        spec_comparison$critical_vars,
        spec_comparison$warning_vars
      )

      if (length(deteriorated) == 0) return(NULL)

      var2exclude <- c("version", "species")

      ref_sim <- workspace$with_version(
        config$reference_version
      )$get_sim(spec, var2exclude = var2exclude)

      if (is.null(ref_sim)) return(NULL)

      logger_info(sprintf("Generating scatter plots for species %s", spec))

      sim <- workspace$get_sim(spec, var2exclude = var2exclude)
      obs <- workspace$get_obs(spec, var2exclude = var2exclude)

      scatter_fn(
        o_dir,
        CroPlotR::split_df2sim(sim),
        CroPlotR::split_df2sim(obs),
        CroPlotR::split_df2sim(ref_sim),
        deteriorated
      )

      NULL
    }
  )

  logger_info(paste0(
    "Plots generation time: ", format_duration(start_time)
  ))

  invisible(NULL)
}
