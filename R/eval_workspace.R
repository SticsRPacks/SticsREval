#'
#' EvalWorkspace class
#'
#' Manage the evaluation workspace
#'
#' @name EvalWorkspace
#' @docType class
#'
#' @examples
#' \dontrun{
#' ws <- EvalWorkspace$new(
#'   data_dir = "/path/to/eval_workspace"
#' )
#' }
#'
#' @export
EvalWorkspace <- R6::R6Class("EvalWorkspace",
  private = list(
    .data_dir = NULL,
    .version  = NULL,

    open_parquet_or_null = function(path, collect, warn_msg) {
      if (!isTRUE(file.exists(path))) {
        logger::log_warn(warn_msg)
        return(NULL)
      }
      ds <- arrow::open_dataset(path)
      if (collect) {
        return(dplyr::collect(ds))
      }
      ds
    },
    get_sim_ds = function() {
      ds_path <- sim_ds_path(private$.data_dir)
      if (!dir.exists(ds_path)) {
        stop(
          "The simulation dataset at ", ds_path, " does not exist. ",
          "Please make sure the evaluation workspace has been properly ",
          "initialized.",
          call. = FALSE
        )
      }
      arrow::open_dataset(ds_path) |>
        dplyr::filter(.data$version == private$.version)
    },
    get_obs_ds = function() {
      ds_path <- obs_ds_path(private$.data_dir)
      if (!dir.exists(ds_path)) {
        stop(
          "The observation dataset at ", ds_path, " does not exist. ",
          "Please make sure the evaluation workspace has been properly ",
          "initialized.",
          call. = FALSE
        )
      }
      arrow::open_dataset(ds_path) |>
        dplyr::filter(.data$version == private$.version)
    },
    remove_init_obs = function() {
      init_dates <- private$get_sim_ds() |>
        dplyr::group_by(.data$situation) |>
        dplyr::summarise(init_date = min(.data$Date, na.rm = TRUE))

      exclude_cols <- c("situation", "species", "version", "Date", "init_date")

      private$get_obs_ds() |>
        dplyr::left_join(init_dates, by = "situation") |>
        dplyr::mutate(
          dplyr::across(
            -dplyr::all_of(exclude_cols),
            ~ dplyr::if_else(.data$Date == .data$init_date, NA_real_, .x)
          )
        ) |>
        dplyr::select(-"init_date") |>
        arrow::write_dataset(
          obs_ds_path(private$.data_dir),
          format = "parquet",
          partitioning = c("version", "species")
        )
    },
    get_by_species = function(
      species = NULL, type = c("sim", "obs"), collect = FALSE,
      usms = NULL, var2exclude = NULL
    ) {
      type <- match.arg(type)
      res <- if (type == "sim") {
        private$get_sim_ds()
      } else {
        private$get_obs_ds()
      }

      if (!is.null(species)) {
        res <- dplyr::filter(res, .data$species == {{ species }})
      }

      if (!is.null(usms)) {
        res <- dplyr::filter(res, .data$situation %in% usms)
      }
      if (!is.null(var2exclude)) {
        cols_to_keep <- setdiff(names(res), var2exclude)
        res <- dplyr::select(res, dplyr::all_of(cols_to_keep))
      }

      if (collect) {
        return(dplyr::collect(res))
      }
      res
    }
  ),
  public = list(
    #' @description Create an evaluation workspace manager
    #'
    #' @param data_dir Path to the evaluation workspace
    #' @param version Optional, the version of STICS to use for the data
    #' extraction. If not defined the last evaluated version will be used.
    initialize = function(data_dir, version = NULL) {
      private$.data_dir <- data_dir
      private$.version <- version
      if (is.null(private$.version)) {
        private$.version <- self$get_stics_version()
      }
    },

    #' @description
    #' Initialize the evaluation workspace
    #'
    #' @param data_workspace Path to the USMs data workspace
    #' @param metadata_file Path to the metadata file
    #' @param stics_exe Patht to the STICS executable
    #' @param must_run_simulations Run the simulations before loading
    #'  simulations data ?
    #' @param parallel_backend a parallel backend which contains the parallelism
    #'  configuration
    init = function(
      data_workspace,
      metadata_file,
      stics_exe,
      must_run_simulations,
      parallel_backend
    ) {
      logger::log_info("Initializing workspace {private$.data_dir} for evaluation...")
      if (!dir.exists(private$.data_dir) &&
          !dir.create(private$.data_dir)
      ) {
        stop("Can't create evaluation workspace", call. = FALSE)
      }
      WorkspaceLoader$new(
        workspace = self,
        backend = parallel_backend,
        usms_workspace = data_workspace,
        metadata_file = metadata_file,
        stics_exe = stics_exe,
        run_simulations = must_run_simulations
      )$load()
      private$remove_init_obs()
    },
    #' @description
    #' Save simulations
    #' 
    #' @param sim the list of simulations
    #' @param usms_species a dataframe which associates a USM to its species
    save_sim = function(sim, usms_species) {
      current_version <- private$.version
      sim_data <- CroPlotR::bind_rows(sim) |>
        dplyr::inner_join(usms_species, by = c(situation = "usm")) |>
        dplyr::mutate(version = current_version)
      arrow::write_dataset(
          sim_data,
          path = sim_ds_path(private$.data_dir),
          format = "parquet",
          partitioning = c("version", "species")
        )
    },
    #' @description
    #' Save observations
    #' 
    #' @param obs the list of observations
    #' @param usms_species a dataframe which associates a USM to its species
    save_obs = function(obs, usms_species) {
      current_version <- private$.version
      obs_data <- CroPlotR::bind_rows(obs, .id = "situation") |>
        dplyr::inner_join(usms_species, by = c(situation = "usm")) |>
        dplyr::mutate(version = current_version)
      arrow::write_dataset(
          obs_data,
          path = obs_ds_path(private$.data_dir),
          format = "parquet",
          partitioning = c("version", "species")
        )
    },
    #' @description
    #' Returns the list of species
    #' 
    #' @returns a list of species as character list
    get_species = function() {
      private$get_obs_ds() |>
        dplyr::distinct(.data$species) |>
        dplyr::arrange(tolower(.data$species)) |>
        dplyr::collect() |>
        dplyr::pull("species")
    },

    #' @description
    #' Returns the list of USMs of a species
    #' 
    #' @param species the species to search for
    #' @param usms Optional, filter the USMs returned by the function

    #' @returns a list of USMs as a character list
    get_species_usm = function(species, usms = NULL) {
      res <- private$get_obs_ds() |>
        dplyr::filter(.data$species == {{ species }}) |>
        dplyr::distinct(.data$situation)
      if (!is.null(usms)) {
        res <- dplyr::filter(res, .data$situation %in% usms)
      }
      res |>
        dplyr::collect() |>
        dplyr::pull("situation")
    },
    #' @description
    #' Return the simulation
    #' 
    #' @param species Optional, if defined filter the simulations with this
    #'  species
    #' @param usms Optional, if defined filter the simulations with these
    #'  USMs
    #' @param var2exclude Optional, if defined remove the variables from
    #'  the returned simulations
    #' @param collect Optional, if `TRUE` a dataframe will be returned,
    #'  otherwise the lazy arrow data object will be returned
    #' 
    #' @returns a dataframe if collect is `TRUE`, a lazy arrow data object
    get_sim = function(
      species = NULL, usms = NULL, var2exclude = NULL, collect = TRUE
    ) {
      private$get_by_species(
        species = species,
        type = "sim",
        usms = usms,
        collect = collect,
        var2exclude = var2exclude
      )
    },
    #' @description
    #' Return the observation
    #' 
    #' @param species Optional, if defined filter the observations with this
    #'  species
    #' @param usms Optional, if defined filter the observations with these
    #'  USMs
    #' @param var2exclude Optional, if defined remove the variables from
    #'  the returned observations
    #' @param collect Optional, if `TRUE` a dataframe will be returned,
    #'  otherwise the lazy arrow data object will be returned
    #' 
    #' @returns a dataframe if collect is `TRUE`, a lazy arrow data object

    get_obs = function(
      species = NULL, usms = NULL, var2exclude = NULL, collect = TRUE
    ) {
      private$get_by_species(
        species = species,
        type = "obs",
        usms = usms,
        collect = collect,
        var2exclude = var2exclude
      )
    },
    #' @description
    #' Save a species statistics
    #' 
    #' @param species the species
    #' @param stats the statistics as a dataframe
    save_stats = function(species, stats) {
      stats <- dplyr::mutate(stats, species = species, version = private$.version)
      arrow::write_dataset(
        stats,
        stats_ds_path(private$.data_dir),
        format = "parquet",
        partitioning = c("version", "species"),
        existing_data_behavior = "delete_matching"
      )
    },
    #' @description
    #' Return statistics for a species
    #' 
    #' @param species the species
    #' @param collect Optional, if `TRUE` returns a dataframe,
    #'  otherwise a lazy arrow data object will be returned
    #' 
    #' @returns the statistics, as a dataframe if collect is `TRUE`,
    #'  a lazy arrow data object otherwise
    get_stats = function(species, collect = FALSE) {
      ds <- private$open_parquet_or_null(
        path = stats_ds_path(private$.data_dir),
        collect = collect,
        warn_msg = paste(
          "No stats file found for species", species, "in", private$.data_dir
        )
      )
      if (is.null(ds)) {
        return(NULL)
      }
      current_version <- private$.version
      res <- dplyr::filter(
        ds,
        .data$version == current_version,
        .data$species == {{ species }}
      )

      if (collect) {
        return(dplyr::collect(res))
      }
      res
    },
    #' @description
    #' Save the RMSE per USM
    #' 
    #' @param species the species
    #' @param rmse_per_usm the RMSE per USMs 
    save_rmse_per_usm = function(species, rmse_per_usm) {
      rmse_per_usm <- dplyr::mutate(
        rmse_per_usm,
        species = species,
        version = private$.version
      )
      arrow::write_dataset(
        rmse_per_usm,
        rmse_per_usm_ds_path(private$.data_dir),
        format = "parquet",
        partitioning = c("version", "species"),
        existing_data_behavior = "delete_matching"
      )
    },
    #' @description
    #' Return the RMSE per USM for a species
    #' 
    #' @param species the species
    #' @param collect Optional, if `TRUE` a dataframe will be returned,
    #'  otherwise a lazy arrow data object will be returned
    #' @param usms Optional, if defined filter the RMSE per USM with these
    #'  USMs
    #' @param var2exclude Optional, if defined remove the variables from
    #'  the returned RMSE per USM
    #' 
    #' @returns the RMSE per USM for a species, as a dataframe if collect is `TRUE`,
    #'  a lazy arrow data object otherwise
    get_rmse_per_usm = function(
      species, collect = FALSE, usms = NULL, var2exclude = NULL
    ) {
      res <- private$open_parquet_or_null(
        path = rmse_per_usm_ds_path(private$.data_dir),
        collect = collect,
        warn_msg = paste(
          "No RMSE per USM file found for species", species, "in", private$.data_dir
        )
      )
      if (is.null(res)) {
        return(res)
      }
      current_version <- private$.version
      res <- dplyr::filter(
        res,
        .data$version == current_version,
        .data$species == {{ species }}
      )
      if (!is.null(usms)) {
        res <- dplyr::filter(res, .data$situation %in% usms)
      }
      if (!is.null(var2exclude)) {
        cols_to_keep <- setdiff(names(res), var2exclude)
        res <- dplyr::select(res, dplyr::all_of(cols_to_keep))
      }
      res
    },
    #' @description
    #' Save the deteriorated USM
    #' 
    #' @param deteriorated the deteriorated USM object
    save_deteriorated_usm = function(deteriorated) {
      d <- deteriorated$get_data() |>
        dplyr::mutate(version = private$.version)
      arrow::write_dataset(
        d,
        deteriorated_ds_path(private$.data_dir),
        format = "parquet",
        partitioning = c("version", "species"),
        existing_data_behavior = "delete_matching"
      )
    },
    #' @description
    #' Get the deteriorated USM comparison for a species
    #' 
    #' @param species the species
    #' @param percentage the percentage
    #' 
    #' @returns a DeterioratedUSMComparison object
    get_deteriorated_usm = function(species, percentage) {
      ds <- private$open_parquet_or_null(
        path = deteriorated_ds_path(private$.data_dir),
        collect = TRUE,
        warn_msg = paste(
          "No deteriorated USM file found for species",
          species, "in", private$.data_dir
        )
      )
      if (is.null(ds)) {
        return(NULL)
      }
      current_version <- private$.version
      res <- dplyr::filter(
        ds,
        .data$version == current_version,
        .data$species == {{ species }}
      )
      DeterioratedUSMComparison$new(data = res, percentage = percentage)
    },
    #' @description
    #' Save the species comparison
    #' 
    #' @param spec_comparison the species comparison object
    save_species_comparison = function(spec_comparison) {
      d <- spec_comparison$get_data() |>
        dplyr::mutate(version = private$.version)
      arrow::write_dataset(
        d,
        comparison_ds_path(private$.data_dir),
        format = "parquet",
        partitioning = c("version", "species"),
        existing_data_behavior = "delete_matching"
      )
    },
    #' @description
    #' Get the species comparison for a species
    #' 
    #' @param species the species
    #' @param percentage the percentage
    #' 
    #' @returns an RmseComparison object
    get_species_comparison = function(
      species, percentage
    ) {
      ds <- private$open_parquet_or_null(
        path = comparison_ds_path(private$.data_dir),
        collect = TRUE,
        warn_msg = paste(
          "No comparison file found for species",
          species, "in", private$.data_dir
        )
      )
      if (is.null(ds)) {
        return(NULL)
      }
      current_version <- private$.version
      res <- dplyr::filter(
        ds,
        .data$version == current_version,
        .data$species == {{ species }}
      )
      RmseComparison$new(
        data = res,
        percentage = percentage
      )
    },
    #' @description
    #' Save the metadata
    #' 
    #' @param metadata the metadata dataframe
    save_metadata = function(metadata) {
      arrow::write_parquet(
        metadata,
        sink = metadata_ds_path(private$.data_dir)
      )
    },
    #' @description
    #' Add an evaluated version to the metadata
    #' 
    #' @param version the STICS version
    add_evaluated_version = function(version) {
      new_line <- data.frame(
        stics_version = version,
        last_evaluated = TRUE
      )

      if (file.exists(metadata_ds_path(private$.data_dir))) {
        metadata <- private$open_parquet_or_null(
          path = metadata_ds_path(private$.data_dir),
          collect = TRUE,
          warn_msg = paste("No metadata file found in", private$.data_dir)
        )

        metadata <- dplyr::mutate(metadata, last_evaluated = FALSE)

        if (version %in% metadata$stics_version) {
          metadata <- dplyr::mutate(
            metadata,
            last_evaluated = dplyr::if_else(
              stics_version == version, TRUE, last_evaluated
            )
          )
        } else {
          metadata <- rbind(metadata, new_line)
        }
      } else {
        metadata <- new_line
      }
      arrow::write_parquet(metadata, sink = metadata_ds_path(private$.data_dir))
    },
    #' @description
    #' Get all evaluated versions
    #' 
    #' @returns a list of versions
    get_all_versions = function() {
      versions <- private$open_parquet_or_null(
        path = metadata_ds_path(private$.data_dir),
        collect = TRUE,
        warn_msg = paste("No metadata file found in", private$.data_dir)
      )
      if (is.null(versions)) return(NULL)
      versions$stics_version
    },
    #' @description
    #' Get the last evaluated STICS version
    #' 
    #' @returns the STICS version
    get_stics_version = function() {
      ds <- private$open_parquet_or_null(
        path = metadata_ds_path(private$.data_dir),
        collect = TRUE,
        warn_msg = paste("No metadata file found in", private$.data_dir)
      )
      if (is.null(ds)) {
        return(NULL)
      }
      res <- dplyr::filter(ds, .data$last_evaluated)
      res$stics_version
    },
    #' @description
    #' Create a new EvalWorkspace with a specific version
    #' 
    #' @param version the version
    #' 
    #' @returns an EvalWorkspace object
    with_version = function(version) {
      EvalWorkspace$new(private$.data_dir, version)
    },

    #' @description
    #' Set the version of the evaluation workspace
    #' 
    #' @param v the version to set
    set_version = function(v) {
      private$.version <- v
    },
    #' @description
    #' Get the current version of the evaluation workspace
    #' 
    #' @returns the current version of the evaluation workspace
    get_version = function() {
      private$.version
    }
  )
)

sim_ds_path <- function(data_dir) {
  file.path(data_dir, "sim")
}

obs_ds_path <- function(data_dir) {
  file.path(data_dir, "obs")
}

stats_ds_path <- function(data_dir) {
  file.path(data_dir, "Criteres_stats")
}

rmse_per_usm_ds_path <- function(data_dir) {
  file.path(data_dir, "RMSE_per_USM")
}

deteriorated_ds_path <- function(data_dir) {
  file.path(data_dir, "Deteriorated_RMSE_per_usm")
}

comparison_ds_path <- function(data_dir) {
  file.path(data_dir, "comparison")
}

metadata_ds_path <- function(data_dir) {
  file.path(data_dir, "metadata.parquet")
}
