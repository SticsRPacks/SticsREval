EvalDataReader <- R6::R6Class("EvalDataReader", # nolint: object_name_linter
  private = list(
    .data_dir = NULL,
    .version = NULL,

    open_ds = function(path, warn_msg) {
      if (!file.exists(path) && !dir.exists(path)) {
        if (warn_msg != "") {
          logger::log_warn(warn_msg)
        }
        return(NULL)
      }
      arrow::open_dataset(path)
    },

    apply_version = function(ds) {
      if (is.null(private$.version)) return(ds)
      dplyr::filter(ds, .data$version == private$.version)
    },

    apply_filters = function(
      ds, species = NULL, usms = NULL, var2exclude = NULL
    ) {

      if (!is.null(species)) {
        ds <- dplyr::filter(ds, .data$species %in% !!species)
      }

      if (!is.null(usms)) {
        ds <- dplyr::filter(ds, .data$situation %in% !!usms)
      }

      if (!is.null(var2exclude)) {
        ds <- dplyr::select(ds, -dplyr::any_of(var2exclude))
      }

      ds
    },

    apply_version_if_needed = function(ds, apply_version) {
      if (!apply_version) return(ds)
      private$apply_version(ds)
    },

    post_process = function(ds, collect) {
      if (is.null(ds)) return(NULL)
      if (collect) dplyr::collect(ds) else ds # nolint: coalesce_linter
    }
  ),

  public = list(
    initialize = function(data_dir, version = NULL) {
      private$.data_dir <- data_dir
      private$.version <- version
    },

    set_version = function(version) {
      private$.version <- version
    },

    read = function(
      path,
      species = NULL,
      usms = NULL,
      var2exclude = NULL,
      collect = FALSE,
      warn_msg = "",
      apply_version = TRUE
    ) {
      logger::log_debug("Reading dataset from", path, "...")
      ds <- private$open_ds(path, warn_msg)
      logger::log_debug("Dataset opened, applying filters...")
      if (is.null(ds)) return(NULL)
      logger::log_debug("Applying version filter...")
      ds <- private$apply_version_if_needed(ds, apply_version)
      logger::log_debug("Applying species, USMs and variable filters...")
      ds <- private$apply_filters(ds, species, usms, var2exclude)

      if (!is.null(species)) {
        n <- ds |>
          dplyr::summarise(n = dplyr::n()) |>
          dplyr::collect() |>
          dplyr::pull(n)
        if (n == 0) {
          if (warn_msg != "") {
            logger::log_warn(warn_msg)
          }
          return(NULL)
        }
      }

      logger::log_debug("Filters applied, post processing dataset...")
      ds <- private$post_process(ds, collect)
      logger::log_debug("Dataset ready.")
      ds
    }
  )
)

EvalDataWriter <- R6::R6Class("EvalDataWriter", # nolint: object_name_linter
  private = list(
    .data_dir = NULL,
    .version = NULL
  ),

  public = list(
    initialize = function(data_dir, version = NULL) {
      private$.data_dir <- data_dir
      private$.version <- version
    },

    set_version = function(version) {
      private$.version <- version
    },

    write_dataset = function(data, path, partitioning) {

      if (!is.null(private$.version)) {
        data <- dplyr::mutate(data, version = private$.version)
      }

      arrow::write_dataset(
        data,
        path = path,
        format = "parquet",
        partitioning = partitioning,
        existing_data_behavior = "delete_matching"
      )
    }
  )
)

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
EvalWorkspace <- R6::R6Class("EvalWorkspace", # nolint: object_name_linter
  private = list(
    .data_dir = NULL,
    .version  = NULL,
    .reader = NULL,
    .writer = NULL
  ),
  public = list(
    #' @description Create an evaluation workspace manager
    #'
    #' @param data_dir Path to the evaluation workspace
    #' @param version Optional, the version of STICS to use for the data
    #' extraction. If not defined the last evaluated version will be used.
    initialize = function(data_dir, version = NULL) {
      private$.data_dir <- data_dir

      private$.reader <- EvalDataReader$new(data_dir)
      if (is.null(version)) {
        version <- self$get_stics_version()
      }

      private$.version <- version
      private$.reader$set_version(version)
      private$.writer <- EvalDataWriter$new(data_dir, version)

    },

    #' @description
    #' Save simulations
    #' @param sim the list of simulations
    #' @param usms_species a dataframe which associates a USM to its species
    save_sim = function(sim, usms_species) {
      sim_data <- CroPlotR::bind_rows(sim) |>
        dplyr::inner_join(usms_species, by = "situation")

      private$.writer$write_dataset(
        sim_data,
        sim_ds_path(private$.data_dir),
        partitioning = c("version", "species")
      )
    },
    #' @description
    #' Save observations
    #' @param obs the list of observations
    #' @param usms_species a dataframe which associates a USM to its species
    save_obs = function(obs, usms_species) {
      obs_data <- CroPlotR::bind_rows(obs, .id = "situation") |>
        dplyr::inner_join(usms_species, by = "situation")

      private$.writer$write_dataset(
        obs_data,
        obs_ds_path(private$.data_dir),
        partitioning = c("version", "species")
      )
    },
    #' @description
    #' Returns the list of species
    #' @returns a list of species as character list
    get_species = function() {
      res <- private$.reader$read(
        path = species_usm_ds_path(private$.data_dir),
        collect = TRUE,
        apply_version = FALSE
      )
      if (is.null(res)) return(NULL)
      res |>
        dplyr::distinct(.data$species) |>
        dplyr::arrange(tolower(.data$species)) |>
        dplyr::pull("species")
    },

    #' @description
    #' Returns the list of USMs of a species
    #' @param species the species to search for
    #' @param usms Optional, filter the USMs returned by the function

    #' @returns a list of USMs as a character list
    get_species_situations = function(species, usms = NULL) {
      res <- private$.reader$read(
        path = species_usm_ds_path(private$.data_dir),
        species = species,
        usms = usms,
        collect = TRUE,
        apply_version = FALSE
      )
      if (is.null(res)) return(NULL)
      res <- dplyr::select(res, species, situation)
      as.data.frame(res, stringsAsFactors = FALSE)
    },
    #' @description
    #' Save the association between species and USMs
    #' @param species_usms a dataframe with two columns: situation and species
    #' the situation column should contain the USM and the species column should
    #' contain the associated species
    save_species_usm = function(species_usms) {
      private$.writer$write_dataset(
        species_usms,
        path = species_usm_ds_path(private$.data_dir),
        partitioning = "species"
      )
    },
    #' @description
    #' Return the simulation
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
      private$.reader$read(
        path = sim_ds_path(private$.data_dir),
        species = species,
        usms = usms,
        var2exclude = var2exclude,
        collect = collect,
        warn_msg = "Simulation dataset not found"
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
      private$.reader$read(
        path = obs_ds_path(private$.data_dir),
        species = species,
        usms = usms,
        var2exclude = var2exclude,
        collect = collect,
        warn_msg = "Observation dataset not found"
      )
    },
    #' @description
    #' Save a species statistics
    #'
    #' @param species the species
    #' @param stats the statistics as a dataframe
    save_stats = function(species, stats) {
      private$.writer$write_dataset(
        data = dplyr::mutate(stats, species = species),
        path = stats_ds_path(private$.data_dir),
        partitioning = c("version", "species")
      )
    },
    #' @description
    #' Save global statistics
    #'
    #' @param stats the global statistics as a dataframe
    save_global_stats = function(stats) {
      private$.writer$write_dataset(
        data = stats,
        path = global_stats_ds_path(private$.data_dir),
        partitioning = "version"
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
      private$.reader$read(
        path = stats_ds_path(private$.data_dir),
        species = species,
        collect = collect,
        warn_msg = paste("No stats for", species)
      )
    },
    #' @description
    #' Return global statistics
    #'
    #' @param collect Optional, if `TRUE` returns a dataframe,
    #'  otherwise a lazy arrow data object will be returned
    #' @returns the global statistics, as a dataframe if collect is `TRUE`,
    #'  a lazy arrow data object otherwise
    get_global_stats = function(collect = FALSE) {
      private$.reader$read(
        path = global_stats_ds_path(private$.data_dir),
        collect = collect,
        warn_msg = "No global stats found"
      )
    },
    #' @description
    #' Save the rRMSE per USM
    #'
    #' @param species the species
    #' @param rrmse_per_usm the rRMSE per USMs
    save_rrmse_per_usm = function(species, rrmse_per_usm) {
      private$.writer$write_dataset(
        data = dplyr::mutate(rrmse_per_usm, species = species),
        path = rrmse_per_usm_ds_path(private$.data_dir),
        partitioning = c("version", "species")
      )
    },
    #' @description
    #' Return the rRMSE per USM for a species
    #'
    #' @param species Optional, the species
    #' @param collect Optional, if `TRUE` a dataframe will be returned,
    #'  otherwise a lazy arrow data object will be returned
    #' @param usms Optional, if defined filter the rRMSE per USM with these
    #'  USMs
    #' @param var2exclude Optional, if defined remove the variables from
    #'  the returned rRMSE per USM
    #'
    #' @returns the rRMSE per USM for a species, as a dataframe if collect is
    #'  `TRUE`, a lazy arrow data object otherwise
    get_rrmse_per_usm = function(
      species = NULL, collect = FALSE, usms = NULL, var2exclude = NULL
    ) {
      private$.reader$read(
        path = rrmse_per_usm_ds_path(private$.data_dir),
        species = species,
        usms = usms,
        var2exclude = var2exclude,
        collect = collect,
        warn_msg = paste("No rRMSE for", species)
      )
    },
    #' @description
    #' Save the deteriorated USM
    #'
    #' @param deteriorated the deteriorated USM object
    save_deteriorated_usm = function(deteriorated) {
      private$.writer$write_dataset(
        data = dplyr::mutate(deteriorated$get_data()),
        path = deteriorated_ds_path(private$.data_dir),
        partitioning = c("version", "species")
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
      res <- private$.reader$read(
        path = deteriorated_ds_path(private$.data_dir),
        species = species,
        collect = TRUE,
        warn_msg = paste("No deteriorated USM for", species)
      )
      if (is.null(res)) return(NULL)
      DeterioratedUSMComparison$new(data = res, percentage = percentage)
    },
    #' @description
    #' Save the species comparison
    #'
    #' @param spec_comparison the species comparison object
    save_species_comparison = function(spec_comparison) {
      private$.writer$write_dataset(
        data = spec_comparison$get_data(),
        path = comparison_ds_path(private$.data_dir),
        partitioning = c("version", "species")
      )
    },
    #' @description
    #' Save the global comparison
    #'
    #' @param spec_comparison the global comparison object
    save_global_comparison = function(spec_comparison) {
      private$.writer$write_dataset(
        data = spec_comparison$get_data(),
        path = global_comparison_ds_path(private$.data_dir),
        partitioning = "version"
      )
    },
    #' @description
    #' Get the species comparison for a species
    #'
    #' @param species the species
    #' @param percentage the percentage
    #'
    #' @returns an RRmseComparison object
    get_species_comparison = function(
      species, percentage
    ) {
      res <- private$.reader$read(
        path = comparison_ds_path(private$.data_dir),
        species = species,
        collect = TRUE,
        warn_msg = paste("No comparison for", species)
      )
      if (is.null(res)) return(NULL)
      RRmseComparison$new(
        data = res,
        percentage = percentage
      )
    },
    #' @description
    #' Get the global comparison
    #'
    #' @param percentage the percentage
    #'
    #' @returns an RRmseComparison object
    get_global_comparison = function(percentage) {
      res <- private$.reader$read(
        path = global_comparison_ds_path(private$.data_dir),
        collect = TRUE,
        warn_msg = "No global comparison found"
      )
      if (is.null(res)) return(NULL)
      RRmseComparison$new(
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
        metadata <- private$.reader$read(
          path = metadata_ds_path(private$.data_dir),
          collect = TRUE,
          apply_version = FALSE,
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
      versions <- private$.reader$read(
        path = metadata_ds_path(private$.data_dir),
        collect = TRUE,
        apply_version = FALSE,
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
      ds <- private$.reader$read(
        path = metadata_ds_path(private$.data_dir),
        collect = TRUE,
        warn_msg = paste("No metadata file found in", private$.data_dir),
        apply_version = FALSE
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
      all_versions <- self$get_all_versions()
      if (is.null(all_versions) || !(version %in% all_versions)) {
        stop(
          "Version ",
          version,
          " not found in the workspace. Available versions: ",
          toString(all_versions),
          call. = FALSE
        )
      }
      EvalWorkspace$new(private$.data_dir, version)
    },

    #' @description
    #' Set the version of the evaluation workspace
    #'
    #' @param v the version to set
    set_version = function(v) {
      private$.version <- v
      private$.reader$set_version(v)
      private$.writer$set_version(v)
    },
    #' @description
    #' Get the current version of the evaluation workspace
    #'
    #' @returns the current version of the evaluation workspace
    get_version = function() {
      private$.version
    },

    #' @description
    #' Remove the initial observations (HR_1 to HR_5, AZnit_1
    #' to AZnit_5, resmes and azomes) from the observations dataset
    #' This function is used to remove the initial observations from the
    #' dataset, which are not relevant for the evaluation and can bias the
    #' results.
    #' The initial observations are defined as the observations of the first
    #' date of each situation (USM) for the variables HR_1 to HR_5
    #' and AZnit_1 to AZnit_5, resmes and azomes.
    #' This function will replace the initial observations by NA in the dataset.
    remove_init_obs = function() {
      sim <- private$.reader$read(
        path = sim_ds_path(private$.data_dir),
        collect = FALSE
      )

      obs <- private$.reader$read(
        path = obs_ds_path(private$.data_dir),
        collect = FALSE
      )
      if (is.null(sim) || is.null(obs)) {
        logger::log_warn(
          "Missing sim or obs dataset, skipping init obs removal"
        )
        return(invisible(NULL))
      }

      init_dates <- sim |>
        dplyr::group_by(.data$situation) |>
        dplyr::summarise(init_date = min(.data$Date, na.rm = TRUE))

      include_cols <- c(
        "HR_1", "HR_2", "HR_3", "HR_4", "HR_5",
        "AZnit_1", "AZnit_2", "AZnit_3", "AZnit_4", "AZnit_5",
        "resmes", "azomes"
      )

      obs |>
        dplyr::left_join(init_dates, by = "situation") |>
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(include_cols),
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
    #' @description
    #' Remove all the simulations from the evaluation workspace
    remove_all_sim = function() {
      if (dir.exists(sim_ds_path(private$.data_dir))) {
        unlink(sim_ds_path(private$.data_dir), recursive = TRUE)
      }
      invisible(NULL)
    },
    #' @description
    #' Remove all the observations from the evaluation workspace
    remove_all_obs = function() {
      if (dir.exists(obs_ds_path(private$.data_dir))) {
        unlink(obs_ds_path(private$.data_dir), recursive = TRUE)
      }
      invisible(NULL)
    }
  )
)

sim_ds_path <- function(data_dir) {
  file.path(data_dir, "sim")
}

obs_ds_path <- function(data_dir) {
  file.path(data_dir, "obs")
}

global_stats_ds_path <- function(data_dir) {
  file.path(data_dir, "global_stats")
}

stats_ds_path <- function(data_dir) {
  file.path(data_dir, "stats")
}

rrmse_per_usm_ds_path <- function(data_dir) {
  file.path(data_dir, "rRMSE_per_USM")
}

deteriorated_ds_path <- function(data_dir) {
  file.path(data_dir, "Deteriorated_rRMSE_per_usm")
}

global_comparison_ds_path <- function(data_dir) {
  file.path(data_dir, "Global_Comparison")
}

comparison_ds_path <- function(data_dir) {
  file.path(data_dir, "comparison")
}

metadata_ds_path <- function(data_dir) {
  file.path(data_dir, "metadata.parquet")
}

species_usm_ds_path <- function(data_dir) {
  file.path(data_dir, "species_usm")
}
