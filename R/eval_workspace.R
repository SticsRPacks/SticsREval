filter_eval_dataset <- function(
  ds, version = NULL, species = NULL, usms = NULL, var2exclude = NULL
) {
  if (!is.null(version)) {
    ds <- dplyr::filter(ds, .data$version == !!version)
  }

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
}

read_eval_dataset <- function(
  path,
  version = NULL,
  species = NULL,
  usms = NULL,
  var2exclude = NULL,
  collect = FALSE,
  warn_msg = ""
) {
  warn <- function() if (warn_msg != "") logger::log_warn(warn_msg)

  logger::log_debug("Reading dataset from", path, "...")
  if (!file.exists(path) && !dir.exists(path)) {
    warn()
    return(NULL)
  }
  ds <- arrow::open_dataset(path)

  logger::log_debug("Applying species, USMs and variable filters...")
  ds <- filter_eval_dataset(ds, version, species, usms, var2exclude)

  if (!is.null(species)) {
    n <- ds |>
      dplyr::summarise(n = dplyr::n()) |>
      dplyr::collect() |>
      dplyr::pull(n)
    if (n == 0) {
      warn()
      return(NULL)
    }
  }

  if (collect) dplyr::collect(ds) else ds # nolint: coalesce_linter
}

write_eval_dataset <- function(data, path, partitioning) {
  arrow::write_dataset(
    data,
    path = path,
    format = "parquet",
    partitioning = partitioning,
    existing_data_behavior = "delete_matching"
  )
}

#' EvalWorkspace class
#'
#' Internal class. Manages the evaluation workspace (a Parquet-backed
#' staging area for simulation/observation/reference data), used
#' internally by \code{\link{evaluate}}.
#'
#' @keywords internal
EvalWorkspace <- R6::R6Class("EvalWorkspace", # nolint: object_name_linter
  private = list(
    .data_dir = NULL
  ),
  public = list(
    #' @description Create an evaluation workspace manager
    #'
    #' @param data_dir Path to the evaluation workspace
    initialize = function(data_dir) {
      private$.data_dir <- data_dir
    },

    #' @description
    #' Save simulations
    #' @param sim the list of simulations
    #' @param usms_species a dataframe which associates a USM to its species
    save_sim = function(sim, usms_species) {
      sim_data <- CroPlotR::bind_rows(sim) |>
        dplyr::inner_join(usms_species, by = "situation") |>
        dplyr::mutate(version = "evaluated")

      write_eval_dataset(
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

      write_eval_dataset(
        obs_data,
        obs_ds_path(private$.data_dir),
        partitioning = "species"
      )
    },

    #' @description
    #' Save reference simulations
    #' @param ref_sim the list of reference simulations
    #' @param usms_species a dataframe which associates a USM to its species
    save_ref_sim = function(ref_sim, usms_species) {
      ref_sim_data <- CroPlotR::bind_rows(ref_sim) |>
        dplyr::inner_join(usms_species, by = "situation") |>
        dplyr::mutate(version = "reference")

      write_eval_dataset(
        ref_sim_data,
        sim_ds_path(private$.data_dir),
        partitioning = c("version", "species")
      )
    },
    #' @description
    #' Returns the list of species
    #' @returns a character vector of species sorted alphabetically (case
    #'  insensitive), or `NULL` if the species/USM dataset doesn't exist
    get_species = function() {
      res <- read_eval_dataset(
        path = species_usm_ds_path(private$.data_dir),
        collect = TRUE
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
    #' @returns a data frame with two columns, `species` and `situation`, or
    #'  `NULL` if the dataset doesn't exist or no USM matches
    get_species_situations = function(species, usms = NULL) {
      res <- read_eval_dataset(
        path = species_usm_ds_path(private$.data_dir),
        species = species,
        usms = usms,
        collect = TRUE
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
      write_eval_dataset(
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
    #' @returns a data frame if `collect` is `TRUE`, a lazy arrow data object
    #'  otherwise, or `NULL` if the dataset doesn't exist or no row matches
    get_sim = function(
      species = NULL, usms = NULL, var2exclude = NULL, collect = TRUE
    ) {
      read_eval_dataset(
        path = sim_ds_path(private$.data_dir),
        version = "evaluated",
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
    #' @returns a data frame if `collect` is `TRUE`, a lazy arrow data object
    #'  otherwise, or `NULL` if the dataset doesn't exist or no row matches
    get_obs = function(
      species = NULL, usms = NULL, var2exclude = NULL, collect = TRUE
    ) {
      read_eval_dataset(
        path = obs_ds_path(private$.data_dir),
        species = species,
        usms = usms,
        var2exclude = var2exclude,
        collect = collect,
        warn_msg = "Observation dataset not found"
      )
    },

    #' @description
    #' Return the reference simulation
    #'
    #' @param species Optional, if defined filter the reference simulations with
    #'  this species
    #' @param usms Optional, if defined filter the reference simulations with
    #'  these USMs
    #' @param var2exclude Optional, if defined remove the variables from
    #'  the returned reference simulations
    #' @param collect Optional, if `TRUE` a dataframe will be returned,
    #'  otherwise the lazy arrow data object will be returned
    #'
    #' @returns a data frame if `collect` is `TRUE`, a lazy arrow data object
    #'  otherwise, or `NULL` if the dataset doesn't exist or no row matches
    get_ref_sim = function(
      species = NULL, usms = NULL, var2exclude = NULL, collect = TRUE
    ) {
      read_eval_dataset(
        path = sim_ds_path(private$.data_dir),
        version = "reference",
        species = species,
        usms = usms,
        var2exclude = var2exclude,
        collect = collect,
        warn_msg = "Reference simulation dataset not found"
      )
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
    #' @returns `NULL`, invisibly. Called for its side effect of rewriting the
    #'  observations dataset.
    remove_init_obs = function() {
      sim <- read_eval_dataset(
        path = sim_ds_path(private$.data_dir),
        collect = FALSE
      )

      # Collected in memory: the result is written back to the same path,
      # and writing to a dataset while it is still being streamed from
      # can overwrite files before they are fully read.
      obs <- read_eval_dataset(
        path = obs_ds_path(private$.data_dir),
        collect = TRUE
      )
      if (is.null(sim) || is.null(obs)) {
        logger::log_warn(
          "Missing sim or obs dataset, skipping init obs removal"
        )
        return(invisible(NULL))
      }

      init_dates <- sim |>
        dplyr::group_by(.data$situation) |>
        dplyr::summarise(init_date = min(.data$Date, na.rm = TRUE)) |>
        dplyr::collect()

      include_cols <- intersect(
        c(
          "HR_1", "HR_2", "HR_3", "HR_4", "HR_5",
          "AZnit_1", "AZnit_2", "AZnit_3", "AZnit_4", "AZnit_5",
          "resmes", "azomes"
        ),
        names(obs)
      )

      obs <- obs |>
        dplyr::left_join(init_dates, by = "situation") |>
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(include_cols),
            ~ dplyr::if_else(.data$Date == .data$init_date, NA, .x)
          )
        ) |>
        dplyr::select(-"init_date")

      write_eval_dataset(
        obs,
        obs_ds_path(private$.data_dir),
        partitioning = "species"
      )
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

species_usm_ds_path <- function(data_dir) {
  file.path(data_dir, "species_usm")
}
