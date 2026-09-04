EvalDataReader <- R6::R6Class("EvalDataReader", # nolint: object_name_linter
  private = list(
    .data_dir = NULL,

    open_ds = function(path, warn_msg) {
      if (!file.exists(path) && !dir.exists(path)) {
        if (warn_msg != "") {
          logger::log_warn(warn_msg)
        }
        return(NULL)
      }
      con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
      dplyr::tbl(con, dplyr::sql(sprintf(
        "SELECT * FROM read_csv('%s/**/*.csv', hive_partitioning = true, union_by_name = true)", # nolint: line_length_linter
        escape_sql_literal(normalizePath(path, mustWork = FALSE))
      )))
    },

    apply_filters = function(
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
    },

    post_process = function(ds, collect) {
      if (is.null(ds)) return(NULL)
      if (!collect) return(ds)
      con <- dbplyr::remote_con(ds)
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
      dplyr::collect(ds)
    }
  ),

  public = list(
    initialize = function(data_dir) {
      private$.data_dir <- data_dir
    },

    read = function(
      path,
      version = NULL,
      species = NULL,
      usms = NULL,
      var2exclude = NULL,
      collect = FALSE,
      warn_msg = ""
    ) {
      logger::log_debug("Reading dataset from", path, "...")
      ds <- private$open_ds(path, warn_msg)
      logger::log_debug("Dataset opened, applying filters...")
      if (is.null(ds)) return(NULL)
      logger::log_debug("Applying species, USMs and variable filters...")
      ds <- private$apply_filters(ds, version, species, usms, var2exclude)

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
    .data_dir = NULL
  ),

  public = list(
    initialize = function(data_dir) {
      private$.data_dir <- data_dir
    },

    write_dataset = function(data, path, partitioning) {
      con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

      duckdb::duckdb_register(con, "eval_write_view", data)
      on.exit(duckdb::duckdb_unregister(con, "eval_write_view"), add = TRUE)

      DBI::dbExecute(con, sprintf(
        "COPY eval_write_view TO '%s' (FORMAT CSV, PARTITION_BY (%s), OVERWRITE_OR_IGNORE true, HEADER true)", # nolint: line_length_linter
        escape_sql_literal(path), toString(partitioning)
      ))

      invisible(NULL)
    }
  )
)

# Escapes single quotes in a path so it can be safely embedded as a SQL
# string literal in a DuckDB query (paths are internal, not user-controlled,
# but this keeps the query construction defensively correct).
escape_sql_literal <- function(path) {
  gsub("'", "''", path, fixed = TRUE)
}

#' EvalWorkspace class
#'
#' Internal class. Manages the evaluation workspace (a CSV-backed,
#' DuckDB-queried staging area for simulation/observation/reference data),
#' used internally by \code{\link{evaluate}}.
#'
#' @keywords internal
EvalWorkspace <- R6::R6Class("EvalWorkspace", # nolint: object_name_linter
  private = list(
    .data_dir = NULL,
    .reader = NULL,
    .writer = NULL
  ),
  public = list(
    #' @description Create an evaluation workspace manager
    #'
    #' @param data_dir Path to the evaluation workspace
    #' @param version Optional, the version of STICS to use for the data
    #' extraction. If not defined the last evaluated version will be used.
    initialize = function(data_dir) {
      private$.data_dir <- data_dir

      private$.reader <- EvalDataReader$new(data_dir)
      private$.writer <- EvalDataWriter$new(data_dir)

    },

    #' @description
    #' Save simulations
    #' @param sim the list of simulations
    #' @param usms_species a dataframe which associates a USM to its species
    save_sim = function(sim, usms_species) {
      sim_data <- CroPlotR::bind_rows(sim) |>
        dplyr::inner_join(usms_species, by = "situation") |>
        dplyr::mutate(version = "evaluated")

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

      private$.writer$write_dataset(
        ref_sim_data,
        sim_ds_path(private$.data_dir),
        partitioning = c("version", "species")
      )
    },
    #' @description
    #' Returns the list of species
    #' @returns a list of species as character list
    get_species = function() {
      res <- private$.reader$read(
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

    #' @returns a list of USMs as a character list
    get_species_situations = function(species, usms = NULL) {
      res <- private$.reader$read(
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
    #'  otherwise the lazy dataset object (a dbplyr tbl) will be returned
    #'
    #' @returns a dataframe if collect is `TRUE`, otherwise a lazy dataset
    #' object (a dbplyr tbl)
    get_sim = function(
      species = NULL, usms = NULL, var2exclude = NULL, collect = TRUE
    ) {
      private$.reader$read(
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
    #'  otherwise the lazy dataset object (a dbplyr tbl) will be returned
    #'
    #' @returns a dataframe if collect is `TRUE`, otherwise a lazy dataset
    #' object (a dbplyr tbl)
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
    #' Return the reference simulation
    #'
    #' @param species Optional, if defined filter the reference simulations with
    #'  this species
    #' @param usms Optional, if defined filter the reference simulations with
    #'  these USMs
    #' @param var2exclude Optional, if defined remove the variables from
    #'  the returned reference simulations
    #' @param collect Optional, if `TRUE` a dataframe will be returned,
    #'  otherwise the lazy dataset object (a dbplyr tbl) will be returned
    get_ref_sim = function(
      species = NULL, usms = NULL, var2exclude = NULL, collect = TRUE
    ) {
      private$.reader$read(
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
    remove_init_obs = function() {
      sim <- private$.reader$read(
        path = sim_ds_path(private$.data_dir),
        collect = TRUE
      )

      obs <- private$.reader$read(
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
        dplyr::summarise(init_date = min(.data$Date, na.rm = TRUE))

      include_cols <- intersect(
        c(
          "HR_1", "HR_2", "HR_3", "HR_4", "HR_5",
          "AZnit_1", "AZnit_2", "AZnit_3", "AZnit_4", "AZnit_5",
          "resmes", "azomes"
        ),
        names(obs)
      )

      updated_obs <- obs |>
        dplyr::left_join(init_dates, by = "situation") |>
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(include_cols),
            ~ dplyr::if_else(.data$Date == .data$init_date, NA_real_, .x)
          )
        ) |>
        dplyr::select(-"init_date")

      private$.writer$write_dataset(
        updated_obs,
        obs_ds_path(private$.data_dir),
        partitioning = "species"
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

species_usm_ds_path <- function(data_dir) {
  file.path(data_dir, "species_usm")
}
