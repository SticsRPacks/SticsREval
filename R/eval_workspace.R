init_eval_workspace <- function(
  data_workspace,
  eval_workspace,
  metadata_file,
  stics_exe,
  must_run_simulations,
  parallel,
  cores
) {
  logger::log_info("Initializing workspace {eval_workspace} for evaluation...")
  if (!dir.exists(eval_workspace) &&
      !dir.create(eval_workspace)
  ) {
    stop("Can't create evaluation workspace", call. = FALSE)
  }
  all_usms <- list.dirs(data_workspace, full.names = FALSE, recursive = FALSE)
  extracted_species_df <- extract_species_from_usms(
    all_usms,
    data_workspace,
    parallel,
    cores
  )
  rotations <- get_rotation_list(metadata_file)
  stics_version <- load_stics_version(eval_workspace, stics_exe)
  load_workspace_sim(
    stics_version,
    eval_workspace,
    extracted_species_df,
    rotations,
    data_workspace,
    must_run_simulations,
    stics_exe,
    parallel,
    cores
  )
  load_workspace_obs(
    stics_version,
    eval_workspace,
    extracted_species_df,
    data_workspace,
    parallel,
    cores
  )
  remove_init_obs(eval_workspace, stics_version)
  rm(extracted_species_df)
}

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

save_sim <- function(data_dir, sim, stics_version, usms_species) {
  CroPlotR::bind_rows(sim) |>
    dplyr::inner_join(usms_species, by = c(situation = "usm")) |>
    dplyr::mutate(version = stics_version) |>
    arrow::write_dataset(
      path = sim_ds_path(data_dir),
      format = "parquet",
      partitioning = c("version", "species")
    )
}

save_obs <- function(data_dir, obs, stics_version, usms_species) {
  CroPlotR::bind_rows(obs, .id = "situation") |>
    dplyr::inner_join(usms_species, by = c(situation = "usm")) |>
    dplyr::mutate(version = stics_version) |>
    arrow::write_dataset(
      path = obs_ds_path(data_dir),
      format = "parquet",
      partitioning = c("version", "species")
    )
}

get_sim_ds <- function(data_dir, stics_version) {
  ds_path <- sim_ds_path(data_dir)
  if (!dir.exists(ds_path)) {
    stop(
      "The simulation dataset at ", ds_path, " does not exist. ",
      "Please make sure the evaluation workspace has been properly ",
      "initialized.",
      call. = FALSE
    )
  }
  arrow::open_dataset(ds_path) |>
    dplyr::filter(.data$version == stics_version)
}

get_obs_ds <- function(data_dir, stics_version) {
  ds_path <- obs_ds_path(data_dir)
  if (!dir.exists(ds_path)) {
    stop(
      "The observation dataset at ", ds_path, " does not exist. ",
      "Please make sure the evaluation workspace has been properly ",
      "initialized.",
      call. = FALSE
    )
  }
  arrow::open_dataset(ds_path) |>
    dplyr::filter(.data$version == stics_version)
}

open_parquet_or_null <- function(path, collect, warn_msg) {
  if (!isTRUE(file.exists(path))) {
    logger::log_warn(warn_msg)
    return(NULL)
  }
  ds <- arrow::open_dataset(path)
  if (collect) {
    return(dplyr::collect(ds))
  }
  ds
}

remove_init_obs <- function(data_dir, stics_version) {

  init_dates <- get_sim_ds(data_dir, stics_version) |>
    dplyr::group_by(.data$situation) |>
    dplyr::summarise(init_date = min(.data$Date, na.rm = TRUE))

  exclude_cols <- c("situation", "species", "version", "Date", "init_date")

  get_obs_ds(data_dir, stics_version) |>
    dplyr::left_join(init_dates, by = "situation") |>
    dplyr::mutate(
      dplyr::across(
        -dplyr::all_of(exclude_cols),
        ~ dplyr::if_else(.data$Date == .data$init_date, NA_real_, .x)
      )
    ) |>
    dplyr::select(-"init_date") |>
    arrow::write_dataset(
      obs_ds_path(data_dir),
      format = "parquet",
      partitioning = c("version", "species")
    )
}

get_species <- function(data_dir, version) {
  get_obs_ds(data_dir, version) |>
    dplyr::distinct(.data$species) |>
    dplyr::arrange(tolower(.data$species)) |>
    dplyr::collect() |>
    dplyr::pull("species")
}

get_species_usm <- function(data_dir, version, species, usms = NULL) {
  res <- get_obs_ds(data_dir, version) |>
    dplyr::filter(.data$species == {{ species }}) |>
    dplyr::distinct(.data$situation)
  if (!is.null(usms)) {
    res <- dplyr::filter(res, .data$situation %in% usms)
  }
  res |>
    dplyr::collect() |>
    dplyr::pull("situation")
}

get_by_species <- function(
  data_dir, version, species = NULL, type = c("sim", "obs"), collect = FALSE,
  usms = NULL, var2exclude = NULL
) {
  type <- match.arg(type)
  res <- if (type == "sim") {
    get_sim_ds(data_dir, version)
  } else {
    get_obs_ds(data_dir, version)
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

#' Get simulated data from the evaluation repository
#'
#' @param data_dir Path to the evaluation repository
#' @param version Version of STICS
#' @param species Optional character vector of species to filter by
#' @param usms Optional character vector of USM names to filter by
#' @param var2exclude Optional character vector of variables to exclude
#'  from simulation results
#' @return A data.frame or Arrow dataset with simulated data
#'
#' @export
get_sim <- function(
  data_dir, version, species = NULL, usms = NULL, var2exclude = NULL
) {
  get_by_species(
    data_dir = data_dir,
    version = version,
    species = species,
    type = "sim",
    usms = usms,
    collect = TRUE,
    var2exclude = var2exclude
  )
}

#' Get observed data from the evaluation repository
#'
#' @param data_dir Path to the evaluation repository
#' @param version Version of STICS
#' @param species Optional character vector of species to filter by
#' @param usms Optional character vector of USM names to filter by
#' @param var2exclude Optional character vector of variables to exclude
#'  from observation
#' @return A data.frame or Arrow dataset with observed data
#'
#' @export
get_obs <- function(
  data_dir, version, species = NULL, usms = NULL, var2exclude = NULL
) {
  get_by_species(
    data_dir = data_dir,
    version = version,
    species = species,
    type = "obs",
    usms = usms,
    collect = TRUE,
    var2exclude = var2exclude
  )
}

save_stats <- function(data_dir, version, species, stats) {
  stats <- dplyr::mutate(stats, species = species, version = version)
  arrow::write_dataset(
    stats,
    stats_ds_path(data_dir),
    format = "parquet",
    partitioning = c("version", "species"),
    existing_data_behavior = "delete_matching"
  )
}

#' Get statistics from the evaluation repository
#'
#' @param data_dir Path to the evaluation repository
#' @param version Version of STICS
#' @param species Character string specifying the species to retrieve
#' statistics for
#' @param collect If TRUE, returns a data.frame. If FALSE, returns an
#' Arrow dataset
#' @return A data.frame or Arrow dataset with statistics for the given
#' species, or NULL if no stats file is found
#'
#' @export
get_stats <- function(data_dir, version, species, collect = FALSE) {
  ds <- open_parquet_or_null(
    path = stats_ds_path(data_dir),
    collect = collect,
    warn_msg = paste(
      "No stats file found for species", species, "in", data_dir
    )
  )
  if (is.null(ds)) {
    return(NULL)
  }
  res <- dplyr::filter(
    ds,
    .data$version == {{ version }},
    .data$species == {{ species }}
  )

  if (collect) {
    return(dplyr::collect(res))
  }
  res
}

save_rmse_per_usm <- function(data_dir, version, species, rmse_per_usm) {
  rmse_per_usm <- dplyr::mutate(
    rmse_per_usm,
    species = species,
    version = version
  )
  arrow::write_dataset(
    rmse_per_usm,
    rmse_per_usm_ds_path(data_dir),
    format = "parquet",
    partitioning = c("version", "species"),
    existing_data_behavior = "delete_matching"
  )
}

get_rmse_per_usm <- function(
  data_dir, version, species, collect = FALSE, usms = NULL, var2exclude = NULL
) {
  res <- open_parquet_or_null(
    path = rmse_per_usm_ds_path(data_dir),
    collect = collect,
    warn_msg = paste(
      "No RMSE per USM file found for species", species, "in", data_dir
    )
  )
  if (is.null(res)) {
    return(res)
  }
  res <- dplyr::filter(
    res,
    .data$version == {{ version }},
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
}

save_deteriorated_usm <- function(data_dir, version, deteriorated) {
  d <- dplyr::mutate(deteriorated, version = version)
  arrow::write_dataset(
    d,
    deteriorated_ds_path(data_dir),
    format = "parquet",
    partitioning = c("version", "species"),
    existing_data_behavior = "delete_matching"
  )
}

get_deteriorated_usm <- function(data_dir, version, species, collect = FALSE) {
  ds <- open_parquet_or_null(
    path = deteriorated_ds_path(data_dir),
    collect = collect,
    warn_msg = paste(
      "No deteriorated USM file found for species",
      species, "in", data_dir
    )
  )
  if (is.null(ds)) {
    return(NULL)
  }
  res <- dplyr::filter(
    ds,
    .data$version == {{ version }},
    .data$species == {{ species }}
  )

  if (collect) {
    return(dplyr::collect(res))
  }
  res
}

save_species_comparison <- function(data_dir, version, spec_comparison) {
  d <- dplyr::mutate(spec_comparison, version = version)
  arrow::write_dataset(
    d,
    comparison_ds_path(data_dir),
    format = "parquet",
    partitioning = c("version", "species"),
    existing_data_behavior = "delete_matching"
  )
}

get_species_comparison <- function(
  data_dir, version, species, collect = FALSE
) {
  ds <- open_parquet_or_null(
    path = comparison_ds_path(data_dir),
    collect = collect,
    warn_msg = paste(
      "No comparison file found for species",
      species, "in", data_dir
    )
  )
  if (is.null(ds)) {
    return(NULL)
  }
  res <- dplyr::filter(
    ds,
    .data$version == {{ version }},
    .data$species == {{ species }}
  )

  if (collect) {
    return(dplyr::collect(res))
  }
  res
}

save_metadata <- function(data_dir, metadata) {
  arrow::write_parquet(
    metadata,
    sink = metadata_ds_path(data_dir)
  )
}

add_evaluated_version <- function(data_dir, version) {
  new_line <- data.frame(
    stics_version = version,
    last_evaluated = TRUE
  )

  if (file.exists(metadata_ds_path(data_dir))) {
    metadata <- open_parquet_or_null(
      path = metadata_ds_path(data_dir),
      collect = TRUE,
      warn_msg = paste("No metadata file found in", data_dir)
    )

    metadata <- dplyr::mutate(metadata, last_evaluated = FALSE)

    if (version %in% metadata$stics_version) {
      metadata <- dplyr::mutate(
        metadata,
        last_evaluated = dplyr::if_else(
          .data$stics_version == version, TRUE, .data$last_evaluated
        )
      )
    } else {
      metadata <- rbind(metadata, new_line)
    }
  } else {
    metadata <- new_line
  }

  arrow::write_parquet(metadata, sink = metadata_ds_path(data_dir))
}

get_all_versions <- function(data_dir) {
  versions <- open_parquet_or_null(
    path = metadata_ds_path(data_dir),
    collect = TRUE,
    warn_msg = paste("No metadata file found in", data_dir)
  )
  if (is.null(versions)) return(NULL)
  versions$stics_version
}

#' Get the STICS version from the metadata dataset
#'
#' @param data_dir Path to the evaluation repository
#' @return A character string with the STICS version, or NULL if no metadata
#' file is found
#'
#' @export
get_stics_version <- function(data_dir) {
  ds <- open_parquet_or_null(
    path = metadata_ds_path(data_dir),
    collect = TRUE,
    warn_msg = paste("No metadata file found in", data_dir)
  )
  if (is.null(ds)) {
    return(NULL)
  }
  res <- dplyr::filter(ds, .data$last_evaluated)
  res$stics_version
}
