DEFAULT_WORKSPACE <- file.path( # nolint: object_name_linter.
  path.expand("~"), ".eval_workspace"
)

init_eval_workspace <- function(
  data_workspace,
  eval_workspace,
  metadata_file,
  stics_exe,
  must_run_simulations,
  parallel,
  cores,
  force = FALSE
) {
  logger::log_info("Initializing workspace {eval_workspace}...")
  if (!dir.exists(eval_workspace) &&
      !dir.create(eval_workspace)
  ) {
    stop("Can't create evaluation workspace", call. = FALSE)
  } else if (dir.exists(eval_workspace)) {
    files <- list.files(eval_workspace, full.names = TRUE)
    if (length(files) > 0 && !force) {
      stop(
        "Workspace ", eval_workspace, " already exists and is not empty. ",
        "Use force = TRUE to overwrite.",
        call. = FALSE
      )
    }
    unlink(files, recursive = TRUE, force = TRUE)
  }
  all_usms <- list.dirs(data_workspace, full.names = FALSE, recursive = FALSE)
  extracted_species_df <- extract_species_from_usms(
    all_usms,
    data_workspace,
    parallel,
    cores
  )
  rotations <- get_rotation_list(metadata_file)
  load_workspace_sim(
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
    eval_workspace,
    extracted_species_df,
    data_workspace,
    parallel,
    cores
  )
  remove_init_obs(eval_workspace)
  rm(extracted_species_df)
}

sim_ds_path <- function(data_dir) {
  file.path(data_dir, "sim")
}

obs_ds_path <- function(data_dir) {
  file.path(data_dir, "obs")
}

stats_ds_path <- function(data_dir, species) {
  file.path(data_dir, species, "Criteres_stats.parquet")
}

rmse_per_usm_ds_path <- function(data_dir, species) {
  file.path(data_dir, species, "RMSE_per_USM.parquet")
}

deteriorated_ds_path <- function(data_dir, species) {
  file.path(data_dir, species, "Deteriorated_RMSE_per_usm.parquet")
}

comparison_ds_path <- function(data_dir, species) {
  file.path(data_dir, species, "Comparison.parquet")
}

save_sim <- function(data_dir, sim, usms_species) {
  CroPlotR::bind_rows(sim) |>
    dplyr::inner_join(usms_species, by = c(situation = "usm")) |>
    arrow::write_dataset(
      path = sim_ds_path(data_dir),
      format = "parquet",
      partitioning = "species"
    )
}

save_obs <- function(data_dir, obs, usms_species) {
  CroPlotR::bind_rows(obs, .id = "situation") |>
    dplyr::inner_join(usms_species, by = c(situation = "usm")) |>
    arrow::write_dataset(
      path = obs_ds_path(data_dir),
      format = "parquet",
      partitioning = "species"
    )
}

get_sim_ds <- function(data_dir) {
  ds_path <- sim_ds_path(data_dir)
  if (!dir.exists(ds_path)) {
    stop(
      "The simulation dataset at ", ds_path, " does not exist. ",
      "Please make sure the evaluation workspace has been properly ",
      "initialized.",
      call. = FALSE
    )
  }
  arrow::open_dataset(ds_path)
}

get_obs_ds <- function(data_dir) {
  ds_path <- obs_ds_path(data_dir)
  if (!dir.exists(ds_path)) {
    stop(
      "The observation dataset at ", ds_path, " does not exist. ",
      "Please make sure the evaluation workspace has been properly ",
      "initialized.",
      call. = FALSE
    )
  }
  arrow::open_dataset(ds_path)
}

open_parquet_or_null <- function(path, collect, warn_msg) {
  if (!file.exists(path)) {
    logger::log_warn(warn_msg)
    return(NULL)
  }
  ds <- arrow::open_dataset(path)
  if (collect) {
    return(dplyr::collect(ds))
  }
  ds
}

remove_init_obs <- function(data_dir) {

  init_dates <- get_sim_ds(data_dir) |>
    dplyr::group_by(situation) |>
    dplyr::summarise(init_date = min(Date, na.rm = TRUE))

  exclude_cols <- c("situation", "species", "Date", "init_date")

  transformed <- get_obs_ds(data_dir) |>
    dplyr::left_join(init_dates, by = "situation") |>
    dplyr::mutate(
      dplyr::across(
        -dplyr::all_of(exclude_cols),
        ~ dplyr::if_else(Date == init_date, NA_real_, .x)
      )
    ) |>
    dplyr::select(-init_date) |>
    arrow::write_dataset(
      obs_ds_path(data_dir),
      format = "parquet",
      partitioning = "species"
    )
}

get_species <- function(data_dir) {
  get_obs_ds(data_dir) |>
    dplyr::distinct(.data$species) |>
    dplyr::arrange(tolower(.data$species)) |>
    dplyr::collect() |>
    dplyr::pull("species")
}

get_species_usm <- function(data_dir, species) {
  get_obs_ds(data_dir) |>
    dplyr::filter(.data$species == {{ species }}) |>
    dplyr::distinct(.data$situation) |>
    dplyr::collect() |>
    dplyr::pull("situation")
}

get_by_species <- function(
  data_dir, species, type = c("sim", "obs"), collect = FALSE
) {
  type <- match.arg(type)
  ds <- if (type == "sim") get_sim_ds(data_dir) else get_obs_ds(data_dir)

  res <- dplyr::filter(ds, .data$species == {{ species }})


  if (collect) {
    return(dplyr::collect(res))
  }
  res
}

save_stats <- function(data_dir, species, stats) {
  arrow::write_parquet(
    stats,
    stats_ds_path(data_dir, species)
  )
}

get_stats <- function(data_dir, species, collect = FALSE) {
  open_parquet_or_null(
    path = stats_ds_path(data_dir, species),
    collect = collect,
    warn_msg = paste(
      "No stats file dound for species", species, "in", data_dir
    )
  )
}

save_rmse_per_usm <- function(data_dir, species, rmse_per_usm) {
  arrow::write_parquet(
    rmse_per_usm,
    rmse_per_usm_ds_path(data_dir, species)
  )
}

get_rmse_per_usm <- function(data_dir, species, collect = FALSE) {
  open_parquet_or_null(
    path = rmse_per_usm_ds_path(data_dir, species),
    collect = collect,
    warn_msg = paste(
      "No RMSE per USM file found for species", species, "in", data_dir
    )
  )
}

save_deteriorated_usm <- function(data_dir, species, deteriorated) {
  arrow::write_parquet(
    deteriorated,
    deteriorated_ds_path(data_dir, species)
  )
}

get_deteriorated_usm <- function(data_dir, species, collect = FALSE) {
  open_parquet_or_null(
    path = deteriorated_ds_path(data_dir, species),
    collect = collect,
    warn_msg = paste(
      "No deteriorated USM file found for species",
      species, "in", data_dir
    )
  )
}

save_species_comparison <- function(data_dir, species, spec_comparison) {
  arrow::write_parquet(
    spec_comparison,
    comparison_ds_path(data_dir, species)
  )
}

get_species_comparison <- function(data_dir, species, collect = FALSE) {
  open_parquet_or_null(
    path = comparison_ds_path(data_dir, species),
    collect = collect,
    warn_msg = paste(
      "No comparison file found for species",
      species, "in", data_dir
    )
  )
}
