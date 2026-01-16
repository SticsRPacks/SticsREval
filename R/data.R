init_tmp_data_dir <- function(prefix = "eval_") {
  .local_eval_env$data_dir <- tempfile(prefix)
  dir.create(.local_eval_env$data_dir)
  .local_eval_env$data_dir
}

clean_tmp_data_dir <- function() {
  unlink(.local_eval_env$data_dir, recursive = TRUE)
}

save_sim <- function(data_dir, sim) {
  arrow::write_parquet(
    x = CroPlotR::bind_rows(sim),
    sink = file.path(data_dir, "sim.parquet")
  )
}

save_obs <- function(data_dir, obs) {
  arrow::write_parquet(
    x = CroPlotR::bind_rows(obs, .id = "situation"),
    sink = file.path(data_dir, "obs.parquet")
  )
}

save_species_sim <- function(
  data_dir,
  sim,
  species
) {
  arrow::write_parquet(
    x = CroPlotR::bind_rows(sim),
    sink = file.path(
      data_dir,
      paste("sim_", species, ".parquet", collapse = "")
    )
  )
}

save_species_obs <- function(
  data_dir,
  obs,
  species
) {
  arrow::write_parquet(
    x = CroPlotR::bind_rows(obs),
    sink = file.path(
      data_dir,
      paste("obs_", species, ".parquet", collapse = "")
    )
  )
}

get_all_sim_situations <- function(data_dir) {
  res <- get_sim_ds(data_dir) %>%
    dplyr::distinct(.data$situation) %>%
    dplyr::collect()
  res$situation
}

get_all_obs_situations <- function(data_dir) {
  res <- get_obs_ds(data_dir) %>%
    dplyr::distinct(.data$situation) %>%
    dplyr::collect()
  res$situation
}

get_sim_by_situations <- function(data_dir, situations) {
  selected_sim <- get_sim_ds(data_dir) %>%
    dplyr::filter(.data$situation %in% situations) %>%
    dplyr::collect()
  selected_sim <- CroPlotR::split_df2sim(selected_sim)
}

get_obs_by_situations <- function(data_dir, situations) {
  selected_obs <- get_obs_ds(data_dir) %>%
    dplyr::filter(.data$situation %in% situations) %>%
    dplyr::collect()
  selected_obs <- CroPlotR::split_df2sim(selected_obs)
}

get_species_sim_ds <- function(data_dir, species) {
  arrow::open_dataset(file.path(
    data_dir,
    paste("sim_", species, ".parquet")
  ))
}

get_sim_ds <- function(data_dir) {
  arrow::open_dataset(file.path(data_dir, "sim.parquet"))
}

get_obs_ds <- function(data_dir) {
  arrow::open_dataset(file.path(data_dir, "obs.parquet"))
}

get_species_sim <- function(data_dir, species) {
  ds <- get_species_sim_ds(data_dir, species)
  CroPlotR::split_df2sim(ds %>% dplyr::collect())
}

get_species_obs <- function(data_dir, species) {
  ds <- arrow::open_dataset(file.path(
    data_dir,
    paste("obs_", species, ".parquet")
  ))
  CroPlotR::split_df2sim(ds %>% dplyr::collect())
}

export_species_sim_ds_to_csv <- function(data_dir, species, destination) {
  ds <- get_species_sim_ds(data_dir, species)
  arrow::write_csv_arrow(
    ds,
    file.path(destination, "Simulations.csv")
  )
}