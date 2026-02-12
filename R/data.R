init_tmp_data_dir <- function(prefix = "eval_") {
  .local_eval_env$data_dir <- tempfile(prefix)
  dir.create(.local_eval_env$data_dir)
  .local_eval_env$data_dir
}

clean_tmp_data_dir <- function() {
  unlink(.local_eval_env$data_dir, recursive = TRUE)
}

get_sim_ds <- function(data_dir) {
  arrow::open_dataset(file.path(data_dir, "sim.parquet"))
}

get_obs_ds <- function(data_dir) {
  arrow::open_dataset(file.path(data_dir, "obs.parquet"))
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

get_all_sim_situations <- function(data_dir) {
  (get_sim_ds(data_dir) %>%
      dplyr::distinct(.data$situation) %>%
      dplyr::collect()
  )$situation
}

get_all_obs_situations <- function(data_dir) {
  (get_obs_ds(data_dir) %>%
      dplyr::distinct(.data$situation) %>%
      dplyr::collect()
  )$situation
}

get_sim_by_situations <- function(data_dir, situation_list) {
  get_sim_ds(data_dir) %>%
    dplyr::filter(.data$situation %in% situation_list)
}

get_obs_by_situations <- function(data_dir, situation_list) {
  get_obs_ds(data_dir) %>%
    dplyr::filter(.data$situation %in% situation_list)
}

get_count <- function(situations) {
  situations %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::collect() %>%
    dplyr::pull(n)
}