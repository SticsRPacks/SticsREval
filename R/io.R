safe_write_csv <- function(data, path) {
  tryCatch({
    readr::write_delim(
      data,
      path,
      delim = ";",
      na = "NA"
    )
  },
  error = function(e) {
    logger::log_error(
      sprintf("❌ Unable to create '%s': %s", path, e$message)
    )
    stop(sprintf("Error: unable to create %s", path), call. = FALSE)
  })
}

read_csv <- function(filepath, delimiter = ";") {
  data <- readr::read_delim(
    filepath,
    delim = delimiter,
    na = c("NA", "NaN"),
    locale = readr::locale(
      decimal_mark = ".",
      date_format = "%Y-%m-%d"
    ),
    show_col_types = is_debug()
  )
  names(data) <- trimws(names(data))
  data
}


read_ref_sim <- function(
  species,
  reference_data_dir
) {
  reference_dir <- file.path(reference_data_dir, species)
  reference_file <- file.path(reference_dir, "Simulations.csv")
  if (!length(reference_file) || !file.exists(reference_file)) {
    return(NULL)
  }
  df <- read_csv(reference_file, delimiter = ",")
  CroPlotR::split_df2sim(df)
}

load_workspace_sim <- function(
  data_dir,
  usms,
  rotations,
  workspace,
  run_simulations,
  stics_exe,
  parallel,
  cores
) {
  if (run_simulations) {
    logger::log_info("Running simulations...")
    sim <- run_simulations(
      stics_exe = stics_exe,
      workspace = workspace,
      usm_names = usms,
      successive = rotations,
      verbose = is_debug(),
      parallel = parallel,
      cores = cores
    )
  } else {
    logger::log_info("Loading simulations data...")
    sim <- SticsRFiles::get_sim(
      workspace = workspace,
      usm = usms,
      verbose = is_debug(),
      parallel = parallel,
      cores = cores
    )
  }
  save_sim(data_dir, sim)
  rm(sim)
  gc()
}

load_workspace_obs <- function(
  data_dir,
  usms,
  workspace,
  parallel,
  cores
) {
  logger::log_info("Loading observations data...")
  obs <- SticsRFiles::get_obs(
    workspace = workspace,
    usm = usms,
    verbose = is_debug(),
    parallel = parallel,
    cores = cores
  )
  save_obs(data_dir, obs)
  rm(obs)
  gc()
}

#' @importFrom dplyr %>%
#' @importFrom rlang .data
get_rotation_list <- function(rotation_file) {
  rotations_data <- read_csv(rotation_file)
  rotations <- rotations_data %>%
    dplyr::filter(.data$rotation != 0) %>%
    dplyr::arrange(.data$rotation, .data$rotation_order) %>%
    dplyr::group_by(.data$rotation) %>%
    dplyr::summarise(usm_vec = list(.data$usm)) %>%
    dplyr::pull(.data$usm_vec)
  logger::log_debug("Found ", length(rotations), " rotations")
  rotations
}
