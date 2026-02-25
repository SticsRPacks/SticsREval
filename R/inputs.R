load_workspace_sim <- function(
  data_dir,
  usms_species,
  rotations,
  workspace,
  run_simulations,
  stics_exe,
  parallel,
  cores
) {
  usms <- unique(usms_species$usm)
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
  save_sim(data_dir, sim, usms_species)
  rm(sim)
  gc()
}

load_workspace_obs <- function(
  data_dir,
  usms_species,
  workspace,
  parallel,
  cores
) {
  logger::log_info("Loading observations data...")
  obs <- SticsRFiles::get_obs(
    workspace = workspace,
    usm = unique(usms_species$usm),
    verbose = is_debug(),
    parallel = parallel,
    cores = cores
  )
  save_obs(data_dir, obs, usms_species)
  rm(obs)
  gc()
}

extract_species_from_usms <- function(usms, workspace, parallel, cores) {
  logger::log_debug("Extracting species from USMs...")
  result <- parallelizable_loop(
    length(usms),
    parallel,
    cores,
    function(i) {
      usm <- usms[i]
      species <- SticsRFiles::get_plant_txt(
        workspace = file.path(workspace, usm)
      )
      list(
        species = species$codeplante,
        usm = usm
      )
    }
  )
  sorted <- dplyr::bind_rows(result)
  logger::log_debug("Found ", length(unique(sorted$species)), " species")
  sorted
}

#' @importFrom rlang .data
get_rotation_list <- function(metadata_file) {
  rotations_data <- read_csv(metadata_file, delimiter = ";")
  rotations <- rotations_data |>
    dplyr::filter(.data$rotation != 0) |>
    dplyr::arrange(.data$rotation, .data$rotation_order) |>
    dplyr::group_by(.data$rotation) |>
    dplyr::summarise(usm_vec = list(.data$usm)) |>
    dplyr::pull(.data$usm_vec)
  logger::log_debug("Found ", length(rotations), " rotations")
  rotations
}