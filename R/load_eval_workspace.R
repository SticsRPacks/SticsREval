# Loads the USMs of `usms_workspace` (all of them, or only `usms`) into the
# evaluation `workspace` (an EvalWorkspace): their species, then the
# observations, simulations and, if `ref_sim_rds` is given, reference
# simulations read from the RDS files. The initial observations are then
# removed (see `EvalWorkspace$remove_init_obs()`). `backend` (a
# ParallelBackend) is used to read the species of the USMs in parallel.
load_eval_workspace <- function(
  usms_workspace,
  sim_rds,
  obs_rds,
  workspace,
  backend,
  ref_sim_rds = NULL,
  usms = NULL
) {
  all_usms <- list.dirs(usms_workspace, full.names = FALSE, recursive = FALSE)
  if (!is.null(usms)) {
    all_usms <- select_available_usms(all_usms, usms, usms_workspace)
  }

  usms_species <- extract_species_from_usms(usms_workspace, all_usms, backend)
  workspace$save_species_usm(usms_species)
  species_situations <- workspace$get_species_situations(species = NULL)

  load_rds_into_workspace(
    obs_rds, "observations", species_situations, workspace$save_obs
  )
  load_rds_into_workspace(
    sim_rds, "simulations", species_situations, workspace$save_sim
  )
  if (!is.null(ref_sim_rds)) {
    load_rds_into_workspace(
      ref_sim_rds, "reference simulations", species_situations,
      workspace$save_ref_sim
    )
  }

  workspace$remove_init_obs()
  invisible(workspace)
}


extract_species_from_usms <- function(usms_workspace, usms, backend) {
  logger::log_debug("Extracting species from USMs...")
  result <- backend$run(
    length(usms),
    function(i) {
      usm <- usms[i]
      species <- SticsRFiles::get_plant_txt(
        workspace = file.path(usms_workspace, usm)
      )
      list(species = species$codeplante, situation = usm)
    }
  )
  usms_species <- dplyr::bind_rows(result)
  logger::log_debug(
    "Found ", length(unique(usms_species$species)), " species"
  )
  usms_species
}


# Reads the `rds` file (a list of data frames named by USM), keeps the USMs
# of `species_situations` and saves them with `save` (one of the
# EvalWorkspace `save_*()` methods).
load_rds_into_workspace <- function(rds, label, species_situations, save) {
  logger::log_info("Loading ", label, " data...")
  per_usm <- readRDS(rds)[unique(species_situations$situation)]
  save(per_usm, species_situations)
  rm(per_usm)
  gc()
  invisible(NULL)
}
