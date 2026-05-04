WorkspaceLoader <- R6::R6Class("WorkspaceLoader",
  private = list(
    workspace = NULL,
    backend = NULL,
    usms_workspace = NULL,
    metadata_file = NULL,
    stics_exe = NULL,
    run_sims = NULL,

    extract_species_from_usms = function(usms) {
      logger::log_debug("Extracting species from USMs...")
      result <- private$backend$run(
        length(usms),
        function(i) {
          usm <- usms[i]
          species <- SticsRFiles::get_plant_txt(
            workspace = file.path(private$usms_workspace, usm)
          )
          list(species = species$codeplante, usm = usm)
        }
      )
      sorted <- dplyr::bind_rows(result)
      logger::log_debug("Found ", length(unique(sorted$species)), " species")
      sorted
    },

    get_rotation_list = function() {
      if (!file.exists(private$metadata_file)) {
        stop("Metadata file not found: ", private$metadata_file, call. = FALSE)
      }
      rotations_data <- read_csv(private$metadata_file, delimiter = ";")
      required_cols  <- c("usm", "rotation", "rotation_order")
      missing_cols <- setdiff(required_cols, names(rotations_data))

      if (length(missing_cols) > 0) {
        stop("Missing columns in metadata file: ", toString(missing_cols), call. = FALSE)
      }
      if (nrow(rotations_data) == 0) return(list())

      original_order <- rotations_data[["rotation_order"]]
      rotations_data <- rotations_data |>
        dplyr::mutate(
          rotation_order = suppressWarnings(as.numeric(.data$rotation_order)),
          rotation = as.character(.data$rotation)
        )

      if (sum(is.na(rotations_data[["rotation_order"]])) > sum(is.na(original_order))) {
        stop("Column must be numeric: rotation_order", call. = FALSE)
      }

      rotations <- rotations_data |>
        dplyr::filter(!is.na(.data$rotation), .data$rotation != "0") |>
        dplyr::arrange(.data$rotation, .data$rotation_order) |>
        dplyr::group_by(.data$rotation) |>
        dplyr::summarise(usm_vec = list(.data$usm)) |>
        dplyr::pull("usm_vec")

      logger::log_debug("Found ", length(rotations), " rotations")
      rotations
    },

    load_stics_version = function() {
      version <- as.character(SticsOnR::get_version_number(private$stics_exe))
      private$workspace$add_evaluated_version(version)
      version
    },

    run_simulations = function(usms_species, rotations) {
      wrapper_options <- SticsOnR::stics_wrapper_options(
        stics_exe = private$stics_exe,
        workspace = private$usms_workspace,
        parallel = private$backend$parallel,
        cores = private$backend$cores,
        successive = rotations,
        verbose = is_debug(),
        time_display = is_debug()
      )
      res <- SticsOnR::stics_wrapper(wrapper_options, situation = unique(usms_species$usm))
      res$sim_list
    },

    load_sim = function(usms_species, rotations, stics_version) {
      if (private$run_sims) {
        logger::log_info("Running simulations...")
        sim <- private$run_simulations(usms_species, rotations)
      } else {
        logger::log_info("Loading simulations data...")
        sim <- SticsRFiles::get_sim(
          workspace = private$usms_workspace,
          usm = unique(usms_species$usm),
          verbose = is_debug(),
          parallel = private$backend$parallel,
          cores = private$backend$cores
        )
      }
      private$workspace$save_sim(sim, usms_species)
      rm(sim)
      gc()
    },

    load_obs = function(usms_species) {
      logger::log_info("Loading observations data...")
      obs <- SticsRFiles::get_obs(
        workspace = private$usms_workspace,
        usm = unique(usms_species$usm),
        verbose = is_debug(),
        parallel = private$backend$parallel,
        cores = private$backend$cores
      )
      private$workspace$save_obs(obs, usms_species)
      rm(obs)
      gc()
    }
  ),

  public = list(
    initialize = function(
      workspace, backend, usms_workspace, metadata_file, stics_exe,
      run_simulations
    ) {
      private$workspace <- workspace
      private$backend <- backend
      private$usms_workspace <- usms_workspace
      private$metadata_file  <- metadata_file
      private$stics_exe <- stics_exe
      private$run_sims <- run_simulations
    },

    load = function() {
      all_usms <- list.dirs(private$usms_workspace, full.names = FALSE, recursive = FALSE)
      usms_species <- private$extract_species_from_usms(all_usms)
      rotations <- private$get_rotation_list()
      stics_version <- private$load_stics_version()
      private$workspace$set_version(stics_version)

      private$load_sim(usms_species, rotations, stics_version)
      private$load_obs(usms_species)
      invisible(private$workspace)
    }
  )
)