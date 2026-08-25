USMSWorkspace <- R6::R6Class("USMSWorkspace", # nolint: object_name_linter
  private = list(
    usms_workspace = NULL,
    stics_exe = NULL,
    metadata_file = NULL,
    sim_rds = NULL,
    obs_rds = NULL,
    ref_sim_rds = NULL,
    usms = NULL,
    workspace = NULL,
    backend = NULL,

    extract_species_from_usms = function(usms) {
      logger::log_debug("Extracting species from USMs...")
      result <- private$backend$run(
        length(usms),
        function(i) {
          usm <- usms[i]
          species <- SticsRFiles::get_plant_txt(
            workspace = file.path(private$usms_workspace, usm)
          )
          list(species = species$codeplante, situation = usm)
        }
      )
      sorted <- dplyr::bind_rows(result)
      logger::log_debug("Found ", length(unique(sorted$species)), " species")
      sorted
    },

    get_rotation_list = function(usm_names = NULL) {
      if (!file.exists(private$metadata_file)) {
        stop(
          "Metadata file not found: ",
          private$metadata_file,
          call. = FALSE
        )
      }
      rotations_data <- read_csv(private$metadata_file, delimiter = ";")
      required_cols  <- c("usm", "rotation", "rotation_order")
      missing_cols <- setdiff(required_cols, names(rotations_data))

      if (length(missing_cols) > 0) {
        stop(
          "Missing columns in metadata file: ", toString(missing_cols),
          call. = FALSE
        )
      }
      if (nrow(rotations_data) == 0) return(list())

      original_order <- rotations_data[["rotation_order"]]
      rotations_data <- rotations_data |>
        dplyr::mutate(
          rotation_order = suppressWarnings(as.numeric(.data$rotation_order)),
          rotation = as.character(.data$rotation)
        )

      if (sum(is.na(rotations_data[["rotation_order"]])) > sum(is.na(original_order))) { # nolint: line_length_linter
        stop("Column must be numeric: rotation_order", call. = FALSE)
      }

      rotations <- rotations_data |>
        dplyr::filter(!is.na(.data$rotation), .data$rotation != "0") |>
        dplyr::arrange(.data$rotation, .data$rotation_order) |>
        dplyr::group_by(.data$rotation) |>
        dplyr::summarise(usm_vec = list(.data$usm)) |>
        dplyr::pull("usm_vec")

      if (!is.null(usm_names)) {
        # Only keep rotations whose USMs are all part of the current run: a
        # rotation chains successive USMs together, so a partially available
        # chain cannot be simulated anyway.
        rotations <- Filter(function(usms) all(usms %in% usm_names), rotations)
      }

      logger::log_debug("Found ", length(rotations), " rotations")
      rotations
    },

    load_sim = function() {
      logger::log_info("Loading simulations data...")
      species_situations <- private$workspace$get_species_situations(
        species = NULL
      )
      sim <- readRDS(private$sim_rds)[
        unique(species_situations$situation)
      ]
      private$workspace$save_sim(sim, species_situations)
      rm(sim)
      gc()
    },

    load_obs = function() {
      logger::log_info("Loading observations data...")
      species_situations <- private$workspace$get_species_situations(
        species = NULL
      )
      obs <- readRDS(private$obs_rds)[
        unique(species_situations$situation)
      ]
      private$workspace$save_obs(obs, species_situations)
      rm(obs)
      gc()
    },

    load_ref_sim = function() {
      if (!is.null(private$ref_sim_rds)) {
        logger::log_info("Loading reference simulations data...")
        species_situations <- private$workspace$get_species_situations(
          species = NULL
        )
        ref_sim <- readRDS(private$ref_sim_rds)[
          unique(species_situations$situation)
        ]
        private$workspace$save_ref_sim(ref_sim, species_situations)
        rm(ref_sim)
        gc()
      }
    }
  ),

  public = list(
    # usms_workspace: path to the Stics text workspace.
    # stics_exe, metadata_file: only needed to call run_simulations().
    # sim_rds, obs_rds, ref_sim_rds, eval_workspace, usms: only needed to
    # call load() (eval_workspace only when `workspace` isn't supplied).
    # parallel, cores: used to build a default `backend` when one isn't
    # supplied. workspace, backend: dependency injection.
    initialize = function(
      usms_workspace,
      stics_exe = NULL,
      metadata_file = NULL,
      sim_rds = NULL,
      obs_rds = NULL,
      ref_sim_rds = NULL,
      eval_workspace = NULL,
      usms = NULL,
      parallel = FALSE,
      cores = NA,
      workspace = NULL,
      backend = NULL
    ) {
      private$usms_workspace <- usms_workspace
      private$stics_exe <- stics_exe
      private$metadata_file <- metadata_file
      private$sim_rds <- sim_rds
      private$obs_rds <- obs_rds
      private$ref_sim_rds <- ref_sim_rds
      private$usms <- usms
      private$backend <- backend %||% ParallelBackend$new(parallel, cores)
      private$workspace <- workspace %||% EvalWorkspace$new(eval_workspace)
    },

    run_simulations = function(usms, var) {
      rotations <- private$get_rotation_list(unique(usms))
      wrapper_options <- SticsOnR::stics_wrapper_options(
        stics_exe = private$stics_exe,
        workspace = private$usms_workspace,
        parallel = private$backend$parallel,
        cores = private$backend$cores,
        successive = rotations,
        verbose = is_debug(),
        time_display = is_debug()
      )
      res <- SticsOnR::stics_wrapper(
        wrapper_options, situation = unique(usms), var = var
      )
      if (res$error) {
        stop(
          "Error running simulations. Set verbose = 2L for more details.",
          call. = FALSE
        )
      }
      res$sim_list
    },

    load = function() {
      all_usms <- list.dirs(
        private$usms_workspace, full.names = FALSE, recursive = FALSE
      )
      if (!is.null(private$usms)) {
        missing_usms <- setdiff(private$usms, all_usms)
        if (length(missing_usms) > 0) {
          stop(
            "The following USMs are not found in the workspace: ",
            toString(missing_usms), call. = FALSE
          )
        }
        all_usms <- private$usms
      }
      usms_species <- private$extract_species_from_usms(all_usms)
      private$workspace$save_species_usm(usms_species)

      private$load_obs()
      private$load_sim()
      private$load_ref_sim()
      private$workspace$remove_init_obs()
      invisible(private$workspace)
    }
  )
)
