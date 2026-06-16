USMSWorkspace <- R6::R6Class("USMSWorkspace", # nolint: object_name_linter
  private = list(
    workspace = NULL,
    backend = NULL,
    config = NULL,

    extract_species_from_usms = function(usms) {
      logger::log_debug("Extracting species from USMs...")
      result <- private$backend$run(
        length(usms),
        function(i) {
          usm <- usms[i]
          species <- SticsRFiles::get_plant_txt(
            workspace = file.path(private$config$usms_workspace, usm)
          )
          list(species = species$codeplante, situation = usm)
        }
      )
      sorted <- dplyr::bind_rows(result)
      logger::log_debug("Found ", length(unique(sorted$species)), " species")
      sorted
    },

    get_rotation_list = function() {
      if (!file.exists(private$config$metadata_file)) {
        stop(
          "Metadata file not found: ",
          private$config$metadata_file,
          call. = FALSE
        )
      }
      rotations_data <- read_csv(private$config$metadata_file, delimiter = ";")
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

      logger::log_debug("Found ", length(rotations), " rotations")
      rotations
    },

    load_stics_version = function() {
      v <- SticsOnR::get_version_number(private$config$stics_exe)
      if (is.na(v)) {
        stop("Can't detect STICS version", call. = FALSE)
      }
      private$workspace$add_evaluated_version(as.character(v))
      v
    },

    load_sim = function(usms_species) {
      if (private$config$run_simulations) {
        logger::log_info("Running simulations...")
        sim <- self$run_simulations(usms_species$situation)
      } else {
        logger::log_info("Loading simulations data...")
        sim <- SticsRFiles::get_sim(
          workspace = private$config$usms_workspace,
          usm = unique(usms_species$situation),
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
        workspace = private$config$usms_workspace,
        usm = unique(usms_species$situation),
        verbose = is_debug(),
        parallel = private$backend$parallel,
        cores = private$backend$cores
      )
      private$workspace$save_obs(obs, usms_species)
      rm(obs)
      gc()
    },

    get_var_from_obs = function() {
      var2exclude <- c("Date", "situation", "species", "version", "Plant")
      obs <- private$workspace$get_obs(var2exclude = var2exclude)
      if (is.null(obs)) {
        stop("Observations data not found in workspace.", call. = FALSE)
      }
      names(obs)
    }
  ),

  public = list(
    initialize = function(
      workspace, backend, config
    ) {
      private$workspace <- workspace
      private$backend <- backend
      private$config <- config
    },

    run_simulations = function(usms, var = NULL) {
      if (is.null(var)) {
        var <- private$get_var_from_obs()
      }
      rotations <- private$get_rotation_list()
      wrapper_options <- SticsOnR::stics_wrapper_options(
        stics_exe = private$config$stics_exe,
        workspace = private$config$usms_workspace,
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
        private$config$usms_workspace, full.names = FALSE, recursive = FALSE
      )
      if (!is.null(private$config$usms)) {
        missing_usms <- setdiff(private$config$usms, all_usms)
        if (length(missing_usms) > 0) {
          stop(
            "The following USMs are not found in the workspace: ",
            toString(missing_usms), call. = FALSE
          )
        }
        all_usms <- private$config$usms
      }
      usms_species <- private$extract_species_from_usms(all_usms)
      private$workspace$save_species_usm(usms_species)
      stics_version <- private$load_stics_version()
      private$workspace$set_version(stics_version)

      private$load_obs(usms_species)
      private$load_sim(usms_species)
      private$workspace$remove_init_obs()
      invisible(private$workspace)
    }
  )
)
