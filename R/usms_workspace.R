USMSWorkspace <- R6::R6Class("USMSWorkspace", # nolint: object_name_linter
  private = list(
    usms_workspace = NULL,
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
    # sim_rds, obs_rds, ref_sim_rds, usms: data to load into the evaluation
    # workspace. eval_workspace: only used when `workspace` isn't supplied.
    # parallel, cores: used to build a default `backend` when one isn't
    # supplied. workspace, backend: dependency injection.
    initialize = function(
      usms_workspace,
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
      private$sim_rds <- sim_rds
      private$obs_rds <- obs_rds
      private$ref_sim_rds <- ref_sim_rds
      private$usms <- usms
      private$backend <- backend %||% ParallelBackend$new(parallel, cores)
      private$workspace <- workspace %||% EvalWorkspace$new(eval_workspace)
    },

    load = function() {
      all_usms <- list.dirs(
        private$usms_workspace, full.names = FALSE, recursive = FALSE
      )
      if (!is.null(private$usms)) {
        missing_usms <- setdiff(private$usms, all_usms)
        if (length(missing_usms) > 0) {
          logger::log_warn(
            "The following USMs are not found in the workspace and will be ",
            "ignored: ", toString(missing_usms)
          )
        }
        all_usms <- intersect(private$usms, all_usms)
        if (length(all_usms) == 0) {
          stop("None of the requested USMs were found in the workspace",
               call. = FALSE)
        }
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
