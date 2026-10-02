replace_private <- function(obj, name, fn) {
  env <- obj$.__enclos_env__$private
  unlockBinding(name, env)
  env[[name]] <- fn
}

make_log_capture <- function() {
  env <- new.env(parent = emptyenv())
  env$logs <- character(0)

  logger::log_appender(
    function(lines, ...) env$logs <- c(env$logs, lines),
    namespace = logger::log_namespaces()
  )

  env
}

# Workspace mock serving `sim`/`obs`/`ref_sim` data frames (with `situation`
# and `species` columns) filtered by species, mimicking EvalWorkspace: a
# dataset with no row for the species is returned as NULL.
mock_data_workspace <- function(sim, obs, ref_sim = NULL) {
  pick <- function(df, species) {
    if (is.null(df)) return(NULL)
    if (!is.null(species)) df <- df[df$species %in% species, , drop = FALSE]
    if (nrow(df) == 0) return(NULL)
    df[, setdiff(names(df), "species"), drop = FALSE]
  }
  list(
    get_species = function() sort(unique(sim$species)),
    get_species_situations = function(species, usms = NULL) {
      unique(sim[sim$species %in% species, c("species", "situation")])
    },
    get_sim = function(species = NULL, ...) pick(sim, species),
    get_obs = function(species = NULL, ...) pick(obs, species),
    get_ref_sim = function(species = NULL, ...) pick(ref_sim, species)
  )
}

# Simulations/observations for species "wheat" and "maize" (2 USMs each,
# 12 dates), with reference simulations for "wheat" only.
make_eval_data <- function() {
  set.seed(1)
  dates <- as.POSIXct(as.Date("2020-01-01") + 0:11)
  sim <- do.call(rbind, lapply(c("wheat", "maize"), function(sp) {
    data.frame(
      situation = rep(paste0(sp, c("_1", "_2")), each = 12),
      Date = rep(dates, 2),
      lai = runif(24, 1, 5),
      species = sp,
      stringsAsFactors = FALSE
    )
  }))
  obs <- sim
  obs$lai <- obs$lai + rnorm(nrow(obs), 0, 0.5)
  ref_sim <- sim[sim$species == "wheat", ]
  ref_sim$lai <- ref_sim$lai + rnorm(nrow(ref_sim), 0, 0.5)
  list(sim = sim, obs = obs, ref_sim = ref_sim)
}
