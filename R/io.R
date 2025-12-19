library(vroom)

safe_write_csv <- function(data, path) {
  tryCatch({
    write.csv2(
      data,
      path,
      quote = FALSE,
      row.names = FALSE
    )
  },
  error = function(e) {
    message(sprintf("❌ Unable to create '%s': %s", basename(path), e$message))
    stop(sprintf("Error: unable to create %s", basename(path)), call. = FALSE)
  })
}

read_csv <- function(filepath) {
  vroom::vroom(
    filepath,
    delim = ";",
    col_names = TRUE,
    na = c("NA", "NaN", "OK", "rejection M=0"),
    show_col_types = is_debug(),
    locale = vroom::locale(
      decimal_mark = ",",
      date_format = "%Y-%m-%d"
    )
  )
}

read_ref_sim <- function(config, species) {
  reference_dir <- file.path(config$reference_data_dir, species)
  reference_file <- file.path(reference_dir, "Simulations.csv")
  if (!length(reference_file) || !file.exists(reference_file)) {
    return(NULL)
  }
  df <- read_csv(reference_file)
  CroPlotR::split_df2sim(df)
}

save_sim <- function(config, species, sim) {
  output_dir <- file.path(config$output_dir, species)
  safe_write_csv(
    CroPlotR::bind_rows(sim),
    file.path(output_dir, "Simulations.csv")
  )
}

load_workspace_sim <- function(config, usms, rotations) {
  if (config$run_simulations) {
    logger::log_info("Running simulations...")
    return(
      run_simulations(
        stics_exe = config$stics_exe,
        workspace = config$workspace,
        usm_names = usms,
        successive = rotations,
        verbose = is_debug(),
        parallel = config$parallel,
        cores = config$cores
      )
    )
  }
  SticsRFiles::get_sim(
    workspace = config$workspace,
    usm = usms,
    verbose = is_debug(),
    parallel = config$parallel,
    cores = config$cores
  )
}

load_workspace_obs <- function(config, usms) {
  SticsRFiles::get_obs(
    workspace = config$workspace,
    usm = usms,
    verbose = is_debug(),
    parallel = config$parallel,
    cores = config$cores
  )
}
