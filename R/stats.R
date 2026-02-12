read_ref_stats <- function(
  species,
  reference_data_dir
) {
  reference_dir <- file.path(reference_data_dir, species)
  reference_file <- file.path(reference_dir, "Criteres_stats.csv")
  if (!length(reference_file) || !file.exists(reference_file)) {
    return(NULL)
  }
  read_csv(reference_file)
}

save_stats <- function(
  stats,
  output_dir
) {
  safe_write_csv(stats, file.path(output_dir, "Criteres_stats.csv"))
}
