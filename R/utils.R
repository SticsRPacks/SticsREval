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
  read.table(
    file = filepath,
    sep = ";",
    header = TRUE,
    stringsAsFactors = FALSE
  )
}

sort_usm_by_species <- function(workspace, usms) {
  library(dplyr)
  result <- lapply(usms, function(usm) {
    species <- SticsRFiles::get_plant_txt(workspace = file.path(workspace, usm))
    list(
      species = species$codeplante,
      usm = usm
    )
  })
  dplyr::bind_rows(result)
}
