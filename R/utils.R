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

#' @importFrom parallel stopCluster
#' @importFrom foreach %dopar% %do%
sort_usm_by_species <- function(workspace, usms, parallel = FALSE, cores = NA) {
  library(dplyr)
  if (parallel) {
    cl <- setup_parallelism(length(usms), cores = cores)
    on.exit(stopCluster(cl))
    `%do_par_or_not%` <- foreach::`%dopar%`
  } else {
    `%do_par_or_not%` <- foreach::`%do%`
  }

  result <- foreach::foreach(
    i = seq_along(usms)
  ) %do_par_or_not% {
    usm <- usms[i]
    species <- SticsRFiles::get_plant_txt(workspace = file.path(workspace, usm))
    list(
      species = species$codeplante,
      usm = usm
    )
  }
  dplyr::bind_rows(result)
}
