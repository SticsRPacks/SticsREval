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
    locale = vroom::locale(
      decimal_mark = ",",
      date_format="%Y-%m-%d"
    )
  )
}

sort_usm_by_species <- function(config, usms) {
  backend <- setup_parallel_backend(config, length(usms))
  on.exit(backend$cleanup(), add = TRUE)
  `%do_par_or_not%` <- backend$do

  library(dplyr)
  result <- foreach::foreach(
    i = seq_along(usms)
  ) %do_par_or_not% {
    usm <- usms[i]
    species <- SticsRFiles::get_plant_txt(
      workspace = file.path(config$workspace, usm)
    )
    list(
      species = species$codeplante,
      usm = usm
    )
  }
  dplyr::bind_rows(result)
}

get_rotation_list <- function(rotation_data) {
  library(dplyr)
  rotation_data %>%
    dplyr::filter(rotation != 0) %>%
    dplyr::arrange(rotation, rotation_order) %>%
    dplyr::group_by(rotation) %>%
    dplyr::summarise(usm_vec = list(usm)) %>%
    dplyr::pull(usm_vec)
}
