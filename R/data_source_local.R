get_local_data_source <- function(workspace, rotation_file) {
  rotations_data <- read_csv(rotation_file)
  new(
    "DataSource",
    usms = list.dirs(workspace, full.names = FALSE, recursive = FALSE),
    rotations = get_rotation_list(rotations_data)
  )
}
