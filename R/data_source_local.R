get_local_data_source <- function(workspace) {
  new(
    "DataSource",
    usms = list.dirs(workspace, full.names = FALSE, recursive = FALSE)
  )
}
