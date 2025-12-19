setGeneric("usms", function(object) standardGeneric("usms"))
setGeneric("rotations", function(object) standardGeneric("rotations"))

setClass(
  "DataSource",
  slots = list(
    usms = "character",
    rotations = "list"
  )
)
setMethod(
  "usms",
  "DataSource",
  function(object) {
    object@usms
  }
)
setMethod(
  "rotations",
  "DataSource",
  function(object) {
    object@rotations
  }
)

get_data_source_from_config <- function(config) {
  if (config$data_source == "sms") {
    return(
      gen_sms_workspace(
        config$sms_path,
        config$stics_path,
        config$workspace
      )
    )
  }
  if (config$data_source == "local") {
    return(get_local_data_source(config$workspace, config$rotation_file))
  }
  stop("Invalid data source: source must be 'sms' or 'local'")
}

#' @importFrom dplyr %>%
get_rotation_list <- function(rotation_data) {
  rotation_data %>%
    dplyr::filter(rotation != 0) %>%
    dplyr::arrange(rotation, rotation_order) %>%
    dplyr::group_by(rotation) %>%
    dplyr::summarise(usm_vec = list(usm)) %>%
    dplyr::pull(usm_vec)
}
