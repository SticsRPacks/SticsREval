validate_configuration <- function(
  workspace,
  data_source,
  stics_exe,
  stics_path,
  sms_path,
  run_simulations,
  rotation_file = NULL
) {
  if (is.null(stics_exe)) stop("Stics executable path must be defined")
  if (is.null(workspace)) stop("Workspace path must be defined")
  if (data_source == "sms") {
    validate_sms_configuration(sms_path, stics_path, run_simulations)
    return(gen_sms_workspace(sms_path, stics_path, workspace))
  }
  if (data_source == "local") {
    if (is.null(rotation_file)) {
      stop("Rotation file must be defined when data source is local")
    }
    return(get_local_data_source(workspace, rotation_file))
  }
  stop("Invalid data source: source must be 'sms' or 'local'")
}
