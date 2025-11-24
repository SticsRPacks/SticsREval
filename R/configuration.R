validate_local <- function() {}

validate_sms <- function(sms_path, stics_path, run_simulations) {
  if (is.null(sms_path) || !dir.exists(sms_path)) {
    stop("SMS path must be a valid path when data source is sms")
  }
  if (is.null(stics_path) || !dir.exists(stics_path)) {
    stop("Stics path must be a valid path when data source is sms")
  }
  if (!run_simulations) stop(
      "run_simulations flag must be True when data source is sms"
    )
}

validate_configuration <- function(
  workspace,
  data_source,
  stics_exe,
  stics_path,
  sms_path,
  run_simulations
) {
  if (is.null(stics_exe)) stop("Stics executable path must be defined")
  if (is.null(workspace)) stop("Workspace path must be defined")
  if (data_source == "sms") {
    validate_sms(sms_path, stics_path, run_simulations)
    return(gen_sms_workspace(sms_path, stics_path, workspace))
  }
  if (data_source == "local") {
    return(get_local_data_source(workspace))
  }
  stop("Invalid data source: source must be 'sms'")
}
