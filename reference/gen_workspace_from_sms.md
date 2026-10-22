# Generate a Stics workspace from SMS data, for evaluation and calibration.

Generate a Stics workspace from SMS data, for evaluation and
calibration.

## Usage

``` r
gen_workspace_from_sms(
  sms_path,
  stics_path,
  output_dir,
  usms = NULL,
  parallel = FALSE,
  cores = NA,
  force_code_shape = TRUE,
  verbose = 1L,
  usms_files = lifecycle::deprecated()
)
```

## Arguments

- sms_path:

  path to the SMS repository

- stics_path:

  path to Stics repository

- output_dir:

  path to the Stics workspace to generate

- usms:

  character vector of USM names to generate. If NULL (default), all
  evaluation and calibration USMs are generated. Use
  [`read_usms_files`](https://sticsrpacks.github.io/SticsREval/reference/read_usms_files.md)
  to read them from USM list files.

- parallel:

  Boolean. Is the computation to be done in parallel ?

- cores:

  Number of cores to use for parallel computation

- force_code_shape:

  Boolean. Should the code_shape parameter be forced to 2 for intercrop
  plant files (see `set_intercrop_code_shape`) ? Default TRUE.

- verbose:

  Integer. Logging verbosity level: 0 = warnings and errors only, 1 =
  info, 2 = debug

- usms_files:

  Deprecated. Use `usms = read_usms_files(usms_files)` instead.

## Value

NULL (invisibly)
