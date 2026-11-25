# Generate a Stics workspace from IDEStics data, for evaluation.

Retrieves the rotations and USMs data from the IDEStics API, completes
them with the model and plant files of the Stics repository, and
generates the corresponding text workspace. The rotations are written to
`metadata.csv` in `output_dir`.

The IDEStics credentials are read from the `IDESTICS_USERNAME` and
`IDESTICS_PASSWORD` environment variables if they are set (e.g. in the
user-level `~/.Renviron`). Otherwise, they are prompted for in an
interactive session, the password being masked.

## Usage

``` r
gen_workspace_from_idestics(
  idestics_url,
  stics_path,
  output_dir,
  stics_version,
  usms,
  parallel = FALSE,
  cores = NA,
  force_code_shape = TRUE,
  verbose = 1L,
  batch_size = 300L,
  max_active = 10L,
  workspaces = NULL,
  repositories = NULL,
  usms_files = lifecycle::deprecated()
)
```

## Arguments

- idestics_url:

  URL of the IDEStics server

- stics_path:

  path to Stics repository

- output_dir:

  path to the Stics workspace to generate

- stics_version:

  Stics version of the USMs to retrieve, as listed by the IDEStics API
  (e.g. `"11.0.0"`)

- usms:

  character vector of USM names to generate. Use
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

- batch_size:

  Number of USMs requested to the IDEStics API per request. `Inf`
  requests all the USMs at once.

- max_active:

  Maximum number of requests to the IDEStics API performed in parallel.

- workspaces:

  character vector of the names of the IDEStics workspaces to retrieve
  the USMs from, e.g. to choose between USMs of the same name found in
  several workspaces. Only active workspaces are considered. `NULL`
  (default) retrieves the USMs from all the active workspaces.

- repositories:

  character vector of the names of the IDEStics repositories to retrieve
  the USMs from. Combined with `workspaces`, only the USMs matching both
  filters are kept. `NULL` (default) does not filter on the
  repositories.

- usms_files:

  Deprecated. Use `usms = read_usms_files(usms_files)` instead.

## Value

NULL (invisibly)
