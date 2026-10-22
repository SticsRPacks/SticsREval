# Run the Balance Closure Test

Checks that the initial and final water and nitrogen balances are equal
(or NA) for each USM, using pre-computed simulation data from `sim_rds`
— see
[`run_simulations`](https://sticsrpacks.github.io/SticsREval/reference/run_simulations.md)
to produce it. If there are discrepancies, it logs a warning with the
details of the issue and stops with an error listing the affected USMs.

## Usage

``` r
balance_closure_test(
  sim_rds,
  output_dir = NULL,
  usms = NULL,
  parallel = FALSE,
  cores = NA,
  verbose = 1L
)
```

## Arguments

- sim_rds:

  path to an `.rds` file containing pre-computed simulation data,
  including the balance variables — see
  [`run_simulations`](https://sticsrpacks.github.io/SticsREval/reference/run_simulations.md)'s
  `vars` argument. The USMs to test are the names of this list,
  optionally restricted by `usms`

- output_dir:

  directory where balance error details (`balance_errors_details.csv`)
  will be written, if any. If NULL (default), details are not exported

- usms:

  character vector of USMs to restrict the test to. If NULL (default),
  all USMs found in `sim_rds` are tested

- parallel:

  Boolean. Is the computation to be done in parallel ?

- cores:

  Number of cores to use for parallel computation

- verbose:

  Integer. Logging verbosity level: 0 = warnings and errors only, 1 =
  info, 2 = debug

## Value

Invisibly, the internal test object. Called for its side effects:
logging a summary of the test, exporting detailed balance errors to
`output_dir` (if defined), and stopping with an error if any USM failed
the test.

## Examples

``` r
if (FALSE) { # \dontrun{
balance_closure_test(
  sim_rds    = "/path/to/simulations.rds",
  output_dir = "/path/to/output_dir",
  parallel   = TRUE,
  cores      = 4
)
} # }
```
