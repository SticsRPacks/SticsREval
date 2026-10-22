# Set intercrop code shape parameter.

Sets the code_shape parameter to 2 for intercrop plant files
(ficplt1.txt and ficplt2.txt). This function only processes USMs that
have a plantfile2 specified and not set to "null".

## Usage

``` r
set_intercrop_code_shape(workspace, usm_df)
```

## Arguments

- workspace:

  path to the workspace containing USM folders

- usm_df:

  data frame containing USM information with a plantfile2 column

## Value

NULL (invisibly). Modifies plant files as a side effect.
