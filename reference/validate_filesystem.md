# Validate filesystem-dependent fields (I/O)

Checks that path fields point to files that actually exist on disk. Kept
separate from
[`validate_schema()`](https://sticsrpacks.github.io/SticsREval/reference/validate_schema.md)
so that plain structural validation never touches the filesystem:
callers decide when this I/O-bound check runs.

## Usage

``` r
validate_filesystem(self, schema)
```

## Arguments

- self:

  A named list, typically `as.list(environment())` taken at the top of
  the calling function.

- schema:

  A list with a `filesystem_checks` element: a list of
  `list(desc = ..., check = function(self) ...)`, e.g. built with
  `check_path_exists()`.

## Value

invisible(TRUE) if valid, otherwise stop() with all errors.
