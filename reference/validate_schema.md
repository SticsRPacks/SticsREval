# Validate a named list of values against a local schema

Collects ALL errors before raising them, rather than stopping at the
first. Purely structural: never touches the filesystem (see
[`validate_filesystem()`](https://sticsrpacks.github.io/SticsREval/reference/validate_filesystem.md)
for that). Callers build a small, function-scoped `schema`
(`list(fields = list(...), cross_validators = list(...))`) for just the
arguments they need to validate — there is no single shared schema
across the package.

## Usage

``` r
validate_schema(self, schema)
```

## Arguments

- self:

  A named list, typically `as.list(environment())` taken at the top of
  the calling function, i.e. its already-resolved arguments.

- schema:

  A list with a `fields` element (named list of
  [`field_spec()`](https://sticsrpacks.github.io/SticsREval/reference/field_spec.md))
  and an optional `cross_validators` element (list of
  `list(desc = ..., check = function(self) ...)`).

## Value

invisible(TRUE) if valid, otherwise stop() with all errors.
