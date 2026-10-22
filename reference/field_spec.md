# Describe a function argument's validation rules

Describe a function argument's validation rules

## Usage

``` r
field_spec(
  default = NULL,
  type = NULL,
  nullable = TRUE,
  choices = NULL,
  min = NULL,
  max = NULL,
  validator = NULL
)
```

## Arguments

- default:

  Default value for the field. Use
  [`required()`](https://sticsrpacks.github.io/SticsREval/reference/required.md)
  for fields that must be explicitly provided and have no sensible
  default. Purely informational unless the caller applies it itself —
  validation functions here only look at the value actually supplied.

- type:

  Character vector. Accepted R types (e.g. "character", "logical").

- nullable:

  Logical. Is NULL a valid value? Defaults to TRUE.

- choices:

  Vector. If provided, the value must belong to this set.

- min:

  Numeric. Minimum allowed value (for numeric types).

- max:

  Numeric. Maximum allowed value (for numeric types).

- validator:

  Function. Custom validation: function(val) returns TRUE if valid, or a
  character error message otherwise.

## Value

A list of class "field_spec"
