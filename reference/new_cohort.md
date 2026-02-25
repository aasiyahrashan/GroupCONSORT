# Create a cohort object and initialise the attrition tracker

Create a cohort object and initialise the attrition tracker

## Usage

``` r
new_cohort(
  data,
  label = "Base cohort",
  id_col = "person_id",
  group_col = "country"
)
```

## Arguments

- data:

  Data frame with one row per observation and a unique patient ID
  column.

- label:

  Label for the baseline step, e.g. `"Base cohort"`.

- id_col:

  Name of the unique patient identifier column.

- group_col:

  Name of the site or country grouping column. `NULL` or a missing
  column both produce a single `"All"` group.

## Value

A `cohort` object.

## Examples

``` r
cgd <- prep_cgd_example()
cohort <- new_cohort(cgd, label = "Randomised", id_col = "id",
                     group_col = "region")
```
