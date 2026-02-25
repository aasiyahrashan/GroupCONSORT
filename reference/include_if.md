# Apply an inclusion filter and record the attrition step

Apply an inclusion filter and record the attrition step

## Usage

``` r
include_if(cohort, condition, label)
```

## Arguments

- cohort:

  A `cohort` object from
  [`new_cohort()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/new_cohort.md).

- condition:

  An unquoted filter expression, e.g. `age >= 18`.

- label:

  Display label for this step.

## Value

The updated `cohort` object.

## Examples

``` r
cgd <- prep_cgd_example()
cohort <- new_cohort(cgd, label = "Randomised", id_col = "id",
                     group_col = "region") |>
  include_if(age >= 5, "Age >= 5 years") |>
  include_if(steroids == 0, "Not on corticosteroids")
```
