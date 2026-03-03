# Apply a per-patient inclusion filter and record the attrition step

Like
[`include_if()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/include_if.md),
but evaluates `condition` within groups defined by `id_col`. Use when
the condition involves window or aggregate functions that should be
scoped per patient, e.g.
`icu_admission_datetime == min(icu_admission_datetime)` to keep only
each patient's first ICU admission row.

## Usage

``` r
include_if_grouped(cohort, condition, label)
```

## Arguments

- cohort:

  A `cohort` object from
  [`new_cohort()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/new_cohort.md).

- condition:

  An unquoted filter expression evaluated per patient group.

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
  include_if_grouped(
    follow_up_days == min(follow_up_days),
    "First observation per patient"
  )
```
