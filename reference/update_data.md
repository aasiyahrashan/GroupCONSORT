# Replace cohort data without modifying the tracker

Use between
[`include_if()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/include_if.md)
steps when mutations need to be applied to `cohort$data` – for example,
data.table joins or column derivations – without recording an attrition
step.

## Usage

``` r
update_data(cohort, new_data)
```

## Arguments

- cohort:

  A `cohort` object from
  [`new_cohort()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/new_cohort.md).

- new_data:

  Replacement data frame. Must still contain the column named by
  `cohort$id_col`.

## Value

The updated `cohort` object with `$data` replaced by `new_data`.
`$tracker`, `$id_col`, and `$group_col` are unchanged.

## Examples

``` r
cgd <- prep_cgd_example()
cohort <- new_cohort(cgd, label = "Randomised", id_col = "id",
                     group_col = "region") |>
  include_if(age >= 5, "Age >= 5 years")

mutated <- dplyr::mutate(get_data(cohort), bmi = weight / (age ^ 2))
cohort <- update_data(cohort, mutated) |>
  include_if(bmi < 30, "BMI < 30")
```
