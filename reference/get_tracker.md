# Extract the attrition tracker from a cohort object

Extract the attrition tracker from a cohort object

## Usage

``` r
get_tracker(cohort)
```

## Arguments

- cohort:

  A `cohort` object.

## Value

A tibble with columns `group`, `step`, `n_remaining`, `n_dropped`.
