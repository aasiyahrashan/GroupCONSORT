# Merge tracker tibbles from independently-processed datasets

Combines two or more tracker tibbles produced in separate environments
or from separate datasets into a single tracker ready for
[`consort_plot()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/consort_plot.md).
Groups that are missing a step (because that criterion did not apply to
their dataset) are carried forward at their last known count and flagged
automatically in the returned `na_cells` data frame.

## Usage

``` r
merge_trackers(..., step_order = NULL, group_order = NULL)
```

## Arguments

- ...:

  Two or more tracker tibbles from
  [`get_tracker()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/get_tracker.md).
  Each must have columns `group`, `step`, `n_remaining`, `n_dropped`.

- step_order:

  Optional character vector giving the desired display order of steps.
  Defaults to the order steps are first encountered across all trackers.

- group_order:

  Optional character vector giving the desired display order of groups.
  Defaults to the order groups are first encountered.

## Value

A named list with two elements:

- `tracker`:

  A single tracker tibble with columns `group`, `step`, `n_remaining`,
  `n_dropped`, suitable for passing to
  [`consort_plot()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/consort_plot.md).

- `na_cells`:

  A data frame with columns `step` and `group` identifying step–group
  combinations that did not apply to a group's source dataset. Pass this
  directly to the `na_cells` argument of
  [`consort_plot()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/consort_plot.md).

## Examples

``` r
cgd <- prep_cgd_example()

# Two separate datasets processed independently
tracker_eu <- cgd |>
  dplyr::filter(region == "Europe") |>
  new_cohort("Randomised", id_col = "id", group_col = "region") |>
  include_if(age >= 5,     "Age >= 5 years") |>
  include_if(weight >= 15, "Weight >= 15 kg") |>
  get_tracker()

tracker_na <- cgd |>
  dplyr::filter(region == "North America") |>
  new_cohort("Randomised", id_col = "id", group_col = "region") |>
  include_if(age >= 5, "Age >= 5 years") |>
  get_tracker()  # weight step not run for this dataset

result <- merge_trackers(tracker_eu, tracker_na)
if (FALSE) { # \dontrun{
consort_plot(result$tracker, na_cells = result$na_cells)
} # }
```
