# Draw a publication-quality CONSORT flowchart

Draw a publication-quality CONSORT flowchart

## Usage

``` r
consort_plot(
  tracker,
  na_cells = NULL,
  step_labels = NULL,
  group_labels = NULL,
  font_size = 1,
  box_width = NULL,
  excl_width = NULL
)
```

## Arguments

- tracker:

  A `cohort` object or a tracker tibble with columns `group`, `step`,
  `n_remaining`, `n_dropped`.

- na_cells:

  A data frame with columns `step` and `group` identifying step–group
  combinations where the step did not apply (shown as "N/A"). `NULL`
  (default) means no N/A cells.

- step_labels:

  Named character vector renaming steps for display.

- group_labels:

  Named character vector renaming groups for display.

- font_size:

  Multiplicative scaling factor for all text. Default `1`.

- box_width:

  Width of main boxes in mm. `NULL` = auto.

- excl_width:

  Width of exclusion boxes in mm. `NULL` = auto.

## Value

A `consort_grob` (a `gTree`). Print it or pass to
[`save_consort_plot()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/save_consort_plot.md).
