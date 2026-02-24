# GroupCONSORT

<!-- badges: start -->
[![R-CMD-check](https://github.com/YOURNAME/GroupCONSORT/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/YOURNAME/GroupCONSORT/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
<!-- badges: end -->

**GroupCONSORT** is a pipe-friendly R package for tracking patient attrition
through inclusion/exclusion criteria and producing publication-quality
[CONSORT](https://www.consort-statement.org/) flowcharts. Unlike most existing
packages it supports **multi-group studies** — flowcharts show per-group
(site / country) counts alongside the trial total, with optional "N/A" marking
for steps that did not apply to a particular group.

---

## Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("aasiyahrashan/GroupCONSORT")
```

---

## Quick start

```r
library(GroupCONSORT)

# prep_cgd_example() ships with the package; uses survival::cgd
cgd <- prep_cgd_example()

cohort <- cgd |>
  new_cohort(label = "Randomised", id_col = "id", group_col = "region") |>
  include_if(age >= 5,           "Age >= 5 years") |>
  include_if(weight >= 15,       "Weight >= 15 kg") |>
  include_if(steroids == 0,      "Not on corticosteroids") |>
  include_if(follow_up_days > 0, "Positive follow-up time")

consort_plot(cohort)
```

The result is a `ggplot2` object you can further theme or combine with
`patchwork`.

---

## Key features

**Pipe-friendly API.** Build up a cohort object with `|>` — no boilerplate,
no intermediate objects required.

**Automatic layout.** Box sizes, widths, and arrow positions are computed from
the actual text content. Flowcharts stay legible whether you have 3 steps or
10, 1 group or 8.

**Group stratification.** Pass `group_col` to `new_cohort()` and per-group
counts appear automatically in every box.

**N/A cell support.** Mark step–group combinations that did not apply (e.g. a
criterion only relevant to one data source) with the `na_cells` argument.

**Rename without re-running.** Use `step_labels` and `group_labels` in
`consort_plot()` to tidy display text without touching the underlying tracker.

**Content-fitting export.** `save_consort_plot()` calculates the right figure
dimensions from the plot layout — no more guessing `fig.width`.

---

## Core functions

| Function | Purpose |
|---|---|
| `new_cohort()` | Start a cohort object from a data frame |
| `include_if()` | Apply a filter step and record attrition |
| `get_tracker()` | Extract the attrition tibble |
| `get_data()` | Extract the filtered data frame |
| `consort_plot()` | Draw the flowchart |
| `save_consort_plot()` | Save at content-fitting dimensions |
| `prep_cgd_example()` | Prepare the built-in CGD example dataset |

---

## More examples

### Single-group study

```r
cgd |>
  new_cohort("Randomised", id_col = "id", group_col = NULL) |>
  include_if(age >= 5,    "Age >= 5 years") |>
  include_if(steroids == 0, "Not on corticosteroids") |>
  consort_plot()
```

### Relabelling for publication

```r
consort_plot(
  cohort,
  step_labels  = c("Not on corticosteroids" = "No concurrent corticosteroid use"),
  group_labels = c("North America" = "N. America")
)
```

### N/A cells for multi-source studies

```r
# Weight criterion only applied to European centres
cohort_multi <- cgd |>
  new_cohort("Randomised", id_col = "id", group_col = "region") |>
  include_if(age >= 5,                                  "Age >= 5 years") |>
  include_if(weight >= 15 | region == "North America",  "Weight >= 15 kg") |>
  include_if(steroids == 0,                             "Not on corticosteroids")

consort_plot(
  cohort_multi,
  na_cells = data.frame(step = "Weight >= 15 kg", group = "North America")
)
```

### Inspecting attrition

```r
get_tracker(cohort)
#> # A tibble: 10 × 4
#>    group         step                    n_remaining n_dropped
#>    <chr>         <chr>                         <int>     <int>
#>  1 Europe        Randomised                       63         0
#>  2 North America Randomised                       65         0
#>  3 Europe        Age >= 5 years                   62         1
#>  ...
```

### Saving

```r
p <- consort_plot(cohort)
save_consort_plot(p, "figures/consort", formats = c("png", "pdf"))
```

The `scale` argument (default `0.75`) controls physical size without affecting
proportions.

---

## Font size and large text

All font sizes scale with the `font_size` argument. Box dimensions recompute
automatically so content never overflows, regardless of font size or label
length. For very long step labels the title text wraps to fit the box width.

---

## Contributing

Bug reports and pull requests are welcome at
<https://github.com/aasiyahrashan/GroupCONSORT/issues>.

---
