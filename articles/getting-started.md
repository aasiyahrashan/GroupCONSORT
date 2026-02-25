# Getting started with GroupCONSORT

GroupCONSORT tracks patient attrition through inclusion/exclusion steps
and produces publication-quality CONSORT flowcharts, with optional
per-group (site / country) breakdowns. The API is a simple pipe chain —
each
[`include_if()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/include_if.md)
call filters the data and records what was dropped.

## Data

We use [`survival::cgd`](https://rdrr.io/pkg/survival/man/cgd.html), a
real placebo-controlled trial that ships with every R installation.
[`prep_cgd_example()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/prep_cgd_example.md)
reduces it to one row per patient and adds a `region` column (North
America / Europe).

``` r
cgd <- prep_cgd_example()
glimpse(cgd)
#> Rows: 128
#> Columns: 11
#> $ id             <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, …
#> $ center         <fct> Scripps Institute, Scripps Institute, Scripps Institute…
#> $ region         <chr> "Europe", "Europe", "Europe", "Europe", "North America"…
#> $ age            <int> 12, 15, 19, 12, 17, 44, 22, 7, 27, 5, 2, 8, 12, 1, 9, 2…
#> $ weight         <dbl> 62.0, 47.5, 72.7, 34.0, 52.7, 45.0, 59.7, 17.4, 82.8, 1…
#> $ sex            <fct> female, male, male, male, male, female, male, male, mal…
#> $ inherit        <fct> autosomal, autosomal, X-linked, X-linked, X-linked, aut…
#> $ steroids       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ treat          <fct> rIFN-g, placebo, rIFN-g, rIFN-g, placebo, rIFN-g, place…
#> $ status         <int> 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0…
#> $ follow_up_days <int> 219, 8, 382, 388, 246, 364, 292, 363, 294, 371, 19, 373…
```

## Building a cohort

Start with
[`new_cohort()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/new_cohort.md),
then pipe
[`include_if()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/include_if.md)
calls. The condition is any ordinary dplyr filter expression.

``` r
cohort <- cgd |>
  new_cohort(label = "Randomised", id_col = "id", group_col = "region") |>
  include_if(age >= 5,           "Age >= 5 years") |>
  include_if(weight >= 15,       "Weight >= 15 kg") |>
  include_if(steroids == 0,      "Not on corticosteroids") |>
  include_if(follow_up_days > 0, "Positive follow-up time")

cohort
#> <cohort>  5 steps | 2 groups
#>   Start: n = 128  |  End: n = 108
```

[`get_tracker()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/get_tracker.md)
returns the underlying tibble — one row per group × step, recording how
many patients remain and how many were dropped.

``` r
get_tracker(cohort)
#> # A tibble: 10 × 4
#>    group         step                    n_remaining n_dropped
#>    <chr>         <chr>                         <int>     <int>
#>  1 Europe        Randomised                      102         0
#>  2 North America Randomised                       26         0
#>  3 Europe        Age >= 5 years                   89        13
#>  4 North America Age >= 5 years                   24         2
#>  5 Europe        Weight >= 15 kg                  87         2
#>  6 North America Weight >= 15 kg                  24         0
#>  7 Europe        Not on corticosteroids           84         3
#>  8 North America Not on corticosteroids           24         0
#>  9 Europe        Positive follow-up time          84         0
#> 10 North America Positive follow-up time          24         0
```

[`get_data()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/get_data.md)
returns the filtered data frame ready for analysis.

``` r
nrow(get_data(cohort))
#> [1] 108
```

## Drawing the flowchart

[`consort_plot()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/consort_plot.md)
returns a ggplot2 object. Use
[`save_consort_plot()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/save_consort_plot.md)
to write it to disk at content-fitted dimensions, then display it with
[`knitr::include_graphics()`](https://rdrr.io/pkg/knitr/man/include_graphics.html).
This pattern ensures the figure is never stretched or compressed by
knitr’s default sizing.

``` r
p <- consort_plot(cohort)
save_consort_plot(p, "elig", formats = "png", dpi = 150)
#> Saved: elig.png
knitr::include_graphics("elig.png")
```

![CONSORT flowchart showing attrition through four eligibility steps,
with North America and Europe shown separately.](elig.png)

## Relabelling for publication

Rename steps and groups for display without touching the tracker. Only
the labels you want to change need to be listed.

``` r
p <- consort_plot(
  cohort,
  step_labels  = c("Not on corticosteroids" = "No concurrent corticosteroid use"),
  group_labels = c("North America" = "N. America")
)
save_consort_plot(p, "label", formats = "png", dpi = 150)
#> Saved: label.png
knitr::include_graphics("label.png")
```

![CONSORT flowchart with relabelled steps and groups.](label.png)

## N/A cells for multi-source studies

When combining data from multiple sources, some steps may not apply to
all groups. Pass a data frame with `step` and `group` columns to
`na_cells` — those exclusion boxes show “N/A” while the main count boxes
always show totals.

``` r
cohort_multi <- cgd |>
  new_cohort("Randomised", id_col = "id", group_col = "region") |>
  include_if(age >= 5,                                  "Age >= 5 years") |>
  include_if(weight >= 15 | region == "North America",  "Weight >= 15 kg") |>
  include_if(steroids == 0,                             "Not on corticosteroids") |>
  include_if(follow_up_days > 0,                        "Positive follow-up time")

p <- consort_plot(
  cohort_multi,
  na_cells = data.frame(step = "Weight >= 15 kg", group = "North America")
)
save_consort_plot(p, "na", formats = "png", dpi = 150)
#> Saved: na.png
knitr::include_graphics("na.png")
```

![CONSORT flowchart with N/A in the exclusion box for North America at
the weight step.](na.png)

## Single-group studies

Set `group_col = NULL` for studies without a meaningful site grouping.

``` r
p <- cgd |>
  new_cohort("Randomised", id_col = "id", group_col = NULL) |>
  include_if(age >= 5,           "Age >= 5 years") |>
  include_if(weight >= 15,       "Weight >= 15 kg") |>
  include_if(steroids == 0,      "Not on corticosteroids") |>
  include_if(follow_up_days > 0, "Positive follow-up time") |>
  consort_plot()
save_consort_plot(p, "single", formats = "png", dpi = 150)
#> Saved: single.png
knitr::include_graphics("single.png")
```

![CONSORT flowchart with no per-group breakdown.](single.png)

## Pre-filtering outside the pipeline

Complex criteria can be computed separately and joined back in before
piping.

``` r
xlinked_ids <- cgd |> filter(inherit == "X-linked") |> pull(id)

cgd |>
  new_cohort("Randomised", id_col = "id", group_col = "region") |>
  include_if(id %in% xlinked_ids, "X-linked inheritance") |>
  include_if(steroids == 0,       "Not on corticosteroids") |>
  get_tracker()
#> # A tibble: 6 × 4
#>   group         step                   n_remaining n_dropped
#>   <chr>         <chr>                        <int>     <int>
#> 1 Europe        Randomised                     102         0
#> 2 North America Randomised                      26         0
#> 3 Europe        X-linked inheritance            71        31
#> 4 North America X-linked inheritance            15        11
#> 5 Europe        Not on corticosteroids          69         2
#> 6 North America Not on corticosteroids          15         0
```

## Saving

The
[`save_consort_plot()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/save_consort_plot.md)
function computes figure dimensions from the plot content so text and
boxes stay consistently sized regardless of how many steps or groups
there are. Figures are saved with a transparent background.

``` r
p <- consort_plot(cohort)
save_consort_plot(p, "consort_flowchart", formats = c("png", "pdf"))
```

The `scale` argument (default `0.75`) controls conversion from internal
plot units to inches — increase it to produce a physically larger figure
without changing layout proportions. The `font_size` argument to
[`consort_plot()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/consort_plot.md)
scales all text and box dimensions together.

## Layout stress tests

These chunks verify the layout holds up under edge cases — many groups,
many steps, long labels, and large font.

``` r
p <- df_many |>
  new_cohort("Screened", id_col = "id", group_col = "site") |>
  include_if(age >= 18, "Age >= 18 years") |>
  include_if(wt  >= 15, "Weight >= 15 kg") |>
  consort_plot()
save_consort_plot(p, "six_groups", formats = "png", dpi = 150)
#> Saved: six_groups.png
knitr::include_graphics("six_groups.png")
```

![Stress test: 6 groups, 2 steps.](six_groups.png)

``` r
p <- df_many |>
  dplyr::mutate(site = ifelse(site %in% c("Site A", "Site B", "Site C"),
                              "North", "South")) |>
  new_cohort("Screened", id_col = "id", group_col = "site") |>
  include_if(age >= 18,  "Age >= 18 years") |>
  include_if(age <= 75,  "Age <= 75 years") |>
  include_if(wt  >= 15,  "Weight >= 15 kg") |>
  include_if(wt  <= 100, "Weight <= 100 kg") |>
  include_if(age >= 20,  "Age >= 20 years") |>
  include_if(wt  >= 10,  "Weight >= 10 kg") |>
  consort_plot()
save_consort_plot(p, "six_steps", formats = "png", dpi = 150)
#> Saved: six_steps.png
knitr::include_graphics("six_steps.png")
```

![Stress test: 6 steps, 2 groups.](six_steps.png)

``` r
p <- df_many |>
  dplyr::mutate(site = ifelse(site %in% c("Site A", "Site B", "Site C"),
                              "Northern Region", "Southern Region")) |>
  new_cohort("All screened participants at enrolment",
             id_col = "id", group_col = "site") |>
  include_if(age >= 18,
    "Participants aged 18 years or older at time of screening visit") |>
  include_if(wt >= 15,
    "Body weight at least 15 kg as recorded at baseline assessment") |>
  consort_plot()
save_consort_plot(p, "long_label", formats = "png", dpi = 150)
#> Saved: long_label.png
knitr::include_graphics("long_label.png")
```

![Stress test: long step labels.](long_label.png)

``` r
p <- df_many |>
  new_cohort("Screened", id_col = "id", group_col = "site") |>
  include_if(age >= 18, "Age >= 18 years") |>
  include_if(wt  >= 15, "Weight >= 15 kg") |>
  consort_plot(font_size = 1.4)
save_consort_plot(p, "large_font", formats = "png", dpi = 150)
#> Saved: large_font.png
knitr::include_graphics("large_font.png")
```

![Stress test: font_size = 1.4.](large_font.png)
