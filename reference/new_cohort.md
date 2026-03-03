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

  Data frame with one or more rows per entity. May contain multiple rows
  per patient (e.g. one row per visit, encounter, or observation). The
  tracker always counts **unique values of `id_col`** (i.e.
  patients/entities), not rows.

- label:

  Label for the baseline step, e.g. `"Base cohort"`.

- id_col:

  Name of the column that uniquely identifies each patient or entity.
  **This is the unit of counting throughout the pipeline.** Every
  [`include_if()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/include_if.md)
  and
  [`include_if_grouped()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/include_if_grouped.md)
  step reports how many distinct `id_col` values remain, regardless of
  how many rows each entity contributes. Make sure this column genuinely
  represents the entity you want to count (e.g. `"patient_id"`, not
  `"visit_id"`).

- group_col:

  Name of the site or country grouping column. `NULL` or a missing
  column both produce a single `"All"` group.

## Value

A `cohort` object.

## Details

### Counting unit

The tracker counts **unique `id_col` values**, not rows. This means
multi-row-per-patient datasets (e.g. repeated encounters) work correctly
out of the box — `n_remaining` and `n_dropped` always refer to
patient/entity counts.

If your data has multiple rows per entity, consider using
[`include_if_grouped()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/include_if_grouped.md)
for filter conditions that involve per-patient aggregates (e.g. keeping
only the first admission).

### Baseline step

The baseline step is always recorded with `n_dropped = 0`. This does not
imply that no filtering occurred before the data was passed to
`new_cohort()` — it simply marks the starting point of the tracked
pipeline.

## Examples

``` r
cgd <- prep_cgd_example()
cohort <- new_cohort(cgd, label = "Randomised", id_col = "id",
                     group_col = "region")
```
