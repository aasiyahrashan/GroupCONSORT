# Split a long CONSORT flowchart across multiple pages

For diagrams that exceed a page height (e.g. many steps with many
groups), this function renders the tracker as a list of `consort_grob`
objects, one per page. Steps are split greedily: each page holds as many
steps as fit within `page_height_mm`. The last step of every page is
repeated as the first step of the next page (overlap), so the reader
always sees where each page continues from.

## Usage

``` r
paginate_consort(
  tracker,
  page_height_mm = 257,
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

  A `cohort` object or tracker tibble (same as
  [`consort_plot()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/consort_plot.md)).

- page_height_mm:

  Usable page height in mm. Default `257` (A4 with standard top/bottom
  margins). For US Letter use approximately `241`.

- na_cells, step_labels, group_labels, font_size, box_width, excl_width:

  Passed through to
  [`consort_plot()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/consort_plot.md)
  for each page.

## Value

A list of `consort_grob` objects. Each element also carries a
`.page_info` attribute: `list(page = i, n_pages = n, steps = <chr>)`.

## Details

Save the result with
[`save_consort_pages()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/save_consort_pages.md).

## Examples

``` r
if (FALSE) { # \dontrun{
pages <- paginate_consort(cohort, page_height_mm = 257)
save_consort_pages(pages, "output/consort")
} # }
```
