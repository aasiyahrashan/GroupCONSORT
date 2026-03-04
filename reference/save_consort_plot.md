# Save a CONSORT flowchart at content-fitting dimensions

Opens a PNG or PDF device sized exactly to the diagram's natural
dimensions (optionally scaled), then draws into it. This avoids the
clipping that occurs when printing to a fixed-size RStudio plot pane.

## Usage

``` r
save_consort_plot(
  plot,
  path,
  formats = c("png", "pdf"),
  scale = 1,
  dpi = 300,
  page_height_mm = 257
)
```

## Arguments

- plot:

  A `consort_grob` from
  [`consort_plot()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/consort_plot.md).

- path:

  Output path **without** extension.

- formats:

  `"png"`, `"pdf"`, or both.

- scale:

  Multiplier on natural mm dimensions. Default `1`.

- dpi:

  PNG resolution. Default `300`.

- page_height_mm:

  Height (mm) above which a pagination hint is shown. Default `257` (A4
  with standard margins). Set `NULL` to suppress.

## Value

`plot`, invisibly.

## Details

If the diagram height exceeds `page_height_mm`, a message is emitted
suggesting
[`paginate_consort()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/paginate_consort.md) +
[`save_consort_pages()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/save_consort_pages.md).
