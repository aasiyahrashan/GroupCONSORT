# Save paginated CONSORT pages to PNG or PDF

Companion to
[`paginate_consort()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/paginate_consort.md).
Saves each page as either:

- **PDF**: a single multi-page file (`path.pdf`). All pages share the
  dimensions of the largest page (PDF does not support variable page
  sizes within one file via `cairo_pdf`).

- **PNG**: one file per page (`path_p01.png`, `path_p02.png`, ...). Each
  PNG is sized to its own content, so pages may differ in height (e.g.
  the last page often has fewer steps).

## Usage

``` r
save_consort_pages(
  pages,
  path,
  formats = c("png", "pdf"),
  scale = 1,
  dpi = 300
)
```

## Arguments

- pages:

  A list of `consort_grob` objects from
  [`paginate_consort()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/paginate_consort.md).

- path:

  Output path **without** extension.

- formats:

  `"png"`, `"pdf"`, or both.

- scale:

  Multiplier on natural mm dimensions. Default `1`.

- dpi:

  PNG resolution. Default `300`.

## Value

`pages`, invisibly.
