# Save a CONSORT flowchart at content-fitting dimensions

Save a CONSORT flowchart at content-fitting dimensions

## Usage

``` r
save_consort_plot(plot, path, formats = c("png", "pdf"), scale = 1, dpi = 300)
```

## Arguments

- plot:

  A `consort_grob` from
  [`consort_plot()`](https://aasiyahrashan.github.io/GroupCONSORT/reference/consort_plot.md).

- path:

  Output path without extension.

- formats:

  `"png"`, `"pdf"`, or both.

- scale:

  Multiplier on natural mm dimensions. Default `1`.

- dpi:

  PNG resolution.

## Value

`plot`, invisibly.
