# Render a data frame as a styled table (HTML / PDF / Word / Markdown)

Internal rendering helper used by the event study report skeleton and
[`es_report()`](https://sipemu.github.io/eventstudy/reference/es_report.md).
Exported so `inst/rmarkdown/` templates can call it without `:::`.

## Usage

``` r
report_table(x, caption = NULL, col.names = NULL, digits = NULL)
```

## Arguments

- x:

  A data frame to render.

- caption:

  Optional character scalar caption, or `NULL`.

- col.names:

  Optional character vector of display column names, or `NULL`. When
  supplied, `x` is renamed before `tt()`/`kable()` so both rendering
  paths show the display names.

- digits:

  Optional integer digit count, or `NULL`. Passed to `tt()` and
  [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html). Pass
  `NULL` for
  pre-[`sprintf()`](https://rdrr.io/r/base/sprintf.html)-formatted
  tables to preserve byte-compatible output.

## Value

`invisible(NULL)`. Side-effect: prints the table via `knit_print`.

## Details

When tinytable is available (see `.tinytable_available()`), renders via
[`tinytable::tt()`](https://vincentarelbundock.github.io/tinytable/man/tt.html)
with a bold header, auto-aligned columns, and optional digit rounding.
Falls back to
[`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) when
tinytable is absent – byte-identical to the prior scattered
[`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) call sites.
