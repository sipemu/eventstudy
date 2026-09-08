# Generate Event Study Report (Multi-Format)

Renders an automated report from a completed event study task in one or
more output formats. Uses a bundled RMarkdown template with configurable
sections. The narrative is assembled ONCE before the format loop –
`es_advise` is never called per format (NARR-01).

## Usage

``` r
generate_report(
  task,
  output_file = "event_study_report.html",
  format = "html",
  title = "Event Study Report",
  author = NULL,
  sections = c("exec_summary", "data_methods", "results", "diagnostics", "robustness",
    "references"),
  cross_sectional = NULL,
  confidence_level = 0.95,
  interactive = TRUE,
  advice = NULL,
  narrative = NULL,
  provider = NULL,
  ...
)
```

## Arguments

- task:

  A fitted `EventStudyTask` or `PanelEventStudyTask`.

- output_file:

  Output file path (extension overridden per format). Default
  `"event_study_report.html"`.

- format:

  Character vector of output formats, any subset of
  `c("html", "pdf", "word", "md")`. Default `"html"`. Multiple formats
  render one file per format sharing a common basename. Missing optional
  toolchains emit one [`message()`](https://rdrr.io/r/base/message.html)
  each and are skipped; only inability to render HTML raises
  [`stop()`](https://rdrr.io/r/base/stop.html).

- title:

  Report title string.

- author:

  Author name (optional, default `""`).

- sections:

  Character vector of sections to include. Default is all six fixed
  sections:
  `c("exec_summary","data_methods","results","diagnostics","robustness","references")`.

- cross_sectional:

  Optional cross-sectional regression results to include.

- confidence_level:

  Confidence level for plots. Default 0.95.

- interactive:

  Logical. Use interactive plotly plots in HTML output. Default TRUE.

- advice:

  An optional grounded `Advice` object returned by
  [`es_advise`](https://sipemu.github.io/eventstudy/reference/es_advise.md)`(task_type = "report_writing")`.
  When supplied and valid, renders an **AI Advisor Interpretation**
  section. A supplied but invalid `advice` is silently coerced to `NULL`
  with one [`warning()`](https://rdrr.io/r/base/warning.html) – the
  report is never broken.

- narrative:

  An optional named list keyed by section (e.g.
  `list(exec_summary = "...", data_methods = "...", results = "...", robustness = "...")`),
  typically from `assemble_report_narrative()`. When `NULL` (default),
  the render output is byte-identical to the v0.63.x baseline. A
  supplied but invalid `narrative` is silently coerced to `NULL` with
  one [`warning()`](https://rdrr.io/r/base/warning.html).

- provider:

  Optional LLM provider passed to `assemble_report_narrative()` when
  `narrative` is `NULL`. If both `provider` and `narrative` are `NULL`,
  a fully offline narrative is assembled. Ignored when a pre-built
  `narrative` list is supplied by the caller.

- ...:

  Additional arguments passed to
  [`rmarkdown::render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).

## Value

A named character vector of output file paths, invisibly, keyed by
format name (e.g.
`c(html = "/tmp/report.html", pdf = "/tmp/report.pdf")`). A
single-format call returns a length-1 named vector: `result[["html"]]`
and `result[[1L]]` both resolve (backward-compatible with prior
single-path callers).

## Examples

``` r
if (FALSE) { # \dontrun{
task <- run_event_study(my_task, ParameterSet$new())
# Single format (backward-compatible)
path <- generate_report(task, format = "html")
# Multi-format
paths <- generate_report(task, format = c("html", "pdf"))
paths[["html"]]
} # }
```
