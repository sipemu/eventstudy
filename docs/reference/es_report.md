# One-Call Event Study Report Orchestrator

Generates a complete event study report in a single call: deep-clones
the task (non-mutation guarantee, REPORT-04), harvests diagnostics via
[`es_diagnostics()`](https://sipemu.github.io/eventstudy/reference/es_diagnostics.md),
and delegates rendering to
[`generate_report()`](https://sipemu.github.io/eventstudy/reference/generate_report.md).
Returns the output file path(s) VISIBLY so the path prints at the REPL
without an explicit [`print()`](https://rdrr.io/r/base/print.html).

## Usage

``` r
es_report(
  task,
  output_file = "event_study_report.html",
  format = "html",
  sections = c("exec_summary", "data_methods", "results", "diagnostics", "robustness",
    "references"),
  provider = NULL,
  title = "Event Study Report",
  author = NULL,
  confidence_level = 0.95,
  interactive = TRUE,
  verbose = getOption("eventstudy.verbose", TRUE),
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
  render one file per format sharing a common basename.

- sections:

  Character vector of sections to include. Default is all six fixed
  sections:
  `c("exec_summary","data_methods","results","diagnostics","robustness","references")`.

- provider:

  Optional LLM provider forwarded to
  [`generate_report()`](https://sipemu.github.io/eventstudy/reference/generate_report.md).
  When `NULL` (default), renders a fully offline report (no API key, no
  network required).

- title:

  Report title string.

- author:

  Author name (optional, default `NULL` -\> `""`).

- confidence_level:

  Confidence level for plots. Default 0.95.

- interactive:

  Logical. Use interactive plotly plots in HTML output. Default `TRUE`.

- verbose:

  Logical; if FALSE, suppress informational messages. Default
  `getOption("eventstudy.verbose", TRUE)`.

- ...:

  Additional arguments forwarded to
  [`generate_report()`](https://sipemu.github.io/eventstudy/reference/generate_report.md)
  and on to
  [`rmarkdown::render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).

## Value

A named character vector of output file path(s), one per rendered
format, returned **visibly** (the path prints at the REPL). Keyed by
format name, e.g. `c(html = "/tmp/report.html")`. A single-format call
returns a length-1 named vector; `result[["html"]]` and `result[[1L]]`
both resolve.

## Details

The narrative is assembled ONCE inside
[`generate_report()`](https://sipemu.github.io/eventstudy/reference/generate_report.md)
(NARR-01): `es_report()` does NOT call `assemble_report_narrative()` or
[`es_advise()`](https://sipemu.github.io/eventstudy/reference/es_advise.md)
directly. A `provider` argument is forwarded to
[`generate_report()`](https://sipemu.github.io/eventstudy/reference/generate_report.md),
which assembles the narrative once before the format loop. When
`provider = NULL` (default), the report is fully offline with no network
or API key required.

## See also

[`generate_report`](https://sipemu.github.io/eventstudy/reference/generate_report.md),
[`es_diagnostics`](https://sipemu.github.io/eventstudy/reference/es_diagnostics.md),
[`run_event_study`](https://sipemu.github.io/eventstudy/reference/run_event_study.md)

Other eventstudy-export:
[`export_results()`](https://sipemu.github.io/eventstudy/reference/export_results.md),
[`generate_report()`](https://sipemu.github.io/eventstudy/reference/generate_report.md),
[`tidy.EventStudyTask()`](https://sipemu.github.io/eventstudy/reference/tidy.EventStudyTask.md)

## Examples

``` r
if (FALSE) { # \dontrun{
task <- run_event_study(my_task, ParameterSet$new())
# Offline single-format (default)
path <- es_report(task)
# Multi-format with AI provider
paths <- es_report(task, format = c("html", "md"), provider = my_provider)
paths[["html"]]
} # }
```
