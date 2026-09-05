# Generate Event Study Report

Renders an automated HTML or PDF report from a completed event study
task. Uses a bundled RMarkdown template with configurable sections.

## Usage

``` r
generate_report(
  task,
  output_file = "event_study_report.html",
  format = c("html", "pdf"),
  title = "Event Study Report",
  author = NULL,
  sections = c("summary", "data", "diagnostics", "single_event", "multi_event",
    "cross_sectional", "appendix"),
  cross_sectional = NULL,
  confidence_level = 0.95,
  interactive = TRUE,
  advice = NULL,
  ...
)
```

## Arguments

- task:

  A fitted `EventStudyTask` or `PanelEventStudyTask`.

- output_file:

  Output file path. Default `"event_study_report.html"`.

- format:

  Output format: `"html"` (default) or `"pdf"`.

- title:

  Report title.

- author:

  Author name (optional).

- sections:

  Character vector of sections to include. Any subset of: `"summary"`,
  `"data"`, `"diagnostics"`, `"single_event"`, `"multi_event"`,
  `"cross_sectional"`, `"appendix"`.

- cross_sectional:

  Optional cross-sectional regression results to include.

- confidence_level:

  Confidence level for plots. Default 0.95.

- interactive:

  Logical. Use interactive plotly plots in HTML output. Default TRUE.

- advice:

  An optional grounded `Advice` object returned by
  [`es_advise`](https://sipemu.github.io/eventstudy/reference/es_advise.md)`(task_type = "report_writing")`.
  When supplied and a valid `Advice`, renders a new **AI Advisor
  Interpretation** section in the report. When `NULL` (the default), the
  existing render path is completely unchanged (byte-identical output).
  A supplied but invalid `advice` (not an `Advice` object) is silently
  coerced to `NULL` with exactly one
  [`warning()`](https://rdrr.io/r/base/warning.html) — the report is
  never broken.

- ...:

  Additional arguments passed to
  [`rmarkdown::render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).

## Value

The path to the generated report (invisibly).
