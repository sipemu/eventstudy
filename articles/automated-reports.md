# Automated Event Study Reports

## Introduction

The EventStudy package can generate comprehensive HTML reports from a
completed event study with a single function call. Reports include
summary tables, diagnostic plots, test statistics, and optional
cross-sectional regression results.

## Requirements

Report generation requires the `rmarkdown` and `knitr` packages:

``` r

install.packages(c("rmarkdown", "knitr"))
```

## Basic Usage

``` r

library(EventStudy)

# Run a complete event study
task <- EventStudyTask$new(firm_data, index_data, request)
ps <- ParameterSet$new()
task <- run_event_study(task, ps)

# Generate report
generate_report(task, output_file = "my_report.html")
```

## Customizing the Report

### Title and Author

``` r

generate_report(
  task,
  output_file = "report.html",
  title = "Earnings Announcement Event Study",
  author = "John Doe"
)
```

### Selecting Sections

Choose which sections to include:

``` r

generate_report(
  task,
  output_file = "report.html",
  sections = c("exec_summary", "data_methods", "diagnostics",
               "results", "robustness", "appendix")
)
```

Available sections:

| Section | Description |
|----|----|
| `exec_summary` | Study overview: model, event window, number of events |
| `data_methods` | Data summary, model, and methodology description |
| `results` | Single- and multi-event AR/CAR/AAR/CAAR results (or panel event-time estimates for a `PanelEventStudyTask`) |
| `diagnostics` | Model fit diagnostics (residual plots, normality tests) |
| `robustness` | Robustness notes and the joint-hypothesis caveat |
| `references` | Knowledge-base references drawn from the diagnostics |
| `appendix` | Technical details and methodology |

Note: `exec_summary`, `data_methods`, `results`, `diagnostics`,
`robustness`, and `references` are the six sections included by default;
`appendix` is opt-in.

### Summary Only

For a quick overview:

``` r

generate_report(
  task,
  output_file = "summary.html",
  sections = c("exec_summary", "results")
)
```

### Including Cross-Sectional Analysis

If you have firm characteristics, include the cross-sectional
regression:

``` r

# Run cross-sectional regression first
cs_result <- cross_sectional_regression(
  task,
  formula = ~ size + leverage,
  data = firm_chars
)

generate_report(
  task,
  output_file = "report.html",
  sections = c("exec_summary", "results", "diagnostics"),
  cross_sectional = cs_result
)
```

## Output Formats

Currently HTML is the primary format. PDF output requires a LaTeX
installation:

``` r

generate_report(
  task,
  output_file = "report.pdf",
  format = "pdf"
)
```

## Panel Event Study Reports

The report function automatically detects `PanelEventStudyTask` and
renders panel-specific sections:

``` r

panel_task <- PanelEventStudyTask$new(
  panel_data = panel_data,
  unit_id = "unit_id",
  time_id = "time_id",
  outcome = "outcome",
  treatment = "treatment",
  treatment_time = "treatment_time"
)
panel_task <- estimate_panel_event_study(panel_task, method = "dynamic_twfe")

generate_report(
  panel_task,
  output_file = "panel_report.html",
  sections = c("exec_summary", "results")
)
```

[`estimate_panel_event_study()`](https://sipemu.github.io/eventstudy/reference/estimate_panel_event_study.md)
returns the (mutated) `PanelEventStudyTask` with results attached, not a
separate results object – pass its return value on to
[`generate_report()`](https://sipemu.github.io/eventstudy/reference/generate_report.md).
Panel-specific content is rendered automatically inside the `results`
section whenever `task` is a `PanelEventStudyTask`.

## Programmatic Report Generation

Generate reports in a loop for multiple configurations:

``` r

models <- list(
  MarketModel$new(),
  MarketAdjustedModel$new(),
  FamaFrench3FactorModel$new()
)

for (i in seq_along(models)) {
  ps <- ParameterSet$new(return_model = models[[i]])
  task <- run_event_study(EventStudyTask$new(firm_data, index_data, request), ps)

  generate_report(
    task,
    output_file = sprintf("report_%s.html", class(models[[i]])[1]),
    title = paste("Event Study:", class(models[[i]])[1])
  )
}
```
