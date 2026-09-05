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
  sections = c("summary", "data", "diagnostics",
               "single_event", "multi_event", "appendix")
)
```

Available sections:

| Section           | Description                                             |
|-------------------|---------------------------------------------------------|
| `summary`         | Study overview: model, event window, number of events   |
| `data`            | Data summary and event timeline                         |
| `diagnostics`     | Model fit diagnostics (residual plots, normality tests) |
| `single_event`    | Individual event AR and CAR results                     |
| `multi_event`     | AAR and CAAR test statistics                            |
| `cross_sectional` | Cross-sectional regression results                      |
| `panel`           | Panel event study results (for PanelEventStudyTask)     |
| `appendix`        | Technical details and methodology                       |

### Summary Only

For a quick overview:

``` r

generate_report(
  task,
  output_file = "summary.html",
  sections = c("summary", "multi_event")
)
```

### Including Cross-Sectional Analysis

If you have firm characteristics, include the cross-sectional
regression:

``` r

# Run cross-sectional regression first
cs_result <- cross_sectional_regression(
  task,
  formula = car ~ size + leverage,
  characteristics = firm_chars
)

generate_report(
  task,
  output_file = "report.html",
  sections = c("summary", "multi_event", "cross_sectional"),
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
  data = panel_data,
  unit_col = "unit_id",
  time_col = "time_id",
  treatment_col = "treatment",
  outcome_col = "outcome"
)
result <- estimate_panel_event_study(panel_task)

generate_report(
  panel_task,
  output_file = "panel_report.html",
  sections = c("summary", "panel")
)
```

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
