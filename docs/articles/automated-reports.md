# Automated Event Study Reports

## Introduction

The EventStudy package can generate comprehensive HTML reports from a
completed event study with a single function call. Reports include
summary tables, diagnostic plots, test statistics, and optional
cross-sectional regression results.

## Requirements

Report generation requires the `rmarkdown` and `knitr` packages:

[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"rmarkdown"``, ``"knitr"``)``)`

## Basic Usage

[`library`](https://rdrr.io/r/base/library.html)`(`[`EventStudy`](https://github.com/sipemu/eventstudy)`)`` `` ``# Run a complete event study`` ``task`` ``<-`` `[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)`$``new``(``firm_data``, ``index_data``, ``request``)`` ``ps`` ``<-`` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(``)`` ``task`` ``<-`` `[`run_event_study`](https://sipemu.github.io/eventstudy/reference/run_event_study.md)`(``task``, ``ps``)`` `` ``# Generate report`` `[`generate_report`](https://sipemu.github.io/eventstudy/reference/generate_report.md)`(``task``, output_file ``=`` ``"my_report.html"``)`

## Customizing the Report

### Title and Author

[`generate_report`](https://sipemu.github.io/eventstudy/reference/generate_report.md)`(`` `` ``task``,`` `` output_file ``=`` ``"report.html"``,`` `` title ``=`` ``"Earnings Announcement Event Study"``,`` `` author ``=`` ``"John Doe"`` ``)`

### Selecting Sections

Choose which sections to include:

[`generate_report`](https://sipemu.github.io/eventstudy/reference/generate_report.md)`(`` `` ``task``,`` `` output_file ``=`` ``"report.html"``,`` `` sections ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"summary"``, ``"data"``, ``"diagnostics"``,`` `` ``"single_event"``, ``"multi_event"``, ``"appendix"``)`` ``)`

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

[`generate_report`](https://sipemu.github.io/eventstudy/reference/generate_report.md)`(`` `` ``task``,`` `` output_file ``=`` ``"summary.html"``,`` `` sections ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"summary"``, ``"multi_event"``)`` ``)`

### Including Cross-Sectional Analysis

If you have firm characteristics, include the cross-sectional
regression:

`# Run cross-sectional regression first`` ``cs_result`` ``<-`` `[`cross_sectional_regression`](https://sipemu.github.io/eventstudy/reference/cross_sectional_regression.md)`(`` `` ``task``,`` `` formula ``=`` ``car`` ``~`` ``size`` ``+`` ``leverage``,`` `` characteristics ``=`` ``firm_chars`` ``)`` `` `[`generate_report`](https://sipemu.github.io/eventstudy/reference/generate_report.md)`(`` `` ``task``,`` `` output_file ``=`` ``"report.html"``,`` `` sections ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"summary"``, ``"multi_event"``, ``"cross_sectional"``)``,`` `` cross_sectional ``=`` ``cs_result`` ``)`

## Output Formats

Currently HTML is the primary format. PDF output requires a LaTeX
installation:

[`generate_report`](https://sipemu.github.io/eventstudy/reference/generate_report.md)`(`` `` ``task``,`` `` output_file ``=`` ``"report.pdf"``,`` `` format ``=`` ``"pdf"`` ``)`

## Panel Event Study Reports

The report function automatically detects `PanelEventStudyTask` and
renders panel-specific sections:

`panel_task`` ``<-`` `[`PanelEventStudyTask`](https://sipemu.github.io/eventstudy/reference/PanelEventStudyTask.md)`$``new``(`` `` data ``=`` ``panel_data``,`` `` unit_col ``=`` ``"unit_id"``,`` `` time_col ``=`` ``"time_id"``,`` `` treatment_col ``=`` ``"treatment"``,`` `` outcome_col ``=`` ``"outcome"`` ``)`` ``result`` ``<-`` `[`estimate_panel_event_study`](https://sipemu.github.io/eventstudy/reference/estimate_panel_event_study.md)`(``panel_task``)`` `` `[`generate_report`](https://sipemu.github.io/eventstudy/reference/generate_report.md)`(`` `` ``panel_task``,`` `` output_file ``=`` ``"panel_report.html"``,`` `` sections ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"summary"``, ``"panel"``)`` ``)`

## Programmatic Report Generation

Generate reports in a loop for multiple configurations:

`models`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`` `` `[`MarketModel`](https://sipemu.github.io/eventstudy/reference/MarketModel.md)`$``new``(``)``,`` `` `[`MarketAdjustedModel`](https://sipemu.github.io/eventstudy/reference/MarketAdjustedModel.md)`$``new``(``)``,`` `` `[`FamaFrench3FactorModel`](https://sipemu.github.io/eventstudy/reference/FamaFrench3FactorModel.md)`$``new``(``)`` ``)`` `` ``for`` ``(``i`` ``in`` `[`seq_along`](https://rdrr.io/r/base/seq.html)`(``models``)``)`` ``{`` `` ``ps`` ``<-`` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(``return_model ``=`` ``models``[[``i``]``]``)`` `` ``task`` ``<-`` `[`run_event_study`](https://sipemu.github.io/eventstudy/reference/run_event_study.md)`(`[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)`$``new``(``firm_data``, ``index_data``, ``request``)``, ``ps``)`` `` `` `[`generate_report`](https://sipemu.github.io/eventstudy/reference/generate_report.md)`(`` `` ``task``,`` `` output_file ``=`` `[`sprintf`](https://rdrr.io/r/base/sprintf.html)`(``"report_%s.html"``, `[`class`](https://rdrr.io/r/base/class.html)`(``models``[[``i``]``]``)``[``1``]``)``,`` `` title ``=`` `[`paste`](https://rdrr.io/r/base/paste.html)`(``"Event Study:"``, `[`class`](https://rdrr.io/r/base/class.html)`(``models``[[``i``]``]``)``[``1``]``)`` `` ``)`` ``}`
