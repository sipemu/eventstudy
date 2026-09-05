# Plot Panel Event Study Results

Create an event study plot from panel estimation results. Can also be
called via
[`plot_event_study()`](https://sipemu.github.io/eventstudy/reference/plot_event_study.md)
when given a PanelEventStudyTask.

## Usage

``` r
plot_panel_event_study(task, confidence_level = 0.95, title = NULL)
```

## Arguments

- task:

  A PanelEventStudyTask with results.

- confidence_level:

  Confidence level for error bars. Default 0.95.

- title:

  Optional plot title.

## Value

A ggplot2 object.
