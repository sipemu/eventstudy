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

## See also

Other eventstudy-tasks:
[`IntradayEventStudyTask`](https://sipemu.github.io/eventstudy/reference/IntradayEventStudyTask.md),
[`PanelEventStudyTask`](https://sipemu.github.io/eventstudy/reference/PanelEventStudyTask.md),
[`SyntheticControlTask`](https://sipemu.github.io/eventstudy/reference/SyntheticControlTask.md),
[`estimate_panel_event_study()`](https://sipemu.github.io/eventstudy/reference/estimate_panel_event_study.md),
[`estimate_synthetic_control()`](https://sipemu.github.io/eventstudy/reference/estimate_synthetic_control.md),
[`nonparametric_intraday_test()`](https://sipemu.github.io/eventstudy/reference/nonparametric_intraday_test.md),
[`plot_synthetic_control()`](https://sipemu.github.io/eventstudy/reference/plot_synthetic_control.md),
[`prepare_intraday_event_study()`](https://sipemu.github.io/eventstudy/reference/prepare_intraday_event_study.md),
[`sc_placebo_test()`](https://sipemu.github.io/eventstudy/reference/sc_placebo_test.md)
