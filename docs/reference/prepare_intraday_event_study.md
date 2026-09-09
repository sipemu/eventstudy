# Prepare Intraday Event Study

Compute returns and assign estimation/event windows for intraday data.
Windows are specified in number of observations (bars) rather than days.

## Usage

``` r
prepare_intraday_event_study(task, parameter_set)
```

## Arguments

- task:

  An IntradayEventStudyTask.

- parameter_set:

  A ParameterSet.

## Value

The task with returns and windows appended.

## See also

Other eventstudy-tasks:
[`IntradayEventStudyTask`](https://sipemu.github.io/eventstudy/reference/IntradayEventStudyTask.md),
[`PanelEventStudyTask`](https://sipemu.github.io/eventstudy/reference/PanelEventStudyTask.md),
[`SyntheticControlTask`](https://sipemu.github.io/eventstudy/reference/SyntheticControlTask.md),
[`estimate_panel_event_study()`](https://sipemu.github.io/eventstudy/reference/estimate_panel_event_study.md),
[`estimate_synthetic_control()`](https://sipemu.github.io/eventstudy/reference/estimate_synthetic_control.md),
[`nonparametric_intraday_test()`](https://sipemu.github.io/eventstudy/reference/nonparametric_intraday_test.md),
[`plot_panel_event_study()`](https://sipemu.github.io/eventstudy/reference/plot_panel_event_study.md),
[`plot_synthetic_control()`](https://sipemu.github.io/eventstudy/reference/plot_synthetic_control.md),
[`sc_placebo_test()`](https://sipemu.github.io/eventstudy/reference/sc_placebo_test.md)
