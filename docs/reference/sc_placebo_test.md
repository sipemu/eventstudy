# Placebo Test for Synthetic Control

Performs a placebo (permutation) test by re-estimating the synthetic
control for each donor unit as a pseudo-treated unit. The p-value is the
rank of the treated unit's RMSPE ratio among all units.

## Usage

``` r
sc_placebo_test(task, n_placebo = NULL)
```

## Arguments

- task:

  A `SyntheticControlTask` with results.

- n_placebo:

  Number of placebo units to use. Default NULL (all donors).

## Value

The task with `results$placebo` populated, a list containing
`rmspe_ratios`, `p_value`, and `placebo_gaps`.

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
[`prepare_intraday_event_study()`](https://sipemu.github.io/eventstudy/reference/prepare_intraday_event_study.md)
