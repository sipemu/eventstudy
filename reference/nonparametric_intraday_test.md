# Non-Parametric Intraday Event Study Test

Implements the non-parametric intraday event study methodology of
Rinaudo & Saha (2014). For each event time, cumulative abnormal returns
(CARs) on the event day are compared to the empirical distribution of
CARs from estimation-period days. Significance is assessed without
distributional assumptions.

## Usage

``` r
nonparametric_intraday_test(
  estimation_window,
  event_window,
  event_times,
  p = 0.05,
  init_window = 5L,
  upper = FALSE
)
```

## Arguments

- estimation_window:

  Data frame with columns `day`, `time`, and `abnormalReturn` containing
  intraday abnormal returns for multiple estimation days.

- event_window:

  Data frame with columns `time` and `abnormalReturn` for the single
  event day.

- event_times:

  Character vector of intraday times at which events occurred (e.g.,
  `c("10:00", "14:30")`).

- p:

  Numeric; significance level in (0, 1). Default 0.05.

- init_window:

  Integer; number of initial observations to accumulate before testing
  significance. Default 5.

- upper:

  Logical; if `TRUE` test for positive abnormal returns (upper tail),
  otherwise test for negative abnormal returns (lower tail). Default
  `FALSE`.

## Value

A named list of tibbles, one per event time. Each tibble contains:

- id:

  Observation index within the significant window.

- CAR:

  Cumulative abnormal return on the event day.

- CI:

  Empirical confidence interval boundary from estimation days.

- fitCI:

  Polynomial-smoothed CI (degree-4 polynomial fit).

When an event time is not significant, returns a single-row tibble with
all values equal to zero.

## References

Rinaudo, J.B. & Saha, A. (2014). Non-parametric intraday event studies.

## See also

Other eventstudy-tasks:
[`IntradayEventStudyTask`](https://sipemu.github.io/eventstudy/reference/IntradayEventStudyTask.md),
[`PanelEventStudyTask`](https://sipemu.github.io/eventstudy/reference/PanelEventStudyTask.md),
[`SyntheticControlTask`](https://sipemu.github.io/eventstudy/reference/SyntheticControlTask.md),
[`estimate_panel_event_study()`](https://sipemu.github.io/eventstudy/reference/estimate_panel_event_study.md),
[`estimate_synthetic_control()`](https://sipemu.github.io/eventstudy/reference/estimate_synthetic_control.md),
[`plot_panel_event_study()`](https://sipemu.github.io/eventstudy/reference/plot_panel_event_study.md),
[`plot_synthetic_control()`](https://sipemu.github.io/eventstudy/reference/plot_synthetic_control.md),
[`prepare_intraday_event_study()`](https://sipemu.github.io/eventstudy/reference/prepare_intraday_event_study.md),
[`sc_placebo_test()`](https://sipemu.github.io/eventstudy/reference/sc_placebo_test.md)

## Examples

``` r
if (FALSE) { # \dontrun{
results <- nonparametric_intraday_test(
  estimation_window = est_data,
  event_window = event_data,
  event_times = c("10:00", "14:30"),
  p = 0.05,
  init_window = 5
)
} # }
```
