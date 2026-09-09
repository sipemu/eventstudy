# Estimate Synthetic Control

Estimate synthetic control weights by minimizing the pre-treatment mean
squared prediction error. Weights are non-negative and sum to one.

## Usage

``` r
estimate_synthetic_control(
  task,
  method = c("quadprog", "optim"),
  covariates = NULL,
  verbose = getOption("eventstudy.verbose", TRUE)
)
```

## Arguments

- task:

  A `SyntheticControlTask`.

- method:

  Optimization method: `"quadprog"` (default, requires quadprog) or
  `"optim"` (uses [`stats::optim`](https://rdrr.io/r/stats/optim.html)
  L-BFGS-B).

- covariates:

  Optional character vector of covariate column names in both treated
  and donor data to include in the matching.

- verbose:

  Logical; if FALSE, suppress informational messages. Default
  `getOption("eventstudy.verbose", TRUE)`.

## Value

The task with `results` populated.

## See also

Other eventstudy-tasks:
[`IntradayEventStudyTask`](https://sipemu.github.io/eventstudy/reference/IntradayEventStudyTask.md),
[`PanelEventStudyTask`](https://sipemu.github.io/eventstudy/reference/PanelEventStudyTask.md),
[`SyntheticControlTask`](https://sipemu.github.io/eventstudy/reference/SyntheticControlTask.md),
[`estimate_panel_event_study()`](https://sipemu.github.io/eventstudy/reference/estimate_panel_event_study.md),
[`nonparametric_intraday_test()`](https://sipemu.github.io/eventstudy/reference/nonparametric_intraday_test.md),
[`plot_panel_event_study()`](https://sipemu.github.io/eventstudy/reference/plot_panel_event_study.md),
[`plot_synthetic_control()`](https://sipemu.github.io/eventstudy/reference/plot_synthetic_control.md),
[`prepare_intraday_event_study()`](https://sipemu.github.io/eventstudy/reference/prepare_intraday_event_study.md),
[`sc_placebo_test()`](https://sipemu.github.io/eventstudy/reference/sc_placebo_test.md)
