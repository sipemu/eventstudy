# Simulate Event Study for Power Analysis

Runs Monte Carlo simulations to estimate the statistical power
(rejection rate) of an event study design. Each simulation generates
synthetic data from a user-specified DGP, runs the full event study
pipeline, and records whether the null hypothesis is rejected.

## Usage

``` r
simulate_event_study(
  n_events = 20,
  event_window = c(-5, 5),
  estimation_window_length = 120,
  abnormal_return = 0,
  return_model = MarketModel$new(),
  test_statistic = "CSectT",
  alpha = 0.05,
  n_simulations = 1000,
  dgp_params = list(),
  seed = NULL
)
```

## Arguments

- n_events:

  Number of events (firms) per simulation. Default 20.

- event_window:

  Event window as `c(start, end)`. Default `c(-5, 5)`.

- estimation_window_length:

  Length of the estimation window. Default 120.

- abnormal_return:

  Injected abnormal return on the event day (day 0). Set to 0 for size
  analysis, positive for power analysis. Default 0.

- return_model:

  An initialized return model object. Default `MarketModel$new()`.

- test_statistic:

  Name of the multi-event test statistic to evaluate. Default
  `"CSectT"`.

- alpha:

  Significance level. Default 0.05.

- n_simulations:

  Number of Monte Carlo replications. Default 1000.

- dgp_params:

  List of DGP parameters: `alpha` (drift), `beta` (market exposure),
  `sigma_firm` (idiosyncratic volatility), `sigma_market` (market
  volatility).

- seed:

  Optional seed for reproducibility.

## Value

An S3 object of class `es_simulation` with components:

- power:

  Rejection rate at the event day

- rejection_by_day:

  Tibble of rejection rates for each event-window day

- test_stats:

  Vector of test statistics at the event day from each sim

- params:

  List of simulation parameters

## See also

Other eventstudy-statistics:
[`ARTTest`](https://sipemu.github.io/eventstudy/reference/ARTTest.md),
[`BHARTTest`](https://sipemu.github.io/eventstudy/reference/BHARTTest.md),
[`BMPTest`](https://sipemu.github.io/eventstudy/reference/BMPTest.md),
[`CARTTest`](https://sipemu.github.io/eventstudy/reference/CARTTest.md),
[`CSectTTest`](https://sipemu.github.io/eventstudy/reference/CSectTTest.md),
[`CalendarTimePortfolioTest`](https://sipemu.github.io/eventstudy/reference/CalendarTimePortfolioTest.md),
[`GeneralizedSignTest`](https://sipemu.github.io/eventstudy/reference/GeneralizedSignTest.md),
[`KolariPynnonenTest`](https://sipemu.github.io/eventstudy/reference/KolariPynnonenTest.md),
[`MultiEventStatisticsSet`](https://sipemu.github.io/eventstudy/reference/MultiEventStatisticsSet.md),
[`PatellZTest`](https://sipemu.github.io/eventstudy/reference/PatellZTest.md),
[`RankTest`](https://sipemu.github.io/eventstudy/reference/RankTest.md),
[`SignTest`](https://sipemu.github.io/eventstudy/reference/SignTest.md),
[`SingleEventStatisticsSet`](https://sipemu.github.io/eventstudy/reference/SingleEventStatisticsSet.md),
[`StatisticsSetBase`](https://sipemu.github.io/eventstudy/reference/StatisticsSetBase.md),
[`adjust_p_values()`](https://sipemu.github.io/eventstudy/reference/adjust_p_values.md),
[`bootstrap_test()`](https://sipemu.github.io/eventstudy/reference/bootstrap_test.md),
[`car_by_group()`](https://sipemu.github.io/eventstudy/reference/car_by_group.md),
[`car_quantiles()`](https://sipemu.github.io/eventstudy/reference/car_quantiles.md),
[`cross_sectional_regression()`](https://sipemu.github.io/eventstudy/reference/cross_sectional_regression.md),
[`model_diagnostics()`](https://sipemu.github.io/eventstudy/reference/model_diagnostics.md),
[`pretrend_test()`](https://sipemu.github.io/eventstudy/reference/pretrend_test.md),
[`validate_task()`](https://sipemu.github.io/eventstudy/reference/validate_task.md)
