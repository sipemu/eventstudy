# Cross-Sectional Regression of CARs

Regress cumulative abnormal returns (CARs) on firm characteristics to
explain cross-sectional variation in event effects. Supports OLS with
heteroskedasticity-consistent (HC) standard errors.

## Usage

``` r
cross_sectional_regression(
  task,
  formula,
  data,
  car_window = NULL,
  robust = TRUE
)
```

## Arguments

- task:

  A fitted EventStudyTask with abnormal returns computed.

- formula:

  A formula with the response on the left (ignored; CAR is always the
  dependent variable) and explanatory variables on the right, e.g.,
  `~ log_market_cap + leverage`.

- data:

  A data frame of firm characteristics. Must contain an `event_id`
  column to merge with CARs.

- car_window:

  Two-element integer vector specifying the CAR window as
  `c(start, end)` relative indices. Default is the full event window.

- robust:

  Logical. If TRUE and the sandwich package is available, compute HC1
  robust standard errors. Default TRUE.

## Value

A list with class `"es_cross_sectional"` containing:

- model:

  The fitted `lm` object

- coefficients:

  Coefficient table with (robust) standard errors

- r_squared:

  R-squared of the regression

- n_obs:

  Number of observations

- car_data:

  The merged CAR + characteristics data

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
[`model_diagnostics()`](https://sipemu.github.io/eventstudy/reference/model_diagnostics.md),
[`pretrend_test()`](https://sipemu.github.io/eventstudy/reference/pretrend_test.md),
[`simulate_event_study()`](https://sipemu.github.io/eventstudy/reference/simulate_event_study.md),
[`validate_task()`](https://sipemu.github.io/eventstudy/reference/validate_task.md)
