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
