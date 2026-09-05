# Estimate Panel Event Study

Estimate a panel event study using two-way fixed effects (TWFE) or
dynamic TWFE. Returns event-time coefficient estimates suitable for
event study plots.

## Usage

``` r
estimate_panel_event_study(
  task,
  method = c("static_twfe", "dynamic_twfe", "sun_abraham", "callaway_santanna",
    "dechaisemartin_dhaultfoeuille", "borusyak_jaravel_spiess"),
  leads = 5,
  lags = 5,
  base_period = -1,
  cluster = NULL,
  ...
)
```

## Arguments

- task:

  A `PanelEventStudyTask`.

- method:

  Estimation method. One of `"static_twfe"`, `"dynamic_twfe"`, or
  `"sun_abraham"`.

- leads:

  Number of pre-treatment periods to include. Default 5.

- lags:

  Number of post-treatment periods to include. Default 5.

- base_period:

  The reference period (relative to treatment) to omit. Default -1 (the
  period just before treatment).

- cluster:

  Name of the clustering variable for standard errors. Defaults to the
  unit ID.

- ...:

  Additional arguments passed to the underlying estimator (used by
  `callaway_santanna`, `dechaisemartin_dhaultfoeuille`, and
  `borusyak_jaravel_spiess` methods).

## Value

The task with `results` populated, containing:

- coefficients:

  Tibble of event-time coefficients with std errors

- model:

  The fitted model object

- method:

  The estimation method used
