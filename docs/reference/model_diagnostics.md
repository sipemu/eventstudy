# Model Diagnostics for Event Study

Run diagnostic tests on fitted event study models. Tests include
normality of residuals, autocorrelation, and basic model fit quality.

## Usage

``` r
model_diagnostics(task, event_id = NULL)
```

## Arguments

- task:

  A fitted EventStudyTask (after fit_model has been called).

- event_id:

  Optional event identifier. If NULL, runs diagnostics for all events.

## Value

A tibble with diagnostic test results for each event.
