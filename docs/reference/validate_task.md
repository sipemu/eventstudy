# Validate an Event Study experiment

Performs validation checks on the event study task: 1. Does the event
date exist in the stock data for each event? 2. Are there sufficient
observations in the estimation window? 3. Are there any gaps in the time
series? 4. Do estimation and event windows overlap?

## Usage

``` r
validate_task(task, parameter_set = NULL, min_estimation_obs = 30)
```

## Arguments

- task:

  The event study task.

- parameter_set:

  The parameter set that defines the event study.

- min_estimation_obs:

  Minimum number of observations required in the estimation window.
  Default is 30.

## Value

The task object (invisibly). Warnings are issued for each problem found.
