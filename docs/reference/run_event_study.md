# Run a Complete Event Study

Convenience wrapper that runs the full event study pipeline: prepare
data, fit models, and calculate test statistics in a single call.

## Usage

``` r
run_event_study(task, parameter_set = ParameterSet$new())
```

## Arguments

- task:

  An EventStudyTask object.

- parameter_set:

  A ParameterSet object defining the event study. Defaults to a new
  ParameterSet with default settings.

## Value

The task object with all results computed.
