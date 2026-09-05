# Prepare data for an Event Study

Perform return calculation for each stock and the corresponding
reference market defined in the task and the parameter set.

## Usage

``` r
prepare_event_study(task, parameter_set)
```

## Arguments

- task:

  An Event Study task.

- parameter_set:

  A parameter set that defines the Event Study.

## Value

The task object with returns and windows appended.
