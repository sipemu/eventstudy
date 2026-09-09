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

## See also

[`run_event_study`](https://sipemu.github.io/eventstudy/reference/run_event_study.md),
[`fit_model`](https://sipemu.github.io/eventstudy/reference/fit_model.md),
[`calculate_statistics`](https://sipemu.github.io/eventstudy/reference/calculate_statistics.md)

Other eventstudy-pipeline:
[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md),
[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md),
[`calculate_statistics()`](https://sipemu.github.io/eventstudy/reference/calculate_statistics.md),
[`fit_model()`](https://sipemu.github.io/eventstudy/reference/fit_model.md),
[`run_event_study()`](https://sipemu.github.io/eventstudy/reference/run_event_study.md)
