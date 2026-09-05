# Prepare Intraday Event Study

Compute returns and assign estimation/event windows for intraday data.
Windows are specified in number of observations (bars) rather than days.

## Usage

``` r
prepare_intraday_event_study(task, parameter_set)
```

## Arguments

- task:

  An IntradayEventStudyTask.

- parameter_set:

  A ParameterSet.

## Value

The task with returns and windows appended.
