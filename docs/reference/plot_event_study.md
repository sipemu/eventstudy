# Plot Event Study Results

Create an event study plot showing abnormal returns (AR), cumulative
abnormal returns (CAR), average abnormal returns (AAR), or cumulative
average abnormal returns (CAAR) with confidence bands.

## Usage

``` r
plot_event_study(
  task,
  type = "car",
  event_id = NULL,
  group = NULL,
  stat_name = "CSectT",
  confidence_level = 0.95,
  title = NULL
)
```

## Arguments

- task:

  A fitted EventStudyTask with statistics computed.

- type:

  Type of plot: "ar", "car", "aar", or "caar".

- event_id:

  Event identifier for single-event plots ("ar", "car").

- group:

  Group name for multi-event plots ("aar", "caar").

- stat_name:

  Name of the multi-event test statistic to use for AAR/CAAR plots.
  Defaults to "CSectT".

- confidence_level:

  Confidence level for the bands. Default is 0.95.

- title:

  Optional plot title.

## Value

A ggplot2 plot object.
