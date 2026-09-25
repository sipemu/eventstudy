# Plot Stocks

Visualize the adjusted close prices or abnormal returns of selected
stocks around a specified event date.

## Usage

``` r
plot_stocks(
  task,
  target_variable = "firm_adjusted",
  add_event_date = FALSE,
  max_symbols = 6,
  sample_symbols = TRUE,
  do_sample = NULL
)
```

## Arguments

- task:

  An EventStudyTask object.

- target_variable:

  A character string specifying the target variable to plot. Default is
  "firm_adjusted".

- add_event_date:

  Add vertical line for event date (TRUE/FALSE).

- max_symbols:

  An integer specifying the maximum number of symbols to display.
  Default is 6.

- sample_symbols:

  Logical; if `TRUE` (default), randomly sample `max_symbols` symbols
  when the task contains more than `max_symbols`. If `FALSE`, take the
  first `max_symbols` in order.

- do_sample:

  *Deprecated.* Renamed to `sample_symbols` in EventStudy 0.66.0.
  Accepts the old name for backward compatibility and emits one
  deprecation warning.

## Value

A plotly plot object.

## See also

Other eventstudy-plots:
[`es_colours`](https://sipemu.github.io/eventstudy/reference/es_colours.md),
[`plot_car_distribution()`](https://sipemu.github.io/eventstudy/reference/plot_car_distribution.md),
[`plot_diagnostics()`](https://sipemu.github.io/eventstudy/reference/plot_diagnostics.md),
[`plot_event_study()`](https://sipemu.github.io/eventstudy/reference/plot_event_study.md),
[`theme_eventstudy()`](https://sipemu.github.io/eventstudy/reference/theme_eventstudy.md)
