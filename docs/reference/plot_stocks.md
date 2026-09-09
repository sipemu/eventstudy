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
  do_sample = TRUE
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

- do_sample:

  A boolean specifying whether to randomly sample the symbols if the
  number exceeds max_symbols. Default is TRUE.

## Value

A plotly plot object.

## See also

Other eventstudy-plots:
[`es_colours`](https://sipemu.github.io/eventstudy/reference/es_colours.md),
[`plot_car_distribution()`](https://sipemu.github.io/eventstudy/reference/plot_car_distribution.md),
[`plot_diagnostics()`](https://sipemu.github.io/eventstudy/reference/plot_diagnostics.md),
[`plot_event_study()`](https://sipemu.github.io/eventstudy/reference/plot_event_study.md),
[`theme_eventstudy()`](https://sipemu.github.io/eventstudy/reference/theme_eventstudy.md)
