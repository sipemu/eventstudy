# Plot Model Diagnostics

Create diagnostic plots for fitted event study models, including
residual plots and Q-Q plots.

## Usage

``` r
plot_diagnostics(task, event_id = NULL)
```

## Arguments

- task:

  A fitted EventStudyTask.

- event_id:

  The event identifier to plot diagnostics for.

## Value

A ggplot2 plot arranged with patchwork-style layout.

## See also

Other eventstudy-plots:
[`es_colours`](https://sipemu.github.io/eventstudy/reference/es_colours.md),
[`plot_car_distribution()`](https://sipemu.github.io/eventstudy/reference/plot_car_distribution.md),
[`plot_event_study()`](https://sipemu.github.io/eventstudy/reference/plot_event_study.md),
[`plot_stocks()`](https://sipemu.github.io/eventstudy/reference/plot_stocks.md),
[`theme_eventstudy()`](https://sipemu.github.io/eventstudy/reference/theme_eventstudy.md)
