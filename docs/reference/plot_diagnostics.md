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
