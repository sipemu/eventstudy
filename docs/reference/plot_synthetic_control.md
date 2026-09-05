# Plot Synthetic Control Results

Create plots for synthetic control analysis: trajectory comparison,
treatment effect gap, or placebo test results.

## Usage

``` r
plot_synthetic_control(task, type = c("trajectory", "gap", "placebo"))
```

## Arguments

- task:

  A `SyntheticControlTask` with results.

- type:

  Plot type: `"trajectory"` (default), `"gap"`, or `"placebo"`.

## Value

A ggplot2 object.
