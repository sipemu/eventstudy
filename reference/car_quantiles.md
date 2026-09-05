# CAR Quantiles

Compute quantiles of the CAR distribution across events.

## Usage

``` r
car_quantiles(task, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), car_window = NULL)
```

## Arguments

- task:

  A fitted EventStudyTask.

- probs:

  Probability vector for quantiles. Default
  `c(0.05, 0.25, 0.5, 0.75, 0.95)`.

- car_window:

  Optional two-element vector for CAR window.

## Value

A named numeric vector of quantiles.
