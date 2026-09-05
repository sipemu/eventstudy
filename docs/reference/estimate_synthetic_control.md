# Estimate Synthetic Control

Estimate synthetic control weights by minimizing the pre-treatment mean
squared prediction error. Weights are non-negative and sum to one.

## Usage

``` r
estimate_synthetic_control(
  task,
  method = c("quadprog", "optim"),
  covariates = NULL
)
```

## Arguments

- task:

  A `SyntheticControlTask`.

- method:

  Optimization method: `"quadprog"` (default, requires quadprog) or
  `"optim"` (uses [`stats::optim`](https://rdrr.io/r/stats/optim.html)
  L-BFGS-B).

- covariates:

  Optional character vector of covariate column names in both treated
  and donor data to include in the matching.

## Value

The task with `results` populated.
