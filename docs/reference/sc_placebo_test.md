# Placebo Test for Synthetic Control

Performs a placebo (permutation) test by re-estimating the synthetic
control for each donor unit as a pseudo-treated unit. The p-value is the
rank of the treated unit's RMSPE ratio among all units.

## Usage

``` r
sc_placebo_test(task, n_placebo = NULL)
```

## Arguments

- task:

  A `SyntheticControlTask` with results.

- n_placebo:

  Number of placebo units to use. Default NULL (all donors).

## Value

The task with `results$placebo` populated, a list containing
`rmspe_ratios`, `p_value`, and `placebo_gaps`.
