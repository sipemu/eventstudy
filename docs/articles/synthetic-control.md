# Synthetic Control Methods

## Introduction

Synthetic control methods (Abadie and Gardeazabal, 2003; Abadie,
Diamond, and Hainmueller, 2010) provide a data-driven approach to
construct a counterfactual for a single treated unit from a weighted
combination of donor (control) units. This is particularly useful when:

- There is a single treated unit (e.g., one firm, one country)
- A traditional event study with many events is not possible
- You want to visualize the treatment effect as a gap between actual and
  synthetic trajectories

The EventStudy package implements synthetic control estimation with two
optimization methods and includes a placebo test for inference.

## Data Setup

Synthetic control requires: - **Treated unit**: time series of the
outcome for the treated unit - **Donor pool**: time series of outcomes
for potential control units - **Treatment time**: when the intervention
occurs

[`library`](https://rdrr.io/r/base/library.html)`(`[`EventStudy`](https://github.com/sipemu/eventstudy)`)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`tibble`](https://tibble.tidyverse.org/)`)`` `` ``# Treated unit`` ``treated_data`` ``<-`` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` time ``=`` ``1``:``100``,`` `` outcome ``=`` `[`cumsum`](https://rdrr.io/r/base/cumsum.html)`(`[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``100``, mean ``=`` ``0.1``, sd ``=`` ``0.5``)``)`` ``)`` ``# Inject treatment effect after period 60`` ``treated_data``$``outcome``[``61``:``100``]`` ``<-`` ``treated_data``$``outcome``[``61``:``100``]`` ``+`` ``5`` `` ``# Donor pool (long format)`` ``donor_data`` ``<-`` `[`do.call`](https://rdrr.io/r/base/do.call.html)`(``rbind``, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(`[`paste0`](https://rdrr.io/r/base/paste.html)`(``"D"``, ``1``:``10``)``, ``function``(``u``)`` ``{`` `` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` unit ``=`` ``u``,`` `` time ``=`` ``1``:``100``,`` `` outcome ``=`` `[`cumsum`](https://rdrr.io/r/base/cumsum.html)`(`[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``100``, mean ``=`` ``0.1``, sd ``=`` ``0.5``)``)`` `` ``)`` ``}``)``)`

## Creating the Task

`task`` ``<-`` `[`SyntheticControlTask`](https://sipemu.github.io/eventstudy/reference/SyntheticControlTask.md)`$``new``(`` `` treated_data ``=`` ``treated_data``,`` `` donor_data ``=`` ``donor_data``,`` `` treatment_time ``=`` ``61`` ``)`` `[`print`](https://rdrr.io/r/base/print.html)`(``task``)`` ``#> SyntheticControlTask`` ``#> Donors: 10`` ``#> Periods: 100`` ``#> Pre-treat: 60`` ``#> Post-treat: 40`` ``#> Treatment: 61`` ``#> Estimated: FALSE`

## Estimation

Two optimization methods are available:

### quadprog (default)

Uses
[`quadprog::solve.QP()`](https://rdrr.io/pkg/quadprog/man/solve.QP.html)
for exact quadratic programming. Requires the `quadprog` package.

`task`` ``<-`` `[`estimate_synthetic_control`](https://sipemu.github.io/eventstudy/reference/estimate_synthetic_control.md)`(``task``, method ``=`` ``"quadprog"``)`

### optim (fallback)

Uses [`stats::optim()`](https://rdrr.io/r/stats/optim.html) with a
softmax reparameterization. No extra dependencies.

`task`` ``<-`` `[`estimate_synthetic_control`](https://sipemu.github.io/eventstudy/reference/estimate_synthetic_control.md)`(``task``, method ``=`` ``"optim"``)`

## Results

After estimation, the task contains:

`# Donor weights (sum to 1, all >= 0)`` ``task``$``results``$``weights`` ``#> D1 D2 D3 D4 D5 ...`` ``#> 0.312 0.000 0.188 0.000 0.500 ...`` `` ``# Average treatment effect on the treated`` ``task``$``results``$``att`` ``#> [1] 4.87`` `` ``# Pre- and post-treatment mean squared prediction error`` ``task``$``results``$``pre_mspe`` ``task``$``results``$``post_mspe`` `` ``# Full trajectory`` ``task``$``results``$``trajectory`` ``#> # A tibble: 100 x 4`` ``#> time treated synthetic gap`` ``#> <int> <dbl> <dbl> <dbl>`` ``#> 1 1 0.234 0.289 -0.0547`` ``#> ...`

## Visualization

Three plot types are available:

### Trajectory Plot

Compare the treated unit to its synthetic counterpart:

[`plot_synthetic_control`](https://sipemu.github.io/eventstudy/reference/plot_synthetic_control.md)`(``task``, type ``=`` ``"trajectory"``)`

### Gap Plot

Visualize the treatment effect (gap between treated and synthetic):

[`plot_synthetic_control`](https://sipemu.github.io/eventstudy/reference/plot_synthetic_control.md)`(``task``, type ``=`` ``"gap"``)`

### Placebo Plot

After running the placebo test, overlay placebo gaps:

`task`` ``<-`` `[`sc_placebo_test`](https://sipemu.github.io/eventstudy/reference/sc_placebo_test.md)`(``task``)`` `[`plot_synthetic_control`](https://sipemu.github.io/eventstudy/reference/plot_synthetic_control.md)`(``task``, type ``=`` ``"placebo"``)`

## Placebo Test

The placebo test re-estimates the synthetic control for each donor unit
as a pseudo-treated unit. The treated unit’s RMSPE ratio is ranked among
all units to produce a p-value:

p = \frac{\text{rank of treated ratio}}{N\_{donors} + 1}

where the RMSPE ratio is \frac{RMSPE\_{post}}{RMSPE\_{pre}}.

`task`` ``<-`` `[`sc_placebo_test`](https://sipemu.github.io/eventstudy/reference/sc_placebo_test.md)`(``task``)`` `` ``# p-value`` ``task``$``results``$``placebo``$``p_value`` `` ``# RMSPE ratios for all donor units`` ``task``$``results``$``placebo``$``rmspe_ratios`

You can limit the number of placebo units for faster computation:

`task`` ``<-`` `[`sc_placebo_test`](https://sipemu.github.io/eventstudy/reference/sc_placebo_test.md)`(``task``, n_placebo ``=`` ``5``)`

## References

- Abadie, A. and Gardeazabal, J. (2003). The Economic Costs of Conflict:
  A Case Study of the Basque Country. *American Economic Review*, 93(1),
  113-132.
- Abadie, A., Diamond, A., and Hainmueller, J. (2010). Synthetic Control
  Methods for Comparative Case Studies. *Journal of the American
  Statistical Association*, 105(490), 493-505.
