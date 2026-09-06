# Methods: Synthetic Control

## 1. Title & Abstract

This article covers the **synthetic control method (SCM)** for
estimating the effect of a single treated unit against a data-driven
weighted combination of untreated “donor” units. SCM is the tool of
choice when one unit is treated, many candidate controls exist, and no
single control is a credible counterfactual on its own. It is estimated
live here on inline synthetic treated-plus-donor data, explicitly
choosing `method = "optim"` (pure base-R
[`stats::optim`](https://rdrr.io/r/stats/optim.html); the package
default is `"quadprog"` when that optional package is installed).

## 2. When to Use This Method

Use SCM for **comparative case studies**: one region, firm, or market
receives a treatment (a law, a shock, an entry) and you need a
counterfactual. Rather than pick one comparison unit, SCM forms a convex
weighted average of donors that tracks the treated unit’s
**pre-treatment** trajectory, then reads the post-treatment gap as the
effect (Abadie et al. 2010).

## 3. Intuition

Blend the donor pool so the blend’s pre-treatment path lies on top of
the treated unit’s path. If the synthetic unit and the real unit were
indistinguishable before treatment, their divergence afterwards is the
treatment effect. The blend weights are non-negative and sum to one,
keeping the synthetic control inside the donors’ support (no
extrapolation).

## 4. Model & Null Hypothesis

The synthetic counterfactual is a convex combination of donor outcomes,
with weights chosen to minimise pre-period mean squared prediction error
(MSPE):

\hat{Y}^{N}\_{1t} = \sum\_{j=2}^{J+1} w_j\\ Y\_{jt}, \qquad w_j \geq
0,\\ \sum_j w_j = 1,

and the treatment effect is the post-treatment gap:

\text{gap}\_t = Y\_{1t} - \hat{Y}^{N}\_{1t}.

The null hypothesis is H_0: \text{gap}\_t = 0 for t \geq t_0 — the
treated unit tracks its synthetic counterfactual after treatment,
i.e. no effect.

## 5. Assumptions

- **Good pre-treatment fit:** a low pre-period MSPE; a poor fit
  invalidates the counterfactual.
- **Convex hull:** the treated unit’s pre-path lies within the donors’
  span (weights on the simplex, no extrapolation).
- **No interference / spillovers:** donors are unaffected by the
  treatment.
- **No anticipation** before t_0.

## 6. Worked Example

We build inline synthetic data in base R with an explicit local
`set.seed(42)`: 5 donors over 80 periods (long format
`unit, time, outcome`) and a treated unit (`time, outcome`) that tracks
the donor average pre-treatment and jumps after t_0 = 60. We explicitly
pass `method = "optim"` (pure
[`stats::optim`](https://rdrr.io/r/stats/optim.html) L-BFGS-B, no extra
dependency); the package default is `"quadprog"` when that optional
package is available.

[`library`](https://rdrr.io/r/base/library.html)`(`[`EventStudy`](https://github.com/sipemu/eventstudy)`)`` `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``42``)`` `` ``periods`` ``<-`` ``1``:``80`` ``ttime`` ``<-`` ``60`` ``donor_ids`` ``<-`` `[`paste0`](https://rdrr.io/r/base/paste.html)`(``"D"``, ``1``:``5``)`` `` ``donor_data`` ``<-`` `[`do.call`](https://rdrr.io/r/base/do.call.html)`(``rbind``, `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``donor_ids``, ``function``(``d``)`` ``{`` `` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``unit ``=`` ``d``, time ``=`` ``periods``, outcome ``=`` `[`cumsum`](https://rdrr.io/r/base/cumsum.html)`(`[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``80``, ``0.1``, ``1``)``)``)`` ``}``)``)`` `` ``# Treated = donor-average pre-treatment + a post-treatment jump; lengths match 80.`` ``donor_mean`` ``<-`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`tapply`](https://rdrr.io/r/base/tapply.html)`(``donor_data``$``outcome``, ``donor_data``$``time``, ``mean``)``)`` ``post_jump`` ``<-`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(``periods`` ``>=`` ``ttime``, ``0.15`` ``*`` ``(``periods`` ``-`` ``ttime`` ``+`` ``1``)``, ``0``)`` ``treated_data`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`` `` time ``=`` ``periods``,`` `` outcome ``=`` ``donor_mean`` ``+`` ``post_jump`` ``+`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``80``, ``0``, ``0.3``)`` ``)`` `` ``task`` ``<-`` `[`SyntheticControlTask`](https://sipemu.github.io/eventstudy/reference/SyntheticControlTask.md)`$``new``(``treated_data``, ``donor_data``, treatment_time ``=`` ``ttime``)`` ``res`` ``<-`` `[`estimate_synthetic_control`](https://sipemu.github.io/eventstudy/reference/estimate_synthetic_control.md)`(``task``, method ``=`` ``"optim"``)`` ``# base R stats::optim`

The `quadprog` quadratic-programming solver is available when that
optional package is installed:

`res_qp`` ``<-`` `[`estimate_synthetic_control`](https://sipemu.github.io/eventstudy/reference/estimate_synthetic_control.md)`(``task``, method ``=`` ``"quadprog"``)`

## 7. Rendered Table

`knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(`` `` `[`head`](https://rdrr.io/r/utils/head.html)`(``res``$``results``$``trajectory``, ``10``)``,`` `` caption ``=`` ``"Treated vs synthetic trajectory with the estimated gap (first 10 periods)."`` ``)`

| time | treated | synthetic |     gap |
|-----:|--------:|----------:|--------:|
|    1 |  0.7993 |    0.3574 |  0.4419 |
|    2 |  0.1998 |    0.3995 | -0.1996 |
|    3 |  0.9621 |    0.8875 |  0.0746 |
|    4 |  1.3397 |    1.2460 |  0.0937 |
|    5 |  1.2972 |    1.3703 | -0.0731 |
|    6 |  1.9037 |    2.1786 | -0.2748 |
|    7 |  2.7954 |    2.6887 |  0.1068 |
|    8 |  3.1292 |    2.8430 |  0.2862 |
|    9 |  4.0566 |    3.4895 |  0.5672 |
|   10 |  3.2637 |    3.7390 | -0.4752 |

Treated vs synthetic trajectory with the estimated gap (first 10
periods). {.table}

## 8. Rendered Plot

`traj`` ``<-`` ``res``$``results``$``trajectory`` `[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``traj``$``time``, ``traj``$``gap``, type ``=`` ``"l"``,`` `` xlab ``=`` ``"Time"``, ylab ``=`` ``"Gap (treated - synthetic)"``,`` `` main ``=`` ``"Synthetic control gap"``)`` `[`abline`](https://rdrr.io/r/graphics/abline.html)`(``v ``=`` ``60``, lty ``=`` ``2``)`` `[`abline`](https://rdrr.io/r/graphics/abline.html)`(``h ``=`` ``0``, col ``=`` ``"grey60"``)`

![Gap between the treated unit and its synthetic control over
time.](methods-synthetic-control_files/figure-html/results-plot-1.png)

Gap between the treated unit and its synthetic control over time.

## 9. Interpretation

Before t_0 = 60 the gap should hover near zero — the synthetic control
tracks the treated unit, confirming a credible counterfactual (low
pre-period MSPE in `res$results$pre_mspe`). After t_0 the gap opens up,
recovering the built-in post-treatment jump; its average is the ATT in
`res$results$att`. A gap that was already non-zero *before* treatment
would signal a poor fit and an untrustworthy estimate. Inference
typically compares this treated-unit gap to the distribution of placebo
gaps obtained by reassigning treatment to each donor.

## References

Abadie, Alberto, Alexis Diamond, and Jens Hainmueller. 2010. “Synthetic
Control Methods for Comparative Case Studies: Estimating the Effect of
California’s Tobacco Control Program.” *Journal of the American
Statistical Association* 105 (490): 493–505.
