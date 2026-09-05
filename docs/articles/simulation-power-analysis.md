# Monte Carlo Simulation and Power Analysis

## Introduction

Before running an event study, it is important to understand whether
your research design has enough statistical power to detect an effect of
a given magnitude. The
[`simulate_event_study()`](https://sipemu.github.io/eventstudy/reference/simulate_event_study.md)
function provides Monte Carlo simulation to estimate:

- **Power**: The probability of rejecting the null hypothesis when the
  alternative is true
- **Size**: The actual rejection rate under the null (should be close to
  the nominal \alpha)

## Basic Usage

[`library`](https://rdrr.io/r/base/library.html)`(`[`EventStudy`](https://github.com/sipemu/eventstudy)`)`` `` ``# Size analysis: no abnormal return injected`` ``size_result`` ``<-`` `[`simulate_event_study`](https://sipemu.github.io/eventstudy/reference/simulate_event_study.md)`(`` `` n_events ``=`` ``20``,`` `` event_window ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``-``5``, ``5``)``,`` `` estimation_window_length ``=`` ``120``,`` `` abnormal_return ``=`` ``0``, ``# H0 is true`` `` n_simulations ``=`` ``1000``,`` `` seed ``=`` ``42`` ``)`` `[`print`](https://rdrr.io/r/base/print.html)`(``size_result``)`` ``#> Event Study Simulation`` ``#> N events: 20`` ``#> Event window: [ -5 , 5 ]`` ``#> Abnormal return: 0`` ``#> Test statistic: CSectT`` ``#> Alpha: 0.05`` ``#> N simulations: 1000`` ``#> Power (day 0): 0.048`

The power at day 0 should be close to the nominal significance level
(0.05) when no abnormal return is injected.

## Power Analysis

To estimate power, inject a non-zero abnormal return:

`# Power analysis: 2% abnormal return on event day`` ``power_result`` ``<-`` `[`simulate_event_study`](https://sipemu.github.io/eventstudy/reference/simulate_event_study.md)`(`` `` n_events ``=`` ``20``,`` `` event_window ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``-``5``, ``5``)``,`` `` estimation_window_length ``=`` ``120``,`` `` abnormal_return ``=`` ``0.02``, ``# 2% effect`` `` n_simulations ``=`` ``1000``,`` `` seed ``=`` ``42`` ``)`` `[`print`](https://rdrr.io/r/base/print.html)`(``power_result``)`` ``#> Power (day 0): 0.723`

## Varying Effect Size

Compare power across different abnormal return magnitudes:

`effects`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``0.005``, ``0.01``, ``0.02``, ``0.05``)`` ``power_curve`` ``<-`` `[`sapply`](https://rdrr.io/r/base/lapply.html)`(``effects``, ``function``(``ar``)`` ``{`` `` ``res`` ``<-`` `[`simulate_event_study`](https://sipemu.github.io/eventstudy/reference/simulate_event_study.md)`(`` `` n_events ``=`` ``20``,`` `` abnormal_return ``=`` ``ar``,`` `` n_simulations ``=`` ``500``,`` `` seed ``=`` ``42`` `` ``)`` `` ``res``$``power`` ``}``)`` `` `[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``effects``, ``power_curve``, type ``=`` ``"b"``,`` `` xlab ``=`` ``"Abnormal Return"``, ylab ``=`` ``"Power"``,`` `` main ``=`` ``"Power Curve"``)`` `[`abline`](https://rdrr.io/r/graphics/abline.html)`(``h ``=`` ``0.05``, lty ``=`` ``2``, col ``=`` ``"grey"``)`

## Varying Sample Size

`sample_sizes`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``5``, ``10``, ``20``, ``50``, ``100``)`` ``power_by_n`` ``<-`` `[`sapply`](https://rdrr.io/r/base/lapply.html)`(``sample_sizes``, ``function``(``n``)`` ``{`` `` ``res`` ``<-`` `[`simulate_event_study`](https://sipemu.github.io/eventstudy/reference/simulate_event_study.md)`(`` `` n_events ``=`` ``n``,`` `` abnormal_return ``=`` ``0.01``,`` `` n_simulations ``=`` ``500``,`` `` seed ``=`` ``42`` `` ``)`` `` ``res``$``power`` ``}``)`` `` `[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``sample_sizes``, ``power_by_n``, type ``=`` ``"b"``,`` `` xlab ``=`` ``"Number of Events"``, ylab ``=`` ``"Power"``,`` `` main ``=`` ``"Power vs Sample Size (AR = 1%)"``)`

## Customizing the DGP

You can control the data-generating process parameters:

`result`` ``<-`` `[`simulate_event_study`](https://sipemu.github.io/eventstudy/reference/simulate_event_study.md)`(`` `` n_events ``=`` ``20``,`` `` abnormal_return ``=`` ``0.01``,`` `` n_simulations ``=`` ``500``,`` `` dgp_params ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`` `` alpha ``=`` ``0.0001``, ``# drift`` `` beta ``=`` ``1.2``, ``# market exposure`` `` sigma_firm ``=`` ``0.015``, ``# idiosyncratic volatility`` `` sigma_market ``=`` ``0.02`` ``# market volatility`` `` ``)``,`` `` seed ``=`` ``42`` ``)`

Higher idiosyncratic volatility makes it harder to detect abnormal
returns, reducing power.

## Different Test Statistics

Compare the power of different test statistics:

`tests`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"CSectT"``, ``"PatellZ"``, ``"BMP"``, ``"SignT"``, ``"RankT"``)`` ``power_by_test`` ``<-`` `[`sapply`](https://rdrr.io/r/base/lapply.html)`(``tests``, ``function``(``ts``)`` ``{`` `` ``res`` ``<-`` `[`simulate_event_study`](https://sipemu.github.io/eventstudy/reference/simulate_event_study.md)`(`` `` n_events ``=`` ``20``,`` `` abnormal_return ``=`` ``0.01``,`` `` test_statistic ``=`` ``ts``,`` `` n_simulations ``=`` ``500``,`` `` seed ``=`` ``42`` `` ``)`` `` ``res``$``power`` ``}``)`` `` `[`barplot`](https://rdrr.io/r/graphics/barplot.html)`(``power_by_test``, main ``=`` ``"Power by Test Statistic (AR = 1%)"``,`` `` ylab ``=`` ``"Power"``, las ``=`` ``2``)`` `[`abline`](https://rdrr.io/r/graphics/abline.html)`(``h ``=`` ``0.05``, lty ``=`` ``2``, col ``=`` ``"grey"``)`

## Rejection by Day

The `rejection_by_day` component shows the rejection rate at each
event-window day:

`result`` ``<-`` `[`simulate_event_study`](https://sipemu.github.io/eventstudy/reference/simulate_event_study.md)`(`` `` n_events ``=`` ``20``,`` `` abnormal_return ``=`` ``0.02``,`` `` n_simulations ``=`` ``500``,`` `` seed ``=`` ``42`` ``)`` `` ``result``$``rejection_by_day`` ``#> # A tibble: 11 x 2`` ``#> relative_index rejection_rate`` ``#> <int> <dbl>`` ``#> 1 -5 0.048`` ``#> 2 -4 0.052`` ``#> ...`` ``#> 6 0 0.723 <-- event day`` ``#> ...`

Only the event day (relative_index = 0) should show elevated rejection.
Other days should be near the nominal level.

## Seed Reproducibility

Results are fully reproducible when a seed is set:

`r1`` ``<-`` `[`simulate_event_study`](https://sipemu.github.io/eventstudy/reference/simulate_event_study.md)`(``n_events ``=`` ``10``, n_simulations ``=`` ``100``, seed ``=`` ``42``)`` ``r2`` ``<-`` `[`simulate_event_study`](https://sipemu.github.io/eventstudy/reference/simulate_event_study.md)`(``n_events ``=`` ``10``, n_simulations ``=`` ``100``, seed ``=`` ``42``)`` `[`identical`](https://rdrr.io/r/base/identical.html)`(``r1``$``power``, ``r2``$``power``)`` ``# TRUE`
