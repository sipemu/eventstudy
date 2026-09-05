# Creating Custom Test Statistics

## Introduction

The EventStudy package supports two types of test statistics:

1.  **Single-event statistics** – Computed for each event individually
    (e.g., AR t-test, CAR t-test)
2.  **Multi-event statistics** – Computed across all events in a group
    (e.g., Cross-Sectional t-test, Patell Z, Sign test)

Both types inherit from `TestStatisticBase` and implement a
[`compute()`](https://dplyr.tidyverse.org/reference/compute.html)
method. This vignette walks through creating custom test statistics of
each type.

## The TestStatisticBase Interface

Every test statistic must:

1.  Inherit from `TestStatisticBase`
2.  Set a `name` field (short code used as column name in results)
3.  Implement `compute(data_tbl, model)` returning a tibble

For single-event tests, `data_tbl` contains one event’s data and `model`
is the fitted model for that event. For multi-event tests, `data_tbl`
contains all events in a group and `model` is a nested tibble of all
models.

## Example: Wilcoxon Signed-Rank Test (Single Event)

A non-parametric test for whether abnormal returns in the event window
are symmetrically distributed around zero:

[`library`](https://rdrr.io/r/base/library.html)`(`[`EventStudy`](https://github.com/sipemu/eventstudy)`)`` `` ``WilcoxonARTest`` ``<-`` ``R6``::`[`R6Class`](https://r6.r-lib.org/reference/R6Class.html)`(``"WilcoxonARTest"``,`` `` inherit ``=`` ``TestStatisticBase``,`` `` public ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`` `` name ``=`` ``"WilcoxonAR"``,`` `` `` compute ``=`` ``function``(``data_tbl``, ``model``)`` ``{`` `` ``event_data`` ``<-`` ``data_tbl`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` ``dplyr``::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``event_window`` ``==`` ``1``)`` `` `` ``ar`` ``<-`` ``event_data``$``abnormal_returns`` `` `` ``# Wilcoxon signed-rank test against 0`` `` ``test_result`` ``<-`` `[`wilcox.test`](https://rdrr.io/r/stats/wilcox.test.html)`(``ar``, mu ``=`` ``0``, conf.int ``=`` ``TRUE``)`` `` `` ``tibble``::`[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` n_obs ``=`` `[`length`](https://rdrr.io/r/base/length.html)`(``ar``)``,`` `` V_statistic ``=`` ``test_result``$``statistic``,`` `` p_value ``=`` ``test_result``$``p.value``,`` `` median_ar ``=`` `[`median`](https://rdrr.io/r/stats/median.html)`(``ar``, na.rm ``=`` ``TRUE``)``,`` `` conf_low ``=`` ``test_result``$``conf.int``[``1``]``,`` `` conf_high ``=`` ``test_result``$``conf.int``[``2``]`` `` ``)`` `` ``}`` `` ``)`` ``)`

## Example: Kolmogorov-Smirnov Test (Multi-Event)

A non-parametric test comparing the distribution of AARs against a
normal distribution:

`KSTest`` ``<-`` ``R6``::`[`R6Class`](https://r6.r-lib.org/reference/R6Class.html)`(``"KSTest"``,`` `` inherit ``=`` ``TestStatisticBase``,`` `` public ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`` `` name ``=`` ``"KS"``,`` `` `` compute ``=`` ``function``(``data_tbl``, ``model``)`` ``{`` `` ``aar_stats`` ``<-`` ``data_tbl`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` ``dplyr``::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``event_window`` ``==`` ``1``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` ``dplyr``::`[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``relative_index``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` ``dplyr``::`[`summarise`](https://dplyr.tidyverse.org/reference/summarise.html)`(`` `` aar ``=`` `[`mean`](https://rdrr.io/r/base/mean.html)`(``abnormal_returns``, na.rm ``=`` ``TRUE``)``,`` `` n_events ``=`` ``dplyr``::`[`n`](https://dplyr.tidyverse.org/reference/context.html)`(``)``,`` `` n_pos ``=`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``abnormal_returns`` ``>`` ``0``, na.rm ``=`` ``TRUE``)``,`` `` n_neg ``=`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``abnormal_returns`` ``<=`` ``0``, na.rm ``=`` ``TRUE``)``,`` `` .groups ``=`` ``"drop"`` `` ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``caar ``=`` `[`cumsum`](https://rdrr.io/r/base/cumsum.html)`(``aar``)``)`` `` `` ``# KS test of AARs against normal`` `` ``ks`` ``<-`` `[`ks.test`](https://rdrr.io/r/stats/ks.test.html)`(``aar_stats``$``aar``, ``"pnorm"``,`` `` mean ``=`` ``0``, sd ``=`` `[`sd`](https://rdrr.io/r/stats/sd.html)`(``aar_stats``$``aar``)``)`` `` `` ``aar_stats`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`` `` ks_D ``=`` ``ks``$``statistic``,`` `` ks_pvalue ``=`` ``ks``$``p.value`` `` ``)`` `` ``}`` `` ``)`` ``)`

## Registering Custom Statistics

Add your test statistic to a statistics set:

`# Single-event: add to SingleEventStatisticsSet`` ``single_stats`` ``<-`` `[`SingleEventStatisticsSet`](https://sipemu.github.io/eventstudy/reference/SingleEventStatisticsSet.md)`$``new``(``)`` ``single_stats``$``add_test``(``WilcoxonARTest``$``new``(``)``)`` `` ``# Multi-event: add to MultiEventStatisticsSet`` ``multi_stats`` ``<-`` `[`MultiEventStatisticsSet`](https://sipemu.github.io/eventstudy/reference/MultiEventStatisticsSet.md)`$``new``(``)`` ``multi_stats``$``add_test``(``KSTest``$``new``(``)``)`` `` ``# Or create a custom set from scratch`` ``custom_single`` ``<-`` `[`StatisticsSetBase`](https://sipemu.github.io/eventstudy/reference/StatisticsSetBase.md)`$``new``(`` `` tests ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`[`ARTTest`](https://sipemu.github.io/eventstudy/reference/ARTTest.md)`$``new``(``)``, `[`CARTTest`](https://sipemu.github.io/eventstudy/reference/CARTTest.md)`$``new``(``)``, ``WilcoxonARTest``$``new``(``)``)`` ``)`

## Using Custom Statistics in the Pipeline

`params`` ``<-`` `[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md)`$``new``(`` `` single_event_statistics ``=`` ``single_stats``,`` `` multi_event_statistics ``=`` ``multi_stats`` ``)`` `` ``task`` ``<-`` `[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)`$``new``(``firm_data``, ``index_data``, ``request_data``)`` ``task`` ``<-`` `[`run_event_study`](https://sipemu.github.io/eventstudy/reference/run_event_study.md)`(``task``, ``params``)`` `` ``# Results appear as columns in the task data`` ``task``$``data_tbl``$``WilcoxonAR`` ``# single-event results`` ``task``$``aar_caar_tbl``$``KS`` ``# multi-event results`

## Built-in Test Statistics

The package includes the following built-in test statistics:

**Single-event:** - `ARTTest` – Abnormal Return t-test - `CARTTest` –
Cumulative Abnormal Return t-test - `BHARTTest` – Buy-and-Hold Abnormal
Return t-test

**Multi-event:** - `CSectTTest` – Cross-Sectional t-test (AAR/CAAR) -
`PatellZTest` – Patell standardized residual test - `SignTest` – Simple
sign test - `GeneralizedSignTest` – Cowan (1992) generalized sign test -
`RankTest` – Corrado (1989) rank test - `BMPTest` – Boehmer, Musumeci &
Poulsen (1991) test - `CalendarTimePortfolioTest` – Calendar-time
portfolio approach

## Key Points

- Inherit from `TestStatisticBase`
- Set a unique `name` field (becomes the column name in results)
- Return a tibble from
  [`compute()`](https://dplyr.tidyverse.org/reference/compute.html)
- For multi-event stats, include `relative_index`, `aar`, `caar`, and
  `car_window` columns for compatibility with plotting functions
- Use `add_test()` to add to existing statistics sets
