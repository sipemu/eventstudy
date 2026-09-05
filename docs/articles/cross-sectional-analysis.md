# Cross-Sectional Analysis of Event Study Results

## Introduction

After running an event study, a natural follow-up question is: **why do
some firms react more strongly than others?** Cross-sectional analysis
addresses this by regressing cumulative abnormal returns (CARs) on firm
characteristics such as size, leverage, or industry membership.

The EventStudy package provides three functions for cross-sectional
analysis:

| Function | Purpose |
|----|----|
| [`cross_sectional_regression()`](https://sipemu.github.io/eventstudy/reference/cross_sectional_regression.md) | OLS regression of CARs on firm characteristics |
| [`car_by_group()`](https://sipemu.github.io/eventstudy/reference/car_by_group.md) | Compare CARs across groups (t-test or ANOVA) |
| [`car_quantiles()`](https://sipemu.github.io/eventstudy/reference/car_quantiles.md) | Compute quantiles of the CAR distribution |

Additionally,
[`plot_car_distribution()`](https://sipemu.github.io/eventstudy/reference/plot_car_distribution.md)
provides visual exploration of the CAR distribution.

[`library`](https://rdrr.io/r/base/library.html)`(`[`EventStudy`](https://github.com/sipemu/eventstudy)`)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`dplyr`](https://dplyr.tidyverse.org)`)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`ggplot2`](https://ggplot2.tidyverse.org)`)`

## Running the Event Study First

Cross-sectional analysis requires a completed event study. Let’s set up
a multi-firm study with two groups:

[`set.seed`](https://rdrr.io/r/base/Random.html)`(``42``)`` `` ``n`` ``<-`` ``300`` ``dates`` ``<-`` `[`format`](https://rdrr.io/r/base/format.html)`(`[`seq`](https://rdrr.io/r/base/seq.html)`(`[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(``"2014-06-01"``)``, by ``=`` ``"day"``, length.out ``=`` ``n``)``,`` `` ``"%d.%m.%Y"``)`` `` ``# Four firms, two groups`` ``firms`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"FIRM_A"``, ``"FIRM_B"``, ``"FIRM_C"``, ``"FIRM_D"``)`` ``firm_tbl`` ``<-`` ``purrr``::`[`map_dfr`](https://purrr.tidyverse.org/reference/map_dfr.html)`(``firms``, ``function``(``sym``)`` ``{`` `` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` symbol ``=`` ``sym``,`` `` date ``=`` ``dates``,`` `` adjusted ``=`` ``100`` ``*`` `[`cumprod`](https://rdrr.io/r/base/cumsum.html)`(``1`` ``+`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``, ``0.0003``, ``0.015``)``)`` `` ``)`` ``}``)`` `` ``index_tbl`` ``<-`` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` symbol ``=`` ``"INDEX_1"``,`` `` date ``=`` ``dates``,`` `` adjusted ``=`` ``1000`` ``*`` `[`cumprod`](https://rdrr.io/r/base/cumsum.html)`(``1`` ``+`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``, ``0.0002``, ``0.012``)``)`` ``)`` `` ``request_tbl`` ``<-`` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` event_id ``=`` ``1``:``4``,`` `` firm_symbol ``=`` ``firms``,`` `` index_symbol ``=`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``"INDEX_1"``, ``4``)``,`` `` event_date ``=`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``dates``[``200``]``, ``4``)``,`` `` group ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Large"``, ``"Large"``, ``"Small"``, ``"Small"``)``,`` `` event_window_start ``=`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``-``10L``, ``4``)``,`` `` event_window_end ``=`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``10L``, ``4``)``,`` `` shift_estimation_window ``=`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``-``11L``, ``4``)``,`` `` estimation_window_length ``=`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``150L``, ``4``)`` ``)`` `` ``task`` ``<-`` `[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)`$``new``(``firm_tbl``, ``index_tbl``, ``request_tbl``)`` ``task`` ``<-`` `[`run_event_study`](https://sipemu.github.io/eventstudy/reference/run_event_study.md)`(``task``)`

## Cross-Sectional Regression

### Basic Usage

[`cross_sectional_regression()`](https://sipemu.github.io/eventstudy/reference/cross_sectional_regression.md)
regresses CARs on firm-level explanatory variables. The `data` argument
must include an `event_id` column for merging:

`firm_chars`` ``<-`` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` event_id ``=`` ``1``:``4``,`` `` log_market_cap ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``10.2``, ``11.5``, ``8.3``, ``9.1``)``,`` `` leverage ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.3``, ``0.5``, ``0.2``, ``0.4``)``,`` `` r_and_d ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.05``, ``0.02``, ``0.08``, ``0.06``)`` ``)`` `` ``result`` ``<-`` `[`cross_sectional_regression`](https://sipemu.github.io/eventstudy/reference/cross_sectional_regression.md)`(`` `` ``task``,`` `` formula ``=`` ``~`` ``log_market_cap`` ``+`` ``leverage``,`` `` data ``=`` ``firm_chars`` ``)`` `` `[`print`](https://rdrr.io/r/base/print.html)`(``result``)`` ``#> Cross-Sectional Regression of CARs`` ``#> ===================================`` ``#> N: 4`` ``#> R-squared: 0.85`` ``#> Adj. R-squared: 0.55`` ``#>`` ``#> Coefficients:`` ``#> estimate std.error statistic p.value`` ``#> (Intercept) 0.1234 0.0567 2.178 0.1615`` ``#> log_market_cap -0.0123 0.0054 -2.278 0.1500`` ``#> leverage -0.0456 0.0321 -1.421 0.2912`

The formula follows R conventions: the left-hand side is ignored (CAR is
always the dependent variable), and the right-hand side specifies the
explanatory variables.

### Robust Standard Errors

By default, heteroskedasticity-consistent (HC1) standard errors are
computed using the `sandwich` package. To use OLS standard errors
instead:

`# HC1 robust standard errors (default)`` ``result_robust`` ``<-`` `[`cross_sectional_regression`](https://sipemu.github.io/eventstudy/reference/cross_sectional_regression.md)`(`` `` ``task``, formula ``=`` ``~`` ``log_market_cap`` ``+`` ``leverage``,`` `` data ``=`` ``firm_chars``, robust ``=`` ``TRUE`` ``)`` `` ``# Plain OLS standard errors`` ``result_ols`` ``<-`` `[`cross_sectional_regression`](https://sipemu.github.io/eventstudy/reference/cross_sectional_regression.md)`(`` `` ``task``, formula ``=`` ``~`` ``log_market_cap`` ``+`` ``leverage``,`` `` data ``=`` ``firm_chars``, robust ``=`` ``FALSE`` ``)`

Install `sandwich` for robust standard errors:
`install.packages("sandwich")`.

### Custom CAR Window

By default, the full event window is used to compute CARs. To focus on a
specific sub-window (e.g., the three-day window around the event):

`result_3day`` ``<-`` `[`cross_sectional_regression`](https://sipemu.github.io/eventstudy/reference/cross_sectional_regression.md)`(`` `` ``task``,`` `` formula ``=`` ``~`` ``log_market_cap`` ``+`` ``leverage``,`` `` data ``=`` ``firm_chars``,`` `` car_window ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``-``1``, ``1``)`` ``# relative indices`` ``)`

### Accessing the Underlying Data

The result object includes the merged CAR + characteristics data, which
is useful for custom analyses:

`# Merged data with CARs and firm characteristics`` ``result``$``car_data`` ``#> # A tibble: 4 x 6`` ``#> event_id firm_symbol group car log_market_cap leverage`` ``#> <int> <chr> <chr> <dbl> <dbl> <dbl>`` ``#> ...`` `` ``# The fitted lm object`` `[`summary`](https://rdrr.io/r/base/summary.html)`(``result``$``model``)`

## Group Comparisons

### car_by_group()

When events naturally fall into groups (e.g., large vs. small firms,
industries, treated vs. control),
[`car_by_group()`](https://sipemu.github.io/eventstudy/reference/car_by_group.md)
tests whether CARs differ across groups:

`group_result`` ``<-`` `[`car_by_group`](https://sipemu.github.io/eventstudy/reference/car_by_group.md)`(``task``)`` `` ``# Summary statistics by group`` ``group_result``$``summary`` ``#> # A tibble: 2 x 7`` ``#> group n mean_car sd_car median_car min_car max_car`` ``#> <chr> <int> <dbl> <dbl> <dbl> <dbl> <dbl>`` ``#> Large 2 0.012 0.008 0.012 0.006 0.018`` ``#> Small 2 -0.005 0.003 -0.005 -0.007 -0.003`` `` ``# Test result`` ``group_result``$``test_name`` ``#> [1] "Welch Two-Sample t-test"`` `` ``group_result``$``test`

The function automatically selects the appropriate test: - **2 groups**:
Welch two-sample t-test - **3+ groups**: One-way ANOVA (Welch)

### Custom CAR Window for Groups

`group_3day`` ``<-`` `[`car_by_group`](https://sipemu.github.io/eventstudy/reference/car_by_group.md)`(``task``, car_window ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``-``1``, ``1``)``)`` ``group_3day``$``summary`

## CAR Quantiles

[`car_quantiles()`](https://sipemu.github.io/eventstudy/reference/car_quantiles.md)
provides a quick summary of the CAR distribution:

[`car_quantiles`](https://sipemu.github.io/eventstudy/reference/car_quantiles.md)`(``task``)`` ``#> 5% 25% 50% 75% 95%`` ``#> -0.02 0.00 0.01 0.02 0.03`` `` ``# Custom quantiles`` `[`car_quantiles`](https://sipemu.github.io/eventstudy/reference/car_quantiles.md)`(``task``, probs ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.01``, ``0.1``, ``0.5``, ``0.9``, ``0.99``)``)`` `` ``# Quantiles for a specific CAR window`` `[`car_quantiles`](https://sipemu.github.io/eventstudy/reference/car_quantiles.md)`(``task``, car_window ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``5``)``)`

## Visualizing the CAR Distribution

### Histogram

[`plot_car_distribution`](https://sipemu.github.io/eventstudy/reference/plot_car_distribution.md)`(``task``)`

### Histogram by Group

[`plot_car_distribution`](https://sipemu.github.io/eventstudy/reference/plot_car_distribution.md)`(``task``, by_group ``=`` ``TRUE``,`` `` title ``=`` ``"CAR Distribution by Firm Size"``)`

### Custom CAR Window

[`plot_car_distribution`](https://sipemu.github.io/eventstudy/reference/plot_car_distribution.md)`(``task``, car_window ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``-``1``, ``1``)``,`` `` title ``=`` ``"3-Day CAR Distribution"``)`

## A Complete Cross-Sectional Workflow

Putting it all together—run the event study, then systematically explore
and explain cross-sectional variation:

`# 1. Run the event study`` ``task`` ``<-`` `[`EventStudyTask`](https://sipemu.github.io/eventstudy/reference/EventStudyTask.md)`$``new``(``firm_tbl``, ``index_tbl``, ``request_tbl``)`` ``task`` ``<-`` `[`run_event_study`](https://sipemu.github.io/eventstudy/reference/run_event_study.md)`(``task``)`` `` ``# 2. Summary statistics`` `[`car_quantiles`](https://sipemu.github.io/eventstudy/reference/car_quantiles.md)`(``task``)`` `` ``# 3. Group comparison`` ``group_result`` ``<-`` `[`car_by_group`](https://sipemu.github.io/eventstudy/reference/car_by_group.md)`(``task``)`` `[`print`](https://rdrr.io/r/base/print.html)`(``group_result``$``summary``)`` `` ``# 4. Visual exploration`` `[`plot_car_distribution`](https://sipemu.github.io/eventstudy/reference/plot_car_distribution.md)`(``task``, by_group ``=`` ``TRUE``)`` `` ``# 5. Cross-sectional regression`` ``firm_chars`` ``<-`` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`` `` event_id ``=`` ``1``:``4``,`` `` log_market_cap ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``10.2``, ``11.5``, ``8.3``, ``9.1``)``,`` `` leverage ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.3``, ``0.5``, ``0.2``, ``0.4``)`` ``)`` `` ``result`` ``<-`` `[`cross_sectional_regression`](https://sipemu.github.io/eventstudy/reference/cross_sectional_regression.md)`(`` `` ``task``,`` `` formula ``=`` ``~`` ``log_market_cap`` ``+`` ``leverage``,`` `` data ``=`` ``firm_chars`` ``)`` `[`print`](https://rdrr.io/r/base/print.html)`(``result``)`` `` ``# 6. Robustness: different CAR windows`` ``windows`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``-``1``, ``1``)``, `[`c`](https://rdrr.io/r/base/c.html)`(``-``5``, ``5``)``, `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``10``)``)`` ``purrr``::`[`walk`](https://purrr.tidyverse.org/reference/map.html)`(``windows``, ``function``(``w``)`` ``{`` `` ``r`` ``<-`` `[`cross_sectional_regression`](https://sipemu.github.io/eventstudy/reference/cross_sectional_regression.md)`(`` `` ``task``, formula ``=`` ``~`` ``log_market_cap`` ``+`` ``leverage``,`` `` data ``=`` ``firm_chars``, car_window ``=`` ``w`` `` ``)`` `` `[`cat`](https://rdrr.io/r/base/cat.html)`(``"\nCAR window ["``, ``w``[``1``]``, ``","``, ``w``[``2``]``, ``"]:\n"``)`` `` `[`print`](https://rdrr.io/r/base/print.html)`(``r``$``coefficients``)`` ``}``)`

## Tips and Best Practices

1.  **Sample size matters.** Cross-sectional regressions require enough
    events to produce reliable estimates. With only a handful of firms,
    results will be fragile.

2.  **Use robust standard errors.** Financial returns are
    heteroskedastic. Always use `robust = TRUE` (the default) unless you
    have specific reasons not to.

3.  **Try multiple CAR windows.** Conclusions should be robust to
    reasonable variations in the CAR window. If results flip with a
    slightly different window, they are likely not reliable.

4.  **Report group comparisons alongside regressions.** The
    [`car_by_group()`](https://sipemu.github.io/eventstudy/reference/car_by_group.md)
    function provides an intuitive summary that complements the
    regression table.

5.  **Check the distribution.** Use
    [`plot_car_distribution()`](https://sipemu.github.io/eventstudy/reference/plot_car_distribution.md)
    to look for outliers, skewness, or bimodality before running
    regressions.

## References

- MacKinlay, A. C. (1997). Event Studies in Economics and Finance.
  *Journal of Economic Literature*, 35(1), 13–39.
- Kothari, S. P. & Warner, J. B. (2007). Econometrics of Event Studies.
  In B. E. Eckbo (Ed.), *Handbook of Corporate Finance: Empirical
  Corporate Finance* (Vol. 1, pp. 3–36). Elsevier.
