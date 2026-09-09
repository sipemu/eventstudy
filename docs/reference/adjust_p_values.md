# Adjust P-Values for Multiple Testing

Computes adjusted p-values for AAR and CAAR test statistics across the
event window, correcting for the multiple comparisons problem. Supports
all methods available in
[`p.adjust`](https://rdrr.io/r/stats/p.adjust.html).

## Usage

``` r
adjust_p_values(task, method = "BH", stat_name = "CSectT", group = NULL)
```

## Arguments

- task:

  A fitted EventStudyTask with `aar_caar_tbl` populated.

- method:

  Adjustment method passed to
  [`p.adjust`](https://rdrr.io/r/stats/p.adjust.html). Common choices:
  `"BH"` (Benjamini-Hochberg, default), `"bonferroni"`, `"holm"`,
  `"hochberg"`, `"BY"`, `"none"`.

- stat_name:

  Name of the multi-event test statistic to adjust. Must match a column
  name in `task$aar_caar_tbl`. Default `"CSectT"`.

- group:

  Optional group name to filter. If NULL, adjusts all groups.

## Value

A tibble with columns from the original test statistic result plus
`p_raw_aar`, `p_adj_aar`, `p_raw_caar`, `p_adj_caar`.

## See also

Other eventstudy-statistics:
[`ARTTest`](https://sipemu.github.io/eventstudy/reference/ARTTest.md),
[`BHARTTest`](https://sipemu.github.io/eventstudy/reference/BHARTTest.md),
[`BMPTest`](https://sipemu.github.io/eventstudy/reference/BMPTest.md),
[`CARTTest`](https://sipemu.github.io/eventstudy/reference/CARTTest.md),
[`CSectTTest`](https://sipemu.github.io/eventstudy/reference/CSectTTest.md),
[`CalendarTimePortfolioTest`](https://sipemu.github.io/eventstudy/reference/CalendarTimePortfolioTest.md),
[`GeneralizedSignTest`](https://sipemu.github.io/eventstudy/reference/GeneralizedSignTest.md),
[`KolariPynnonenTest`](https://sipemu.github.io/eventstudy/reference/KolariPynnonenTest.md),
[`MultiEventStatisticsSet`](https://sipemu.github.io/eventstudy/reference/MultiEventStatisticsSet.md),
[`PatellZTest`](https://sipemu.github.io/eventstudy/reference/PatellZTest.md),
[`RankTest`](https://sipemu.github.io/eventstudy/reference/RankTest.md),
[`SignTest`](https://sipemu.github.io/eventstudy/reference/SignTest.md),
[`SingleEventStatisticsSet`](https://sipemu.github.io/eventstudy/reference/SingleEventStatisticsSet.md),
[`StatisticsSetBase`](https://sipemu.github.io/eventstudy/reference/StatisticsSetBase.md),
[`bootstrap_test()`](https://sipemu.github.io/eventstudy/reference/bootstrap_test.md),
[`car_by_group()`](https://sipemu.github.io/eventstudy/reference/car_by_group.md),
[`car_quantiles()`](https://sipemu.github.io/eventstudy/reference/car_quantiles.md),
[`cross_sectional_regression()`](https://sipemu.github.io/eventstudy/reference/cross_sectional_regression.md),
[`model_diagnostics()`](https://sipemu.github.io/eventstudy/reference/model_diagnostics.md),
[`pretrend_test()`](https://sipemu.github.io/eventstudy/reference/pretrend_test.md),
[`simulate_event_study()`](https://sipemu.github.io/eventstudy/reference/simulate_event_study.md),
[`validate_task()`](https://sipemu.github.io/eventstudy/reference/validate_task.md)
