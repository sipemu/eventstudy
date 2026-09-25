# Calendar-Time Portfolio Test

Aggregates event-firm returns into calendar-time portfolios and tests
whether the portfolio intercept (alpha) is significantly different from
zero, using the Brown and Warner (1980, 1985)
crude-dependence-adjustment: the standard deviation of the
ESTIMATION-window cross-event average abnormal return (AAR) series is
used as the (calendar-)time-series standard deviation, so the
denominator is independent of the event window and of any event-window
shock magnitude.

For each relative event day, the test forms an equal-weighted portfolio
of all event firms' abnormal returns and computes \\caltime_t = AAR_t /
sd(AAR\_{estimation})\\, with degrees of freedom `n_estimation_days - 1`
(the number of finite estimation-window AAR observations), exposed as
`attr(result, "caltime_df")`.

## References

Brown, S. J. and Warner, J. B. (1980). Measuring security price
performance. *Journal of Financial Economics*, 8(3), 205–258.

Brown, S. J. and Warner, J. B. (1985). Using daily stock returns: The
case of event studies. *Journal of Financial Economics*, 14(1), 3–31.

## See also

Other eventstudy-statistics:
[`ARTTest`](https://sipemu.github.io/eventstudy/reference/ARTTest.md),
[`BHARTTest`](https://sipemu.github.io/eventstudy/reference/BHARTTest.md),
[`BMPTest`](https://sipemu.github.io/eventstudy/reference/BMPTest.md),
[`CARTTest`](https://sipemu.github.io/eventstudy/reference/CARTTest.md),
[`CSectTTest`](https://sipemu.github.io/eventstudy/reference/CSectTTest.md),
[`GeneralizedSignTest`](https://sipemu.github.io/eventstudy/reference/GeneralizedSignTest.md),
[`KolariPynnonenTest`](https://sipemu.github.io/eventstudy/reference/KolariPynnonenTest.md),
[`MultiEventStatisticsSet`](https://sipemu.github.io/eventstudy/reference/MultiEventStatisticsSet.md),
[`PatellZTest`](https://sipemu.github.io/eventstudy/reference/PatellZTest.md),
[`RankTest`](https://sipemu.github.io/eventstudy/reference/RankTest.md),
[`SignTest`](https://sipemu.github.io/eventstudy/reference/SignTest.md),
[`SingleEventStatisticsSet`](https://sipemu.github.io/eventstudy/reference/SingleEventStatisticsSet.md),
[`StatisticsSetBase`](https://sipemu.github.io/eventstudy/reference/StatisticsSetBase.md),
[`TestStatisticBase`](https://sipemu.github.io/eventstudy/reference/TestStatisticBase.md),
[`adjust_p_values()`](https://sipemu.github.io/eventstudy/reference/adjust_p_values.md),
[`bootstrap_test()`](https://sipemu.github.io/eventstudy/reference/bootstrap_test.md),
[`car_by_group()`](https://sipemu.github.io/eventstudy/reference/car_by_group.md),
[`car_quantiles()`](https://sipemu.github.io/eventstudy/reference/car_quantiles.md),
[`cross_sectional_regression()`](https://sipemu.github.io/eventstudy/reference/cross_sectional_regression.md),
[`model_diagnostics()`](https://sipemu.github.io/eventstudy/reference/model_diagnostics.md),
[`pretrend_test()`](https://sipemu.github.io/eventstudy/reference/pretrend_test.md),
[`simulate_event_study()`](https://sipemu.github.io/eventstudy/reference/simulate_event_study.md),
[`validate_task()`](https://sipemu.github.io/eventstudy/reference/validate_task.md)

## Super class

[`TestStatisticBase`](https://sipemu.github.io/eventstudy/reference/TestStatisticBase.md)
-\> `CalendarTimePortfolioTest`

## Public fields

- `name`:

  Short code of the test statistic.

## Methods

### Public methods

- [`CalendarTimePortfolioTest$compute()`](#method-CalendarTimePortfolioTest-compute)

- [`CalendarTimePortfolioTest$clone()`](#method-CalendarTimePortfolioTest-clone)

Inherited methods

- [`TestStatisticBase$initialize()`](https://sipemu.github.io/eventstudy/reference/TestStatisticBase.html#method-initialize)

------------------------------------------------------------------------

### `CalendarTimePortfolioTest$compute()`

Computes the calendar-time portfolio test.

#### Usage

    CalendarTimePortfolioTest$compute(data_tbl, model)

#### Arguments

- `data_tbl`:

  The data for multiple events with calculated abnormal returns.

- `model`:

  The fitted models (unused directly).

------------------------------------------------------------------------

### `CalendarTimePortfolioTest$clone()`

The objects of this class are cloneable with this method.

#### Usage

    CalendarTimePortfolioTest$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
