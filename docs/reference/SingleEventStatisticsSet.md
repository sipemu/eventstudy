# Single Event Statistic Set

Contains a set of single event test statistics as, e.g., AR and CAR t
test. These are the default test statistics.

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
[`StatisticsSetBase`](https://sipemu.github.io/eventstudy/reference/StatisticsSetBase.md),
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

[`StatisticsSetBase`](https://sipemu.github.io/eventstudy/reference/StatisticsSetBase.md)
-\> `SingleEventStatisticsSet`

## Public fields

- `tests`:

  Container with the statistical tests.

## Methods

### Public methods

- [`SingleEventStatisticsSet$clone()`](#method-SingleEventStatisticsSet-clone)

Inherited methods

- [`StatisticsSetBase$add_test()`](https://sipemu.github.io/eventstudy/reference/StatisticsSetBase.html#method-add_test)
- [`StatisticsSetBase$initialize()`](https://sipemu.github.io/eventstudy/reference/StatisticsSetBase.html#method-initialize)

------------------------------------------------------------------------

### `SingleEventStatisticsSet$clone()`

The objects of this class are cloneable with this method.

#### Usage

    SingleEventStatisticsSet$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
