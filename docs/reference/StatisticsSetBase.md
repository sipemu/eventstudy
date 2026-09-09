# Adds a test statistic to this container.

Base class for test statistic container

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
[`adjust_p_values()`](https://sipemu.github.io/eventstudy/reference/adjust_p_values.md),
[`bootstrap_test()`](https://sipemu.github.io/eventstudy/reference/bootstrap_test.md),
[`car_by_group()`](https://sipemu.github.io/eventstudy/reference/car_by_group.md),
[`car_quantiles()`](https://sipemu.github.io/eventstudy/reference/car_quantiles.md),
[`cross_sectional_regression()`](https://sipemu.github.io/eventstudy/reference/cross_sectional_regression.md),
[`model_diagnostics()`](https://sipemu.github.io/eventstudy/reference/model_diagnostics.md),
[`pretrend_test()`](https://sipemu.github.io/eventstudy/reference/pretrend_test.md),
[`simulate_event_study()`](https://sipemu.github.io/eventstudy/reference/simulate_event_study.md),
[`validate_task()`](https://sipemu.github.io/eventstudy/reference/validate_task.md)

## Public fields

- `tests`:

  Container with the statistical tests. Test container initialization

## Methods

### Public methods

- [`StatisticsSetBase$new()`](#method-StatisticsSetBase-initialize)

- [`StatisticsSetBase$add_test()`](#method-StatisticsSetBase-add_test)

- [`StatisticsSetBase$clone()`](#method-StatisticsSetBase-clone)

------------------------------------------------------------------------

### `StatisticsSetBase$new()`

Initializes a test statistic container. Single event and multiple event
test statistics are collected in separated containers.

#### Usage

    StatisticsSetBase$new(tests = NULL)

#### Arguments

- `tests`:

  List of test statistics.

------------------------------------------------------------------------

### `StatisticsSetBase$add_test()`

#### Usage

    StatisticsSetBase$add_test(test)

#### Arguments

- `test`:

  A single event study tests.

------------------------------------------------------------------------

### `StatisticsSetBase$clone()`

The objects of this class are cloneable with this method.

#### Usage

    StatisticsSetBase$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
