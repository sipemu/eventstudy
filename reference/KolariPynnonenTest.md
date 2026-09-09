# Kolari-Pynnönen Adjusted BMP Test

Adjusts the BMP (Boehmer, Musumeci, Poulsen 1991) test for
cross-sectional correlation of abnormal returns using the Kolari and
Pynnönen (2010) correction. The adjustment scales the BMP statistic by a
factor that accounts for the average pairwise correlation of
standardized abnormal residuals in the estimation window.

## References

Kolari, J. W. and Pynnönen, S. (2010). Event Study Testing with
Cross-sectional Correlation of Abnormal Returns. *The Review of
Financial Studies*, 23(11), 3996–4025.

## See also

Other eventstudy-statistics:
[`ARTTest`](https://sipemu.github.io/eventstudy/reference/ARTTest.md),
[`BHARTTest`](https://sipemu.github.io/eventstudy/reference/BHARTTest.md),
[`BMPTest`](https://sipemu.github.io/eventstudy/reference/BMPTest.md),
[`CARTTest`](https://sipemu.github.io/eventstudy/reference/CARTTest.md),
[`CSectTTest`](https://sipemu.github.io/eventstudy/reference/CSectTTest.md),
[`CalendarTimePortfolioTest`](https://sipemu.github.io/eventstudy/reference/CalendarTimePortfolioTest.md),
[`GeneralizedSignTest`](https://sipemu.github.io/eventstudy/reference/GeneralizedSignTest.md),
[`MultiEventStatisticsSet`](https://sipemu.github.io/eventstudy/reference/MultiEventStatisticsSet.md),
[`PatellZTest`](https://sipemu.github.io/eventstudy/reference/PatellZTest.md),
[`RankTest`](https://sipemu.github.io/eventstudy/reference/RankTest.md),
[`SignTest`](https://sipemu.github.io/eventstudy/reference/SignTest.md),
[`SingleEventStatisticsSet`](https://sipemu.github.io/eventstudy/reference/SingleEventStatisticsSet.md),
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

[`TestStatisticBase`](https://sipemu.github.io/eventstudy/reference/TestStatisticBase.md)
-\> `KolariPynnonenTest`

## Public fields

- `name`:

  Short code of the test statistic.

## Methods

### Public methods

- [`KolariPynnonenTest$compute()`](#method-KolariPynnonenTest-compute)

- [`KolariPynnonenTest$clone()`](#method-KolariPynnonenTest-clone)

Inherited methods

- [`TestStatisticBase$initialize()`](https://sipemu.github.io/eventstudy/reference/TestStatisticBase.html#method-initialize)

------------------------------------------------------------------------

### `KolariPynnonenTest$compute()`

Computes the Kolari-Pynnönen adjusted BMP test.

#### Usage

    KolariPynnonenTest$compute(data_tbl, model)

#### Arguments

- `data_tbl`:

  The data for a multiple event with calculated abnormal returns.

- `model`:

  The fitted model containing sigma estimates.

------------------------------------------------------------------------

### `KolariPynnonenTest$clone()`

The objects of this class are cloneable with this method.

#### Usage

    KolariPynnonenTest$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
