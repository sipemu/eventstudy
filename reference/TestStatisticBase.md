# TestStatisticBase

Base (abstract) class for single-event and multi-event test statistics.
Subclass this to plug a custom test statistic into
[`calculate_statistics()`](https://sipemu.github.io/eventstudy/reference/calculate_statistics.md)
/
[`run_event_study()`](https://sipemu.github.io/eventstudy/reference/run_event_study.md).

A subclass must implement `compute(data_tbl, model)`:

- `data_tbl` – a single event's rows with an `abnormal_returns` column
  already populated by the fitted model.

- `model` – the fitted `ModelBase` (or list-mock) instance for that
  event; its `statistics` field carries `sigma`, `degree_of_freedom`,
  `residuals` and (since 2026-09-24) `n_params`.

[`compute()`](https://dplyr.tidyverse.org/reference/compute.html)
returns a tibble with the statistic's result columns for that event;
multi-event subclasses (see `R/multi_event_test_statistics.R`) aggregate
across all events in a group.

`confidence_level` and `confidence_type` are validated and stored at
construction (`confidence_type` must be one of `"two-sided"` (default),
`"less"` or `"greater"`); every p-value computed anywhere in EventStudy
is currently two-sided, so a non-default `confidence_type` is accepted
but ignored, with one warning at construction (A10, 2026-09-24).

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

- `name`:

  Short code of the test statistic.

- `confidence_level`:

  The chosen confidence level.

- `confidence_type`:

  Side of the test. One of `"two-sided"` (default), `"less"` or
  `"greater"`. A10 (2026-09-24): every p-value computed anywhere in
  EventStudy is currently two-sided; a non-default value is validated
  but has NO effect on any computed statistic – it is stored and a
  one-time warning is emitted at construction. One-sided p-values are
  not implemented.

## Methods

### Public methods

- [`TestStatisticBase$new()`](#method-TestStatisticBase-initialize)

- [`TestStatisticBase$compute()`](#method-TestStatisticBase-compute)

- [`TestStatisticBase$clone()`](#method-TestStatisticBase-clone)

------------------------------------------------------------------------

### `TestStatisticBase$new()`

Initializes the test statistic. This includes the confidence level and
the type of the test ('less', greater' or 'two-sided')

#### Usage

    TestStatisticBase$new(confidence_level = 0.95, confidence_type = "two-sided")

#### Arguments

- `confidence_level`:

  The confidence level for the confidence band. Must be anumber between
  0 and 1.

- `confidence_type`:

  Side of the test statistic: `"two-sided"` (default), `"less"` or
  `"greater"`. Every p-value in EventStudy is currently two-sided;
  supplying `"less"` or `"greater"` emits one warning that the value is
  stored but ignored by every compute() method.

------------------------------------------------------------------------

### `TestStatisticBase$compute()`

Computes the test test statistics for a single event.

#### Usage

    TestStatisticBase$compute(data_tbl, model)

#### Arguments

- `data_tbl`:

  The data for a single event with calculated abnormal returns.

- `model`:

  The fitted model that includes the necessary information for
  calculating the test statistic.

------------------------------------------------------------------------

### `TestStatisticBase$clone()`

The objects of this class are cloneable with this method.

#### Usage

    TestStatisticBase$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
