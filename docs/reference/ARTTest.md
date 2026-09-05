# Abnormal Return T Statistic (ART)

The AR t-test is a statistical method used to determine whether the
abnormal return of a security on a specific day is significantly
different from zero. This test helps researchers identify whether the
event of interest has a significant impact on the security’s return at a
particular point in time.

See also <https://eventstudy.de/statistics/ar_car_statistics.html>

## Super class

[`TestStatisticBase`](https://sipemu.github.io/eventstudy/reference/TestStatisticBase.md)
-\> `ARTTest`

## Public fields

- `name`:

  Short code of the test statistic.

## Methods

### Public methods

- [`ARTTest$compute()`](#method-ARTTest-compute)

- [`ARTTest$clone()`](#method-ARTTest-clone)

Inherited methods

- [`TestStatisticBase$initialize()`](https://sipemu.github.io/eventstudy/reference/TestStatisticBase.html#method-initialize)

------------------------------------------------------------------------

### `ARTTest$compute()`

Computes the test AR test statistics for a single event.

#### Usage

    ARTTest$compute(data_tbl, model)

#### Arguments

- `data_tbl`:

  The data for a single event with calculated abnormal returns.

- `model`:

  The fitted model that includes the necessary information for
  calculating the test statistic.

------------------------------------------------------------------------

### `ARTTest$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ARTTest$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
