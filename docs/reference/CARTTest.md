# Cumulative Abnormal Return T Statistic (CART)

The CAR t-test is a statistical method used to determine whether the
cumulative abnormal return of a security over an event window is
significantly different from zero. This test helps researchers identify
whether the event of interest has a significant impact on the security’s
return over the entire event window, considering the cumulative effects
of the event.

See also <https://eventstudy.de/statistics/ar_car_statistics.html>

## Super class

[`TestStatisticBase`](https://sipemu.github.io/eventstudy/reference/TestStatisticBase.md)
-\> `CARTTest`

## Public fields

- `name`:

  Short code of the test statistic.

## Methods

### Public methods

- [`CARTTest$compute()`](#method-CARTTest-compute)

- [`CARTTest$clone()`](#method-CARTTest-clone)

Inherited methods

- [`TestStatisticBase$initialize()`](https://sipemu.github.io/eventstudy/reference/TestStatisticBase.html#method-initialize)

------------------------------------------------------------------------

### `CARTTest$compute()`

Computes the test CAR test statistics for a single event.

#### Usage

    CARTTest$compute(data_tbl, model)

#### Arguments

- `data_tbl`:

  The data for a single event with calculated abnormal returns.

- `model`:

  The fitted model that includes the necessary information for
  calculating the test statistic.

------------------------------------------------------------------------

### `CARTTest$clone()`

The objects of this class are cloneable with this method.

#### Usage

    CARTTest$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
