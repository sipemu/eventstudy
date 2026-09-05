# Sign Test for Multiple Events

Tests whether the proportion of positive abnormal returns differs
significantly from 0.5 under the null hypothesis. The test statistic is
approximately distributed as N(0, 1).

## Super class

[`TestStatisticBase`](https://sipemu.github.io/eventstudy/reference/TestStatisticBase.md)
-\> `SignTest`

## Public fields

- `name`:

  Short code of the test statistic.

## Methods

### Public methods

- [`SignTest$compute()`](#method-SignTest-compute)

- [`SignTest$clone()`](#method-SignTest-clone)

Inherited methods

- [`TestStatisticBase$initialize()`](https://sipemu.github.io/eventstudy/reference/TestStatisticBase.html#method-initialize)

------------------------------------------------------------------------

### `SignTest$compute()`

Computes the sign test for multiple events.

#### Usage

    SignTest$compute(data_tbl, model)

#### Arguments

- `data_tbl`:

  The data for a multiple event with calculated abnormal returns.

- `model`:

  The fitted model (unused).

------------------------------------------------------------------------

### `SignTest$clone()`

The objects of this class are cloneable with this method.

#### Usage

    SignTest$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
