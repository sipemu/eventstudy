# Patell or Standardized Residual Test (PatellZTest)

The Patell or Standardized Residual Test is a statistical tool employed
in event studies to evaluate the null hypothesis that the average
abnormal return at the event date is zero. The test statistics is
approximately distributed as N(0, 1).

See also <https://eventstudy.de/statistics/aar_caar_statistics.html>

## Super class

[`TestStatisticBase`](https://sipemu.github.io/eventstudy/reference/TestStatisticBase.md)
-\> `PatellZTest`

## Public fields

- `name`:

  Short code of the test statistic.

## Methods

### Public methods

- [`PatellZTest$compute()`](#method-PatellZTest-compute)

- [`PatellZTest$clone()`](#method-PatellZTest-clone)

Inherited methods

- [`TestStatisticBase$initialize()`](https://sipemu.github.io/eventstudy/reference/TestStatisticBase.html#method-initialize)

------------------------------------------------------------------------

### `PatellZTest$compute()`

Computes the Patell Z test statistics for multiple events.

#### Usage

    PatellZTest$compute(data_tbl, model)

#### Arguments

- `data_tbl`:

  The data for a multiple event with calculated abnormal returns.

- `model`:

  The fitted model.

------------------------------------------------------------------------

### `PatellZTest$clone()`

The objects of this class are cloneable with this method.

#### Usage

    PatellZTest$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
