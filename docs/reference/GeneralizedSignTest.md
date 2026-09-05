# Generalized Sign Test (Cowan 1992)

Adjusts the sign test for the expected proportion of positive abnormal
returns estimated from the estimation window, rather than assuming 0.5.
This accounts for asymmetry in the return distribution.

## Super class

[`TestStatisticBase`](https://sipemu.github.io/eventstudy/reference/TestStatisticBase.md)
-\> `GeneralizedSignTest`

## Public fields

- `name`:

  Short code of the test statistic.

## Methods

### Public methods

- [`GeneralizedSignTest$compute()`](#method-GeneralizedSignTest-compute)

- [`GeneralizedSignTest$clone()`](#method-GeneralizedSignTest-clone)

Inherited methods

- [`TestStatisticBase$initialize()`](https://sipemu.github.io/eventstudy/reference/TestStatisticBase.html#method-initialize)

------------------------------------------------------------------------

### `GeneralizedSignTest$compute()`

Computes the generalized sign test for multiple events.

#### Usage

    GeneralizedSignTest$compute(data_tbl, model)

#### Arguments

- `data_tbl`:

  The data for a multiple event with calculated abnormal returns.

- `model`:

  The fitted model (unused).

------------------------------------------------------------------------

### `GeneralizedSignTest$clone()`

The objects of this class are cloneable with this method.

#### Usage

    GeneralizedSignTest$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
