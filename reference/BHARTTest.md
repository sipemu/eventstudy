# Buy-and-Hold Abnormal Return T Test (BHARTTest)

Tests whether the BHAR for a single event is significantly different
from zero. The BHAR is the difference between compounded firm returns
and compounded benchmark returns over the event window.

## Super class

[`TestStatisticBase`](https://sipemu.github.io/eventstudy/reference/TestStatisticBase.md)
-\> `BHARTTest`

## Public fields

- `name`:

  Short code of the test statistic.

## Methods

### Public methods

- [`BHARTTest$compute()`](#method-BHARTTest-compute)

- [`BHARTTest$clone()`](#method-BHARTTest-clone)

Inherited methods

- [`TestStatisticBase$initialize()`](https://sipemu.github.io/eventstudy/reference/TestStatisticBase.html#method-initialize)

------------------------------------------------------------------------

### `BHARTTest$compute()`

Computes the BHAR t test for a single event.

#### Usage

    BHARTTest$compute(data_tbl, model)

#### Arguments

- `data_tbl`:

  The data for a single event with calculated abnormal returns.

- `model`:

  The fitted model.

------------------------------------------------------------------------

### `BHARTTest$clone()`

The objects of this class are cloneable with this method.

#### Usage

    BHARTTest$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
