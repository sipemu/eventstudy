# Rank Test (Corrado 1989)

Non-parametric rank test for event studies. Ranks abnormal returns
across the combined estimation and event windows, which is robust to
non-normality of abnormal returns.

## Super class

[`TestStatisticBase`](https://sipemu.github.io/eventstudy/reference/TestStatisticBase.md)
-\> `RankTest`

## Public fields

- `name`:

  Short code of the test statistic.

## Methods

### Public methods

- [`RankTest$compute()`](#method-RankTest-compute)

- [`RankTest$clone()`](#method-RankTest-clone)

Inherited methods

- [`TestStatisticBase$initialize()`](https://sipemu.github.io/eventstudy/reference/TestStatisticBase.html#method-initialize)

------------------------------------------------------------------------

### `RankTest$compute()`

Computes the Corrado rank test for multiple events.

#### Usage

    RankTest$compute(data_tbl, model)

#### Arguments

- `data_tbl`:

  The data for a multiple event with calculated abnormal returns.

- `model`:

  The fitted model (unused).

------------------------------------------------------------------------

### `RankTest$clone()`

The objects of this class are cloneable with this method.

#### Usage

    RankTest$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
