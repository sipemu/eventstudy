# Cross-Sectional T Test (CSectTTest)

The Cross-Sectional Test (CSect T) is a statistical tool employed in
event studies to evaluate the null hypothesis that the average abnormal
return at the event date is zero. The test statistic is distributed as
\\t\_{N-1}\\, where N is the number of events in that group.

See also <https://eventstudy.de/statistics/aar_caar_statistics.html>

## Super class

[`TestStatisticBase`](https://sipemu.github.io/eventstudy/reference/TestStatisticBase.md)
-\> `CSectTTest`

## Public fields

- `name`:

  Short code of the test statistic.

## Methods

### Public methods

- [`CSectTTest$compute()`](#method-CSectTTest-compute)

- [`CSectTTest$clone()`](#method-CSectTTest-clone)

Inherited methods

- [`TestStatisticBase$initialize()`](https://sipemu.github.io/eventstudy/reference/TestStatisticBase.html#method-initialize)

------------------------------------------------------------------------

### `CSectTTest$compute()`

Computes the AAR/CAAR cross-sectional t test statistics.

#### Usage

    CSectTTest$compute(data_tbl, model)

#### Arguments

- `data_tbl`:

  The data for a multiple event with calculated abnormal returns.

- `model`:

  The fitted model: not necessary for this test statistics.

------------------------------------------------------------------------

### `CSectTTest$clone()`

The objects of this class are cloneable with this method.

#### Usage

    CSectTTest$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
