# BMP Test (Boehmer, Musumeci, Poulsen 1991)

Standardized cross-sectional test that is robust to event-induced
variance increases. The BMP test standardizes abnormal returns by their
forecast error corrected standard deviation, then applies a
cross-sectional t-test to these standardized residuals.

## Super class

[`TestStatisticBase`](https://sipemu.github.io/eventstudy/reference/TestStatisticBase.md)
-\> `BMPTest`

## Public fields

- `name`:

  Short code of the test statistic.

## Methods

### Public methods

- [`BMPTest$compute()`](#method-BMPTest-compute)

- [`BMPTest$clone()`](#method-BMPTest-clone)

Inherited methods

- [`TestStatisticBase$initialize()`](https://sipemu.github.io/eventstudy/reference/TestStatisticBase.html#method-initialize)

------------------------------------------------------------------------

### `BMPTest$compute()`

Computes the BMP standardized cross-sectional test.

#### Usage

    BMPTest$compute(data_tbl, model)

#### Arguments

- `data_tbl`:

  The data for a multiple event with calculated abnormal returns.

- `model`:

  The fitted model containing sigma estimates.

------------------------------------------------------------------------

### `BMPTest$clone()`

The objects of this class are cloneable with this method.

#### Usage

    BMPTest$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
