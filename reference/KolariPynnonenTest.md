# Kolari-Pynnönen Adjusted BMP Test

Adjusts the BMP (Boehmer, Musumeci, Poulsen 1991) test for
cross-sectional correlation of abnormal returns using the Kolari and
Pynnönen (2010) correction. The adjustment scales the BMP statistic by a
factor that accounts for the average pairwise correlation of
standardized abnormal residuals in the estimation window.

## References

Kolari, J. W. and Pynnönen, S. (2010). Event Study Testing with
Cross-sectional Correlation of Abnormal Returns. *The Review of
Financial Studies*, 23(11), 3996–4025.

## Super class

[`TestStatisticBase`](https://sipemu.github.io/eventstudy/reference/TestStatisticBase.md)
-\> `KolariPynnonenTest`

## Public fields

- `name`:

  Short code of the test statistic.

## Methods

### Public methods

- [`KolariPynnonenTest$compute()`](#method-KolariPynnonenTest-compute)

- [`KolariPynnonenTest$clone()`](#method-KolariPynnonenTest-clone)

Inherited methods

- [`TestStatisticBase$initialize()`](https://sipemu.github.io/eventstudy/reference/TestStatisticBase.html#method-initialize)

------------------------------------------------------------------------

### `KolariPynnonenTest$compute()`

Computes the Kolari-Pynnönen adjusted BMP test.

#### Usage

    KolariPynnonenTest$compute(data_tbl, model)

#### Arguments

- `data_tbl`:

  The data for a multiple event with calculated abnormal returns.

- `model`:

  The fitted model containing sigma estimates.

------------------------------------------------------------------------

### `KolariPynnonenTest$clone()`

The objects of this class are cloneable with this method.

#### Usage

    KolariPynnonenTest$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
