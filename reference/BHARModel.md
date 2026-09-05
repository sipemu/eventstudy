# Buy-and-Hold Abnormal Returns (BHAR) Model

Implements Buy-and-Hold Abnormal Returns for long-horizon event studies.
BHAR compounds returns over the event window instead of summing:
\$\$BHAR_i = \prod(1 + R\_{i,t}) - \prod(1 + R\_{benchmark,t})\$\$

The benchmark is the market/index return by default. This model is
appropriate for long-horizon studies (months/years) where compounding
effects matter.

## Super class

[`ModelBase`](https://sipemu.github.io/eventstudy/reference/ModelBase.md)
-\> `BHARModel`

## Public fields

- `model_name`:

  Name of the model.

## Methods

### Public methods

- [`BHARModel$fit()`](#method-BHARModel-fit)

- [`BHARModel$abnormal_returns()`](#method-BHARModel-abnormal_returns)

- [`BHARModel$clone()`](#method-BHARModel-clone)

------------------------------------------------------------------------

### `BHARModel$fit()`

Fit the BHAR model. Computes estimation window statistics.

#### Usage

    BHARModel$fit(data_tbl)

#### Arguments

- `data_tbl`:

  Data frame or tibble.

------------------------------------------------------------------------

### `BHARModel$abnormal_returns()`

Calculate abnormal returns using buy-and-hold compounding.

#### Usage

    BHARModel$abnormal_returns(data_tbl)

#### Arguments

- `data_tbl`:

  Data frame or tibble.

------------------------------------------------------------------------

### `BHARModel$clone()`

The objects of this class are cloneable with this method.

#### Usage

    BHARModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
