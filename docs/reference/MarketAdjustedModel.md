# Market Adjusted Model

The Market Adjusted Model is another simple approach used in event
studies to estimate the expected returns of a stock and calculate its
abnormal returns during an event window. This model is less complex than
the Market Model, as it assumes that a stock’s expected return is equal
to the market return, without considering any stock-specific factors.
The Market Adjusted Model is particularly useful in situations where the
estimation of individual stock parameters (such as alpha and beta) is
not feasible or desired, and a basic benchmark for comparison is needed.

## Super class

[`ModelBase`](https://sipemu.github.io/eventstudy/reference/ModelBase.md)
-\> `MarketAdjustedModel`

## Public fields

- `model_name`:

  Name of the model.

## Methods

### Public methods

- [`MarketAdjustedModel$fit()`](#method-MarketAdjustedModel-fit)

- [`MarketAdjustedModel$abnormal_returns()`](#method-MarketAdjustedModel-abnormal_returns)

- [`MarketAdjustedModel$clone()`](#method-MarketAdjustedModel-clone)

------------------------------------------------------------------------

### `MarketAdjustedModel$fit()`

fit Fit the model with given data.

#### Usage

    MarketAdjustedModel$fit(data_tbl)

#### Arguments

- `data_tbl`:

  Data frame or tibble containing the data to fit.

------------------------------------------------------------------------

### `MarketAdjustedModel$abnormal_returns()`

abnormal_returns Calculate the abnormal returns with given data.

#### Usage

    MarketAdjustedModel$abnormal_returns(data_tbl)

#### Arguments

- `data_tbl`:

  Data frame or tibble containing the data to calculate abnormal
  returns.

------------------------------------------------------------------------

### `MarketAdjustedModel$clone()`

The objects of this class are cloneable with this method.

#### Usage

    MarketAdjustedModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
