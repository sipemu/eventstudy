# Market Model

The Market Model is a widely used method in event studies to estimate
the expected returns of a stock and calculate its abnormal returns
during an event window. The model is based on a simple linear regression
framework and captures the relationship between a stock’s return and the
return of a market index, such as the S&P 500 or the Dow Jones
Industrial Average. The underlying assumption of the Market Model is
that a stock’s return is primarily influenced by market movements, along
with a stock-specific idiosyncratic component.

## Super class

[`ModelBase`](https://sipemu.github.io/eventstudy/reference/ModelBase.md)
-\> `MarketModel`

## Public fields

- `model_name`:

  Name of the model.

- `formula`:

  The formula applied for calculating the market model

- `use_hac`:

  Logical. Use HAC (Newey-West) standard errors.

- `hac_lag`:

  Integer or NULL. Lag truncation for Newey-West. NULL uses the
  automatic bandwidth selection.

## Methods

### Public methods

- [`MarketModel$new()`](#method-MarketModel-initialize)

- [`MarketModel$set_formula()`](#method-MarketModel-set_formula)

- [`MarketModel$fit()`](#method-MarketModel-fit)

- [`MarketModel$abnormal_returns()`](#method-MarketModel-abnormal_returns)

- [`MarketModel$clone()`](#method-MarketModel-clone)

------------------------------------------------------------------------

### `MarketModel$new()`

Create a new MarketModel.

#### Usage

    MarketModel$new(use_hac = FALSE, hac_lag = NULL)

#### Arguments

- `use_hac`:

  Logical. Use HAC (Newey-West) standard errors. Requires the sandwich
  package.

- `hac_lag`:

  Integer or NULL. Lag truncation for Newey-West.

------------------------------------------------------------------------

### `MarketModel$set_formula()`

Set the formula

#### Usage

    MarketModel$set_formula(formula)

#### Arguments

- `formula`:

  A formula.

------------------------------------------------------------------------

### `MarketModel$fit()`

Fit the model with given data.

#### Usage

    MarketModel$fit(data_tbl)

#### Arguments

- `data_tbl`:

  Data frame or tibble containing the data to fit.

------------------------------------------------------------------------

### `MarketModel$abnormal_returns()`

Calculate the abnormal returns with given data.

#### Usage

    MarketModel$abnormal_returns(data_tbl)

#### Arguments

- `data_tbl`:

  Data frame or tibble containing the data to calculate abnormal
  returns.

------------------------------------------------------------------------

### `MarketModel$clone()`

The objects of this class are cloneable with this method.

#### Usage

    MarketModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
