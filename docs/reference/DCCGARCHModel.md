# DCC-GARCH Model

Event study model using Dynamic Conditional Correlation GARCH for
time-varying beta estimation. Requires the rmgarch package. The
bivariate DCC-GARCH model captures both time-varying volatility and
time-varying correlation between firm and market returns, yielding a
time-varying beta: \\\beta_t = Cov(R\_{firm}, R\_{market})\_t /
Var(R\_{market})\_t\\.

## Super class

[`ModelBase`](https://sipemu.github.io/eventstudy/reference/ModelBase.md)
-\> `DCCGARCHModel`

## Public fields

- `model_name`:

  Name of the model.

- `garch_order`:

  GARCH order for each univariate model. Default c(1,1).

- `dcc_order`:

  DCC order. Default c(1,1).

## Methods

### Public methods

- [`DCCGARCHModel$new()`](#method-DCCGARCHModel-initialize)

- [`DCCGARCHModel$fit()`](#method-DCCGARCHModel-fit)

- [`DCCGARCHModel$abnormal_returns()`](#method-DCCGARCHModel-abnormal_returns)

- [`DCCGARCHModel$clone()`](#method-DCCGARCHModel-clone)

------------------------------------------------------------------------

### `DCCGARCHModel$new()`

Create a new DCCGARCHModel.

#### Usage

    DCCGARCHModel$new(garch_order = c(1, 1), dcc_order = c(1, 1))

#### Arguments

- `garch_order`:

  GARCH(p,q) order for univariate models.

- `dcc_order`:

  DCC(a,b) order.

------------------------------------------------------------------------

### `DCCGARCHModel$fit()`

Fit the DCC-GARCH model on the estimation window.

#### Usage

    DCCGARCHModel$fit(data_tbl)

#### Arguments

- `data_tbl`:

  Data frame or tibble with firm_returns, index_returns,
  estimation_window, event_window columns.

------------------------------------------------------------------------

### `DCCGARCHModel$abnormal_returns()`

Calculate abnormal returns using the last conditional beta.

#### Usage

    DCCGARCHModel$abnormal_returns(data_tbl)

#### Arguments

- `data_tbl`:

  Data frame or tibble.

------------------------------------------------------------------------

### `DCCGARCHModel$clone()`

The objects of this class are cloneable with this method.

#### Usage

    DCCGARCHModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
