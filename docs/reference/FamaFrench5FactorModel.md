# Fama-French Five-Factor Model

Implements the Fama and French (2015) five-factor model: \$\$R_i - R_f =
\alpha + \beta_m (R_m - R_f) + \beta_s SMB + \beta_h HML + \beta_r RMW +
\beta_c CMA + \epsilon\$\$

Requires columns: `excess_return`, `market_excess`, `smb`, `hml`, `rmw`,
`cma`.

## Super classes

[`ModelBase`](https://sipemu.github.io/eventstudy/reference/ModelBase.md)
-\>
[`LinearFactorModel`](https://sipemu.github.io/eventstudy/reference/LinearFactorModel.md)
-\> `FamaFrench5FactorModel`

## Public fields

- `model_name`:

  Name of the model.

- `formula`:

  The five-factor regression formula.

- `required_columns`:

  Required data columns.

## Methods

### Public methods

- [`FamaFrench5FactorModel$new()`](#method-FamaFrench5FactorModel-initialize)

- [`FamaFrench5FactorModel$abnormal_returns()`](#method-FamaFrench5FactorModel-abnormal_returns)

- [`FamaFrench5FactorModel$clone()`](#method-FamaFrench5FactorModel-clone)

Inherited methods

- [`LinearFactorModel$fit()`](https://sipemu.github.io/eventstudy/reference/LinearFactorModel.html#method-fit)

------------------------------------------------------------------------

### `FamaFrench5FactorModel$new()`

Create a new FamaFrench5FactorModel.

#### Usage

    FamaFrench5FactorModel$new(use_hac = FALSE, hac_lag = NULL)

#### Arguments

- `use_hac`:

  Logical. Use HAC (Newey-West) standard errors.

- `hac_lag`:

  Integer or NULL. Lag truncation for Newey-West.

------------------------------------------------------------------------

### `FamaFrench5FactorModel$abnormal_returns()`

Calculate abnormal returns using the five-factor model.

#### Usage

    FamaFrench5FactorModel$abnormal_returns(data_tbl)

#### Arguments

- `data_tbl`:

  Data frame or tibble.

------------------------------------------------------------------------

### `FamaFrench5FactorModel$clone()`

The objects of this class are cloneable with this method.

#### Usage

    FamaFrench5FactorModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
