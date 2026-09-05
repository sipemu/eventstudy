# Fama-French Three-Factor Model

Implements the Fama and French (1993) three-factor model: \$\$R_i - R_f
= \alpha + \beta_m (R_m - R_f) + \beta_s SMB + \beta_h HML +
\epsilon\$\$

The data must contain columns: `excess_return` (firm return minus
risk-free), `market_excess` (market return minus risk-free), `smb`, and
`hml`. These can be joined via a factor table in `EventStudyTask`.

## Super classes

[`ModelBase`](https://sipemu.github.io/eventstudy/reference/ModelBase.md)
-\>
[`LinearFactorModel`](https://sipemu.github.io/eventstudy/reference/LinearFactorModel.md)
-\> `FamaFrench3FactorModel`

## Public fields

- `model_name`:

  Name of the model.

- `formula`:

  The three-factor regression formula.

- `required_columns`:

  Required data columns.

## Methods

### Public methods

- [`FamaFrench3FactorModel$new()`](#method-FamaFrench3FactorModel-initialize)

- [`FamaFrench3FactorModel$abnormal_returns()`](#method-FamaFrench3FactorModel-abnormal_returns)

- [`FamaFrench3FactorModel$clone()`](#method-FamaFrench3FactorModel-clone)

Inherited methods

- [`LinearFactorModel$fit()`](https://sipemu.github.io/eventstudy/reference/LinearFactorModel.html#method-fit)

------------------------------------------------------------------------

### `FamaFrench3FactorModel$new()`

Create a new FamaFrench3FactorModel.

#### Usage

    FamaFrench3FactorModel$new(use_hac = FALSE, hac_lag = NULL)

#### Arguments

- `use_hac`:

  Logical. Use HAC (Newey-West) standard errors.

- `hac_lag`:

  Integer or NULL. Lag truncation for Newey-West.

------------------------------------------------------------------------

### `FamaFrench3FactorModel$abnormal_returns()`

Calculate abnormal returns using the three-factor model.

#### Usage

    FamaFrench3FactorModel$abnormal_returns(data_tbl)

#### Arguments

- `data_tbl`:

  Data frame or tibble.

------------------------------------------------------------------------

### `FamaFrench3FactorModel$clone()`

The objects of this class are cloneable with this method.

#### Usage

    FamaFrench3FactorModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
