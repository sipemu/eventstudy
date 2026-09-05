# Carhart Four-Factor Model

Implements the Carhart (1997) four-factor model, which extends the
Fama-French three-factor model with a momentum factor: \$\$R_i - R_f =
\alpha + \beta_m (R_m - R_f) + \beta_s SMB + \beta_h HML + \beta\_{mom}
MOM + \epsilon\$\$

Requires columns: `excess_return`, `market_excess`, `smb`, `hml`, `mom`.

## Super classes

[`ModelBase`](https://sipemu.github.io/eventstudy/reference/ModelBase.md)
-\>
[`LinearFactorModel`](https://sipemu.github.io/eventstudy/reference/LinearFactorModel.md)
-\> `Carhart4FactorModel`

## Public fields

- `model_name`:

  Name of the model.

- `formula`:

  The four-factor regression formula.

- `required_columns`:

  Required data columns.

## Methods

### Public methods

- [`Carhart4FactorModel$new()`](#method-Carhart4FactorModel-initialize)

- [`Carhart4FactorModel$abnormal_returns()`](#method-Carhart4FactorModel-abnormal_returns)

- [`Carhart4FactorModel$clone()`](#method-Carhart4FactorModel-clone)

Inherited methods

- [`LinearFactorModel$fit()`](https://sipemu.github.io/eventstudy/reference/LinearFactorModel.html#method-fit)

------------------------------------------------------------------------

### `Carhart4FactorModel$new()`

Create a new Carhart4FactorModel.

#### Usage

    Carhart4FactorModel$new(use_hac = FALSE, hac_lag = NULL)

#### Arguments

- `use_hac`:

  Logical. Use HAC (Newey-West) standard errors.

- `hac_lag`:

  Integer or NULL. Lag truncation for Newey-West.

------------------------------------------------------------------------

### `Carhart4FactorModel$abnormal_returns()`

Calculate abnormal returns using the four-factor model.

#### Usage

    Carhart4FactorModel$abnormal_returns(data_tbl)

#### Arguments

- `data_tbl`:

  Data frame or tibble.

------------------------------------------------------------------------

### `Carhart4FactorModel$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Carhart4FactorModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
