# Linear Factor Model Base

Base class for multi-factor OLS models used in event studies. All factor
models (Market Model, Fama-French, Carhart) share the same estimation
approach: OLS regression of excess returns on factor returns during the
estimation window.

## See also

Other eventstudy-models:
[`BHARModel`](https://sipemu.github.io/eventstudy/reference/BHARModel.md),
[`Carhart4FactorModel`](https://sipemu.github.io/eventstudy/reference/Carhart4FactorModel.md),
[`ComparisonPeriodMeanAdjustedModel`](https://sipemu.github.io/eventstudy/reference/ComparisonPeriodMeanAdjustedModel.md),
[`DCCGARCHModel`](https://sipemu.github.io/eventstudy/reference/DCCGARCHModel.md),
[`FamaFrench3FactorModel`](https://sipemu.github.io/eventstudy/reference/FamaFrench3FactorModel.md),
[`FamaFrench5FactorModel`](https://sipemu.github.io/eventstudy/reference/FamaFrench5FactorModel.md),
[`GARCHModel`](https://sipemu.github.io/eventstudy/reference/GARCHModel.md),
[`LogReturn`](https://sipemu.github.io/eventstudy/reference/LogReturn.md),
[`MarketAdjustedModel`](https://sipemu.github.io/eventstudy/reference/MarketAdjustedModel.md),
[`MarketModel`](https://sipemu.github.io/eventstudy/reference/MarketModel.md),
[`RollingWindowModel`](https://sipemu.github.io/eventstudy/reference/RollingWindowModel.md),
[`SimpleReturn`](https://sipemu.github.io/eventstudy/reference/SimpleReturn.md),
[`VolatilityModel`](https://sipemu.github.io/eventstudy/reference/VolatilityModel.md),
[`VolumeModel`](https://sipemu.github.io/eventstudy/reference/VolumeModel.md)

## Super class

[`ModelBase`](https://sipemu.github.io/eventstudy/reference/ModelBase.md)
-\> `LinearFactorModel`

## Public fields

- `model_name`:

  Name of the model.

- `formula`:

  The regression formula.

- `required_columns`:

  Columns required in the data.

- `use_hac`:

  Logical. Use HAC (Newey-West) standard errors.

- `hac_lag`:

  Integer or NULL. Lag truncation for Newey-West.

## Methods

### Public methods

- [`LinearFactorModel$new()`](#method-LinearFactorModel-initialize)

- [`LinearFactorModel$fit()`](#method-LinearFactorModel-fit)

- [`LinearFactorModel$abnormal_returns()`](#method-LinearFactorModel-abnormal_returns)

- [`LinearFactorModel$clone()`](#method-LinearFactorModel-clone)

------------------------------------------------------------------------

### `LinearFactorModel$new()`

Create a new LinearFactorModel.

#### Usage

    LinearFactorModel$new(use_hac = FALSE, hac_lag = NULL)

#### Arguments

- `use_hac`:

  Logical. Use HAC (Newey-West) standard errors.

- `hac_lag`:

  Integer or NULL. Lag truncation for Newey-West.

------------------------------------------------------------------------

### `LinearFactorModel$fit()`

Fit the linear factor model via OLS on the estimation window.

#### Usage

    LinearFactorModel$fit(data_tbl)

#### Arguments

- `data_tbl`:

  Data frame or tibble containing the data to fit.

------------------------------------------------------------------------

### `LinearFactorModel$abnormal_returns()`

Calculate abnormal returns as observed minus predicted.

#### Usage

    LinearFactorModel$abnormal_returns(data_tbl)

#### Arguments

- `data_tbl`:

  Data frame or tibble.

------------------------------------------------------------------------

### `LinearFactorModel$clone()`

The objects of this class are cloneable with this method.

#### Usage

    LinearFactorModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
