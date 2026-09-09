# Volatility Event Study Model

Model for volatility-based event studies. Computes abnormal volatility
as the ratio of event-window squared returns to estimation-window
variance. The abnormal measure is written to the `abnormal_returns`
column for compatibility with existing test statistics.

## See also

Other eventstudy-models:
[`BHARModel`](https://sipemu.github.io/eventstudy/reference/BHARModel.md),
[`Carhart4FactorModel`](https://sipemu.github.io/eventstudy/reference/Carhart4FactorModel.md),
[`ComparisonPeriodMeanAdjustedModel`](https://sipemu.github.io/eventstudy/reference/ComparisonPeriodMeanAdjustedModel.md),
[`DCCGARCHModel`](https://sipemu.github.io/eventstudy/reference/DCCGARCHModel.md),
[`FamaFrench3FactorModel`](https://sipemu.github.io/eventstudy/reference/FamaFrench3FactorModel.md),
[`FamaFrench5FactorModel`](https://sipemu.github.io/eventstudy/reference/FamaFrench5FactorModel.md),
[`GARCHModel`](https://sipemu.github.io/eventstudy/reference/GARCHModel.md),
[`LinearFactorModel`](https://sipemu.github.io/eventstudy/reference/LinearFactorModel.md),
[`LogReturn`](https://sipemu.github.io/eventstudy/reference/LogReturn.md),
[`MarketAdjustedModel`](https://sipemu.github.io/eventstudy/reference/MarketAdjustedModel.md),
[`MarketModel`](https://sipemu.github.io/eventstudy/reference/MarketModel.md),
[`RollingWindowModel`](https://sipemu.github.io/eventstudy/reference/RollingWindowModel.md),
[`SimpleReturn`](https://sipemu.github.io/eventstudy/reference/SimpleReturn.md),
[`VolumeModel`](https://sipemu.github.io/eventstudy/reference/VolumeModel.md)

## Super class

[`ModelBase`](https://sipemu.github.io/eventstudy/reference/ModelBase.md)
-\> `VolatilityModel`

## Public fields

- `model_name`:

  Name of the model.

## Methods

### Public methods

- [`VolatilityModel$fit()`](#method-VolatilityModel-fit)

- [`VolatilityModel$abnormal_returns()`](#method-VolatilityModel-abnormal_returns)

- [`VolatilityModel$clone()`](#method-VolatilityModel-clone)

------------------------------------------------------------------------

### `VolatilityModel$fit()`

Fit the volatility model. Estimates expected variance from estimation
window.

#### Usage

    VolatilityModel$fit(data_tbl)

#### Arguments

- `data_tbl`:

  Data frame or tibble.

------------------------------------------------------------------------

### `VolatilityModel$abnormal_returns()`

Calculate abnormal volatility (squared returns / expected variance - 1).

#### Usage

    VolatilityModel$abnormal_returns(data_tbl)

#### Arguments

- `data_tbl`:

  Data frame or tibble.

------------------------------------------------------------------------

### `VolatilityModel$clone()`

The objects of this class are cloneable with this method.

#### Usage

    VolatilityModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
