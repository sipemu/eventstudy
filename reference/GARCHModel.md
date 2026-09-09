# GARCH Model

Event study model using GARCH(1,1) for time-varying volatility
estimation. Uses the rugarch package to fit a GARCH(1,1) model with a
market return regressor in the mean equation during the estimation
window. Abnormal returns are computed as the difference between observed
returns and the GARCH conditional mean. The time-varying sigma from
GARCH can be used for standardized test statistics.

## See also

Other eventstudy-models:
[`BHARModel`](https://sipemu.github.io/eventstudy/reference/BHARModel.md),
[`Carhart4FactorModel`](https://sipemu.github.io/eventstudy/reference/Carhart4FactorModel.md),
[`ComparisonPeriodMeanAdjustedModel`](https://sipemu.github.io/eventstudy/reference/ComparisonPeriodMeanAdjustedModel.md),
[`DCCGARCHModel`](https://sipemu.github.io/eventstudy/reference/DCCGARCHModel.md),
[`FamaFrench3FactorModel`](https://sipemu.github.io/eventstudy/reference/FamaFrench3FactorModel.md),
[`FamaFrench5FactorModel`](https://sipemu.github.io/eventstudy/reference/FamaFrench5FactorModel.md),
[`LinearFactorModel`](https://sipemu.github.io/eventstudy/reference/LinearFactorModel.md),
[`LogReturn`](https://sipemu.github.io/eventstudy/reference/LogReturn.md),
[`MarketAdjustedModel`](https://sipemu.github.io/eventstudy/reference/MarketAdjustedModel.md),
[`MarketModel`](https://sipemu.github.io/eventstudy/reference/MarketModel.md),
[`RollingWindowModel`](https://sipemu.github.io/eventstudy/reference/RollingWindowModel.md),
[`SimpleReturn`](https://sipemu.github.io/eventstudy/reference/SimpleReturn.md),
[`VolatilityModel`](https://sipemu.github.io/eventstudy/reference/VolatilityModel.md),
[`VolumeModel`](https://sipemu.github.io/eventstudy/reference/VolumeModel.md)

## Super class

[`ModelBase`](https://sipemu.github.io/eventstudy/reference/ModelBase.md)
-\> `GARCHModel`

## Public fields

- `model_name`:

  Name of the model.

- `garch_order`:

  GARCH order as c(p, q). Default c(1,1).

## Methods

### Public methods

- [`GARCHModel$fit()`](#method-GARCHModel-fit)

- [`GARCHModel$abnormal_returns()`](#method-GARCHModel-abnormal_returns)

- [`GARCHModel$clone()`](#method-GARCHModel-clone)

------------------------------------------------------------------------

### `GARCHModel$fit()`

Fit the GARCH model on the estimation window.

#### Usage

    GARCHModel$fit(data_tbl)

#### Arguments

- `data_tbl`:

  Data frame or tibble with firm_returns, index_returns,
  estimation_window, event_window columns.

------------------------------------------------------------------------

### `GARCHModel$abnormal_returns()`

Calculate abnormal returns from the GARCH model.

#### Usage

    GARCHModel$abnormal_returns(data_tbl)

#### Arguments

- `data_tbl`:

  Data frame or tibble.

------------------------------------------------------------------------

### `GARCHModel$clone()`

The objects of this class are cloneable with this method.

#### Usage

    GARCHModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
