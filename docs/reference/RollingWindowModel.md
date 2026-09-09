# Rolling Window Model

Event study model with time-varying parameters estimated via a rolling
OLS window over the estimation period. The last rolling window's
parameters are used for event-window prediction. This captures parameter
instability that is common in financial return data.

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
[`SimpleReturn`](https://sipemu.github.io/eventstudy/reference/SimpleReturn.md),
[`VolatilityModel`](https://sipemu.github.io/eventstudy/reference/VolatilityModel.md),
[`VolumeModel`](https://sipemu.github.io/eventstudy/reference/VolumeModel.md)

## Super class

[`ModelBase`](https://sipemu.github.io/eventstudy/reference/ModelBase.md)
-\> `RollingWindowModel`

## Public fields

- `model_name`:

  Name of the model.

- `window_size`:

  Rolling window size. Default 60.

- `min_obs`:

  Minimum observations required. Default 30.

## Methods

### Public methods

- [`RollingWindowModel$new()`](#method-RollingWindowModel-initialize)

- [`RollingWindowModel$fit()`](#method-RollingWindowModel-fit)

- [`RollingWindowModel$abnormal_returns()`](#method-RollingWindowModel-abnormal_returns)

- [`RollingWindowModel$clone()`](#method-RollingWindowModel-clone)

------------------------------------------------------------------------

### `RollingWindowModel$new()`

Create a new RollingWindowModel.

#### Usage

    RollingWindowModel$new(window_size = 60L, min_obs = 30L)

#### Arguments

- `window_size`:

  Size of the rolling window.

- `min_obs`:

  Minimum observations for a valid window.

------------------------------------------------------------------------

### `RollingWindowModel$fit()`

Fit the rolling window model on the estimation window.

#### Usage

    RollingWindowModel$fit(data_tbl)

#### Arguments

- `data_tbl`:

  Data frame or tibble with firm_returns, index_returns,
  estimation_window, event_window columns.

------------------------------------------------------------------------

### `RollingWindowModel$abnormal_returns()`

Calculate abnormal returns using the last rolling window parameters.

#### Usage

    RollingWindowModel$abnormal_returns(data_tbl)

#### Arguments

- `data_tbl`:

  Data frame or tibble.

------------------------------------------------------------------------

### `RollingWindowModel$clone()`

The objects of this class are cloneable with this method.

#### Usage

    RollingWindowModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
