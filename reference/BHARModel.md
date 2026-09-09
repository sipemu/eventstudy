# Buy-and-Hold Abnormal Returns (BHAR) Model

Implements Buy-and-Hold Abnormal Returns for long-horizon event studies.
BHAR compounds returns over the event window instead of summing:
\$\$BHAR_i = \prod(1 + R\_{i,t}) - \prod(1 + R\_{benchmark,t})\$\$

The benchmark is the market/index return by default. This model is
appropriate for long-horizon studies (months/years) where compounding
effects matter.

## See also

Other eventstudy-models:
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
[`VolatilityModel`](https://sipemu.github.io/eventstudy/reference/VolatilityModel.md),
[`VolumeModel`](https://sipemu.github.io/eventstudy/reference/VolumeModel.md)

## Super class

[`ModelBase`](https://sipemu.github.io/eventstudy/reference/ModelBase.md)
-\> `BHARModel`

## Public fields

- `model_name`:

  Name of the model.

## Methods

### Public methods

- [`BHARModel$fit()`](#method-BHARModel-fit)

- [`BHARModel$abnormal_returns()`](#method-BHARModel-abnormal_returns)

- [`BHARModel$clone()`](#method-BHARModel-clone)

------------------------------------------------------------------------

### `BHARModel$fit()`

Fit the BHAR model. Computes estimation window statistics.

#### Usage

    BHARModel$fit(data_tbl)

#### Arguments

- `data_tbl`:

  Data frame or tibble.

------------------------------------------------------------------------

### `BHARModel$abnormal_returns()`

Calculate abnormal returns using buy-and-hold compounding.

#### Usage

    BHARModel$abnormal_returns(data_tbl)

#### Arguments

- `data_tbl`:

  Data frame or tibble.

------------------------------------------------------------------------

### `BHARModel$clone()`

The objects of this class are cloneable with this method.

#### Usage

    BHARModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
