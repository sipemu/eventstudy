# Volume Event Study Model

Model for volume-based event studies. Computes abnormal volume as the
difference between observed volume and expected volume from the
estimation window mean. The data must contain a `firm_volume` column
(and optionally `index_volume` for market-adjusted volume).

The existing test statistics infrastructure works on the
`abnormal_returns` column, so this model writes abnormal volume to that
same column for compatibility.

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
[`VolatilityModel`](https://sipemu.github.io/eventstudy/reference/VolatilityModel.md)

## Super class

[`ModelBase`](https://sipemu.github.io/eventstudy/reference/ModelBase.md)
-\> `VolumeModel`

## Public fields

- `model_name`:

  Name of the model.

- `log_transform`:

  Whether to log-transform volume. Default TRUE.

## Methods

### Public methods

- [`VolumeModel$new()`](#method-VolumeModel-initialize)

- [`VolumeModel$fit()`](#method-VolumeModel-fit)

- [`VolumeModel$abnormal_returns()`](#method-VolumeModel-abnormal_returns)

- [`VolumeModel$clone()`](#method-VolumeModel-clone)

------------------------------------------------------------------------

### `VolumeModel$new()`

Create a new VolumeModel.

#### Usage

    VolumeModel$new(log_transform = TRUE)

#### Arguments

- `log_transform`:

  Whether to log-transform volume before analysis.

------------------------------------------------------------------------

### `VolumeModel$fit()`

Fit the volume model. Computes expected volume from estimation window.

#### Usage

    VolumeModel$fit(data_tbl)

#### Arguments

- `data_tbl`:

  Data frame or tibble with firm_volume column.

------------------------------------------------------------------------

### `VolumeModel$abnormal_returns()`

Calculate abnormal volume.

#### Usage

    VolumeModel$abnormal_returns(data_tbl)

#### Arguments

- `data_tbl`:

  Data frame or tibble.

------------------------------------------------------------------------

### `VolumeModel$clone()`

The objects of this class are cloneable with this method.

#### Usage

    VolumeModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
