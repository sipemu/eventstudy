# Volume Event Study Model

Model for volume-based event studies. Computes abnormal volume as the
difference between observed volume and expected volume from the
estimation window mean. The data must contain a `firm_volume` column
(and optionally `index_volume` for market-adjusted volume).

The existing test statistics infrastructure works on the
`abnormal_returns` column, so this model writes abnormal volume to that
same column for compatibility.

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
