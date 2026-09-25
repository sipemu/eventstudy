# ModelBase

Base (abstract) class for return models used inside an EventStudyTask.
Each single event gets its own model instance, fitted on that event's
estimation-window data alone. Subclass this to plug a custom return
model into
[`run_event_study()`](https://sipemu.github.io/eventstudy/reference/run_event_study.md)
/
[`fit_model()`](https://sipemu.github.io/eventstudy/reference/fit_model.md).

A subclass must implement two methods:

- `fit(data_tbl)` – fit the model on `data_tbl` (a single event's rows,
  estimation and event window together) and populate
  `private$.statistics` plus set `private$.is_fitted`.

- `abnormal_returns(data_tbl)` – return `data_tbl` with an
  `abnormal_returns` column added for the event window.

The `statistics` active binding exposes a named list consumed by the
test statistics layer. At minimum a subclass should populate: `sigma`,
`degree_of_freedom`, `residuals`, `forecast_error_corrected_sigma` and
`forecast_error_corrected_sigma_car`. Since 2026-09-24, a model may also
set `statistics$n_params` (the number of parameters estimated from the
estimation window); `PatellZTest` reads this field for its Q_i
adjustment and falls back to 2 when a subclass does not report it.

Degenerate-input handling: subclasses that implement the shared contract
(see `R/contract.R`) read `self$degenerate_mode`, `self$event_id` and
`self$firm_symbol`, which are threaded onto the cloned model instance by
[`fit_model()`](https://sipemu.github.io/eventstudy/reference/fit_model.md)
before `fit()` is called.

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
[`ReturnCalculation`](https://sipemu.github.io/eventstudy/reference/ReturnCalculation.md),
[`RollingWindowModel`](https://sipemu.github.io/eventstudy/reference/RollingWindowModel.md),
[`SimpleReturn`](https://sipemu.github.io/eventstudy/reference/SimpleReturn.md),
[`VolatilityModel`](https://sipemu.github.io/eventstudy/reference/VolatilityModel.md),
[`VolumeModel`](https://sipemu.github.io/eventstudy/reference/VolumeModel.md)

## Public fields

- `model_name`:

  Name of the model.

- `degenerate_mode`:

  Resolved degenerate-input mode injected by fit_model() before fit() is
  called. Subclasses that implement the degenerate-input contract
  (MarketModel and Phase-2 models) read this field inside fit() via
  .resolve_degenerate_mode(self\$degenerate_mode).

- `event_id`:

  Event identifier threaded from the outer data_tbl row. Used in
  degenerate-input error/warning messages.

- `firm_symbol`:

  Firm identifier threaded from the outer data_tbl row. Used in
  degenerate-input error/warning messages.

## Active bindings

- `statistics`:

  Read-only field to get statistics.

- `model`:

  Read-only field to get the fitted model.

- `is_fitted`:

  Read-only field to check if the model is fitted. Statistics object
  contains different model specific KPIs that describes the fitted
  model.

## Methods

### Public methods

- [`ModelBase$fit()`](#method-ModelBase-fit)

- [`ModelBase$abnormal_returns()`](#method-ModelBase-abnormal_returns)

- [`ModelBase$clone()`](#method-ModelBase-clone)

------------------------------------------------------------------------

### `ModelBase$fit()`

Fits the model with given data.

#### Usage

    ModelBase$fit(data_tbl)

#### Arguments

- `data_tbl`:

  A data frame or tibble containing the data to fit.

------------------------------------------------------------------------

### `ModelBase$abnormal_returns()`

Calculate the abnormal returns with given data and fitted model.

#### Usage

    ModelBase$abnormal_returns(data_tbl)

#### Arguments

- `data_tbl`:

  Data frame or tibble containing the data to calculate abnormal
  returns.

------------------------------------------------------------------------

### `ModelBase$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ModelBase$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
