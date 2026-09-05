# ModelBase

Base model class for event study. Each single event study will get its
own model initialization and fitting. Therefore, the input DataFrame
contains the data for a single Event Study. For custom models, except
the child modles of the market model, several statistics must be
included, namely sigma, degree_of_freedom, first_order_auto_correlation,
residuals, forecast_error_corrected_sigma, and
forecast_error_corrected_sigma_car. Part of these statistics are
necessary for calculating the Event Study statistics.

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
