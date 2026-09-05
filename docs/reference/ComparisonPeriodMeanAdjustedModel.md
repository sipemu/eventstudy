# Comparison Period Mean Adjusted Model

The Comparison Period Mean Adjusted Model is another relatively simple
approach used in event studies to estimate the expected returns of a
stock and calculate its abnormal returns during an event window. This
model is based on the assumption that a stock’s expected return during
the event window is equal to its average return during a comparison
period (typically a pre-event period). This model is particularly useful
when researchers want to control for a stock’s historical performance
and do not wish to rely on market return data.

## Super class

[`ModelBase`](https://sipemu.github.io/eventstudy/reference/ModelBase.md)
-\> `ComparisonPeriodMeanAdjustedModel`

## Public fields

- `model_name`:

  Name of the model.

## Methods

### Public methods

- [`ComparisonPeriodMeanAdjustedModel$fit()`](#method-ComparisonPeriodMeanAdjustedModel-fit)

- [`ComparisonPeriodMeanAdjustedModel$abnormal_returns()`](#method-ComparisonPeriodMeanAdjustedModel-abnormal_returns)

- [`ComparisonPeriodMeanAdjustedModel$clone()`](#method-ComparisonPeriodMeanAdjustedModel-clone)

------------------------------------------------------------------------

### `ComparisonPeriodMeanAdjustedModel$fit()`

Fit the model with given data.

#### Usage

    ComparisonPeriodMeanAdjustedModel$fit(data_tbl)

#### Arguments

- `data_tbl`:

  Data frame or tibble containing the data to fit.

------------------------------------------------------------------------

### `ComparisonPeriodMeanAdjustedModel$abnormal_returns()`

Calculate the abnormal returns with given data.

#### Usage

    ComparisonPeriodMeanAdjustedModel$abnormal_returns(data_tbl)

#### Arguments

- `data_tbl`:

  Data frame or tibble containing the data to calculate abnormal
  returns.

------------------------------------------------------------------------

### `ComparisonPeriodMeanAdjustedModel$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ComparisonPeriodMeanAdjustedModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
