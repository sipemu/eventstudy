# ReturnCalculation

Base (abstract) class for return calculation strategies. Subclass this
to plug a custom return calculation into
[`prepare_event_study()`](https://sipemu.github.io/eventstudy/reference/prepare_event_study.md)
/ `EventStudyTask$new()` via `ParameterSet$return_calculation`.

A subclass must implement
`calculate_return(tbl, in_column, out_column)`: given a tibble of prices
in `in_column`, add a column named `out_column` with the calculated
return (e.g. simple or log return). See `SimpleReturn` and `LogReturn`
for reference implementations.

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
[`ModelBase`](https://sipemu.github.io/eventstudy/reference/ModelBase.md),
[`RollingWindowModel`](https://sipemu.github.io/eventstudy/reference/RollingWindowModel.md),
[`SimpleReturn`](https://sipemu.github.io/eventstudy/reference/SimpleReturn.md),
[`VolatilityModel`](https://sipemu.github.io/eventstudy/reference/VolatilityModel.md),
[`VolumeModel`](https://sipemu.github.io/eventstudy/reference/VolumeModel.md)

## Public fields

- `name`:

  Name of the return calculation.

## Methods

### Public methods

- [`ReturnCalculation$calculate_return()`](#method-ReturnCalculation-calculate_return)

- [`ReturnCalculation$clone()`](#method-ReturnCalculation-clone)

------------------------------------------------------------------------

### `ReturnCalculation$calculate_return()`

Calculates the return for a single stock.

#### Usage

    ReturnCalculation$calculate_return(
      tbl,
      in_column = "adjusted",
      out_column = "adjusted_return"
    )

#### Arguments

- `tbl`:

  The dataframe with the stock price.

- `in_column`:

  The column name of the price infromation.

- `out_column`:

  The column name were the return will be saved.

------------------------------------------------------------------------

### `ReturnCalculation$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ReturnCalculation$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
