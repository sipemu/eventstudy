# Event Study Parameter Set

The parameter set defines the Event Study, e.g, the return calculation,
the event study model, the AR, CAR, AAR, and CAAR test statistics that
should be applied.

## Public fields

- `return_calculation`:

  A R6 object for calculating returns.

- `return_model`:

  A R6 object for fitting the desired model.

- `single_event_statistics`:

  single event test statistic R6 object.

- `multi_event_statistics`:

  multi event test statistic R6 object.

- `study_type`:

  Type of event study: "return" (default), "volume", or "volatility".
  Affects axis labels in plots.

- `degenerate_handling`:

  Controls how degenerate estimation data is handled. One of `"lenient"`
  (default), `"strict"`, or `NULL` (defer to package option or built-in
  default). See `?degenerate-input-contract` for details.

## Methods

### Public methods

- [`ParameterSet$new()`](#method-ParameterSet-initialize)

- [`ParameterSet$print()`](#method-ParameterSet-print)

- [`ParameterSet$clone()`](#method-ParameterSet-clone)

------------------------------------------------------------------------

### `ParameterSet$new()`

Initialize the parameters that defines the Event Study that should be
applied.

#### Usage

    ParameterSet$new(
      return_calculation = SimpleReturn$new(),
      return_model = MarketModel$new(),
      single_event_statistics = SingleEventStatisticsSet$new(),
      multi_event_statistics = MultiEventStatisticsSet$new(),
      degenerate_handling = NULL
    )

#### Arguments

- `return_calculation`:

  An initialized return calculation class. Defaults to SimpleReturn.

- `return_model`:

  An initialized event study model. Defaults to MarketModel.

- `single_event_statistics`:

  Definition of single event test statistics. Defaults to
  SingleEventStatisticsSet (AR T and CAR T tests).

- `multi_event_statistics`:

  Definition of multiple event test statistics. Defaults to
  MultiEventStatisticsSet (CSect T test).

- `degenerate_handling`:

  One of `"lenient"`, `"strict"`, or `NULL`. See
  `?degenerate-input-contract`.

------------------------------------------------------------------------

### `ParameterSet$print()`

Print a summary of the parameter set.

#### Usage

    ParameterSet$print(...)

------------------------------------------------------------------------

### `ParameterSet$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ParameterSet$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
