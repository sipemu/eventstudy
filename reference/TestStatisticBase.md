# Base class for Event Study test statistics

Base class for Event Study test statistics

## Public fields

- `name`:

  Short code of the test statistic.

- `confidence_level`:

  The chosen confidence level.

- `confidence_type`:

  Type of the test. Defaults to 'two-sided'. Alternatives are 'less' or
  greater'.

## Methods

### Public methods

- [`TestStatisticBase$new()`](#method-TestStatisticBase-initialize)

- [`TestStatisticBase$compute()`](#method-TestStatisticBase-compute)

- [`TestStatisticBase$clone()`](#method-TestStatisticBase-clone)

------------------------------------------------------------------------

### `TestStatisticBase$new()`

Initializes the test statistic. This includes the confidence level and
the type of the test ('less', greater' or 'two-sided')

#### Usage

    TestStatisticBase$new(confidence_level = 0.95, confidence_type = "two-sided")

#### Arguments

- `confidence_level`:

  The confidence level for the confidence band. Must be anumber between
  0 and 1.

- `confidence_type`:

  Side of the test statistic.

------------------------------------------------------------------------

### `TestStatisticBase$compute()`

Computes the test test statistics for a single event.

#### Usage

    TestStatisticBase$compute(data_tbl, model)

#### Arguments

- `data_tbl`:

  The data for a single event with calculated abnormal returns.

- `model`:

  The fitted model that includes the necessary information for
  calculating the test statistic.

------------------------------------------------------------------------

### `TestStatisticBase$clone()`

The objects of this class are cloneable with this method.

#### Usage

    TestStatisticBase$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
