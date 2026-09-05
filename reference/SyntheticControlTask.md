# Synthetic Control Task

Task container for synthetic control analysis. Holds treated unit data,
donor pool data, treatment time, and results after estimation.

## Public fields

- `treated_data`:

  Tibble with `time` and `outcome` columns for the treated unit.

- `donor_data`:

  Tibble in long format with `unit`, `time`, and `outcome` columns for
  donor units.

- `treatment_time`:

  The time period when treatment begins.

- `results`:

  Estimation results (populated after estimation).

## Methods

### Public methods

- [`SyntheticControlTask$new()`](#method-SyntheticControlTask-initialize)

- [`SyntheticControlTask$print()`](#method-SyntheticControlTask-print)

- [`SyntheticControlTask$clone()`](#method-SyntheticControlTask-clone)

------------------------------------------------------------------------

### `SyntheticControlTask$new()`

Create a new SyntheticControlTask.

#### Usage

    SyntheticControlTask$new(treated_data, donor_data, treatment_time)

#### Arguments

- `treated_data`:

  Tibble with `time` and `outcome`.

- `donor_data`:

  Long-format tibble with `unit`, `time`, `outcome`.

- `treatment_time`:

  Period when treatment begins.

------------------------------------------------------------------------

### `SyntheticControlTask$print()`

Print summary.

#### Usage

    SyntheticControlTask$print(...)

------------------------------------------------------------------------

### `SyntheticControlTask$clone()`

The objects of this class are cloneable with this method.

#### Usage

    SyntheticControlTask$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
