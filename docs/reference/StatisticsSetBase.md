# Adds a test statistic to this container.

Base class for test statistic container

## Public fields

- `tests`:

  Container with the statistical tests. Test container initialization

## Methods

### Public methods

- [`StatisticsSetBase$new()`](#method-StatisticsSetBase-initialize)

- [`StatisticsSetBase$add_test()`](#method-StatisticsSetBase-add_test)

- [`StatisticsSetBase$clone()`](#method-StatisticsSetBase-clone)

------------------------------------------------------------------------

### `StatisticsSetBase$new()`

Initializes a test statistic container. Single event and multiple event
test statistics are collected in separated containers.

#### Usage

    StatisticsSetBase$new(tests = NULL)

#### Arguments

- `tests`:

  List of test statistics.

------------------------------------------------------------------------

### `StatisticsSetBase$add_test()`

#### Usage

    StatisticsSetBase$add_test(test)

#### Arguments

- `test`:

  A single event study tests.

------------------------------------------------------------------------

### `StatisticsSetBase$clone()`

The objects of this class are cloneable with this method.

#### Usage

    StatisticsSetBase$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
