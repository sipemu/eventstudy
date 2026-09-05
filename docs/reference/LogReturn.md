# R6 class for log return calculation

R6 class for log return calculation

## Super class

[`ReturnCalculation`](https://sipemu.github.io/eventstudy/reference/ReturnCalculation.md)
-\> `LogReturn`

## Public fields

- `name`:

  Name of the log return calculation.

## Methods

### Public methods

- [`LogReturn$calculate_return()`](#method-LogReturn-calculate_return)

- [`LogReturn$clone()`](#method-LogReturn-clone)

------------------------------------------------------------------------

### `LogReturn$calculate_return()`

Calculates the return for a single stock.

#### Usage

    LogReturn$calculate_return(
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

### `LogReturn$clone()`

The objects of this class are cloneable with this method.

#### Usage

    LogReturn$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
