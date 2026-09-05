# R6 class for simple return calculation

R6 class for simple return calculation

## Super class

[`ReturnCalculation`](https://sipemu.github.io/eventstudy/reference/ReturnCalculation.md)
-\> `SimpleReturn`

## Public fields

- `name`:

  Name of the return calculation.

## Methods

### Public methods

- [`SimpleReturn$calculate_return()`](#method-SimpleReturn-calculate_return)

- [`SimpleReturn$clone()`](#method-SimpleReturn-clone)

------------------------------------------------------------------------

### `SimpleReturn$calculate_return()`

Calculates the simple return for a single stock.

#### Usage

    SimpleReturn$calculate_return(
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

### `SimpleReturn$clone()`

The objects of this class are cloneable with this method.

#### Usage

    SimpleReturn$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
