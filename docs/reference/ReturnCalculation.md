# R6 base class for return calculation

R6 base class for return calculation

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
