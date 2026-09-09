# Format Cross-Sectional Regression Results

Builds the character vector rendered by `print.es_cross_sectional` (one
element per output line). The coefficient table is rendered via
[`base::print.data.frame`](https://rdrr.io/r/base/print.dataframe.html)
and spliced in at the same position.

## Usage

``` r
# S3 method for class 'es_cross_sectional'
format(x, ...)
```

## Arguments

- x:

  An `es_cross_sectional` object.

- ...:

  Additional arguments (unused).

## Value

A character vector, one element per printed line.
