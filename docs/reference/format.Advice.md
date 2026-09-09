# Format method for Advice objects

Builds the character vector rendered by `print.Advice` (one element per
output line). The opt-in Advisor Pro footer is captured here (a no-op
unless `options(eventstudy.advisor_pro_footer = TRUE)`), so
`print.Advice` must not re-emit it.

## Usage

``` r
# S3 method for class 'Advice'
format(x, ...)
```

## Arguments

- x:

  An object of class `"Advice"`.

- ...:

  Additional arguments (ignored).

## Value

A character vector, one element per printed line.
