# Format method for es_advice objects

Builds the character vector rendered by `print.es_advice` (one element
per output line). The opt-in Advisor Pro footer is captured here (a
no-op unless `options(eventstudy.advisor_pro_footer = TRUE)`), so
`print.es_advice` must not re-emit it.

## Usage

``` r
# S3 method for class 'es_advice'
format(x, ...)
```

## Arguments

- x:

  An object of class `"es_advice"`.

- ...:

  Additional arguments (ignored).

## Value

A character vector, one element per printed line.
