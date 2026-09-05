# Print method for Advice objects

Prints a structured summary of the grounded AI advice, including source,
task type, grounding guard status, interpretation, recommendations with
evidence, and caveats. Follows the package convention of
`print.es_advice` (cat-based, invisible return).

## Usage

``` r
# S3 method for class 'Advice'
print(x, ...)
```

## Arguments

- x:

  An object of class `"Advice"`.

- ...:

  Additional arguments (ignored).

## Value

Invisibly returns `x`.
