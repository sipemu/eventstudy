# Print method for es_advice objects

Prints a structured summary of the offline advice, listing each matched
rule with its severity, citation key, and recommendation. Follows the
package convention of `print.es_diagnostics` and `print.es_simulation`
(cat-based, invisible return).

## Usage

``` r
# S3 method for class 'es_advice'
print(x, ...)
```

## Arguments

- x:

  An object of class `"es_advice"`.

- ...:

  Additional arguments (ignored).

## Value

Invisibly returns `x`.
