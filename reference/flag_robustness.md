# Flag Robustness Issues via Offline KB Matching

Evaluates the KB decision table (category `"robustness"` rules) against
the diagnostic signals extracted from a fitted `EventStudyTask` or a
precomputed `es_diagnostics` object, and returns a severity-ranked
`es_advice` S3 object.

## Usage

``` r
flag_robustness(x, provider = NULL, ...)

# Default S3 method
flag_robustness(x, provider = NULL, ...)

# S3 method for class 'EventStudyTask'
flag_robustness(x, provider = NULL, ...)

# S3 method for class 'es_diagnostics'
flag_robustness(x, provider = NULL, ...)
```

## Arguments

- x:

  A fitted `EventStudyTask` (after
  [`fit_model()`](https://sipemu.github.io/eventstudy/reference/fit_model.md))
  or a precomputed `es_diagnostics` object returned by
  [`es_diagnostics()`](https://sipemu.github.io/eventstudy/reference/es_diagnostics.md).

- provider:

  Accepted but ignored in the offline path — present only so the Phase 7
  call shape is forward-compatible. Default `NULL`.

- ...:

  Additional arguments (currently ignored).

## Value

An S3 object of class `"es_advice"` — see
[`recommend_stat`](https://sipemu.github.io/eventstudy/reference/recommend_stat.md)
for field descriptions. Rules are filtered to
`category == "robustness"`.

## Details

No LLM provider, network connection, or API key is required. The
returned `es_advice` object has the same shape as the Phase 7 Advice
contract, flagged `is_deterministic = TRUE` and `source = "offline_kb"`.

## See also

[`recommend_stat`](https://sipemu.github.io/eventstudy/reference/recommend_stat.md),
[`es_diagnostics`](https://sipemu.github.io/eventstudy/reference/es_diagnostics.md),
[`es_kb`](https://sipemu.github.io/eventstudy/reference/es_kb.md)

## Examples

``` r
if (FALSE) { # \dontrun{
task <- run_event_study(my_task, ParameterSet$new())
advice <- flag_robustness(task)
print(advice)
} # }
```
