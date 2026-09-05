# Recommend Test Statistics via Offline KB Matching

Evaluates the KB decision table (category `"stat_choice"` rules) against
the diagnostic signals extracted from a fitted `EventStudyTask` or a
precomputed `es_diagnostics` object, and returns a severity-ranked
`es_advice` S3 object.

## Usage

``` r
recommend_stat(x, provider = NULL, ...)

# Default S3 method
recommend_stat(x, provider = NULL, ...)

# S3 method for class 'EventStudyTask'
recommend_stat(x, provider = NULL, ...)

# S3 method for class 'es_diagnostics'
recommend_stat(x, provider = NULL, ...)
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

An S3 object of class `"es_advice"` — a named list with:

- `source`:

  `"offline_kb"` (character).

- `is_deterministic`:

  `TRUE` — advice is rule-based, not LLM-generated.

- `rules_matched`:

  Named list of matched rule records (severity-ranked: `"error"` first,
  then `"warning"`, then `"info"`), each with fields `id`,
  `recommendation`, `citation` (list of `author`/`year`/`key`/`venue`),
  `severity`, `category`.

- `diagnostics_ref`:

  The `es_diagnostics` list that was evaluated (possibly computed
  on-the-fly from the task).

## Details

No LLM provider, network connection, or API key is required. Both
functions are the always-available offline grounding layer (ADV-08). The
returned `es_advice` object has the same shape as the Phase 7 Advice
contract, flagged `is_deterministic = TRUE` and `source = "offline_kb"`.

## See also

[`flag_robustness`](https://sipemu.github.io/eventstudy/reference/flag_robustness.md),
[`es_diagnostics`](https://sipemu.github.io/eventstudy/reference/es_diagnostics.md),
[`es_kb`](https://sipemu.github.io/eventstudy/reference/es_kb.md)

## Examples

``` r
if (FALSE) { # \dontrun{
task <- run_event_study(my_task, ParameterSet$new())
advice <- recommend_stat(task)
print(advice)
} # }
```
