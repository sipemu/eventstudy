# Grounded AI Advice for Event Study Results

Produces a grounded `Advice` S3 object by routing through a task-type
dispatch, calling an optional LLM provider, parsing the JSON response,
and running the runtime grounding guard — which drops any recommendation
whose `evidence[]` cites a diagnostic key absent from the computed
diagnostics or a value mismatching beyond numeric tolerance.

## Usage

``` r
es_advise(diagnostics, task_type, provider = NULL, model = NULL, ...)
```

## Arguments

- diagnostics:

  An `es_diagnostics` object returned by
  [`es_diagnostics()`](https://sipemu.github.io/eventstudy/reference/es_diagnostics.md).

- task_type:

  Character. One of `"interpret"`, `"recommend_stat"`,
  `"recommend_model"`, `"flag_robustness"`, `"design_discussion"`,
  `"report_writing"`.

- provider:

  An optional provider R6 object (from
  [`provider()`](https://sipemu.github.io/eventstudy/reference/provider.md)).
  Required for LLM-only task types. When `NULL` and task type is
  KB-based, falls back to the Phase 5 offline path.

- model:

  Optional character model identifier. Reserved for forward
  compatibility: the effective model is the one the provider was
  constructed with (see
  [`provider()`](https://sipemu.github.io/eventstudy/reference/provider.md)),
  so set the model there. Accepted here without error so calling code
  can pass it, but it does not override the provider's configured model.

- ...:

  Additional arguments (currently ignored; reserved for future use).

## Value

For KB task types without a provider: an `es_advice` S3 object (Phase 5
offline path, `is_deterministic = TRUE`). For all other paths: an
`Advice` S3 object with fields `source`, `is_deterministic`,
`task_type`, `interpretation`, `recommendations`, `caveats`,
`n_dropped`.

## Details

**Grounding guarantee:** Every returned recommendation is provably tied
to a value the package actually computed
([`es_diagnostics()`](https://sipemu.github.io/eventstudy/reference/es_diagnostics.md)).
The guard is enforced in R, independent of the prompt (ADV-04).

**Task type routing:**

- `recommend_stat`, `flag_robustness`:

  No provider: returns the Phase 5 `es_advice` object (offline KB path —
  deterministic, `is_deterministic = TRUE`). With provider: KB produces
  grounded evidence\[\], LLM adds prose; returns an `Advice` object
  (`is_deterministic = FALSE`).

- `interpret`, `recommend_model`, `design_discussion`, `report_writing`:

  LLM-required: [`stop()`](https://rdrr.io/r/base/stop.html) when
  `provider = NULL` (ADV-06). With provider: LLM produces full advice;
  guard runs.

**Failure discipline:** Any provider failure, malformed JSON, or empty
response degrades to one
[`warning()`](https://rdrr.io/r/base/warning.html) + an empty `Advice`
object — never a crash, never a fabricated result (mirrors
`.handle_degenerate()`).

## See also

[`es_diagnostics`](https://sipemu.github.io/eventstudy/reference/es_diagnostics.md),
[`recommend_stat`](https://sipemu.github.io/eventstudy/reference/recommend_stat.md),
[`flag_robustness`](https://sipemu.github.io/eventstudy/reference/flag_robustness.md),
[`provider`](https://sipemu.github.io/eventstudy/reference/provider.md)

## Examples

``` r
if (FALSE) { # \dontrun{
task    <- run_event_study(my_task, ParameterSet$new())
diag    <- es_diagnostics(task)

# Offline KB path (no LLM):
advice_kb <- es_advise(diag, task_type = "recommend_stat")
print(advice_kb)   # es_advice S3

# LLM-grounded path:
p         <- provider("openai")
advice    <- es_advise(diag, task_type = "recommend_stat", provider = p)
print(advice)      # Advice S3 with grounding guarantee
} # }
```
