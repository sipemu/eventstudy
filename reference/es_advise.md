# Grounded AI Advice for Event Study Results

Produces a grounded `Advice` S3 object by routing through a task-type
dispatch, calling an optional LLM provider, parsing the JSON response,
and running the runtime grounding guard – which drops any recommendation
whose `evidence[]` cites a diagnostic key absent from the computed
diagnostics or a value mismatching beyond numeric tolerance.

## Usage

``` r
es_advise(
  diagnostics,
  task_type,
  provider = NULL,
  model = NULL,
  section_hint = NULL,
  ...
)
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

- section_hint:

  Optional character scalar naming the report section to scope the
  narrative to (e.g. `"exec_summary"`, `"results"`, `"robustness"`).
  `NULL` (default) leaves the prompt byte-identical to its pre-Phase-18
  form. Only applied for the `"report_writing"` task type.

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

  No provider: returns the Phase 5 `es_advice` object (offline KB path –
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
object – never a crash, never a fabricated result (mirrors
`.handle_degenerate()`).

## See also

[`es_diagnostics`](https://sipemu.github.io/eventstudy/reference/es_diagnostics.md),
[`recommend_stat`](https://sipemu.github.io/eventstudy/reference/recommend_stat.md),
[`flag_robustness`](https://sipemu.github.io/eventstudy/reference/flag_robustness.md),
[`provider`](https://sipemu.github.io/eventstudy/reference/provider.md),
[`run_event_study`](https://sipemu.github.io/eventstudy/reference/run_event_study.md)

Other eventstudy-advisor:
[`AnthropicProvider`](https://sipemu.github.io/eventstudy/reference/AnthropicProvider.md),
[`CustomProvider`](https://sipemu.github.io/eventstudy/reference/CustomProvider.md),
[`OpenAICompatProvider`](https://sipemu.github.io/eventstudy/reference/OpenAICompatProvider.md),
[`ProviderBase`](https://sipemu.github.io/eventstudy/reference/ProviderBase.md),
[`es_diagnostics()`](https://sipemu.github.io/eventstudy/reference/es_diagnostics.md),
[`es_kb()`](https://sipemu.github.io/eventstudy/reference/es_kb.md),
[`flag_robustness()`](https://sipemu.github.io/eventstudy/reference/flag_robustness.md),
[`provider()`](https://sipemu.github.io/eventstudy/reference/provider.md),
[`recommend_stat()`](https://sipemu.github.io/eventstudy/reference/recommend_stat.md)

## Examples

``` r
# Offline KB path (no LLM, no network, no credentials):
# \donttest{
data(dieselgate)
task    <- EventStudyTask$new(dieselgate$firm, dieselgate$index, dieselgate$request)
task    <- run_event_study(task, ParameterSet$new())
diag    <- es_diagnostics(task)
advice_kb <- es_advise(diag, task_type = "recommend_stat")
print(advice_kb)   # es_advice S3
#> Offline Event Study Advice
#> ==========================
#> Source:           offline_kb 
#> Deterministic:    TRUE 
#> Rules matched:    2 
#> 
#> [WARNING] KB-NONNORM-NONPAR  (citation: BrownWarner1985)
#>   Recommendation: Non-normality detected in estimation-window residuals for >= 50% of events (Shapiro-Wilk p < 0.05). Consider non-parametric alternatives: Sign Test or Rank Test (Corrado 1989) are robust to departures from normality. Brown & Warner (1985) document that parametric tests lose size control under non-normal return distributions. [Threshold: 50% of events — ASSUMED, adjustable] 
#> 
#> [WARNING] KB-VAR-INCREASE-BMP  (citation: BMP1991)
#>   Recommendation: High CAR dispersion (IQR > 0.10 or SD > 0.15) suggests event-induced variance increase, which inflates Patell Z rejection rates. Use the BMP (Boehmer-Musumeci-Poulsen) test, which standardizes by the event-window variance and is specifically designed for this case. [Thresholds: IQR > 0.10, SD > 0.15 — ASSUMED, adjustable] 
#> 
# }

# LLM-grounded path (requires a network connection and provider credentials):
if (FALSE) { # \dontrun{
p         <- provider("openai")
advice    <- es_advise(diag, task_type = "recommend_stat", provider = p)
print(advice)      # Advice S3 with grounding guarantee
} # }
```
