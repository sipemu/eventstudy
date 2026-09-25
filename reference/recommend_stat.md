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

  Accepted but ignored in the offline path – present only so the Phase 7
  call shape is forward-compatible. Default `NULL`.

- ...:

  Additional arguments (currently ignored).

## Value

An S3 object of class `"es_advice"` – a named list with:

- `source`:

  `"offline_kb"` (character).

- `is_deterministic`:

  `TRUE` – advice is rule-based, not LLM-generated.

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

Other eventstudy-advisor:
[`AnthropicProvider`](https://sipemu.github.io/eventstudy/reference/AnthropicProvider.md),
[`CustomProvider`](https://sipemu.github.io/eventstudy/reference/CustomProvider.md),
[`OpenAICompatProvider`](https://sipemu.github.io/eventstudy/reference/OpenAICompatProvider.md),
[`ProviderBase`](https://sipemu.github.io/eventstudy/reference/ProviderBase.md),
[`es_advise()`](https://sipemu.github.io/eventstudy/reference/es_advise.md),
[`es_diagnostics()`](https://sipemu.github.io/eventstudy/reference/es_diagnostics.md),
[`es_kb()`](https://sipemu.github.io/eventstudy/reference/es_kb.md),
[`flag_robustness()`](https://sipemu.github.io/eventstudy/reference/flag_robustness.md),
[`provider()`](https://sipemu.github.io/eventstudy/reference/provider.md)

## Examples

``` r
# \donttest{
data(dieselgate)
task <- EventStudyTask$new(dieselgate$firm, dieselgate$index, dieselgate$request)
task <- run_event_study(task, ParameterSet$new())
advice <- recommend_stat(task)
print(advice)
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
```
