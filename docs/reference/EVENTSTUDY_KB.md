# Grounding Knowledge Base for Event Study Diagnostics

`EVENTSTUDY_KB` is a pure-R list of rule records that maps diagnostic
conditions (read from an
[`es_diagnostics`](https://sipemu.github.io/eventstudy/reference/es_diagnostics.md)
object) to grounded methodological recommendations, each carrying a
structured academic citation and a severity level.

Package-level list; use
[`es_kb()`](https://sipemu.github.io/eventstudy/reference/es_kb.md) to
access.

## Usage

``` r
EVENTSTUDY_KB
```

## Details

The KB is built at package-load time from calls to `.kb_rule()` and
never depends on a network connection or LLM provider. It is the
correctness-critical layer that the offline advice functions
([`recommend_stat`](https://sipemu.github.io/eventstudy/reference/recommend_stat.md),
[`flag_robustness`](https://sipemu.github.io/eventstudy/reference/flag_robustness.md))
and the Phase 7 LLM layer both evaluate against.

**KB-04 scope note:** Phase 5 delivers the KB data structure only —
exported and serializable, ready for Phase 7 system-prompt injection.
The actual prompt-injection behavior is a Phase 7 deliverable and is
deliberately out of scope here.

## Rule record fields

Each element of `EVENTSTUDY_KB` is a list with:

- `id`:

  Unique character identifier (e.g. `"KB-NORM-PATELL"`).

- `category`:

  Either `"stat_choice"` (steers which test to use) or `"robustness"`
  (data-quality / reliability warning).

- `condition`:

  A `function(diag)` that accepts an `es_diagnostics` list and returns a
  length-1 logical. Returns `FALSE` when any required field is `NA`
  (NA-safe; never errors on missing data).

- `recommendation`:

  Character. The methodological action to take.

- `citation`:

  Named list: `author` (character), `year` (integer), `key` (character),
  `venue` (character).

- `severity`:

  One of `"info"`, `"warning"`, `"error"`.

## Threshold notes

The following thresholds are literature-informed defaults and are marked
**\[ASSUMED\]** — they can be tuned without breaking the KB contract:

- Shapiro-Wilk p-value threshold: 0.05

- Proportion for normality-holds rule: \\= 70% of events

- Proportion for non-normality rule: \\= 50% of events

- Durbin-Watson bounds: \[1.5, 2.5\]

- R-squared low-fit threshold: \< 0.05

- Small-N threshold: \< 10 valid events

- Degenerate-event threshold: \< 80% of events fitted

- High-dispersion CAR IQR threshold: \> 0.10 or SD \> 0.15
