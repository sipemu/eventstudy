# Interpreting `es_diagnostics()` Output

`es_diagnostics(task, max_events = 20L)` extracts already-computed
statistical signals from a fitted `EventStudyTask` into a flat, JSON-ready
S3 list of class `"es_diagnostics"`. It recomputes nothing -- every field is
read from `task$data_tbl` / `task$aar_caar_tbl` as already populated by
`fit_model()` / `calculate_statistics()`. This file explains what each real
field means, how the offline knowledge-base rules key off it, and how to act
on it.

Verified against the live object (2026-09-24, `earnings_surprises` bundled
dataset): `str(es_diagnostics(task), max.level = 2)`. Every field path named
below exists in the returned object.

---

## Top-Level Sections

```r
str(es_diagnostics(task), max.level = 1)
#> List of 6
#>  $ meta             :List of 4
#>  $ estimation_window :List of 7
#>  $ event_window      :List of 5
#>  $ cross_sectional    :List of 6
#>  $ contract_state    :List of 5
#>  $ aggregate_summary :NULL or List of 6
```

| Field | Type | What it tells you |
|-------|------|-------------------|
| `meta` | list | Event counts and which events are shown in full detail |
| `estimation_window` | list of vectors (length `n_events_shown`) | Fit quality over the estimation period, per shown event |
| `event_window` | list of vectors (length `n_events_shown`) | AR/CAR signal in the event window, per shown event |
| `cross_sectional` | list of scalars | Aggregated across **all** events (not just the shown ones) |
| `contract_state` | list of vectors (length `n_events_shown`) | Per-event degenerate-input contract state |
| `aggregate_summary` | list or `NULL` | Summary of events beyond `max_events` (`NULL` when nothing is truncated) |

Events are ranked by anomaly score before capping at `max_events`:
degenerate (unfitted) events always rank first, then fitted events by
`abs(final_car)` descending (`.rank_events_for_cap()`). So the "shown" set
is not the first N events in the task -- it is the N most interesting ones.

---

## `meta`

| Sub-field | Type | Meaning |
|-----------|------|---------|
| `n_events_total` | integer | Total events in the task |
| `n_events_shown` | integer | `min(max_events, n_events_total)` -- events with full per-event detail |
| `n_events_summarized` | integer | `n_events_total - n_events_shown` -- events folded into `aggregate_summary` |
| `event_ids_shown` | integer vector | `event_id` values for the shown events, in anomaly-score order |

---

## `estimation_window` (per shown event, length `n_events_shown`)

| Sub-field | Meaning | Concern if... |
|-----------|---------|----------------|
| `r2` | Estimation-window model R-squared | `< 0.05` in >= 50% of shown events (KB-LOWFIT-WARN) -- weak fit inflates the AR/CAR standard error (MacKinlay 1997 S3.1) |
| `sigma` | Residual standard deviation (used directly by ART/CART) | Large relative to typical AR magnitude -- low test power |
| `degree_of_freedom` | Estimation-window residual df | Small -- t-distribution has fat tails, wider CIs |
| `acf1` | First-order residual autocorrelation | Large in magnitude alongside a `dw_stat` outside [1.5, 2.5] |
| `shapiro_p` | Shapiro-Wilk normality test p-value on estimation-window residuals | `< 0.05` in >= 50% of events (KB-NONNORM-NONPAR) -- non-parametric tests (Sign, Rank) preferred; `> 0.05` in >= 70% (KB-NORM-PATELL) -- Patell Z is appropriate |
| `dw_stat` | Durbin-Watson statistic | Outside `[1.5, 2.5]` in >= 50% of events (KB-AC-WARN) -- serial autocorrelation understates OLS standard errors; consider `MarketModel(use_hac = TRUE)` |
| `ljung_box_p` | Ljung-Box test p-value for residual autocorrelation | `< 0.05` -- corroborates a `dw_stat` autocorrelation flag |

Each entry is `NA_real_` when the model for that event was not fitted or
residuals were insufficient for the test.

---

## `event_window` (per shown event, length `n_events_shown`)

| Sub-field | Meaning |
|-----------|---------|
| `ar_t` | ART t-statistic on the **last** event-window day |
| `ar_p` | Two-sided p-value for `ar_t` |
| `car_t` | CART t-statistic over the full event window |
| `car_p` | Two-sided p-value for `car_t` |
| `final_car` | Cumulative abnormal return at the end of the event window |

All `NA_real_` when the `ART`/`CART` single-event test statistic columns are
absent from `task$data_tbl` (i.e. `single_event_statistics` was not run, or
was configured without `ARTTest`/`CARTTest`).

---

## `cross_sectional` (scalars, aggregated across ALL events)

| Sub-field | Meaning | Concern if... |
|-----------|---------|----------------|
| `n_events` | Total events in the task | -- |
| `n_valid_events` | Number of fitted (non-degenerate) events | `n_valid_events / n_events < 0.8` (KB-DEGEN-EVENTS) -- more than 20% of events did not fit; inspect `contract_state` |
| `car_iqr` | Interquartile range of final CAR across events | `> 0.10` (KB-VAR-INCREASE-BMP, combined with `car_sd`) -- suggests event-induced variance increase; prefer BMP over Patell Z |
| `car_sd` | Standard deviation of final CAR across events | `> 0.15` (KB-VAR-INCREASE-BMP) -- same signal as `car_iqr` |
| `n_overlap_pairs` | Count of event-window pairs that overlap in calendar time | `> 0` (KB-OVERLAP-KP) -- abnormal returns are cross-sectionally correlated; use `KolariPynnonenTest` |
| `any_overlap` | Logical: is `n_overlap_pairs > 0` | -- |

Degrades to `NA` for the multi-event fields when `task$aar_caar_tbl` is
`NULL` (i.e. `calculate_statistics()` was run without
`multi_event_statistics`).

Also `n_valid_events` alone, independent of `n_events`: `< 10` (KB-SMALL-N)
-- the cross-sectional t-test's large-sample approximation is unreliable
below ~10 events; prefer Sign Test or Rank Test.

---

## `contract_state` (per shown event, length `n_events_shown`)

| Sub-field | Meaning |
|-----------|---------|
| `is_fitted` | Logical: did the model fit for this event |
| `na_ar_count` | Count of NA abnormal returns in the event window |
| `na_est_count` | Count of NA returns in the estimation window |
| `insufficient_obs` | Logical: did the model report the "insufficient estimation observations" degenerate condition |
| `zero_var_index` | Logical: did the model report zero/near-zero variance in the index returns |

`sum(!diag$contract_state$is_fitted, na.rm = TRUE)` gives the count of
degenerate events among those **shown**; combine with `meta$n_events_shown`
for a shown-set fit rate. For the fit rate across **all** events (including
summarized ones), use `cross_sectional$n_valid_events / meta$n_events_total`.

---

## `aggregate_summary` (remainder events beyond `max_events`, or `NULL`)

| Sub-field | Meaning |
|-----------|---------|
| `n_summarized` | Count of events folded into this summary |
| `mean_r2` | Mean estimation-window R-squared across summarized events |
| `median_r2` | Median estimation-window R-squared across summarized events |
| `mean_final_car` | Mean final CAR across summarized events |
| `n_fitted` | Count of fitted (non-degenerate) summarized events |
| `n_degenerate` | Count of unfitted (degenerate) summarized events |

`NULL` whenever `n_events_total <= max_events` (nothing was truncated) --
always check `!is.null(diag$aggregate_summary)` before reading its fields.

---

## Offline Knowledge Base: How Rules Consume These Fields

`recommend_stat(diag)` and `flag_robustness(diag)` filter `es_kb()`
(`EVENTSTUDY_KB`, 8 rules) by `category` (`"stat_choice"` /
`"robustness"`) and evaluate each rule's `condition(diag)` against the real
field paths above. Every rule carries an academic citation and a severity
(`"info"` / `"warning"` / `"error"`):

| Rule ID | Category | Condition (on `diag` fields) | Recommendation |
|---------|----------|-------------------------------|-----------------|
| `KB-NORM-PATELL` | stat_choice | `mean(estimation_window$shapiro_p > 0.05) >= 0.70` | Patell Z appropriate |
| `KB-NONNORM-NONPAR` | stat_choice | `mean(estimation_window$shapiro_p < 0.05) >= 0.50` | Sign / Rank Test (non-parametric) |
| `KB-VAR-INCREASE-BMP` | stat_choice | `cross_sectional$car_iqr > 0.10` OR `cross_sectional$car_sd > 0.15` | BMP test |
| `KB-OVERLAP-KP` | stat_choice | `cross_sectional$n_overlap_pairs > 0` | Kolari-Pynnonen adjusted BMP |
| `KB-AC-WARN` | robustness | `dw_stat` outside `[1.5, 2.5]` in `>= 50%` of shown events | HAC standard errors (`MarketModel(use_hac = TRUE)`) |
| `KB-LOWFIT-WARN` | robustness | `estimation_window$r2 < 0.05` in `>= 50%` of shown events | Multi-factor model (FF3/FF5/Carhart4) or Market-Adjusted |
| `KB-DEGEN-EVENTS` | robustness | `cross_sectional$n_valid_events / meta$n_events_total < 0.8` | Inspect `contract_state`; extend estimation window |
| `KB-SMALL-N` | robustness | `cross_sectional$n_valid_events < 10` | Non-parametric tests (Sign, Rank) |

All numeric thresholds above are `[ASSUMED]` heuristics documented as such
in `R/knowledge_base.R` (adjustable, not derived from a formal power
analysis) except `KB-OVERLAP-KP`'s direction, which is `[CITED: Kolari &
Pynnonen 2010]`. Run `es_kb()` to inspect the live rule list, including each
rule's full `recommendation` text and `citation`.

---

## Degrade Path — No API Key

When `provider()` resolves to no configured LLM:

```
es_diagnostics(task)       <- always available, no network
       |
       v
recommend_stat(diag)       <- offline KB rules, no network
flag_robustness(diag)      <- offline KB rules, no network
```

**Decision tree:**

```
Do you have an LLM API key?
  YES -> es_advise(diag, task_type = "interpret", provider = provider(...))
  NO  -> recommend_stat(diag) + flag_robustness(diag)   [offline KB path]
```

The offline path:
- Never errors for lack of a key
- Never fabricates a number -- only fires rules whose `condition(diag)`
  matches the real fields extracted above
- Returns an `es_advice` S3 object with matched KB rule IDs, severities,
  and citations
- `source` field is `"offline_kb"` -- always declared so callers know it is
  rule-based, never LLM-generated

---

## Example: Reading a Diagnostic Output

```r
diag <- es_diagnostics(task)

# Estimation-window fit health (per shown event)
diag$estimation_window$r2
# If < 0.05 in >= 50% of events -> KB-LOWFIT-WARN fires; consider a
# multi-factor model

# Normality of estimation-window residuals (per shown event)
diag$estimation_window$shapiro_p
# If < 0.05 in >= 50% of events -> KB-NONNORM-NONPAR fires; non-parametric
# statistics recommended

# Overall fit rate across ALL events (not just the shown ones)
diag$cross_sectional$n_valid_events / diag$meta$n_events_total
# If < 0.8 -> KB-DEGEN-EVENTS fires; inspect diag$contract_state

# Was anything truncated?
if (!is.null(diag$aggregate_summary)) {
  diag$aggregate_summary$n_degenerate
}

# Get offline KB advice without any API key
recommend_stat(diag)    # fires stat_choice KB rules
flag_robustness(diag)   # fires robustness KB rules
```
