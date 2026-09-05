# Harvest Diagnostics from a Fitted EventStudyTask

Extracts already-computed statistical signals from a fitted
`EventStudyTask` into a flat, JSON-ready S3 list. This is the
always-available grounding foundation for the knowledge base and offline
advice layer — it recomputes nothing; all signals already exist in the
fitted task.

## Usage

``` r
es_diagnostics(task, max_events = 20L)
```

## Arguments

- task:

  A fitted `EventStudyTask` (after
  [`fit_model()`](https://sipemu.github.io/eventstudy/reference/fit_model.md)
  and optionally
  [`calculate_statistics()`](https://sipemu.github.io/eventstudy/reference/calculate_statistics.md)
  have been called).

- max_events:

  Integer. Maximum number of events to include in the per-event sections
  (`estimation_window`, `event_window`, `contract_state`). Events are
  ranked by anomaly score (degenerate events first, then by absolute
  final CAR). The remainder is summarised in `aggregate_summary`.
  Default: 20.

## Value

A named list of class `"es_diagnostics"` with six sections:

- `meta`:

  List: `n_events_total` (integer), `n_events_shown` (integer),
  `n_events_summarized` (integer), `event_ids_shown` (integer vector).

- `estimation_window`:

  List of plain numeric vectors (length `n_events_shown`): `r2`,
  `sigma`, `degree_of_freedom`, `shapiro_p`, `dw_stat`, `ljung_box_p`,
  `acf1`. Each entry is `NA_real_` when the model was not fitted or
  residuals are insufficient for the test.

- `event_window`:

  List of plain numeric vectors (length `n_events_shown`): `ar_t` (last
  event-window day AR t-stat), `ar_p` (two-sided p-value), `car_t`
  (full-window CAR t-stat), `car_p` (two-sided p-value), `final_car`.
  All `NA_real_` when the `ART`/`CART` columns are absent.

- `cross_sectional`:

  List of scalars aggregated across ALL events: `n_events` (total),
  `n_valid_events` (fitted count), `car_iqr`, `car_sd`,
  `n_overlap_pairs`, `any_overlap`. Multi-event fields degrade to `NA`
  when `task$aar_caar_tbl` is `NULL`.

- `contract_state`:

  List of logical/numeric vectors (length `n_events_shown`):
  `is_fitted`, `na_ar_count`, `na_est_count`, `insufficient_obs`,
  `zero_var_index`.

- `aggregate_summary`:

  Named list summarising remainder events (those beyond `max_events`):
  `n_summarized`, `mean_r2`, `median_r2`, `mean_final_car`, `n_fitted`,
  `n_degenerate`. `NULL` when no events are summarised (all shown).

## See also

[`model_diagnostics`](https://sipemu.github.io/eventstudy/reference/model_diagnostics.md),
[`recommend_stat`](https://sipemu.github.io/eventstudy/reference/recommend_stat.md),
[`flag_robustness`](https://sipemu.github.io/eventstudy/reference/flag_robustness.md)
