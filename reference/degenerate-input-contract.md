# Degenerate-Input Contract for EventStudy Models

The degenerate-input contract defines how all EventStudy return models
behave when the estimation data is degenerate. Two modes are supported:

**Lenient (default):** The model sets `is_fitted = FALSE`, emits exactly
one [`warning()`](https://rdrr.io/r/base/warning.html) per
`(event_id, firm_symbol)` per fit call, and propagates `NA` through all
abnormal returns and downstream statistics. No event is silently
dropped; zeros are never substituted for `NA`.

**Strict:** The model raises a descriptive
[`stop()`](https://rdrr.io/r/base/stop.html) error naming the component,
`event_id`, and `firm_symbol`, plus the specific reason for the
degeneracy.

**Configuration:**

- Via `ParameterSet`: `ParameterSet$new(degenerate_handling = "strict")`

- Via package option:
  `options(EventStudy.degenerate_handling = "strict")`

The ParameterSet field takes precedence over the package option; if
neither is set, the default `"lenient"` mode is used.

**Degenerate conditions covered:**

- Fewer than 2 finite observations in the estimation window
  (insufficient observations for OLS).

- Zero or near-zero variance in index returns
  (`sd < .Machine$double.eps`), making OLS estimation undefined.

- Single-event group (relevant for multi-event statistics; applied in
  Phase 2).

- `NA` propagation from upstream pipeline steps.

**NA propagation semantics:** When `is_fitted = FALSE`,
`model$abnormal_returns()` returns a tibble with
`abnormal_returns = NA_real_` for all rows. All downstream test
statistics that depend on fitted models then receive `NA` inputs and
propagate `NA` to their outputs. The event is retained in the output
tibble – it is never silently dropped.

**Multi-event exclusion (2026-09-24 re-evaluation, item A2):** A
single-event's `NA` propagation above is not enough for cumulative
(CAR-based) multi-event statistics (CSectT, PatellZ, Sign,
GeneralizedSign, BMP, KolariPynnonen): an event with ZERO finite
event-window abnormal returns must be EXCLUDED from every such
statistic's group, not merely NA-propagated, because a cumulative sum
that
[`coalesce()`](https://dplyr.tidyverse.org/reference/coalesce.html)s a
fully-degenerate event's abnormal returns to 0 would silently understate
the true cross-sectional dispersion. The exclusion is applied once per
group (see `.exclude_all_na_events`) before any statistic is computed,
and reported exactly once: silently if the excluded event's model
already emitted the one contract warning guaranteed by CONTRACT-04 (an
unfitted `ModelBase` instance), or via a single additional warning
naming the excluded `event_id`(s) otherwise. Strict mode errors instead.
This exclusion rule leaves the pre-existing STATS-03 convention for a
PARTIAL gap unchanged: an event with at least one finite event-window
abnormal return, but a missing value on some individual day, still
contributes 0 to that event's CAR on the missing day(s) via
`coalesce(abnormal_returns, 0)`.

## Details

Degenerate-Input Contract for EventStudy Models

## See also

[`ParameterSet`](https://sipemu.github.io/eventstudy/reference/ParameterSet.md),
[`MarketModel`](https://sipemu.github.io/eventstudy/reference/MarketModel.md)
