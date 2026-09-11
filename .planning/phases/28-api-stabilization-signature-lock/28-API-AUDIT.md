# EventStudy Phase 28 — Public API Signature Audit

**Audit date:** 2026-09-11
**Package version audited:** 0.65.0 (pre-reconciliation)
**Target version:** 0.66.0

## Methodology

1. Load all 77+ exported symbols via `getNamespaceExports("EventStudy")`.
2. Filter to functions (excluding R6 class objects).
3. For each function, capture `formals()` and compare argument naming, ordering,
   and defaults against the rest of the public API surface.
4. Flag as outlier when an argument name deviates from the established pattern
   for the same semantic concept (e.g. a parameter that toggles random sampling
   named `do_sample` while R convention and the rest of the package use noun-first
   names).
5. Assign disposition:
   - **aligned** — rename applied in-place with a backward-compatible old-name
     shim using `.deprecate_arg()`. Shim emits exactly one deprecation warning
     and forwards the value; behavior on new name is byte-identical to before.
   - **scheduled-for-deprecation** — rename too risky or too far-reaching this
     phase; documented here with rationale; source left unchanged.
6. Each reconciled rename gets a NEWS.md entry under `# EventStudy 0.66.0` and
   a test in `tests/testthat/test-deprecation.R`.

## Scope

**Included:**
- All functions exported via `export()` in NAMESPACE
- All S3 methods registered via `S3method()` in NAMESPACE (first-arg `x` is
  mandated by the S3 generic and is therefore not an outlier)

**Excluded from outlier classification (correct by convention):**
- `tidy.EventStudyTask(x, ...)` — S3 `tidy` generic mandates `x`
- `flag_robustness(x, ...)` — S3 generic mandates `x`
- `recommend_stat(x, ...)` — S3 generic mandates `x`
- `es_advise(diagnostics, ...)` — domain-first arg matches semantic (takes a
  diagnostics object, not a task)
- `provider(type, fn, ...)` — factory function; `type` is semantically correct
- `nonparametric_intraday_test(estimation_window, event_window, ...)` — takes
  numeric window vectors, not a task object; correct by design
- `simulate_event_study(n_events, ...)` — simulation entry point; no pre-existing
  task to pass; correct by design
- `download_stock_data(symbols, from, ...)` — data acquisition; no task yet; correct
- `download_factor_data(model, ...)` — same as above
- `download_risk_free_rate(frequency, ...)` — same as above
- `theme_eventstudy(base_size, ...)` — ggplot2 theme; correct by convention
- `es_kb()` — zero arguments; correct

## Outlier Register

| # | Symbol | Old Argument | New Argument | Disposition | Rationale |
|---|--------|-------------|-------------|-------------|-----------|
| O-01 | `plot_stocks` | `do_sample` | `sample_symbols` | aligned | `do_sample` is verb-first naming (`do_<verb>`); R convention and the rest of the package use noun-first or adjective-first argument names. `sample_symbols` is noun-first, matches `max_symbols` (the sibling parameter in the same function), and clearly describes the parameter's role: whether to randomly subsample when there are more symbols than `max_symbols`. Low-risk rename: utility plot function, not on the computation-critical path. Shim: `do_sample = NULL` explicit old-name parameter that calls `.deprecate_arg()` and forwards to `sample_symbols`. |
| O-02 | `bootstrap_test` | `statistic` | `stat_name` | scheduled-for-deprecation | `bootstrap_test(statistic = "both")` is inconsistent with `adjust_p_values(stat_name = "CSectT")`, `plot_event_study(stat_name = "CSectT")`, and `tidy.EventStudyTask(stat_name = "CSectT")`, all of which use `stat_name` for the same selector concept. However, `statistic = "both"` has a unique value ("both") not shared by any other function — renaming `statistic` also risks confusion with the `statistic` concept in hypothesis testing. Deferred to a separate deprecation pass in Phase 29 or later to avoid scope-creep and to allow a careful scan of all existing documentation/vignette references. No code change this phase. |
| O-03 | `export_results` | `which` | `type` | scheduled-for-deprecation | `export_results(which = c("ar", "car", "aar", "model"))` uses `which` while `tidy.EventStudyTask(type = c("ar", "car", "aar", "model"))` uses `type` for the exact same selection. However, `which` is a valid R convention for selection predicates (base `which()`), so this is a style preference rather than a clear error. Deferred: the rename would need to propagate through vignette examples and the export code path is more complex than a pure forwarding shim. Tracked here for the plan-03 snapshot so the snapshot records the CURRENT `which` name explicitly. |
| O-04 | Options namespace | `eventstudy.verbose` | `EventStudy.verbose` | scheduled-for-deprecation | The package option `eventstudy.verbose` (used in `run_event_study`, `es_report`, `validate_task`, etc.) uses all-lowercase `eventstudy.*`, while the degenerate-handling option uses `EventStudy.*` (capital E). Standardizing to `EventStudy.*` is the right long-term direction, but changing the option name here would silently break `.Rprofile` setups of existing users who set `options(eventstudy.verbose = FALSE)`. This requires a two-phase approach: read both names with a fallback for one release, then drop the old name. Deferred to a standalone deprecation task after the Phase 30 CRAN submission to avoid introducing a breaking change in a correctness-focused release. No code change this phase. |

## Summary

| Disposition | Count | Symbols |
|-------------|-------|---------|
| aligned | 1 | `plot_stocks` (`do_sample` → `sample_symbols`) |
| scheduled-for-deprecation | 3 | `bootstrap_test` (`statistic`), `export_results` (`which`), options namespace (`eventstudy.verbose`) |

**Total outliers identified:** 4
**Total reconciled this phase:** 1
**Remaining scheduled for future phases:** 3
