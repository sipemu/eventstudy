---
phase: quick
plan: 260924-sfl
subsystem: testing
tags: [testthat, r-cmd-check, statistical-correctness, degenerate-input-contract, cran]
status: complete
completed: "2026-09-24"
duration: "~5h across 3 executor agents (Task 3: ~2h)"

dependency_graph:
  requires: []
  provides:
    - "A1-A12 statistical silent-wrong-defect fixes (Task 1)"
    - "B1-B8 API/documentation consistency fixes (Task 2)"
    - "C1-C10 test-suite quality hardening + WARN-0 leaked-warnings cleanup (Task 3)"
    - "Green CRAN release gate for EventStudy 0.66.0.9000"
  affects: [R/, tests/testthat/, vignettes/statistical-conventions.Rmd, NEWS.md, DESCRIPTION]

tech_stack:
  added: []
  patterns:
    - "muffle_short_window() helper (tests/testthat/helper-warnings.R): mutes ONLY the eventstudy_short_estimation_window advisory by class, never a blanket suppressWarnings()"
    - "Nested expect_warning() to capture multiple independent warnings from a single call without leaking the 2nd/3rd occurrence"
    - "Hand-replication regression tests: reimplement the algorithm independently in the test body (bootstrap clustering, Patell Q_i, LinearFactorModel FEC) instead of asserting only shape/finiteness"
    - "relationship = \"many-to-many\" declared explicitly on intentional many-to-many dplyr joins to silence dplyr >=1.1 diagnostics without changing row output"

key_files:
  modified:
    - R/models.R
    - R/models_time_varying.R
    - R/multi_event_test_statistics.R
    - R/single_event_test_statistics.R
    - R/contract.R
    - R/execute.R
    - R/bootstrap.R
    - R/cross_sectional.R
    - R/p_adjustment.R
    - R/export.R
    - R/return_calculation.R
    - R/task.R
    - R/EventStudy-package.R
    - DESCRIPTION
    - NAMESPACE
    - NEWS.md
    - _pkgdown.yml
    - vignettes/statistical-conventions.Rmd
    - vignettes/modern-did-estimators.Rmd
    - vignettes/automated-reports.Rmd
    - .claude/skills/es-advisor/reference/interpreting-diagnostics.md
    - .claude/skills/es-capabilities/SKILL.md
    - "27 tests/testthat/*.R files (Task 3 alone; see Task Commits)"
  created:
    - tests/testthat/test_reeval_statistical_fixes.R
    - tests/testthat/test_public_surface.R
    - tests/testthat/helper-warnings.R

decisions:
  - "A12c (orchestrator revision after Task 1 review): bootstrap weights stay clustered by firm_symbol (not event_id) -- coarser firm clusters are robust to cross-event correlation of a recurring firm. Locked with a hand-replication test in Task 3 rather than changed."
  - "C10 (orchestrator addendum after Task 1 review): target WARN 0 in the full NOT_CRAN suite, added as a new requirement after Task 1's fixes newly triggered 91 leaked warnings from tests exercising degenerate paths without asserting the warning explicitly."
  - "Two production-code fixes were made during Task 3's final-gate verification, both zero-behaviour-change: (1) added missing 'caar_t' to globalVariables() in R/EventStudy-package.R -- an R CMD check NOTE latent since Task 1's A12 bootstrap fix; (2) declared relationship = \"many-to-many\" on the EventStudyTask firm/request join in R/task.R -- a dplyr >=1.1 diagnostic warning latent since before this quick task, first exercised by Task 3's new C2 bootstrap-firm-clustering test which is the first test to construct a recurring-firm task via the full public EventStudyTask$new() constructor. Both are suppression-only annotations; neither changes any computed value or output row."

actuals:
  tokens: 20700
  tasks: 3
  commits: 14
---

# Phase quick Plan 260924-sfl: Fix all findings from the 2026-09-24 package re-evaluation

**All 29 locked findings (A1-A12 statistical silent-wrong defects, B1-B8 API/doc drift, C1-C10 test-suite quality) fixed and locked with regression tests; full release gate green — `R CMD check --as-cran` shows only the pre-existing CRAN incoming-feasibility NOTE, 0 WARNING, 0 ERROR, and the NOT_CRAN test suite runs FAIL 0 / WARN 0 / PASS 2733 (up from a 2561-test, WARN-6 baseline).**

## Tasks Completed

| Task | Name | Commits | Key files |
|------|------|---------|-----------|
| 1 | Section A — statistical silent-wrong defects A1-A12 | `6d16c86`..`f3df862`, `c4d0ea9` | R/models.R, R/multi_event_test_statistics.R, R/bootstrap.R, R/cross_sectional.R, R/contract.R, tests/testthat/test_reeval_statistical_fixes.R (new), vignettes/statistical-conventions.Rmd |
| 2 | Section B — API/documentation consistency B1-B8 | `1fb9a5f`..`87ec249` | R/models.R, R/single_event_test_statistics.R, R/return_calculation.R, R/export.R, R/data_download.R, DESCRIPTION, NAMESPACE, tests/testthat/test_public_surface.R (new) |
| 3 | Section C — test-suite quality C1-C10 + final release gate | `5ce8db3`..`f0c8570` | 27 tests/testthat/*.R files, tests/testthat/helper-warnings.R (new), R/EventStudy-package.R, R/task.R, NEWS.md |

**Total: 29 commits, 99 files changed, 3755 insertions / 637 deletions across all three tasks.**

## Task 1: Statistical Correctness Fixes (A1-A12)

### RED evidence (pre-fix, HEAD 92a1882)

`tests/testthat/test_reeval_statistical_fixes.R` was written first and run against pre-fix HEAD: 24 of 26 tests failed for the exact reason each item's `<behavior>` block predicted (RollingWindowModel beta bias under NA-firm/index-outlier co-occurrence, all-NA events silently coalesced into CAR-based statistics, KP falling back to an adjustment factor of 1, Rank/GSign pooling a recurring firm's data across events, cross-sectional regression silently summing NA CARs as 0, Patell's `k` derived from `length(resid) - df` instead of the model's actual parameter count, CalTime's denominator scaling with the event-window shock instead of being independent of it, bootstrap's observed CAAR construction diverging from its own draws, etc.). The remaining 2 tests were unchanged-behaviour guards (asserting a convention Task 1 deliberately left untouched, e.g. STATS-03's partial-gap-contributes-0 rule) and passed on both pre- and post-fix code by design.

### Fixes (all pass post-fix)

- **A1** RollingWindowModel restricts each rolling window to complete (firm, index) pairs before computing β/σ; `fit()` and `calculate_statistics()` share one valid-count definition.
- **A2** A shared `.exclude_all_na_events()` helper (new `R/contract.R`) excludes any event with zero finite event-window ARs from every CAR-based multi-event statistic, applied once per group with exactly one warning (not once per configured statistic); the pre-existing STATS-03 partial-gap convention is unchanged.
- **A3** KolariPynnonenTest never substitutes an adjustment factor of 1 — degenerate estimation-window SAR series are excluded from `r_bar` (reported once); the per-day adjustment uses that day's `n_valid_events`, not the first day's count.
- **A4** RankTest and GeneralizedSignTest group by `event_id`, not `firm_symbol`, so a recurring firm's data is never pooled across its events.
- **A5** `cross_sectional_regression()` errors on duplicated `event_id`, errors on an out-of-window `car_window`, warns once on unmatched task events, and drops (with one warning) any event whose CAR is NA because an AR inside the window is missing.
- **A6/A8** OLS models require `n_valid >= n_params + 1`; a fit with fewer than 30 observations emits one advisory warning (collapsed to ONE summary warning across N short events by commit `c4d0ea9`); Patell's `k` comes from `model$statistics$n_params`; MarketModel's FEC uses complete pairs only; MarketAdjustedModel's FEC is exactly `sigma` (correction factor 1, since it estimates nothing).
- **A7** CalendarTimePortfolioTest denominator redefined to the Brown-Warner (1980, 1985) estimation-window AAR standard deviation, exposed as `attr(result, "caltime_df")`; `adjust_p_values()`/`tidy()` consume it via a shared helper.
- **A9** `n_pos`/`n_neg` use strict `> 0` / `<= 0` uniformly across CSectT, Patell, and CalTime, matching SignTest's convention.
- **A10** `TestStatisticBase$confidence_type` is validated; a non-default value emits one "currently ignored" warning (one-sided p-values are not implemented).
- **A11** Docs-only: BMP/KP roxygen corrected to say standardization is by model sigma, not forecast-error-corrected sigma.
- **A12** Bootstrap's observed CAAR is now built from the same per-event CAR construction the draws use (not a separate `cumsum(aar)`); NA bootstrap draws are excluded from both the exceedance numerator and the valid-draw denominator. **A12c was revised by the orchestrator after reviewing Task 1**: bootstrap weights stay clustered by `firm_symbol` (a deliberate prior design choice), locked in Task 3 with a hand-replication test rather than changed.

## Task 2: API and Documentation Consistency (B1-B8)

- **B1/B5** `ModelBase`, `TestStatisticBase`, `ReturnCalculation` exported with subclass-contract roxygen; `generics::tidy(task)` dispatches via `@exportS3Method`. New `tests/testthat/test_public_surface.R` exercises evaluated (non-`eval=FALSE`) subclasses of all three through the real pipeline.
- **API snapshot** (commit `163078d`) accepted as an intentional surface change: adds `ModelBase`, `ReturnCalculation`, `TestStatisticBase` to exports/r6_classes and `S3method(generics::tidy,EventStudyTask)` to s3_methods — no other diff.
- **B2/B3/B7/B8** Vignette/README/skill signature drift fixed against real `formals()` (PanelEventStudyTask, cross_sectional_regression, run_event_study, validate_task); `interpreting-diagnostics.md` rewritten to the real `es_diagnostics()` structure, verified field-by-field.
- **B4/B6** pkgdown reference reorganized (statistical-conventions article, explicit dataset listing, Contracts & Lifecycle section, report_table de-internalized); unused `DT`/`patchwork` dropped from Suggests; `download_stock_data()`'s quantmod fallback guards `zoo::index()` with its own `requireNamespace()` check.

## Task 3: Test-Suite Quality (C1-C10) + Final Release Gate

### C1 — `adjust_p_values()` explicit-formula tests

Added tests for the KolariPynnonenTest, GeneralizedSignTest, RankTest, and CalendarTimePortfolioTest branches, comparing `p_raw_aar`/`p_raw_caar` against explicit `2*pt(-abs(t), df)` / `2*pnorm(-abs(z))` (RankTest's `p_raw_caar` asserted NA).

### C2 — vacuous regression tests made real

- **Bootstrap firm-clustering**: replaced the `expect_true(all(p >= 0 & p <= 1))` shape check with a hand replication of the wild-bootstrap algorithm (`set.seed()` + `sample(c(-1,1), n_firms, ...)`, one weight per unique `firm_symbol` shared across all of that firm's events), asserting exact equality with the package's output, then showing an event-level-clustering alternative on the same seed differs — numerically locking A12c.
- **Patell Q_i (k=2, k=4)**: replaced `is.finite()`-only checks with an independent hand computation (`.hand_patell_aar_z()`) of `aar_z` from the task's own estimation-window ARs, FEC sigma, and `Q_i = (m-k)/(m-k-2)`.
- **LinearFactorModel FEC**: replaced the loose `fec >= sigma` bound with the exact hat-matrix formula `sigma*sqrt(1+h_t)`, `h_t = x_t'(X'X)^{-1}x_t`, computed independently via `model.matrix()`/`solve(crossprod(...))`.
- **HAC SEs**: replaced "differs from OLS" with exact equality against `sandwich::NeweyWest()` on the same `lm()` fit.

### C3 — panel TWFE golden checks

`static_twfe`/`dynamic_twfe` tightened from loose numeric bounds (`< 1.0`, `< 3`, `mean > 0`) to exact golden checks against a direct `lm()` two-way-fixed-effects fit (with `sandwich::vcovCL` for the cluster-robust SE) on the same deterministic seeded fixture.

### C4 — real `.rank_events_for_cap()` test

Replaced the unconditional `skip()` with a real test: an event genuinely unfitted via a degenerate estimation window (its one contract warning asserted explicitly with `expect_warning()`) always has anomaly score `Inf` and always survives a `max_events` cap, regardless of its (NA) CAR magnitude.

### C5 — de-guarded tests

Removed the DCC-GARCH test's `tryCatch()`-to-`TRUE` handler and `if (is_fitted)` guard (now asserts `is_fitted` unconditionally, skipping only on a narrowly-matched non-convergence warning via `withCallingHandlers`); removed the `if (model$is_fitted)` guard around the MarketModel FEC "effective obs count" test (n_valid=110 always fits).

### C6 — narrowed `.try_download()`

Only network/HTTP-style failures (condition class or a narrow message pattern) convert to `skip()`; any other error (a parse/logic bug) now re-signals and fails the test. Two new direct tests of the helper.

### C7 — exhaustive random-draw seeding

A parse-based scan (test_that blocks containing `rnorm`/`runif`/`sample`/etc. without their own `set.seed()`) found 24 genuinely unseeded blocks across `test_edge_cases.R`, `test_models.R`, `test_multi_event_statistics.R`, `test_cross_sectional.R`, `test_synthetic_control.R`, and `test_intraday.R`; each got a deterministic seed. `test_intraday.R`'s `Sys.time()`-based fixtures replaced with fixed UTC `POSIXct` literals.

### C8 — numeric assertions for execute/export

`test_execute.R`: MarketModel alpha/beta/AR vs. `lm()`; MarketAdjusted/ComparisonPeriodMean AR vs. the arithmetic formula; FF3 AR vs. `lm()`+`predict()` on the factors. `test_export.R`: a CSV round-trip test asserting exported AR/CAR/AAR values equal the task's own values exactly.

### C9 — test-file organization

Relocated the full-pipeline CalendarTimePortfolioTest case from `test_bhar_test_statistics.R` to `test_multi_event_statistics.R` (not a BHAR test, not a duplicate — it exercises the real pipeline rather than a hand-built fixture); replaced the `deparse()`+`grepl("browser()")` source-grep check in `test_aar_test_statistics.R` with a behavioural `PatellZTest$compute()` test against a hand computation; folded a real numeric `CSectTTest` test into `test_caar_test_statistics.R`.

### C10 — no leaked warnings (WARN 0)

After Task 1, the full suite reported WARN 91 (baseline 6) — every degenerate-path test Task 1's fixes newly warn on, run without asserting the warning. Fixed systematically:

- New `muffle_short_window()` helper (`tests/testthat/helper-warnings.R`) mutes ONLY the `eventstudy_short_estimation_window` class, applied at the 3 shared fixture builders (`golden_multi_event_fixture()`, `invariant_fit_event_ar()`, the all-NA-window test's direct fit) plus ~16 individually-short `model$fit()` call sites across `test_golden_values.R`/`test_invariants.R` — never a blanket `suppressWarnings()`.
- Every degenerate-path call that legitimately warns (A2 all-NA-event exclusion, A3 KP <2-usable-events, A7 CalTime no-estimation-window, A5 rank-deficient design after event drop, A10 `confidence_type="less"`, the plot_stocks `do_sample=` deprecation which fires TWO warnings when `{lifecycle}` is installed) now wraps the call with `expect_warning()` naming the specific message — nested when more than one independent warning fires from the same call.
- A new locking test in `test_execute.R` asserts `fit_model()` collapses N short-window events into exactly ONE `eventstudy_short_estimation_window` warning listing the event ids.

### Final gate

| Check | Result |
|---|---|
| `devtools::document()` | No diff in `man/`/`NAMESPACE` |
| `NOT_CRAN=true` full suite (`devtools::test()`-equivalent) | **FAIL 0 \| WARN 0 \| SKIP 36 \| PASS 2733** (baseline before this quick task: FAIL 0 \| WARN 6 \| SKIP 37 \| PASS 2561) |
| `R CMD build` + `R CMD check --as-cran --no-manual` (scratch dir, `_R_CHECK_FORCE_SUGGESTS_=false`) | **Status: 1 NOTE** (the pre-existing "New submission / Package was archived on CRAN" incoming-feasibility NOTE only); test suite inside the check: **FAIL 0 \| WARN 0 \| SKIP 104 \| PASS 2595** (higher SKIP under `NOT_CRAN` unset, as `skip_on_cran()`-gated tests activate) |
| `pkgdown::check_pkgdown()` | No problems found |
| Non-ASCII baseline (`.github/non-ascii-baseline.txt`) | Unchanged, `diff -q` exit 0 |

**Two zero-behaviour-change production fixes were required to reach this gate** (documented as deviations below): a missing `caar_t` `globalVariables()` entry, and an explicit `relationship = "many-to-many"` declaration on an intentionally many-to-many join — both pure diagnostic-suppression, neither changes any computed value.

## Task Commits

Task 1 (6d16c86..f3df862, plus c4d0ea9):
1. `6d16c86` test: failing regression tests for re-evaluation findings A1-A12
2. `385845f` fix: A1,A6,A8 return model degenerate-input and FEC fixes
3. `26f034c` fix: A2,A3,A4,A7,A9,A11 multi-event statistics + contract dispatcher
4. `84036ec` fix: A5 cross_sectional_regression sample-integrity hardening
5. `110d75e` fix: A12 bootstrap observed/draw alignment and NA-draw denominator
6. `78fe3f9` fix: A10 confidence_type validation + A7 caltime_df consumers
7. `f304391` test: update existing tests for A2/A5/A6/A8/A9 behavior changes
8. `f3df862` docs: re-pin golden values, update conventions vignette, NEWS, version
9. `c4d0ea9` fix: A6 collapse per-event short-window warnings into one

Task 2 (1fb9a5f..87ec249):
10. `1fb9a5f` fix: B1/B5 export base classes and register generics::tidy
11. `163078d` test: accept API snapshot for B1/B5 exports
12. `1228487` docs: B2/B3/B7/B8 fix vignette/skill signature drift
13. `a5953ba` fix: B4 advertise report_table, pkgdown reference/article fixes
14. `517ea92` fix: B6 drop unused DT/patchwork Suggests, guard zoo::index()
15. `87ec249` docs: NEWS 0.66.0.9000 API and documentation consistency section

Task 3 (5ce8db3..f0c8570):
16. `5ce8db3` test: C1 explicit-formula adjust_p_values tests + C10 warning helper
17. `995b029` test: C2 make bootstrap/HAC/FEC regression tests real
18. `8909ad1` test: C2/C9/C10 Patell Q_i hand computation, relocate CalTime test, explicit warnings
19. `8fad31c` test: C3 panel TWFE golden checks, C4 real rank_events_for_cap test
20. `ad528e2` test: C5 de-guard DCC-GARCH test, C6 narrow try_download matching
21. `6166177` test: C6 narrow .try_download() to network/HTTP failures only
22. `c85de30` test: C5/C7/C10 de-guard FEC test, seed fixtures, explicit warnings
23. `2283b5a` test: C7 seed random draws, fix Sys.time() fixtures, C10 explicit warnings
24. `d04d9ec` test: C8 numeric assertions for execute/export pipelines
25. `788a047` test: C9 test-file organization and behavioural replacements
26. `07ea64a` test: C10 no leaked warnings across golden/invariant/contract-matrix suites
27. `2a8df44` docs: NEWS Testing subsection for C1-C10
28. `03f9971` fix: add missing caar_t to globalVariables() (final gate NOTE)
29. `f0c8570` fix: declare many-to-many relationship in EventStudyTask firm/request join

## Files Created/Modified

See frontmatter `key-files`. In full: 99 files across R/, tests/testthat/ (37 files), vignettes/, .claude/skills/, DESCRIPTION, NAMESPACE, NEWS.md, _pkgdown.yml.

## Decisions Made

See frontmatter `decisions`. Summary: A12c bootstrap clustering unit was reviewed and kept as firm-level (not changed); C10 (WARN-0 target) was added by the orchestrator mid-stream after Task 1's fixes introduced 91 previously-absent warnings; two production-code, zero-behaviour-change diagnostic-suppression fixes (`caar_t` global var, many-to-many join declaration) were made during Task 3's final-gate verification since they were latent NOTEs/warnings only exercised by new test coverage this quick task added.

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 1 - Bug] Missing `caar_t` NSE column in `globalVariables()`**
- **Found during:** Task 3 final gate, `R CMD check --as-cran` "checking R code for possible problems"
- **Issue:** Task 1's A12 bootstrap fix (commit `110d75e`) added `dplyr::select(relative_index, observed_caar, caar_t)` inside `bootstrap_test()` but only added `observed_caar` (not its sibling `caar_t`) to `globalVariables()`, producing a "no visible binding for global variable 'caar_t'" NOTE.
- **Fix:** Added `"caar_t"` to the `2026-09-24 re-evaluation fixes` group in `R/EventStudy-package.R`'s `globalVariables()` call. Zero behaviour change (pure R CMD check NOTE suppression).
- **Files modified:** R/EventStudy-package.R
- **Verification:** `devtools::document()` no diff; `R CMD check --as-cran` "R code for possible problems" step went from NOTE to OK; `test_bootstrap.R` unaffected (all pass).
- **Committed in:** `03f9971`

**2. [Rule 1 - Bug] Undeclared many-to-many join in `EventStudyTask$append_index_tbl()`**
- **Found during:** Task 3's new C2 bootstrap-firm-clustering test (`test_bootstrap.R`), the first test in the suite to construct a task with a firm recurring across multiple events via the full public `EventStudyTask$new()` constructor.
- **Issue:** `firm_tbl %>% dplyr::left_join(request_join_idx_tbl, by = "firm_symbol")` in `R/task.R` has always produced a genuine many-to-many join whenever a firm recurs (each of the firm's price rows must be duplicated once per event it belongs to — the intended, correct shape). dplyr >=1.1's join-relationship diagnostics flag this by default as a leaked warning; devtools::test()/NOT_CRAN runs happened not to surface it (an environment/timing artifact of testthat's warning capture across sessions), but R CMD check's fresh-install test run did, consistently, across two independent reruns.
- **Fix:** Declared `relationship = "many-to-many"` explicitly on that join. Zero behaviour change — the row output is bit-identical; only the diagnostic is silenced, documenting the pre-existing intended shape.
- **Files modified:** R/task.R
- **Verification:** `devtools::document()` no diff; re-ran `test_bootstrap.R` (0 warnings) and the full `NOT_CRAN` suite (0 warnings) and a fresh `R CMD check --as-cran` (0 warnings, the caar_t NOTE also gone, confirming both fixes together yield the single expected pre-existing NOTE).
- **Committed in:** `f0c8570`

---

**Total deviations:** 2 auto-fixed (both Rule 1 — R CMD check NOTE/warning suppression, zero behaviour change, caught during final-gate verification).
**Impact on plan:** Both fixes were necessary to reach the plan's mandated final-gate criteria ("no new NOTE/WARNING", "fix causes, never weaken checks") and are outside Task 3's nominal "tests only" file list, but are the minimal, surgical, non-behavioral changes the gate itself required. No scope creep beyond what the gate demanded.

## Issues Encountered

- **R CMD check's WARN was not reproducible under `devtools::test()`/`testthat::test_dir()` with `NOT_CRAN` unset via `devtools::load_all()`.** Diagnosing the `caar_t` NOTE and the many-to-many warning required actually running the built-and-installed tarball's `tests/testthat.R` (via `R CMD check`), not just `devtools::load_all()` + `testthat::test_dir()` — the two code paths surfaced different diagnostics for reasons not fully isolated (most likely testthat's own warning-capture state differing between a `load_all()`-attached namespace and a freshly-loaded installed package namespace). Resolved by treating the actual `R CMD check` run as authoritative for the final gate, and reproducing/fixing each flagged issue directly against source, then re-verifying with a full, fresh rebuild+recheck cycle.
- **First `test_data_download.R` C6 edit was silently lost in an earlier draft** (the change was reasoned through in-context but the corresponding `Edit` tool call was never issued before an interim test run appeared to "pass" against the unmodified file). Caught before committing by re-reading the file and finding the old `.try_download()` still in place; applied properly and re-verified before commit `6166177`.

## User Setup Required

None — no external service configuration required.

## Next Phase Readiness

- EventStudy 0.66.0.9000 is release-gate-clean: `R CMD check --as-cran` shows only the standard "new/reactivated CRAN submission" NOTE, the full test suite is FAIL 0 / WARN 0, `pkgdown::check_pkgdown()` passes, and the non-ASCII baseline is stable.
- All 29 locked findings from the 2026-09-24 re-evaluation (A1-A12, B1-B8, C1-C10) are implemented, tested, and documented in `NEWS.md` under `# EventStudy 0.66.0.9000` and in `vignettes/statistical-conventions.Rmd`'s audit log.
- No blockers. The orchestrator should verify the SUMMARY, update STATE.md/ROADMAP.md, and this quick task can be considered ready for `/gsd-ship` (subject to any additional orchestrator-level review).

---
*Phase: quick*
*Completed: 2026-09-24*
