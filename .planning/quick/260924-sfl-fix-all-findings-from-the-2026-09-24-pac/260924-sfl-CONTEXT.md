# Quick 260924-sfl — Context (locked decisions)

Source: 2026-09-24 package re-evaluation (R CMD check --as-cran clean: 1 expected NOTE; tests 0 FAIL).
User instruction: "Fix all of your findings." Decisions below are LOCKED — do not revisit.

Repro scripts from the audit (useful as starting points for regression tests):
`/tmp/claude-1000/-home-simonm-projects-datascience-eventstudy/fd6167d4-69a3-44c9-9b33-e4299cb74f20/scratchpad/{a,b,c,d,e,f}.R`

Global rules:
- Every fix ships with a regression test that FAILS on the pre-fix code (verify by reasoning or by running against HEAD).
- Degenerate input handling goes through the existing contract machinery (strict → error, lenient → NA/exclude + ONE warning), same pattern as WR-01 / STATS-04. See `R/shape_contracts.R` and memory note on contract-mode threading.
- Where a fix changes documented behaviour, update `vignettes/statistical-conventions.Rmd` (the audit table + "Audit result" line) and any affected golden values in `tests/testthat/test_golden_values.R` with an explanation comment.
- Add a NEWS.md section `# EventStudy 0.66.0.9000` (development version; bump DESCRIPTION Version to 0.66.0.9000 and Date to 2026-09-24). 0.66.0 is already submitted to CRAN — do not touch CRAN-SUBMISSION or cran-comments.md.
- Update the API snapshot (`test-api-snapshot.R` / `_snaps`) only for intentional surface changes, and say so.
- Final gate: `devtools::document()`, full test suite with NOT_CRAN=true green, `R CMD check --as-cran --no-manual` no new NOTE/WARNING, `pkgdown::check_pkgdown()` passes.

## A. Statistical defects (verified)

A1. `R/models_time_varying.R:91-94` RollingWindowModel: compute x_bar, y_bar, ss_xx, ss_xy on COMPLETE PAIRS only (both firm and index finite). df of the reported sigma = n_complete_pairs_in_last_window - 2; make fit() and calculate_statistics() use the same valid-count definition. Test: firm NA every 3rd day → rolling beta matches lm() on the same window within 1e-10.

A2. All-NA events in cumulative multi-event stats (`R/multi_event_test_statistics.R` ~45 CSectT, ~175 Patell, ~241 Sign, ~487 BMP, ~660 KP; `R/bootstrap.R:69`): an event with ZERO finite event-window ARs must be EXCLUDED from every cumulative (CAR-based) statistic and count, with one contract warning (strict mode: error). Keep the existing documented STATS-03 convention for a partial gap (missing AR mid-window contributes 0) unchanged. Test: 5 valid + 1 all-NA event → caar_t, caar_z, cbmp_t, csign_z, kp cumulative equal the 5-event values exactly.

A3. KP (`R/multi_event_test_statistics.R:694-710`): never silently fall back to kp_adj = 1. Compute r_bar over events with a usable (finite, non-constant) estimation-window standardized series only; degenerate events are excluded from r_bar with one contract warning. If fewer than 2 usable events remain, KP statistics are NA with a warning. Also fix the KP window-size term: use the per-day n (not `n_valid_events[1]`). Test: 5 correlated + 1 degenerate event → kp_t equals the 5-event-r_bar value, not BMP.

A4. RankTest (`:377`) and GeneralizedSignTest p_hat (`:289`): group by `event_id`, not `firm_symbol`. Update conventions doc ("within each event"). Test: two events sharing a firm → identical results to the same data with distinct firm symbols.

A5. `R/cross_sectional.R` `cross_sectional_regression()`:
- duplicate `event_id` in `data` → error naming the duplicated ids;
- events in the task without a matching row in `data` → one warning listing count/ids (then dropped);
- `car_window` extending outside the available event window → error;
- missing AR inside `car_window` → that event's CAR is NA and it is excluded with one warning (no na.rm summing).
Tests for each.

A6. `R/models.R` MarketModel (~187) and the other OLS-based models sharing the guard: require n_valid >= 3 (sigma needs df >= 1) else not fitted via contract; emit one warning when n_valid < 30 (the `validate_task()` default `min_estimation_obs`) — "estimation window has only n valid observations". Patell: `m` = number of VALID (finite-residual) estimation observations, `k` = number of estimated parameters taken explicitly from the model (MarketModel 2, FF3 4, Carhart 5, FF5 6, CPMA 1, MarketAdjusted 0, etc.), NOT `length(resid) - df`. Also fix the MarketModel FEC so mean(Rm_est) and SS_market use only rows used by lm() (complete pairs). Tests.

A7. `CalendarTimePortfolioTest` (`:552-580`): replace the self-referential denominator with the Brown & Warner (1980) crude-dependence-adjustment: ts_sd = sd of the ESTIMATION-window AAR series (cross-event mean AR per estimation relative day), caltime_t = AAR_t / ts_sd, ccaltime_t = CAAR_t / (ts_sd * sqrt(L)); df = n_estimation_days - 1. Keep class name and output columns (API stable); rewrite the roxygen + conventions section to describe exactly this and cite Brown & Warner (1980, 1985). Update `adjust_p_values()` df for this branch accordingly. Test: doubling the event-day shock doubles caltime_t on that day (denominator independent of event window).

A8. MarketAdjustedModel FEC: nothing is estimated → forecast-error correction factor is 1 (FEC sigma == sigma), and Patell k = 0. ComparisonPeriodMeanAdjustedModel keeps sqrt(1 + 1/m). Update conventions doc + golden values.

A9. Sign conventions: CSectT (`:32`), Patell (`:160`), CalendarTime (`:544`) `n_pos` use `>= 0`; change to strict `> 0` (and `n_neg` to `<= 0`) matching the documented Sign-test convention.

A10. `TestStatisticBase` `confidence_type`/`confidence_level` are never used: validate `confidence_type` with match.arg(c("two-sided","less","greater")) and, since every p-value in the package is two-sided, emit one warning at construction when a non-default `confidence_type` is supplied ("currently ignored: all p-values are two-sided"). Document this in the roxygen. (Do NOT implement one-sided p-values.)

A11. Documented conventions — NO formula change, docs only:
- AR/CAR t-tests keep the documented constant-sigma approximation. Note in the conventions vignette that `forecast_error_corrected_sigma_car` is computed but intentionally not used by ARTTest/CARTTest.
- BMP/KP standardize by MODEL sigma (documented + golden-pinned). Fix the contradicting BMP roxygen (~`:431`) to say model sigma.

A12. Bootstrap (`R/bootstrap.R`) — verify then fix if real: (a) observed CAAR and bootstrap CAAR must be the same statistic under NA ARs (`:63` vs `:129`); (b) NA bootstrap draws must be excluded from the p-value denominator, not counted as "not exceeding"; (c) REVISED 2026-09-24 after planning: weights STAY clustered by firm_symbol (deliberate earlier fix; coarser firm clusters are robust to cross-event correlation of a recurring firm) — no change, just lock it with a hand-replication test (C2). Tests for each confirmed issue.

## B. API / documentation consistency

B1. Export `ModelBase`, `TestStatisticBase`, `ReturnCalculation` (@export) — vignettes/README/skill subclass them. Update API snapshot. Add an evaluated (not eval=FALSE) minimal subclass test in tests.
B2. Fix `PanelEventStudyTask$new()` calls in `vignettes/modern-did-estimators.Rmd:48` and `vignettes/automated-reports.Rmd:134` to the real signature `(panel_data, unit_id, time_id, outcome, treatment, treatment_time)`; fix `cross_sectional_regression(characteristics=)` in `automated-reports.Rmd:103`. Grep all eval=FALSE chunks in vignettes + README for other signature drift and fix.
B3. Rewrite `.claude/skills/es-advisor/reference/interpreting-diagnostics.md` to the REAL `es_diagnostics()` structure (meta, estimation_window{r2, sigma, degree_of_freedom, acf1, shapiro_p, dw_stat, ljung_box_p}, event_window{ar_t, ar_p, car_t, car_p, final_car}, cross_sectional{...}, contract_state, aggregate_summary) — confirm by running it on `earnings_surprises`; restate thresholds in terms of real fields.
B4. `_pkgdown.yml`: add `statistical-conventions` article; add `dieselgate`, `earnings_surprises`, `eventstudy-deprecation`, `eventstudy-shape-contracts`, `report_table` to reference. Remove `\keyword{internal}` from `report_table` (it is exported and advertised). `pkgdown::check_pkgdown()` must pass.
B5. Register `tidy`: `@exportS3Method generics::tidy` for `tidy.EventStudyTask` (delayed registration), add `generics` to Suggests; keep the existing `export(tidy.EventStudyTask)`. Test that `generics::tidy(task)` dispatches (skip_if_not_installed("generics")).
B6. Remove unused `DT` and `patchwork` from Suggests. Guard `zoo::index()` (`R/data_download.R:58`) with its own requireNamespace("zoo") check.
B7. Update `.claude/skills/es-capabilities/SKILL.md` signatures for `run_event_study(task, parameter_set, report, report_args, verbose)` and `validate_task(task, parameter_set, min_estimation_obs, verbose)`; verify every other signature listed there against formals().
B8. Argument naming (n_boot / n_simulations / n_placebo, min_obs / min_estimation_obs, estimation_window / estimation_window_length): NO rename (CRAN-submitted, API-locked, and the concepts differ). Document the distinction in the relevant @param text only if ambiguous. No code change.

## C. Test-suite quality

C1. `adjust_p_values()` (`R/p_adjustment.R:77-105`): tests for KP, generalized sign, rank and calendar-time branches that compare against explicit `2*pt(-abs(t), df)` / `2*pnorm(-abs(z))` values.
C2. Make vacuous regression tests real: `test_bootstrap.R:187` (use multiple events per firm/cluster and assert clustering effect), `test_multi_event_statistics.R:373,390` (assert Q_i numerically), `test_models.R:917` (compute h_t and assert FEC exactly), `test_models.R:756` (assert HAC SE equals sandwich::vcovHAC-based value when sandwich installed).
C3. Panel tests `test_panel.R:119,145,151`: tighten to golden values (deterministic seeded fixture, tolerance ~1e-8 against a stored reference or against the direct lm()/fixest-free computation).
C4. `test_es_diagnostics.R:145-157`: replace the always-skip with a real test of `.rank_events_for_cap` (unfitted events always included).
C5. DCC test (`test_models_time_varying.R:145-158`): remove the tryCatch→expect_true(TRUE) and the `if (is_fitted)` guards (also `test_edge_cases.R:1010`); non-convergence may skip ONLY via an explicit, narrowly-scoped convergence check with a message; other errors must fail.
C6. `test_data_download.R:5-11` `.try_download()`: only convert network/HTTP errors into skip(); parse/logic errors must fail.
C7. Add `set.seed()` to each test that draws random numbers without its own seed (list in audit: test_edge_cases.R:15,39,62,84,468,858,907,930,1505,1633; test_models.R:634,859; test_multi_event_statistics.R:167,801,817,827; test_cross_sectional.R:109,250; test_synthetic_control.R:270,439,467; test_intraday.R:73,88 — re-scan to catch all). Replace `Sys.time()` fixtures in test_intraday.R with fixed POSIXct in UTC.
C8. Strengthen shape-only tests where it matters most: `test_execute.R` (assert at least one numeric result per model against a direct computation) and the key stats in `test_export.R` (exported values equal task values). Full elimination of all 393 shape-only tests is out of scope — do the high-value ones.
C9. Organization: move the calendar-time test out of `test_bhar_test_statistics.R` (duplicate of test_multi_event_statistics.R:255 — delete if duplicate); replace the source-grep `browser()` check in `test_aar_test_statistics.R:48` with a behavioural test; fold the single weak test in `test_caar_test_statistics.R` into a real CSectT numeric test. File naming (test- vs test_): leave as is.

## C10 (added by orchestrator after Task 1 review, 2026-09-24) — no leaked warnings

After Task 1 the suite reports WARN 91 (baseline was 6). Every warning a test triggers must be EXPECTED, not leaked:
- Intentionally short golden/invariant fixtures (test_golden_values.R, test_invariants.R, test_edge_cases.R:308/1058, test_numerical_stability.R): add a helper in tests/testthat/helper-*.R, e.g. `muffle_short_window <- function(expr) withCallingHandlers(expr, eventstudy_short_estimation_window = function(w) invokeRestart("muffleWarning"))`, muffling ONLY that class (commit c4d0ea9 made the A6 advisory a classed condition). Do not use blanket suppressWarnings().
- Tests that exercise a degenerate path (KP <2 usable events, CalTime no estimation window, all-NA event exclusion, unmatched cross-sectional events, confidence_type = "less", rank-deficient design, do_sample deprecation, simulated provider failure): wrap with expect_warning(..., class= or regexp=) asserting the specific warning.
- Add a test: fit_model() on a task with >= 3 short-window events emits EXACTLY ONE eventstudy_short_estimation_window warning listing the event ids.
- Target: `WARN 0` in the NOT_CRAN full-suite run (hard ceiling: no unexpected warnings at all).
