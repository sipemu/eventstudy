---
phase: 260924-sfl-fix-all-findings-from-the-2026-09-24-pac
plan: 01
type: execute
wave: 1
depends_on: []
files_modified:
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
  - R/report.R
  - R/data_download.R
  - R/EventStudy-package.R
  - DESCRIPTION
  - NAMESPACE
  - NEWS.md
  - _pkgdown.yml
  - man/ModelBase.Rd
  - man/TestStatisticBase.Rd
  - man/ReturnCalculation.Rd
  - man/report_table.Rd
  - man/CalendarTimePortfolioTest.Rd
  - man/BMPTest.Rd
  - man/KolariPynnonenTest.Rd
  - man/bootstrap_test.Rd
  - man/cross_sectional_regression.Rd
  - man/degenerate-input-contract.Rd
  - vignettes/statistical-conventions.Rmd
  - vignettes/modern-did-estimators.Rmd
  - vignettes/automated-reports.Rmd
  - .claude/skills/es-advisor/reference/interpreting-diagnostics.md
  - .claude/skills/es-capabilities/SKILL.md
  - tests/testthat/test_reeval_statistical_fixes.R
  - tests/testthat/test_public_surface.R
  - tests/testthat/test_golden_values.R
  - tests/testthat/test_bootstrap.R
  - tests/testthat/test_multi_event_statistics.R
  - tests/testthat/test_models.R
  - tests/testthat/test_models_time_varying.R
  - tests/testthat/test_edge_cases.R
  - tests/testthat/test_cross_sectional.R
  - tests/testthat/test_p_adjustment.R
  - tests/testthat/test_panel.R
  - tests/testthat/test_es_diagnostics.R
  - tests/testthat/test_data_download.R
  - tests/testthat/test_execute.R
  - tests/testthat/test_export.R
  - tests/testthat/test_synthetic_control.R
  - tests/testthat/test_intraday.R
  - tests/testthat/test_bhar_test_statistics.R
  - tests/testthat/test_aar_test_statistics.R
  - tests/testthat/test_caar_test_statistics.R
  - tests/testthat/_snaps/api-snapshot.md
  - .github/non-ascii-baseline.txt
autonomous: true
requirements: [C10, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B1, B2, B3, B4, B5, B6, B7, B8, C1, C2, C3, C4, C5, C6, C7, C8, C9]

estimate:
  tokens: 480000
  raw_tokens: 480000
  tasks: 3
  confidence: low

must_haves:
  truths:
    - "A1: RollingWindowModel beta/alpha/sigma computed on complete (firm, index) pairs match lm() on the same window within 1e-10 when firm returns are NA every 3rd day"
    - "A2: A multi-event group with 5 valid events plus 1 event whose event-window ARs are all NA yields caar_t, caar_z, cbmp_t, csign_z, cgsign_z and ckp_t identical to the 5-event run; lenient emits exactly one warning per degenerate event across the whole pipeline, strict errors; a partial mid-window gap still contributes 0 (STATS-03 unchanged)"
    - "A3: KolariPynnonenTest never silently falls back to the unadjusted BMP value; r_bar uses only usable events, fewer than 2 usable events gives NA plus a warning, and the adjustment uses the per-day event count"
    - "A4: RankTest and GeneralizedSignTest give identical results whether a firm recurs across events or every event has a distinct firm symbol"
    - "A5: cross_sectional_regression errors on duplicated event_id and on a car_window outside the event window, warns once on unmatched task events and once on events whose CAR is NA because an AR inside car_window is missing"
    - "A6/A8: OLS models with fewer than n_params + 1 valid estimation observations go through the contract; 3..29 valid obs fit with exactly one short-window warning; Patell uses m = valid estimation obs and k = model n_params; MarketModel FEC uses complete pairs only; MarketAdjustedModel FEC sigma equals sigma"
    - "A7: CalendarTimePortfolioTest uses the Brown-Warner (1980, 1985) estimation-window AAR standard deviation, so doubling a pure event-day shock exactly doubles caltime_t on that day, and adjust_p_values()/tidy() use df = n_estimation_days - 1"
    - "A9/A10: n_pos counts strictly positive ARs in every multi-event statistic; a non-default confidence_type warns once that it is ignored and an invalid one errors"
    - "A12a/b: bootstrap observed CAAR and bootstrap CAAR are the same statistic under NA ARs; NA bootstrap draws are excluded from the p-value denominator"
    - "B1/B5: ModelBase, TestStatisticBase, ReturnCalculation are exported and subclassable in an evaluated test; generics::tidy(task) dispatches to tidy.EventStudyTask"
    - "B2/B3/B4/B7: every vignette/README/skill call matches the real formals(); interpreting-diagnostics.md documents only fields that es_diagnostics() actually returns; pkgdown::check_pkgdown() passes"
    - "C1-C9: no vacuous or always-skip tests remain at the audited sites; random tests are seeded; final gate (document, NOT_CRAN full suite, R CMD check --as-cran --no-manual, check_pkgdown) is green with no new NOTE/WARNING"
  artifacts:
    - path: "tests/testthat/test_reeval_statistical_fixes.R"
      provides: "Failing-first regression tests for A1-A10 and A12, each test_that name prefixed with its item id"
    - path: "tests/testthat/test_public_surface.R"
      provides: "Evaluated subclass tests for exported base classes (B1) and generics::tidy dispatch (B5)"
    - path: "vignettes/statistical-conventions.Rmd"
      provides: "Updated audit table, Audit result lines and audit summary reflecting A1-A12"
    - path: "NEWS.md"
      provides: "# EventStudy 0.66.0.9000 section listing every A/B/C change"
    - path: "R/contract.R"
      provides: "Shared all-NA-event exclusion helper used by multi-event statistics and bootstrap_test"
  key_links:
    - from: "R/execute.R calculate_statistics()"
      to: "R/contract.R all-NA exclusion helper"
      via: "resolved degenerate mode passed into the multi-event dispatcher, applied once before every statistic"
      pattern: "degenerate_mode"
    - from: "model statistics$n_params (every model class)"
      to: "PatellZTest Q_i"
      via: "per-event k joined on event_id"
      pattern: "n_params"
    - from: "CalendarTimePortfolioTest attribute caltime_df"
      to: "adjust_p_values() caltime branch and tidy() p.value"
      via: "shared df helper in R/p_adjustment.R"
      pattern: "caltime_df"
    - from: "R/export.R roxygen @exportS3Method generics::tidy"
      to: "NAMESPACE S3method(generics::tidy,EventStudyTask)"
      via: "devtools::document()"
      pattern: "S3method\\(generics::tidy"
---

<objective>
Fix every finding from the 2026-09-24 package re-evaluation, exactly as locked in
260924-sfl-CONTEXT.md: (A) statistical silent-wrong defects A1-A12, (B) API/documentation
consistency B1-B8, (C) test-suite quality C1-C9, then run the full release gate.

Purpose: the package's core value is "never a silently incorrect statistical result". The audit
found places where degenerate or recurring-firm inputs produce plausible-looking wrong numbers,
docs/skills that point at non-existent signatures or fields, and tests that cannot fail. This plan
removes all three classes of defect without changing behaviour on valid inputs (except where a
locked decision redefines a statistic, which is then documented and golden-pinned).

Output: fixed R sources, two new test files, strengthened existing tests, updated conventions
vignette + golden values, NEWS section for 0.66.0.9000, fixed vignettes/skills/pkgdown config,
refreshed API snapshot, and a green final gate.

Execution model: the three tasks are run by three SEPARATE executor agents in sequence
(Task 1, then Task 2, then Task 3). Each task is self-contained: re-read the files it lists,
do not assume memory of the previous task beyond what is committed in git.
</objective>

<execution_context>
@~/.claude/gsd-core/workflows/execute-plan.md
@~/.claude/gsd-core/templates/summary.md
</execution_context>

<context>
@.planning/STATE.md
@.claude/CLAUDE.md
@.planning/quick/260924-sfl-fix-all-findings-from-the-2026-09-24-pac/260924-sfl-CONTEXT.md

Project rules that bind every task (from CLAUDE.md, STATE.md invariants and project memory):
- R 4.1+, R6, testthat 3e only. No new Imports. Optional packages stay in Suggests behind requireNamespace().
- Degenerate-input contract lives in R/contract.R: `.resolve_degenerate_mode(ps_value)` (ParameterSet field, then option `EventStudy.degenerate_handling`, then "lenient") and `.handle_degenerate(mode, condition, component, event_id, firm_symbol, private_env)` (strict: stop(call.=FALSE); lenient: exactly one warning(call.=FALSE), and if private_env is given it sets .is_fitted FALSE and .degenerate_handled TRUE). ParameterSet is NOT passed to model$fit(); execute.R threads degenerate_mode/event_id/firm_symbol onto the cloned model as fields.
- CONTRACT-04 / one-warning invariant: a degenerate event emits exactly ONE warning across the whole pipeline. An event already reported by its model (unfitted ModelBase instance) must not produce a second warning downstream.
- Arithmetic models (MarketAdjusted, ComparisonPeriodMean, BHAR) must NOT get zero-variance guards; only `< 2 obs` is model-level degenerate for them. FEC/df use finite counts, never nrow().
- Error style: classed errors via rlang::abort(msg, class = c("eventstudy_error_<kind>", "eventstudy_error")) as already used in R/cross_sectional.R and R/export.R.
- Non-ASCII: the CI step greps R/, man/figures/, inst/ for non-ASCII bytes and diffs against .github/non-ascii-baseline.txt (grep -rn, so line numbers are part of the baseline). New code and roxygen must be pure ASCII: in roxygen prose transliterate (Pynnonen, not a \u escape - \uXXXX inside #' breaks Rd); in R string literals use \uXXXX escapes.
- New NSE column names used inside dplyr verbs must be added to globalVariables() in R/EventStudy-package.R (or referenced via .data$) so R CMD check stays NOTE-free.
- Valid-input behaviour must not change except where a locked item redefines it; the ~2560-test suite must stay green (last audit run: FAIL 0 | WARN 6 | SKIP 37 | PASS 2561).
- 0.66.0 is already submitted to CRAN: never touch CRAN-SUBMISSION or cran-comments.md.

Audit repro scripts (starting points for regression fixtures; they may have been cleaned up - if
missing, the fixture descriptions inside each task are sufficient):
/tmp/claude-1000/-home-simonm-projects-datascience-eventstudy/fd6167d4-69a3-44c9-9b33-e4299cb74f20/scratchpad/{a_funcs,a,b,c,d,e,f}.R
and a vignette/README/skill signature-drift scanner at .../scratchpad/chk.R.
The a_funcs.R fixture pattern: per event a tibble with event_id, firm_symbol, group="g",
relative_index = c(-(L1+5):-6, -5:5), estimation_window/event_window 0/1 flags and
abnormal_returns; a model tibble (event_id, model = list(list(statistics = list(sigma,
degree_of_freedom, residuals, forecast_error_corrected_sigma)))) so statistics can be called
directly as `SomeTest$new()$compute(data_tbl, model_tbl)`.

Key source anchors (line numbers at HEAD 92a1882):
- R/multi_event_test_statistics.R: CSectTTest 12 (n_pos 32, CAR cumsum 45), PatellZTest 77 (k from length(resid)-df 91-103, m = n() 108-118, n_pos 160, csar 175), SignTest 202 (car 241), GeneralizedSignTest 274 (p_hat grouping 287-296, car 328), RankTest 362 (ranking grouping 375-384), BMPTest 437 (roxygen 428-433, csar 487), CalendarTimePortfolioTest 523 (n_pos 544, ts_sd 552-582), KolariPynnonenTest 611 (csar 660, r_bar 674-699, adj 701-713).
- R/models.R: ModelBase 9 (calculate_forecast_error_correction 85-110), MarketModel 140 (n_valid guard 187-202, statistics 298-360 incl. FEC inputs 347-359), MarketAdjustedModel 382 (guard 398, FEC 463-471), ComparisonPeriodMeanAdjustedModel 491 (FEC 575-583), CustomModel 590 (inherits MarketModel), LinearFactorModel 627 (guard 672-690), FF3 859, FF5 913, Carhart 968, GARCHModel 1023, BHARModel 1217, VolumeModel 1357, VolatilityModel 1489.
- R/models_time_varying.R: RollingWindowModel 10 (fit 34-126, rolling loop 83-106), DCCGARCHModel 220.
- R/execute.R: fit_model 77, calculate_statistics ~130-220, .calculate_multiple_event_test_statistics 244.
- R/bootstrap.R: whole file (167 lines). R/cross_sectional.R: cross_sectional_regression 29, inner_join 52, .extract_cars 150 (na.rm sum 171). R/p_adjustment.R: branches 60-105. R/export.R: tidy.EventStudyTask 256, p-value helper .compute_pval ~364. R/single_event_test_statistics.R: TestStatisticBase 2-49.
</context>

<tasks>

<task type="auto" tdd="true">
  <name>Task 1: Section A - fix statistical silent-wrong defects A1-A12 with failing-first regression tests, conventions vignette, golden values, NEWS and version bump</name>
  <files>tests/testthat/test_reeval_statistical_fixes.R (new), R/contract.R, R/execute.R, R/multi_event_test_statistics.R, R/models.R, R/models_time_varying.R, R/cross_sectional.R, R/bootstrap.R, R/single_event_test_statistics.R, R/p_adjustment.R, R/export.R, R/EventStudy-package.R, tests/testthat/test_golden_values.R, tests/testthat/test_bootstrap.R, vignettes/statistical-conventions.Rmd, NEWS.md, DESCRIPTION, man/*.Rd (regenerated); plus any existing test file whose assertion encodes a behaviour that a locked A-item deliberately changes (expected: test_multi_event_statistics.R, test_models.R, test_edge_cases.R, test_cross_sectional.R, test_p_adjustment.R, test_contract*.R, test_invariants.R, test_simulation.R)</files>
  <behavior>
    All tests live in tests/testthat/test_reeval_statistical_fixes.R; every test_that() description starts with its item id ("A1: ...", "A2: ...", ...). Each must FAIL on HEAD 92a1882 and pass after the fix.
    - A1: 150-day estimation window, index returns rnorm, firm = 0.001 + 1.2*index + noise; set firm NA every 3rd day and add +0.2 index outliers only on those NA days; RollingWindowModel$new(window_size = 150, min_obs = 30)$fit(): beta and alpha equal coef(lm(firm ~ index)) (complete pairs) within 1e-10; reported sigma equals summary(lm)$sigma (df = n_complete - 2) within 1e-10; statistics$degree_of_freedom equals n_complete - 2.
    - A2 (direct compute): 5 valid events + 1 event whose ARs are all NA (list-model fixture, sigma NA for that event). CSectTTest caar_t, PatellZTest caar_z, BMPTest cbmp_t, SignTest csign_z, GeneralizedSignTest cgsign_z, KolariPynnonenTest ckp_t on the 6-event input are identical (expect_identical or tolerance 0) to the 5-event input; exactly one warning naming the excluded event_id is emitted per compute call; with options(EventStudy.degenerate_handling = "strict") (withr::local_options) the call errors naming the event_id.
    - A2 (pipeline, f.R pattern): 4 firms with create_mock_firm_data/create_mock_index_data/create_mock_request, firm F4's estimation-window prices set NA so its model is unfitted; run_event_study with CSectT + KP + PatellZ: total warnings == 1 (the model's), and last-day caar_t / caar_z / ckp_t equal the 3-firm run exactly.
    - A2 (STATS-03 unchanged): one NA AR mid-window in one event; CSectT caar_t equals a hand computation where the missing AR contributes 0 to that event's CAR.
    - A3: 5 correlated events (common base series + idiosyncratic noise) + 1 event whose estimation-window ARs are constant (sigma 0.02 in the model): kp_t equals bmp_t * sqrt((1 - r5) / (1 + (n_t - 1) * r5)) where r5 = mean off-diagonal cor over the 5 usable events and n_t = per-day n_valid_events (6), and kp_t differs from bmp_t; exactly one warning naming the excluded event. With only 1 usable event: kp_t and ckp_t all NA and one warning. Per-day n: make one event's AR NA on a single event day and assert that day's kp_t uses n_t = N - 1.
    - A4: same seeded 4-event data once with firms F1..F4 and once with firms F1,F1,F2,F2: RankTest rank_z and GeneralizedSignTest gsign_z / cgsign_z identical.
    - A5 (create_fitted_mock_task(n_firms = 4)): data with a duplicated event_id -> error of class eventstudy_error_bad_argument whose message names the duplicated id; data missing 2 of 4 events -> exactly one warning mentioning the count/ids, n_obs == 2; car_window = c(-20, 2) when the event window is [-5, 5] -> error; one AR set NA inside car_window -> exactly one warning, that event absent from the regression (n_obs reduced by 1), other events' CARs unchanged.
    - A6: MarketModel with exactly 2 valid estimation obs -> lenient: not fitted + exactly one warning, strict: error; 20 valid obs -> fitted + exactly one warning matching "estimation window has only 20 valid observations"; 120 obs -> expect_no_warning. FamaFrench3FactorModel with 4 valid obs -> contract. MarketModel FEC with firm NA on rows where the index has extreme values: forecast_error_corrected_sigma equals sigma * sqrt(1 + 1/m + (Rm_t - mean_cp)^2 / SS_cp) computed on complete pairs only, within 1e-12. Patell: statistics$n_params is 2 (MarketModel), 4 (FF3), 5 (Carhart), 6 (FF5), 1 (CPMA), 0 (MarketAdjusted); a pipeline Patell aar_z with FF3 and one event with 10 NA estimation firm returns equals a hand computation using m = valid count and k = 4 within 1e-10.
    - A7: 20 events, estimation ARs rnorm, event-day-0 ARs set to exactly s for every event: caltime_t on day 0 with shock 2s is exactly 2 * caltime_t with shock s (denominator independent of the event window); caltime_t equals AAR_t / sd(estimation-window AAR series) hand-computed; attr(result, "caltime_df") == n_estimation_days - 1; adjust_p_values() p_raw_aar equals 2 * pt(-abs(caltime_t), n_estimation_days - 1).
    - A8: MarketAdjustedModel forecast_error_corrected_sigma equals rep(sigma, n_event) exactly; ComparisonPeriodMeanAdjustedModel still equals sigma * sqrt(1 + 1/m).
    - A9: an AR exactly 0 on a day is counted in n_neg and not in n_pos for CSectTTest, PatellZTest and CalendarTimePortfolioTest.
    - A10: TestStatisticBase subclass constructed with confidence_type = "bogus" errors; "less" or "greater" emits exactly one warning containing "currently ignored"; the default is silent.
    - A12a: one NA AR mid-window: bootstrap_test observed_caar equals the mean over events of per-event CAR (NA contributing 0) and the observed CAAR t is built from the same per-event CARs as the draws; on NA-free input observed_caar equals cumsum(observed_aar) within 1e-14.
    - A12b: 2 events whose day-k ARs are +0.01 and -0.01: half the Rademacher draws have zero sd (NA t); boot_p_aar on day k equals exactly 1 (pre-fix it is about 0.5).
    - A12c (REVISED, orchestrator 2026-09-24): bootstrap weights STAY clustered by firm_symbol (deliberate prior fix; firm clusters capture cross-event correlation of a recurring firm). No behaviour change; covered in C2 by a hand-replication test.
  </behavior>
  <action>
Implements CONTEXT items A1-A12 (locked). Work test-first:

Step 1 - RED. Write tests/testthat/test_reeval_statistical_fixes.R exactly per the behavior block (seed every random fixture with set.seed; use helpers from tests/testthat/helper-mock-data.R: create_fitted_mock_task, create_mock_task, create_mock_model_data, create_mock_factor_model_data, create_mock_firm_data, create_mock_index_data, create_mock_request). Run it against HEAD and confirm every test fails for the reason its item describes; record the pre-fix failure of each item for the SUMMARY. Commit as "test(260924-sfl): failing regression tests for re-evaluation findings A1-A12".

Step 2 - shared all-NA exclusion (A2). In R/contract.R add an internal helper (suggested name .exclude_all_na_events(data_tbl, model, mode, component)) that finds event_ids with zero finite event-window abnormal_returns and removes their rows from data_tbl and from the model tibble. Events whose model object is a ModelBase instance with is_fitted FALSE were already reported by fit() (one-warning invariant, CONTRACT-04) and are excluded silently. All other excluded events are reported once per call through .handle_degenerate(mode, condition = "<k> event(s) with no finite event-window abnormal returns excluded from multi-event statistics (event_id: ...)", component, private_env = NULL): strict errors, lenient warns once. Extend the degenerate-input-contract roxygen to state this multi-event exclusion rule and that the STATS-03 partial-gap convention (a missing AR inside an otherwise valid event contributes 0 to that event's CAR) is unchanged. In R/execute.R, calculate_statistics() resolves mode via .resolve_degenerate_mode(parameter_set$degenerate_handling) and passes it as a new degenerate_mode argument to .calculate_multiple_event_test_statistics(), which applies the helper once per group before looping over the tests (so N configured statistics produce one warning, not N). Every CAR-based compute() (CSectT, Patell, Sign, GeneralizedSign, BMP, KP) also calls the helper at its top with .resolve_degenerate_mode(NULL) so direct compute() calls are protected; after the central pass this is a no-op. Exclusion happens at the top, so every output column of the 6-event run equals the 5-event run.

Step 3 - multi-event statistics in R/multi_event_test_statistics.R.
A3 (KP): build the estimation-window SAR matrix, then keep only usable events: at least 2 finite SARs, finite sigma, and strictly positive finite sd (non-constant). Report excluded events once via .handle_degenerate (component "KolariPynnonenTest"). If fewer than 2 usable events remain, or any remaining off-diagonal correlation is non-finite, or the adjustment's numerator/denominator is invalid, set kp_t and ckp_t to NA and report once through the contract - never substitute an adjustment factor of 1. r_bar = mean off-diagonal correlation of the usable events (numerically identical to the current formula when every event is usable). Replace the single first-row event count with the per-day count: the AAR adjustment uses that day's n_valid_events, the cumulative adjustment uses that day's n_valid of the cumulative SARs.
A4: in RankTest rank and centre within each event (group by event_id instead of the firm symbol column); in GeneralizedSignTest estimate p_hat per event_id then average.
A6 (Patell part): m per event = number of finite estimation-window abnormal_returns for that event (not row count); k per event = the model's statistics$n_params (joined by event_id), falling back to 2 only when a model does not report n_params (user subclasses and list mocks); delete the derivation of k from residual length minus df. Keep the Q_i = (m - k)/(m - k - 2) formula and its m > k + 2 guard.
A7 (CalendarTimePortfolioTest): ts_sd = sd of the estimation-window AAR series (group estimation_window == 1 rows by relative_index, mean AR with na.rm, keep finite values); n_estimation_days = number of finite estimation AARs; caltime_t = aar / ts_sd; ccaltime_t = caar / (ts_sd * sqrt(L)) with L = 1..n event days; keep the n_events >= 2 guard; if n_estimation_days < 2 or ts_sd is not finite and positive, NA plus one contract report. Attach attr(result, "caltime_df") <- n_estimation_days - 1 after the final select. Keep class name, name 'CalTimeT' and output columns unchanged. Rewrite the roxygen to describe exactly this (Brown-Warner crude dependence adjustment) with @references Brown and Warner (1980) J. Financial Economics 8(3) 205-258 and (1985) J. Financial Economics 14(1) 3-31. In R/p_adjustment.R add an internal df helper that returns attr caltime_df when present (fallback to the legacy nrow - 1 only for objects created before this change, with a comment saying why) and use it in the caltime branch of adjust_p_values(); use the same helper for caltime_t/ccaltime_t in tidy()'s .compute_pval in R/export.R so no consumer pairs the new statistic with a cross-sectional df.
A9: CSectTTest, PatellZTest and CalendarTimePortfolioTest count n_pos with strict > 0 and n_neg with <= 0 (matching SignTest).
A11 (docs only, no formula change): fix the BMPTest roxygen to say ARs are standardized by the MODEL sigma (not forecast-error-corrected sigma); state the same for KolariPynnonenTest.

Step 4 - models.
A1 (R/models_time_varying.R RollingWindowModel): inside each rolling window restrict to complete pairs (both firm and index finite) before computing x_bar, y_bar, ss_xx, ss_xy, residuals and sigma; sigma denominator = n_complete_pairs_in_window - 2. Make private calculate_statistics() use the same complete-pair count for df (n_complete_pairs_in_last_window - 2) and FEC, so fit() and calculate_statistics() share one valid-count definition. Set statistics$n_params = 2.
A6 (R/models.R): MarketModel (CustomModel inherits it) and LinearFactorModel require n_valid >= n_params + 1 (MarketModel 3; FF3 5; Carhart 6; FF5 7), otherwise route through .handle_degenerate with condition "insufficient estimation observations (<n> valid, need <n_params + 1>)". When the model does fit with n_valid < 30 (the validate_task() default min_estimation_obs; define an internal constant in R/contract.R with a comment pointing at validate_task, do not change validate_task's formals), emit exactly one plain warning(call. = FALSE) "<model_name> [event_id=..] [firm=..]: estimation window has only <n> valid observations (recommended minimum 30); estimates may be unreliable" in both modes (it is advisory, not degenerate). Do not add this guard/warning to arithmetic models (MarketAdjusted, CPMA, BHAR keep their < 2 guard). MarketModel FEC: pass only complete-pair rows' index returns to calculate_forecast_error_correction so mean(Rm_est) and SS_market use exactly the rows lm() used. Every model class sets statistics$n_params in its statistics computation: MarketModel/CustomModel 2, RollingWindow 2, LinearFactorModel family = number of fitted coefficients (FF3 4, Carhart 5, FF5 6), ComparisonPeriodMeanAdjusted 1, MarketAdjusted 0; for GARCH, DCC-GARCH, BHAR, Volume and Volatility read each model's mean equation and set the number of parameters it estimates from the estimation window (0 when nothing is estimated), documenting the value in a one-line comment. Document statistics$n_params in the ModelBase roxygen as the field Patell uses (optional for user subclasses; default 2).
A8: MarketAdjustedModel estimates nothing, so its forecast_error_corrected_sigma = rep(sigma, n_event) (correction factor 1) and n_params = 0; ComparisonPeriodMeanAdjustedModel keeps sigma * sqrt(1 + 1/m).

Step 5 - A5 (R/cross_sectional.R). In cross_sectional_regression(): duplicated event_id in data -> rlang::abort class c("eventstudy_error_bad_argument", "eventstudy_error") naming the duplicated ids; task events without a matching data row -> one warning listing count and ids, then dropped (keep the existing "No matching event_id" error when nothing matches); car_window outside the available event window of any event (compare with the min/max relative_index of event_window == 1 rows per event) -> classed bad_argument error naming car_window, the available range and offending event_ids. In .extract_cars() a CAR is NA whenever any AR inside the window is missing (sum without na.rm); cross_sectional_regression() then drops NA-CAR events with exactly one warning listing them. Other .extract_cars() consumers (car_by_group, car_quantiles, plot_car_distribution) inherit the NA-CAR semantics without new warnings; note this in NEWS.

Step 6 - bootstrap (R/bootstrap.R). Apply the Step-2 helper to ar_data (mode .resolve_degenerate_mode(NULL)). A12(a): verified real (observed CAAR is cumsum of AAR while draws use the mean of per-event CARs) - compute observed CAR per event (STATS-03 coalesce), then per day observed_caar = mean(car), caar_t = sqrt(n) * mean(car) / sd(car), i.e. the identical construction the draws use; on NA-free input this equals cumsum(aar). A12(b): verified real (NA comparisons are forced to FALSE while the denominator stays n_boot + 1) - track per day the number of draws with a finite bootstrap statistic and use p = (exceed + 1) / (n_valid_draws + 1), NA when the observed statistic is NA or no draw is finite. A12(c) REVISED by orchestrator: do NOT change the clustering unit - weights stay drawn per firm_symbol (a deliberate earlier fix; firm-level clusters are the coarser, correlation-robust choice when a firm recurs). Leave the firm-clustering roxygen as is. The test_bootstrap.R ~line 187 rewrite (C2) belongs to Task 3.

Step 7 - A10 (R/single_event_test_statistics.R TestStatisticBase). initialize(): validate confidence_type with match.arg(c("two-sided", "less", "greater")); when it is not "two-sided" emit exactly one warning that the value is currently ignored because all p-values in EventStudy are two-sided; keep confidence_level stored. Grep all R/ for initialize overrides in TestStatisticBase subclasses and for callers passing confidence_type, and make them consistent. Document this in the field and param roxygen. Do not implement one-sided p-values.

Step 8 - existing tests. Run the full suite. For each failure decide: if the assertion encodes behaviour a locked A-item deliberately changes (e.g. CalTime values, MarketAdjusted FEC, Patell k, n_pos counts, new short-window warning in a fixture with fewer than 30 estimation obs, firm-level bootstrap clustering), update it with a comment citing the item id and expect the new warning explicitly (expect_warning with a regexp), never by blanket suppressWarnings or loosened tolerances. Any other failure is a regression in the fix - fix the code, not the test.

Step 9 - golden values and docs. In tests/testthat/test_golden_values.R update only the values the fixes change (at minimum Market Adjusted FEC, Calendar-time portfolio t, and any Patell/KP/Rank/GSign pin affected), each with a comment "re-pinned 2026-09-24: <item id> - <reason>". In vignettes/statistical-conventions.Rmd update each affected component section's text and its "Audit result" line (Rolling-Window A1; Market Model FEC + short-window/n_params guard A6; Market Adjusted FEC A8; CSectT A2/A9; Patell A2/A6/A9; BMP A2/A11; Sign A2; Generalized Sign A2/A4 "within each event"; Rank A4 "within each event"; Calendar-Time A7/A9 Brown-Warner; KP A2/A3; add rows/notes for bootstrap_test A12, cross_sectional_regression A5, TestStatisticBase confidence_type A10), add to the AR/CAR t-tests section that forecast_error_corrected_sigma_car is computed but intentionally not used by ARTTest/CARTTest (constant-sigma approximation, A11), state that BMP/KP standardize by model sigma (A11), and rewrite the audit log table and the "Audit summary" fix count/list accordingly. Add NEWS.md top section "# EventStudy 0.66.0.9000" with a "Statistical correctness fixes (2026-09-24 re-evaluation)" subsection, one bullet per A-item stating old vs new behaviour. Set DESCRIPTION Version: 0.66.0.9000 and Date: 2026-09-24.

Step 10 - regenerate and verify. Run devtools::document(); run the new test file and the full suite with NOT_CRAN=true; install to a scratch library and render the vignettes that execute changed code (cross-sectional-analysis, inference-robustness, statistical-conventions) with devtools::build_rmd(..., output_dir = tempdir()) to catch new errors/warnings early. Keep all new code/roxygen ASCII-only.

Commit in logical groups after Step 1 (each commit leaves the new test file's covered items green): models (A1, A6, A8), multi-event statistics + contract helper + dispatcher (A2, A3, A4, A7, A9, A11), cross-sectional (A5), bootstrap (A12), base class + p-value df (A10, A7 consumers), docs (golden values, conventions vignette, NEWS, DESCRIPTION, man/). Use messages of the form "fix(260924-sfl): <items> <summary>".
  </action>
  <verify>
    <automated>cd /home/simonm/projects/datascience/eventstudy && Rscript -e 'devtools::document()' && NOT_CRAN=true Rscript -e 'devtools::test(filter = "reeval_statistical_fixes", stop_on_failure = TRUE)' && NOT_CRAN=true Rscript -e 'devtools::test(stop_on_failure = TRUE)' && test "$(grep -v '^[[:space:]]*#' R/multi_event_test_statistics.R | grep -c 'group_by(firm_symbol)')" -eq 0 && for id in A1 A2 A3 A4 A5 A6 A7 A8 A9 A10 A12; do grep -q "\"$id:" tests/testthat/test_reeval_statistical_fixes.R || { echo "MISSING $id"; exit 1; }; done && grep -q '^# EventStudy 0.66.0.9000' NEWS.md && grep -q '^Version: 0.66.0.9000' DESCRIPTION && grep -q 'caltime_df' R/p_adjustment.R && grep -q 'n_params' R/multi_event_test_statistics.R</automated>
  </verify>
  <done>Every A-item has a regression test in test_reeval_statistical_fixes.R that failed on HEAD 92a1882 (recorded in SUMMARY) and now passes; full NOT_CRAN suite FAIL 0; conventions vignette audit table/Audit result lines/audit summary, golden values (with re-pin comments), NEWS 0.66.0.9000 section and DESCRIPTION version/date updated; all changes committed.</done>
</task>

<task type="auto">
  <name>Task 2: Section B - API and documentation consistency B1-B8 (exports, vignette/skill signatures, pkgdown, generics::tidy, Suggests hygiene)</name>
  <files>R/models.R, R/single_event_test_statistics.R, R/return_calculation.R, R/export.R, R/report.R, R/data_download.R, DESCRIPTION, NAMESPACE, man/ModelBase.Rd, man/TestStatisticBase.Rd, man/ReturnCalculation.Rd, man/report_table.Rd, _pkgdown.yml, vignettes/modern-did-estimators.Rmd, vignettes/automated-reports.Rmd (+ any other vignette / README.md chunk found by the drift scan), .claude/skills/es-advisor/reference/interpreting-diagnostics.md, .claude/skills/es-capabilities/SKILL.md, tests/testthat/test_public_surface.R (new), tests/testthat/_snaps/api-snapshot.md, NEWS.md</files>
  <action>
Implements CONTEXT items B1-B8 (locked). Precondition: Task 1 is committed (git log shows the 260924-sfl fix commits); re-read files fresh.

B1: add @export to the roxygen blocks of ModelBase (R/models.R line ~1-9), TestStatisticBase (R/single_event_test_statistics.R line ~1-2) and ReturnCalculation (R/return_calculation.R line ~1-2). Give each a proper @title/@description documenting the subclass contract (ModelBase: fit(data_tbl), abnormal_returns(data_tbl), the statistics fields consumed downstream including sigma, degree_of_freedom, residuals, forecast_error_corrected_sigma(_car) and the optional n_params added in Task 1; TestStatisticBase: compute(data_tbl, model) plus the confidence_type behaviour from Task 1; ReturnCalculation: calculate_return(tbl, in_column, out_column)) and @family eventstudy-models / eventstudy-statistics as appropriate. devtools::document() must emit no R6 "undocumented method/field" messages for these classes. In _pkgdown.yml move ModelBase and ReturnCalculation from the internal section to "Return Models" and TestStatisticBase to "Test Statistics". Create tests/testthat/test_public_surface.R with EVALUATED (no eval=FALSE, no skip) subclass tests: a ModelBase subclass (e.g. a constant-mean demo model setting private$.is_fitted and the statistics fields, n_params = 1) and a TestStatisticBase subclass (mean AR per relative day) run through run_event_study() on create_mock_task(), asserting the numbers equal a direct computation from the task data; a ReturnCalculation subclass whose output equals SimpleReturn's; and an assertion that all three names are in getNamespaceExports("EventStudy").

B5: in R/export.R keep @export on tidy.EventStudyTask and add @exportS3Method generics::tidy. After devtools::document() NAMESPACE must contain both export(tidy.EventStudyTask) and S3method(generics::tidy,EventStudyTask); if roxygen collapses them into one, add the S3method line with @rawNamespace instead. Add generics to DESCRIPTION Suggests (it is already installed as a hard dependency of dplyr; no install needed). In test_public_surface.R add a test with skip_if_not_installed("generics") asserting generics::tidy(task) is identical to tidy.EventStudyTask(task) for create_fitted_mock_task().

API snapshot (B1 + B5): run the snapshot test, review the diff, and accept only if it contains exactly the three new R6 exports and the new S3method line; then testthat::snapshot_accept("api-snapshot"). Name it "intentional surface change (B1, B5)" in the commit message.

B2: in vignettes/modern-did-estimators.Rmd (~line 48) and vignettes/automated-reports.Rmd (~line 134) rewrite the PanelEventStudyTask$new() calls to the real initialize signature (panel_data, unit_id, time_id, outcome, treatment, treatment_time) - read R/panel_event_study.R initialize (~line 45) for the exact meaning and value type of each argument and adjust the surrounding chunk so it would run. In vignettes/automated-reports.Rmd (~line 103) replace the non-existent characteristics argument of cross_sectional_regression() with data, and check the formula form against the function's roxygen. Then scan every code chunk (evaluated or eval=FALSE) in vignettes/*.Rmd, vignettes/articles/*.Rmd if present, README.md, .claude/skills/*/SKILL.md, .claude/skills/*/reference/*.md and inst/rmarkdown/templates/*/skeleton/skeleton.Rmd for calls to exported functions and R6 $new() constructors whose named arguments are not in formals() (use the audit scanner scratchpad/chk.R if it still exists, otherwise write an equivalent getParseData()-based scan in the session scratchpad) and fix every hit. Re-run the scan until it reports zero drift.

B3: build the earnings_surprises task exactly as in the R/data-earnings-surprises.R example (EventStudyTask$new(earnings_surprises$firm, earnings_surprises$index, earnings_surprises$request) then run_event_study with ParameterSet$new()), call es_diagnostics(task) and inspect str(result, max.level = 3). Rewrite .claude/skills/es-advisor/reference/interpreting-diagnostics.md to that real structure: meta, estimation_window (r2, sigma, degree_of_freedom, acf1, shapiro_p, dw_stat, ljung_box_p), event_window (ar_t, ar_p, car_t, car_p, final_car), cross_sectional, contract_state, aggregate_summary - documenting the actual sub-fields present, and restate every interpretation threshold in terms of those real field paths. Verify with a scratch script that every field path named in the document exists in the returned object.

B4: _pkgdown.yml: add statistical-conventions to an articles section (e.g. alongside the Core Workflow or Methods material); list dieselgate and earnings_surprises explicitly in "Data & Datasets" (replace the has_keyword datasets selector with the explicit names so each dataset appears exactly once); add eventstudy-deprecation and eventstudy-shape-contracts to a reference section (e.g. Diagnostics, or a new "Contracts & Lifecycle" section); add report_table to "Export & Reporting". In R/report.R (~line 546) drop the internal keywords tag from report_table's roxygen (it is exported and advertised) and re-document so man/report_table.Rd no longer carries the internal keyword. pkgdown::check_pkgdown() must report no problems.

B6: remove DT and patchwork from DESCRIPTION Suggests (confirmed unused in R/, vignettes/, inst/, tests/, README.md - re-grep before removing). In R/data_download.R, in the quantmod branch of download_stock_data() and before any download, guard the zoo::index() use with its own requireNamespace("zoo", quietly = TRUE) check that stops with an "install.packages('zoo')" message (existing style in R/export.R). No behavioural test is possible without uninstalling zoo (zoo is a hard dependency of xts/quantmod, so the branch is only reachable with quantmod installed); record this in the SUMMARY as the declared exception to the regression-test rule.

B7: update .claude/skills/es-capabilities/SKILL.md so run_event_study reads (task, parameter_set, report, report_args, verbose) and validate_task reads (task, parameter_set, min_estimation_obs, verbose); then verify every other signature in SKILL.md (and its reference/*.md maps) against formals() / R6 initialize formals of the loaded package and fix mismatches.

B8: no rename and no code change. Review the @param text for n_boot / n_simulations / n_placebo, min_obs / min_estimation_obs and estimation_window / estimation_window_length; only where the wording is ambiguous, add one clarifying sentence distinguishing the concepts. Do not change any formals (the API snapshot must show no formals diff).

Add a "API and documentation consistency" subsection to the existing NEWS.md "# EventStudy 0.66.0.9000" section with one bullet per B-item that changes something user-visible (B1, B2, B4, B5, B6, B7). Run devtools::document() and the full NOT_CRAN suite. Commit in groups: exports + tidy + snapshot (B1, B5), vignettes/skills (B2, B3, B7, B8), pkgdown + report_table (B4), Suggests + zoo guard (B6), using messages "fix(260924-sfl): ..." / "docs(260924-sfl): ...".
  </action>
  <verify>
    <automated>cd /home/simonm/projects/datascience/eventstudy && Rscript -e 'devtools::document()' && grep -q '^export(ModelBase)' NAMESPACE && grep -q '^export(TestStatisticBase)' NAMESPACE && grep -q '^export(ReturnCalculation)' NAMESPACE && grep -q '^export(tidy.EventStudyTask)' NAMESPACE && grep -q 'S3method(generics::tidy,EventStudyTask)' NAMESPACE && test "$(grep -c 'keyword{internal}' man/report_table.Rd)" -eq 0 && test "$(grep -cE '^[[:space:]]+(DT|patchwork),?$' DESCRIPTION)" -eq 0 && grep -qE '^[[:space:]]+generics' DESCRIPTION && grep -q 'requireNamespace("zoo"' R/data_download.R && test "$(grep -c 'characteristics =' vignettes/automated-reports.Rmd)" -eq 0 && test "$(grep -c 'unit_col' vignettes/modern-did-estimators.Rmd vignettes/automated-reports.Rmd | awk -F: '{s+=$2} END {print s}')" -eq 0 && Rscript -e 'pkgdown::check_pkgdown()' && NOT_CRAN=true Rscript -e 'devtools::test(filter = "public_surface|api-snapshot", stop_on_failure = TRUE)' && NOT_CRAN=true Rscript -e 'devtools::test(stop_on_failure = TRUE)'</automated>
  </verify>
  <done>Base classes exported and exercised by an evaluated subclass test; generics::tidy dispatches; API snapshot updated with only the intentional B1/B5 additions; vignette/README/skill signature scan reports zero drift; interpreting-diagnostics.md matches the real es_diagnostics() object; check_pkgdown passes; DT/patchwork gone, generics added, zoo guarded; NEWS updated; full NOT_CRAN suite FAIL 0; committed.</done>
</task>

<task type="auto">
  <name>Task 3: Section C - test-suite quality C1-C9, then the final release gate (document, full NOT_CRAN suite, R CMD check --as-cran --no-manual, check_pkgdown, non-ASCII baseline)</name>
  <files>tests/testthat/test_p_adjustment.R, tests/testthat/test_bootstrap.R, tests/testthat/test_multi_event_statistics.R, tests/testthat/test_models.R, tests/testthat/test_panel.R, tests/testthat/test_es_diagnostics.R, tests/testthat/test_models_time_varying.R, tests/testthat/test_edge_cases.R, tests/testthat/test_data_download.R, tests/testthat/test_cross_sectional.R, tests/testthat/test_synthetic_control.R, tests/testthat/test_intraday.R, tests/testthat/test_execute.R, tests/testthat/test_export.R, tests/testthat/test_bhar_test_statistics.R, tests/testthat/test_aar_test_statistics.R, tests/testthat/test_caar_test_statistics.R, NEWS.md, .github/non-ascii-baseline.txt (only if line numbers shifted)</files>
  <action>
Implements CONTEXT items C1-C9 (locked) and the global final gate. Precondition: Tasks 1 and 2 are committed; re-read every file fresh (Task 1 changed CalTime, KP, Patell, bootstrap and several existing tests). Behaviour of package code must not change in this task - only tests (and NEWS/baseline).

C1: in tests/testthat/test_p_adjustment.R add tests for the KP, generalized sign, rank and calendar-time branches of adjust_p_values() (R/p_adjustment.R ~60-105): build each statistic's tibble through the real pipeline or compute(), then assert p_raw_aar / p_raw_caar equal explicit 2 * pt(-abs(t), df) (KP: df = pmax(n_valid_events - 1, 1); calendar-time: df = attr(stat_tbl, "caltime_df") from Task 1) or 2 * pnorm(-abs(z)) (gsign, rank; rank p_raw_caar is NA) within 1e-12, and p_adj equals stats::p.adjust of those.

C2 (make vacuous tests real):
- tests/testthat/test_bootstrap.R ~line 187 clustering test (C2): rewrite with several events per firm and assert FIRM-level clustering numerically - a hand replication of a small n_boot run (same seed, one weight per unique firm_symbol, shared by that firm's events) reproduces boot_p_aar and boot_p_caar exactly.
- tests/testthat/test_multi_event_statistics.R ~373 and ~390 (Patell Q_i for k = 2 and k > 2): compute the expected Patell aar_z by hand from the pipeline's estimation-window ARs, sigmas/FEC and Q_i = (m - k)/(m - k - 2) with m = valid estimation obs and k = 2 (MarketModel) / 4 (FF3), and assert equality within 1e-10.
- tests/testthat/test_models.R ~917 (LinearFactorModel FEC): build the estimation design matrix of the fitted lm on complete cases, compute h_t = x_t' (X'X)^{-1} x_t for each event-window day and assert forecast_error_corrected_sigma equals sigma * sqrt(1 + h_t) within 1e-12.
- tests/testthat/test_models.R ~756 (HAC SEs): with skip_if_not_installed("sandwich"), assert statistics$se_hac equals sqrt(diag(sandwich::NeweyWest(<the same lm fit>, lag = <same lag as the model uses>))) within 1e-12. The implementation uses sandwich::NeweyWest (a Bartlett-kernel member of the vcovHAC family); assert against that estimator and add a comment saying so - do not change the package's estimator.

C3: tests/testthat/test_panel.R ~119, ~145, ~151: replace loose bounds (differences < 1, all < 3, mean > 0) with golden checks on a deterministic seeded fixture: for the native static/dynamic TWFE paths compare estimates (and std.error where the method is native) to the direct lm() two-way fixed-effects computation (factor(unit) + factor(time) + treatment / event-time dummies) within 1e-8; where the estimator depends on an optional package, keep skip_if_not_installed and pin a stored reference value computed once from the seeded fixture with tolerance 1e-8 and a comment naming how it was produced.

C4: tests/testthat/test_es_diagnostics.R ~145-157: replace the unconditional skip with a real test of .rank_events_for_cap (read R/es_diagnostics.R for its signature): build a task with more events than the cap, make one event unfitted by re-fitting that row's model on a degenerate estimation window (expect its single contract warning explicitly), and assert the unfitted event is always included in the capped selection regardless of its CAR, plus the cap size is respected.

C5: tests/testthat/test_models_time_varying.R ~145-158 (DCC): remove the tryCatch whose error handler records a trivially-true expectation and the is_fitted guard. Fit while capturing warnings with withCallingHandlers; skip only when the fit is not fitted AND a captured condition message matches a narrow non-convergence pattern (e.g. "converg"), with a skip message quoting it; otherwise assert is_fitted is TRUE and run every assertion unconditionally so any other error fails the test. tests/testthat/test_edge_cases.R ~1010: replace the conditional on is_fitted with expect_true(model$is_fitted) followed by the assertions (re-check the FEC expectation against Task 1's complete-pair FEC).

C6: tests/testthat/test_data_download.R lines 5-11: .try_download() must convert only network/HTTP failures into skip() - match condition classes from curl/httr2 when present and otherwise a narrow message pattern (could not resolve host, timed out/timeout, HTTP status 4xx/5xx, cannot open URL/connection, SSL, unreachable, rate-limited, the package's own "Failed to download" message); every other error is re-signalled so parse/logic errors fail. Add a small test of .try_download itself: a network-style message yields a condition of class "skip" (expect_condition(..., class = "skip")) and a logic error such as "subscript out of bounds" propagates as an error.

C7: re-scan every tests/testthat/test*.R for test_that() blocks that draw random numbers (rnorm, runif, sample, rbinom, rt, rexp, simulate_event_study, bootstrap_test without seed, or helpers that do not seed internally - check helper-mock-data.R) without their own set.seed(); the audit list is test_edge_cases.R 15, 39, 62, 84, 468, 858, 907, 930, 1505, 1633; test_models.R 634, 859; test_multi_event_statistics.R 167, 801, 817, 827; test_cross_sectional.R 109, 250; test_synthetic_control.R 270, 439, 467; test_intraday.R 73, 88 (line numbers may have shifted after Tasks 1-2). Add a set.seed(<distinct integer>) as the first statement of each such block. In tests/testthat/test_intraday.R replace the wall-clock current-time fixtures (~lines 72 and 87) with fixed timestamps, e.g. as.POSIXct("2024-01-15 09:30:00", tz = "UTC") + 1:10 (and its as.character form).

C8: tests/testthat/test_execute.R - for each model the file exercises, add at least one numeric assertion against a direct computation (MarketModel alpha/beta equal coef(lm()) on the estimation window and one event-day AR equals firm - (alpha + beta * index); MarketAdjusted AR = firm - index; CPMA AR = firm - mean(estimation firm returns); factor models vs lm with the factors) within 1e-10. tests/testthat/test_export.R - for the key statistics, read the exported CSV back and assert the exported numeric values equal the task values (e.g. CSectT aar/caar/caar_t, per-event car_t) within 1e-10 (CSV writes about 15 significant digits). Leave the remaining shape-only tests (full elimination is out of scope per CONTEXT C8).

C9: the calendar-time test in tests/testthat/test_bhar_test_statistics.R (~line 44) - compare with tests/testthat/test_multi_event_statistics.R ~255; delete it if it duplicates, otherwise move it into test_multi_event_statistics.R. In tests/testthat/test_aar_test_statistics.R (~line 48) replace the deparse-and-grep source check for a debugging call with a behavioural test: PatellZTest$new()$compute() on a small fixture returns without interaction and yields the expected finite aar_z values against a hand computation. Replace the single weak test in tests/testthat/test_caar_test_statistics.R with a real CSectTTest numeric test (aar = mean AR per day, caar = cumsum, caar_t = sqrt(n) * caar / sd(per-event CAR) hand-computed, within 1e-10). Leave test- vs test_ file naming as is.

NEWS: add a short "Testing" subsection to the "# EventStudy 0.66.0.9000" section summarising C1-C9.

Final gate (all must pass; fix causes, never weaken checks):
1. devtools::document() produces no diff in man/ or NAMESPACE (git status --porcelain man NAMESPACE is empty after it).
2. Full suite: NOT_CRAN=true devtools::test(stop_on_failure = TRUE) -> FAIL 0; record FAIL/WARN/SKIP/PASS in the SUMMARY (baseline before this quick task: FAIL 0 | WARN 6 | SKIP 37 | PASS 2561; PASS should grow, SKIP should not grow except for documented Suggests gating).
3. Build and check out-of-tree in the session scratchpad (so no tarball lands in the repo): R CMD build on the repo, then R CMD check --as-cran --no-manual on EventStudy_0.66.0.9000.tar.gz. Status must be at most the one pre-existing NOTE "checking CRAN incoming feasibility" (it may now additionally mention the development version number 0.66.0.9000 - same NOTE, acceptable) with 0 WARNINGs and 0 ERRORs, and no other NOTE (no undefined globals, no undeclared/unguarded Suggests, no Rd issues). The check takes several minutes: run it in the background (or with a 600000 ms timeout) and poll.
4. pkgdown::check_pkgdown() reports no problems.
5. Non-ASCII guard: run the CI command (grep -rnP '[^\x00-\x7F]' R/ man/figures/ inst/ | sort) and diff against .github/non-ascii-baseline.txt. If the only differences are line-number shifts of lines already in the baseline, refresh the baseline file with the new output; any genuinely new non-ASCII content must be removed (transliterate or escape) instead.

Commit per logical group (C1-C2, C3-C4, C5-C6, C7, C8-C9, NEWS/baseline) with messages "test(260924-sfl): ...", then a final "chore(260924-sfl): final gate green" commit only if the gate required file changes.
  </action>
  <verify>
    <automated>cd /home/simonm/projects/datascience/eventstudy && Rscript -e 'devtools::document()' && test -z "$(git status --porcelain man NAMESPACE)" && test "$(grep -c 'expect_true(TRUE)' tests/testthat/test_models_time_varying.R)" -eq 0 && test "$(grep -c 'Sys.time()' tests/testthat/test_intraday.R)" -eq 0 && test "$(grep -c 'browser' tests/testthat/test_aar_test_statistics.R)" -eq 0 && test "$(grep -c 'Cannot mutate R6 private field' tests/testthat/test_es_diagnostics.R)" -eq 0 && NOT_CRAN=true Rscript -e 'devtools::test(stop_on_failure = TRUE)' && Rscript -e 'pkgdown::check_pkgdown()' && grep -rnP '[^\x00-\x7F]' R/ man/figures/ inst/ 2>/dev/null | sort | diff -q .github/non-ascii-baseline.txt - && S=/tmp/claude-1000/-home-simonm-projects-datascience-eventstudy/fd6167d4-69a3-44c9-9b33-e4299cb74f20/scratchpad/gate && mkdir -p "$S" && cd "$S" && R CMD build /home/simonm/projects/datascience/eventstudy > build.log 2>&1 && R CMD check --as-cran --no-manual EventStudy_0.66.0.9000.tar.gz > check.log 2>&1 && grep -E '^Status:' "$S/check.log" && test "$(grep -cE '\.\.\. (WARNING|ERROR)' "$S/EventStudy.Rcheck/00check.log")" -eq 0 && test "$(grep -c '\.\.\. NOTE' "$S/EventStudy.Rcheck/00check.log")" -le 1</automated>
  </verify>
  <done>C1-C9 implemented with numeric/behavioural assertions at every audited site; random tests seeded; intraday fixtures fixed in UTC; document() clean; NOT_CRAN full suite FAIL 0 with counts recorded; R CMD check --as-cran --no-manual shows only the pre-existing incoming-feasibility NOTE and 0 WARNING/ERROR; check_pkgdown passes; non-ASCII baseline consistent; NEWS updated; all committed.</done>
</task>

</tasks>

<threat_model>
## Trust Boundaries

| Boundary | Description |
|----------|-------------|
| user data -> statistics layer | Untrusted, possibly degenerate return panels (all-NA events, recurring firms, short windows) cross into test-statistic computation; the integrity of reported numbers is the asset |
| user data -> cross_sectional_regression join | User-supplied characteristics table joined to task CARs; duplicates/partial matches can silently change the sample |
| network -> download_stock_data | Remote price data and optional packages (quantmod/zoo) at runtime |
| repo docs -> LLM advisor (skills) | .claude skill files are consumed by an AI agent; wrong field names/signatures cause fabricated guidance |
| test suite -> release decision | Vacuous tests can mask regressions and green-light a wrong release |

## STRIDE Threat Register

| Threat ID | Category | Component | Severity | Disposition | Mitigation Plan |
|-----------|----------|-----------|----------|-------------|-----------------|
| T-sfl-01 | Tampering (result integrity) | Multi-event statistics, KP r_bar, CalTime denominator, Patell m/k, Rolling OLS (A1-A4, A6-A9) | high | mitigate | Task 1 fixes each defect, routes degenerate input through .handle_degenerate (strict error / lenient NA-or-exclude + one warning), and locks it with failing-first tests plus golden re-pins |
| T-sfl-02 | Tampering (sample integrity) | cross_sectional_regression join and .extract_cars (A5) | high | mitigate | Classed error on duplicate event_id and out-of-range car_window; one warning listing unmatched and NA-CAR events; tests per case |
| T-sfl-03 | Repudiation | Silent exclusion of events from aggregates | medium | mitigate | Every exclusion is reported exactly once with event ids (or already reported by the model per CONTRACT-04); strict mode errors; documented in contract roxygen, conventions vignette and NEWS |
| T-sfl-04 | Tampering (inference integrity) | bootstrap_test observed-vs-draw mismatch, NA draws, cluster unit (A12) | high | mitigate | Same CAR construction for observed and draws, NA draws excluded from denominator, firm-level clustering kept (deliberate) and locked by a hand-replication test |
| T-sfl-05 | Denial of Service | download_stock_data unguarded zoo::index (B6) | low | mitigate | requireNamespace("zoo") guard with actionable install message before any download |
| T-sfl-06 | Spoofing (misinformation) | es-advisor interpreting-diagnostics.md, es-capabilities SKILL.md (B3, B7) | medium | mitigate | Rewritten from the live es_diagnostics() object and formals(); scratch script asserts every documented field path exists |
| T-sfl-07 | Tampering (test integrity) | Always-pass / blanket-skip tests (C2, C4, C5, C6) | medium | mitigate | Replace with numeric assertions; skips limited to narrowly-matched non-convergence or network conditions; grep gates in Task 3 verify |
| T-sfl-SC | Tampering (supply chain) | DESCRIPTION Suggests (+generics, -DT, -patchwork) | low | accept | No package-manager install tasks (npm/pip/cargo gate not applicable); generics is an r-lib CRAN package already installed as a hard Imports dependency of dplyr; removals reduce surface |
</threat_model>

<verification>
- Task 1: test_reeval_statistical_fixes.R green with every A-item id present; full NOT_CRAN suite FAIL 0; conventions vignette + golden values + NEWS + DESCRIPTION updated.
- Task 2: NAMESPACE exports/S3method present; API snapshot diff limited to B1/B5; drift scan zero; check_pkgdown passes; full suite FAIL 0.
- Task 3: grep gates for removed vacuous patterns; document() clean; full suite FAIL 0; R CMD check --as-cran --no-manual at most the pre-existing incoming-feasibility NOTE, 0 WARNING, 0 ERROR; non-ASCII baseline consistent.

Multi-source coverage audit (quick mode: GOAL + CONTEXT; no REQUIREMENTS.md IDs, no RESEARCH.md):

| Source | Item | Covered by |
|--------|------|-----------|
| GOAL | Fix all findings of the 2026-09-24 re-evaluation | Tasks 1-3 |
| CONTEXT global | Regression test failing pre-fix per fix | Task 1 Step 1 (A); Task 2 B1/B5 tests; B6 zoo guard declared exception (unreachable without uninstalling a quantmod hard dependency); B2/B3/B7 verified by scans; B8/A11 docs-only |
| CONTEXT global | Contract machinery for degenerate input | Task 1 Steps 2-3, 5-6 |
| CONTEXT global | Conventions vignette + golden values | Task 1 Step 9 |
| CONTEXT global | NEWS 0.66.0.9000, DESCRIPTION Version/Date, no CRAN-SUBMISSION/cran-comments edits | Task 1 Step 9, Tasks 2-3 append |
| CONTEXT global | API snapshot only for intentional changes | Task 2 (B1, B5) |
| CONTEXT global | Final gate | Task 3 |
| CONTEXT A1-A12 | Statistical defects | Task 1 (A1 Step 4; A2 Steps 2-3,6; A3/A4/A7/A9/A11 Step 3; A5 Step 5; A6/A8 Steps 3-4; A10 Step 7; A12 Step 6) |
| CONTEXT B1-B8 | API/docs consistency | Task 2 |
| CONTEXT C1-C9 | Test-suite quality | Task 3 (C2 bootstrap site: firm-clustering hand-replication test) |

No item is unplanned; nothing deferred.
</verification>

<success_criteria>
- All 29 CONTEXT items (A1-A12, B1-B8, C1-C9) implemented as locked, each traceable to a commit and (where applicable) a test named with its id.
- No silently wrong statistic remains for the audited degenerate/recurring-firm inputs; valid-input results unchanged except the locked redefinitions (CalTime A7, MarketAdjusted FEC A8, Patell m/k A6, sign counts A9, Rank/GSign grouping A4, bootstrap clustering/p-values A12), all documented in the conventions vignette, golden values and NEWS.
- Final gate green: devtools::document() clean, NOT_CRAN full suite FAIL 0, R CMD check --as-cran --no-manual with no new NOTE/WARNING, pkgdown::check_pkgdown() passes.
</success_criteria>

<output>
Create `.planning/quick/260924-sfl-fix-all-findings-from-the-2026-09-24-pac/260924-sfl-SUMMARY.md` when done, including per-item pre-fix failure evidence (Task 1), the API snapshot diff (Task 2), final test counts and the R CMD check Status line (Task 3).
</output>
