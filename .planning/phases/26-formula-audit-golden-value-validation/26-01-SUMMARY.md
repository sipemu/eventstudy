# Phase 26: Formula Audit & Golden-Value Validation — Summary

**Completed:** 2026-09-11
**Plan:** 26-01-PLAN.md (single plan, wave 1, 6 tasks, tracer-first)
**Requirements:** CORR-01, CORR-02
**Status:** COMPLETE — gate green, 0 failures, no new CRAN finding

## Outcome

Every return model (13+) and test statistic (8+) was audited against its published
academic formula, its convention choices documented in a new CRAN-shipped vignette
(`vignettes/statistical-conventions.Rmd`), and its key outputs pinned to golden
reference constants. **Two genuine correctness bugs were found and fixed** (with the
fixes preceding their golden pins and locked by regression tests), and all other
formulas verified as matching their published conventions. A wrong statistical number
can no longer silently pass, and a correct one can no longer be falsely failed by a
convention mismatch.

## What each task delivered

- **Task 1 (tracer) — `9023fe0`:** End-to-end slice for the Market Model + AR/CAR
  t-tests (MacKinlay 1997). Created the four durable artifacts: the vignette (with
  placeholder stubs for the remaining models/statistics), `data-raw/derive-golden-values.R`
  (non-shipped, `estudy2`/`eventstudies`-optional, degrades cleanly when absent),
  `tests/testthat/helper-golden-data.R`, and `tests/testthat/test_golden_values.R`.
  PermutationTest recorded honestly as an unwired stub.
- **Task 2 — `ff384d3`:** Remaining OLS/factor models — MarketAdjusted,
  ComparisonPeriodMean, FF3/FF5, Carhart4 (LinearFactorModel core), CustomModel.
  6 golden blocks; 3 new fixtures. Cited MacKinlay 1997, Brown-Warner 1985,
  Fama-French 1993/2015, Carhart 1997. No formula errors.
- **Task 3 — `0b9687c`:** Specialized/time-varying models — BHAR, Volume, Volatility,
  Rolling-Window (closed-form pins on deterministic fixtures); GARCH/DCC-GARCH
  documented + pinned as skip-guarded algebraic identities (no version-fragile fitted
  constants). Cited Barber-Lyon 1997 / Lyon-Barber-Tsai 1999, Engle 1982, Bollerslev
  1990. No formula errors.
- **Task 4 — `150faa3`:** Cross-sectional statistics — CSectT (Brown-Warner 1985),
  Patell Z incl. the `Q_i=(m-k)/(m-k-2)` FEC denominator + N>=2 guard (Patell 1976),
  BMP (Boehmer-Musumeci-Poulsen 1991), plus the general BHART form. Multi-event
  fixture added. No formula errors.
- **Task 5 — `d777113`:** Nonparametric / correlation-robust statistics — Sign,
  GeneralizedSign (Cowan 1992), Rank (Corrado 1989), CalendarTimePortfolio
  (Jaffe 1974 / Fama 1998), KolariPynnonen (2010). **Found and fixed 2 genuine bugs
  (see audit log).** Vignette completed (all sections + final audit-log summary).
- **Task 6 (gate) — this commit:** Full-suite + coverage-matrix + scoped clean-check
  gate; this SUMMARY.

## Audit log (fix vs. document)

**GENUINE ERRORS FIXED (2)** — both the same latent idiom: `ifelse(scalar_condition,
vector, scalar)` inside `dplyr::mutate`, where base `ifelse()` returns a result the
length of its length-1 condition, silently collapsing the per-day statistic vector to
its day-0 value and recycling it across every event day. No published convention makes
these statistics identical on every day, so these are genuine errors (not conventions).
Fixed with a plain `if()` guard on the scalar denominator; each fix precedes its golden
pin and is locked by a regression test asserting per-day values are distinct. No
pre-existing assertion pinned the buggy behavior — confirming they were latent.

1. `RankTest$rank_z` (Corrado 1989) — was `1.586, 1.586, 1.586`; now `1.586, -0.952, 1.269`.
2. `CalendarTimePortfolioTest$caltime_t`/`ccaltime_t` — were all `1.647`; now correct
   per-day (`1.647, -0.282, 1.139` and `1.647, 0.966, 1.446`).

**VERIFIED, DOCUMENTED (no change):** MarketModel, MarketAdjusted, ComparisonPeriodMean,
FF3/FF5, Carhart4, CustomModel, BHAR, Volume, Volatility, RollingWindow, GARCH/DCC
(skip-guarded identity), CSectT, PatellZ, BMP, Sign, GeneralizedSign, KolariPynnonen —
all match their published formulas (each closed-form constant cross-checked against the
real package pipeline to <1e-14 / ~17 digits). No `R/` source changed for these.

## Coverage matrix

- Vignette: 30 convention section headings — every 13+ model and 8+ statistic has a
  cited convention section (return type, forecast-error correction, degrees of freedom,
  p-value sidedness, Patell denominator where applicable).
- Golden tests: 23 `test_that` blocks in `test_golden_values.R`, backed by 9 deterministic
  fixtures in `helper-golden-data.R`. Every deterministic model/statistic has >=1 pin;
  GARCH/DCC-GARCH carry the documented skip-guarded identity rationale in lieu of a
  version-fragile fitted constant.

## Gate / verify output

- **Full suite** (`devtools::test_local`): `FAIL_TOTAL: 0`, `PASS_TOTAL: 2429`,
  `SKIP_TOTAL: 31`, `WARN_TOTAL: 4`. The 4 warnings are pre-existing/intentional test
  scenarios (rank-deficient design in `test_edge_cases.R:817`; simulated provider-failure
  fallback in `test_report_narrative_asm.R:245`), not regressions.
- **Vignette:** builds under `R CMD build` (`creating vignettes ... OK`, `BUILD_OK`) and
  renders standalone (`VIGNETTE_OK`). Pure ASCII (0 non-ASCII bytes).
- **Tarball hygiene:** `DERIVE_IN_TARBALL: FALSE`, `DATARAW_IN_TARBALL: FALSE`
  (`^data-raw$` in `.Rbuildignore`). `grep -Ec "estudy2|eventstudies" DESCRIPTION` = 0 —
  the derivation-only packages never entered DESCRIPTION and are never required at test time.
- **Version:** DESCRIPTION stays `0.65.0` — docs + tests only, no valid-input behavior
  change beyond the 2 documented bug fixes.
- No new NOTE/WARNING relative to the Phase 25 baseline (0 ERROR / 0 WARNING / 1 expected
  new-submission NOTE).

## Commits

1. `9023fe0` — tracer (Market Model AR/CAR t)
2. `ff384d3` — OLS/factor return models
3. `0b9687c` — specialized/time-varying return models
4. `150faa3` — cross-sectional statistics (CSectT/Patell/BMP)
5. `d777113` — nonparametric/correlation statistics (+ 2 bug fixes)
6. (this) — Task 6 gate + SUMMARY

## Deviations

- The Task 6 gate ran green, but two successive gate executors exhausted their context
  on the final large SUMMARY write; verify outputs were captured to files and the
  orchestrator composed and committed this SUMMARY from those results plus each task's
  reported audit decisions. No verification was skipped — all `<verify>` commands ran
  and are recorded above.
- No `--as-cran` full-check was run inside the gate (mirrors Phase 25's scoped approach);
  the vignette-inclusive `R CMD build` plus the Phase 25 clean baseline stand in, per the
  Task 6 spec's slow-check fallback. A full `--as-cran` check is exercised in Phase 30.
