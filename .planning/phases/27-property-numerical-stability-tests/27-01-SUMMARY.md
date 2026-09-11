---
phase: 27-property-numerical-stability-tests
plan: 01
subsystem: testing
tags: [invariant-tests, property-tests, numerical-stability, degenerate-input-contract, testthat, cran]

# Dependency graph
requires:
  - phase: 26-formula-audit-golden-value-validation
    provides: golden-value pins + helper-golden-data.R fixture idioms + degenerate-input contract (R/contract.R)
provides:
  - Registry-driven universal invariant tests (CAR==cumsum(AR), boundary/degenerate windows, monotonic growth) across the full 14-model registry
  - Cross-method-consistency invariants on a documented representative subset (return-strategy, OLS/adjusted family, statistic-side, scale invariance)
  - Statistic-layer invariants (CAAR==cumsum(AAR), uniform STATS-04 n_events==1 -> NA, KP->BMP reduction at r_bar==0)
  - Additive numerical-stability guards for four sensitive paths (ill-conditioned OLS design, bootstrap degeneracy, long-window CAR overflow, GARCH/DCC non-convergence), each routed through the degenerate-input contract and locked by a regression test
affects: [28-api-shape-contracts, 29-ci-install-testing, 30-cran-resubmission]

# Actuals (#2632)
actuals:
  tokens: 14366
  tasks: 6
  commits: 7

# Tech tracking
tech-stack:
  added: []
  patterns:
    - "Table-driven model/statistic registries (list of data rows) so coverage grows by adding a row, never a code path"
    - "Statistics-layer NA+one-warning guards mirroring the model .handle_degenerate contract on overflow/degenerate paths"
    - "Deterministic invariant fixtures (fixed literals, no set.seed) feeding the real prepare/fit/statistics pipeline"

key-files:
  created:
    - tests/testthat/helper-invariant-data.R
    - tests/testthat/test_invariants.R
    - tests/testthat/test_numerical_stability.R
  modified:
    - R/models.R
    - R/single_event_test_statistics.R
    - R/multi_event_test_statistics.R

key-decisions:
  - "No new dependency: hand-rolled parameterized loops over registries in base testthat 3e (patrick/hedgehog stay absent, grep-asserted 0)"
  - "Universal identities on EVERY model; heavier cross-method/statistic-layer invariants on a documented representative subset (invalid model x statistic pairings not forced)"
  - "Every guard is additive and routes through the degenerate-input contract (NA + one warning lenient / stop strict); valid-input math untouched"
  - "Overflow guard gated on is.infinite|is.nan ONLY (never plain NA) so it never emits a second contract warning on a legitimately-degenerate all-NA CAR"

patterns-established:
  - "Invariant registry idiom: a dropped registry row silently drops coverage; the Task 6 coverage matrix asserts every model appears"
  - "Fix-precedes-assertion: a genuine degenerate-path bug is fixed with its regression test locking the corrected behavior; RED verified to fail without the fix"

requirements-completed: [CORR-03, CORR-04]

coverage:
  - id: D1
    description: "Universal identities (CAR==cumsum(AR), boundary/degenerate windows, monotonic growth) across the full 14-model registry (GARCH/DCC skip-guarded)"
    requirement: CORR-03
    verification:
      - kind: unit
        ref: "tests/testthat/test_invariants.R#CAR == cumsum(AR) holds for every registered return model"
        status: pass
    human_judgment: false
  - id: D2
    description: "Cross-method-consistency invariants on the representative subset (return-strategy, OLS/adjusted family coincidence, statistic-side AAR/CAAR, scale invariance)"
    requirement: CORR-03
    verification:
      - kind: unit
        ref: "tests/testthat/test_invariants.R#OLS/adjusted family coincides when benchmark == fitted line"
        status: pass
    human_judgment: false
  - id: D3
    description: "Statistic-layer invariants: CAAR==cumsum(AAR), uniform STATS-04 n_events==1 -> NA across the multi-event set, KP->BMP reduction at r_bar==0"
    requirement: CORR-03
    verification:
      - kind: unit
        ref: "tests/testthat/test_invariants.R#STATS-04 uniform: single event -> NA z/t for every multi-event statistic"
        status: pass
    human_judgment: false
  - id: D4
    description: "Additive numerical-stability guards for the four sensitive paths, each routed through the contract and regression-locked"
    requirement: CORR-04
    verification:
      - kind: unit
        ref: "tests/testthat/test_numerical_stability.R#CAR cumulation that overflows to Inf -> NA car_t + one warning"
        status: pass
    human_judgment: false
  - id: D5
    description: "Additive-only, no regression, no new CRAN finding: full suite green, DESCRIPTION has 0 patrick/hedgehog, scoped R CMD check preserves the Phase 25 baseline"
    requirement: CORR-04
    verification:
      - kind: integration
        ref: "Rscript -e 'testthat::test_local()' -> FAIL_TOTAL: 0 / PASS_TOTAL: 2521 / SKIP: 37"
        status: pass
      - kind: other
        ref: "grep -Ec 'patrick|hedgehog' DESCRIPTION -> 0"
        status: pass
    human_judgment: false

# Metrics
duration: ~40min
completed: 2026-09-11
status: complete
---

# Phase 27 Plan 01: Property & Numerical-Stability Tests Summary

**Registry-driven cross-cutting invariants (CAR==cumsum(AR), cross-method consistency, boundary/degenerate windows, STATS-04 uniformity, KP->BMP reduction) across the whole model/statistic matrix, plus four additive contract-routed numerical-stability guards — the durable regression net that stops silently-wrong statistics on degenerate/overflow/ill-conditioned inputs.**

## Performance

- **Duration:** ~40 min (Tasks 3–6; Tasks 1–2 pre-completed)
- **Tasks:** 6 (Tasks 1–2 pre-existing; Tasks 3–6 executed this run)
- **Files modified:** 6 (3 test/helper created earlier + extended; 3 R source guarded)

## Accomplishments
- Universal identities (CAR==cumsum(AR), length-1 window CAR==AR, all-NA window -> all-NA CAR, monotonic growth) asserted on every one of the 14 registered return models (GARCH/DCC skip-guarded), at absolute tolerance 1e-10.
- Cross-method-consistency invariants on the documented representative subset: LogReturn~SimpleReturn first-order (relative 1e-2) + exact log1p bridge (1e-10); MarketModel==MarketAdjusted==ComparisonMean on an alpha=0/beta=1/index==0 fixture (relative 1e-6); CSectT AAR==mean(AR) and CAAR==cumsum(AAR) (1e-10); CAR t-stat scale invariance (1e-8).
- Statistic-layer invariants: CAAR==cumsum(AAR) for CSectT and Patell; a uniform STATS-04 loop asserting n_events==1 -> NA z/t for the ENTIRE multi-event statistic set; KolariPynnonen reduces to BMP at r_bar==0 via a new mutually-orthogonal-SAR fixture.
- Four additive numerical-stability guards, each routed through the degenerate-input contract and locked by a regression test: ill-conditioned OLS design (Task 1 tracer), bootstrap degenerate resample, long-window CAR overflow, GARCH/DCC non-convergence.
- Full suite green (2521 passing, 0 failures, 37 skipped); DESCRIPTION carries 0 patrick/hedgehog; scoped R CMD check preserves the Phase 25 baseline (0 ERROR, 0 code WARNING, 1 expected new-submission NOTE).

## Coverage Matrix

| Model (registry) | Universal identities exercised |
|---|---|
| MarketModel, MarketAdjustedModel, ComparisonPeriodMeanAdjustedModel, CustomModel, LinearFactorModel, FF3, FF5, Carhart4, VolumeModel, VolatilityModel, RollingWindowModel | CAR==cumsum(AR), length-1 CAR==AR, all-NA window, monotonic growth |
| BHARModel | BHAR-specific compounded-path identity (car==reported path) + the same window identities |
| GARCHModel, DCCGARCHModel | identity form only, skip_if_not_installed-guarded (version-fragile) |

| Statistic | Invariant exercised |
|---|---|
| CSectTTest | AAR==mean(AR), CAAR==cumsum(AAR), n_events==1 -> NA |
| PatellZTest | underlying CAAR==cumsum(AAR), n_events==1 -> NA |
| BMPTest | KP-reduction reference, n_events==1 -> NA |
| SignTest, GeneralizedSignTest | n_events==1 -> NA (uniform STATS-04) |
| KolariPynnonenTest | reduces to BMP at r_bar==0, n_events==1 -> NA |
| CalendarTimePortfolioTest | n_events==1 -> NA (uniform STATS-04) |
| ARTTest/CARTTest | scale invariance, CAR==cumsum(AR) |

| Sensitive path | Guard + regression test |
|---|---|
| Ill-conditioned OLS design (rcond < sqrt(eps)) | R/models.R market fit -> .handle_degenerate; test_numerical_stability.R (NA+one warning lenient / stop strict + positive controls) |
| Bootstrap degenerate resample | R/bootstrap.R (already handled: obs t NA -> p NA); test locks NA p-values, never spurious 0/1 |
| Long-window CAR precision/overflow | R/single_event_test_statistics.R CARTest (is.infinite\|is.nan -> NA + one warning); test locks finite-window identity + overflow NA |
| GARCH/DCC non-convergence | R/models.R GARCHModel + R/models_time_varying.R DCCGARCHModel (already handled); skip-guarded contract locks |

## Fix Log

**1. [Rule 1 - Bug] GeneralizedSignTest returned a finite-but-invalid z with a single event**
- **Found during:** Task 5 (uniform STATS-04 probe)
- **Issue:** `gsign_z`/`cgsign_z` used only a `denom > 0` guard, so n_events==1 produced a finite z where every other multi-event statistic correctly returned NA.
- **Fix:** Added `n_valid_events >= 2` (and `n_valid >= 2` for the cumulative form), matching SignTest — degenerate path only, valid multi-event inputs untouched.
- **Files:** R/multi_event_test_statistics.R
- **Verification:** RED confirmed (stash-test) to fail without the fix; multi-event + golden + contract suites stay green.
- **Committed in:** ad32a57

**2. [Rule 1 - Bug] CalendarTimePortfolioTest returned a finite t with a single (pooled) event**
- **Found during:** Task 5
- **Issue:** A calendar-time PORTFOLIO test pools ARs across events; with n_events==1 the portfolio degenerates to one firm's series yet still reported a finite `caltime_t`/`ccaltime_t`.
- **Fix:** Added `n_events >= 2` gate (cross-sectional/multi-event validity requirement), NA otherwise. Degenerate path only.
- **Files:** R/multi_event_test_statistics.R
- **Verification:** Same RED/GREEN as above; regression suites green.
- **Committed in:** ad32a57

**3. [Rule 1 - Bug] Self-introduced: CAR overflow guard over-broad (fired on plain NA)**
- **Found during:** Task 6 (full-suite gate)
- **Issue:** The Task 4 overflow guard used `!is.finite(car)`, which matched plain NA. An unfitted degenerate model returns all-NA abnormal returns (already warned once at fit time), so the guard emitted a SECOND contract warning — breaking CONTRACT-04 (exactly one warning per degenerate firm).
- **Fix:** Restricted the guard to `is.infinite(car) | is.nan(car)` (true overflow only); NA CAR stays silent.
- **Files:** R/single_event_test_statistics.R
- **Verification:** test_contract.R CONTRACT-04 green; overflow regression test still green; full suite 0 failures.
- **Committed in:** b2f9718

## Task Commits

1. **Task 1 (tracer): CAR==cumsum(AR) tracer + ill-conditioned OLS guard** - `4c69d6b` (test, pre-existing)
2. **Task 2: universal identities across the full model registry** - `f2c5305` (test, pre-existing)
3. **Task 3: cross-method-consistency invariants (representative subset)** - `ec0d78d` (test)
4. **Task 4 (RED): bootstrap-degeneracy lock + CAR overflow regression** - `9eb9f95` (test)
5. **Task 4 (GREEN): CARTest overflow guard + GARCH non-convergence locks** - `fff00da` (fix)
6. **Task 5: uniform STATS-04 fixes + statistic-layer invariants** - `ad32a57` (fix)
7. **Task 6: full-suite gate — overflow guard gated on Inf/NaN only** - `b2f9718` (fix)

_TDD tasks 3–5: invariant assertions held on correct code (RED/GREEN collapse where no source change is needed); tasks 4–5 carried genuine degenerate-path fixes committed with their locking regression tests per the discrepancy protocol._

## Decisions Made
- Represented the KP->BMP reduction with a purpose-built orthogonal-SAR fixture (mean-zero, dot-product-0 estimation patterns on MarketAdjustedModel) so r_bar is exactly 0 and kp_adj==1 deterministically.
- Log~simple return consistency asserted at RELATIVE 1e-2 (documented: the O(r/2) relative approximation bound for sub-1% moves), with an exact `log1p` bridge at 1e-10 as the precise pin.
- CalendarTimePortfolio single-event -> NA treated as in-scope for the uniform STATS-04 invariant (a one-unit portfolio is not a valid multi-event calendar-time test), per the plan's explicit "every multi-event statistic" behavior spec.

## Deviations from Plan

### Auto-fixed Issues
Three Rule 1 bug fixes (see Fix Log above): two genuine degenerate-path bugs in the multi-event statistics (GeneralizedSign, CalendarTimePortfolio) discovered by the uniform STATS-04 invariant, and one self-introduced over-broad guard caught by the Task 6 full-suite gate.

---

**Total deviations:** 3 auto-fixed (3 Rule 1 bugs)
**Impact on plan:** All fixes confined to degenerate/overflow paths; valid-input behavior byte-for-byte unchanged (positive controls + preserved Phase 26 golden pins + preserved pass count). No scope creep, no new dependency.

## Issues Encountered
- `devtools::test_local` is not exported in devtools 2.5.2 — used `testthat::test_local(".")` for the full-suite gate.
- Scoped R CMD check emits a vignette-index WARNING ("Directory 'inst/doc' does not exist") — this is a `--no-vignettes`/`--no-build-vignettes` scope artifact, NOT a code-level finding (per planning_constraints 6), so the Phase 25 baseline of 0 ERROR / 0 code WARNING / 1 expected NOTE is preserved.

## Next Phase Readiness
- The cross-cutting invariant net and the four sensitive-path guards are in place and green; Phase 28 (API/shape contracts) and Phase 30 (CRAN resubmission) build on this stable base.
- No blockers. No user setup required.

## Self-Check: PASSED

- Created files verified present: helper-invariant-data.R, test_invariants.R, test_numerical_stability.R, 27-01-SUMMARY.md
- Task commits verified in git log: ec0d78d, 9eb9f95, fff00da, ad32a57, b2f9718 (plus pre-existing 4c69d6b, f2c5305)

---
*Phase: 27-property-numerical-stability-tests*
*Completed: 2026-09-11*
