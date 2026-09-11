---
phase: 27-property-numerical-stability-tests
verified: 2026-09-11T19:45:32Z
status: passed
score: 5/5 must-haves verified
behavior_unverified: 0
overrides_applied: 0
---

# Phase 27: Property & Numerical-Stability Tests Verification Report

**Phase Goal:** Cross-cutting invariants hold across the whole model/statistic matrix and the sensitive numeric paths are protected against precision, overflow, and conditioning failures — without introducing cross-platform CI flakiness.
**Verified:** 2026-09-11T19:45:32Z
**Status:** passed
**Re-verification:** No — initial verification

---

## Goal Achievement

### Observable Truths

| # | Truth | Status | Evidence |
|---|-------|--------|----------|
| 1 | CAR == cumsum(AR) and boundary/degenerate window and monotonic growth on EVERY registered return model (CORR-03) | VERIFIED | `test_invariants.R` passes 73 tests, 4 skipped for rugarch (skip_if_not_installed-guarded, correct per plan). Loop covers 14 registry rows: 12 non-GARCH + 2 GARCH/DCC (guarded). |
| 2 | Cross-method-consistency and heavier invariants on a documented representative subset; invalid model x statistic pairings not forced (CORR-03) | VERIFIED | `test_invariants.R`: OLS/adjusted-family coincidence (alpha=0/beta=1 fixture), return-strategy consistency, CSectT AAR==mean(AR)+CAAR==cumsum(AAR), scale invariance, KP->BMP at r_bar==0, STATS-04 uniform NA loop — all passing. Subset rationale documented inline. |
| 3 | Numerical-stability guards for all four sensitive paths (ill-conditioned OLS, bootstrap degeneracy, long-window CAR overflow, GARCH non-convergence), each NA+one-warning via the contract (CORR-04) | VERIFIED | `test_numerical_stability.R` 19 passing, 2 skipped (rugarch/rmgarch). Guards confirmed in source: `rcond` guard in `R/models.R` (line 218-237 routing via `.handle_degenerate()`); `is.infinite|is.nan` overflow guard in `R/single_event_test_statistics.R` (lines 151-161); bootstrap already handled (test locks it); GARCH/DCC contract locks skip-guarded. |
| 4 | Every assertion documents its tolerance inline (absolute 1e-10/1e-8 for algebraic identities, relative 1e-6 for cross-method) so CI does not flake (CORR-04) | VERIFIED | Inspected both test files. All `expect_equal` calls carry explicit `tolerance =` argument with an inline comment. No bare `expect_equal` without tolerance on numeric comparisons found. |
| 5 | Guards are additive: valid-input behavior unchanged, no new hard Imports, full suite green, Phase 25 baseline preserved (SC5) | VERIFIED | `grep -Ec "patrick|hedgehog" DESCRIPTION` = 0. Golden pins `test_golden_values.R`: 70 passed / 0 failed (Phase 26 baseline intact). Summary reports full suite 2521 passing / 0 failed / 37 skipped. Three Rule-1 bug fixes confined to degenerate paths only; positive controls in stability tests confirm valid-input unchanged. |

**Score:** 5/5 truths verified (0 present-but-behavior-unverified)

---

### Required Artifacts

| Artifact | Expected | Status | Details |
|----------|----------|--------|---------|
| `tests/testthat/helper-invariant-data.R` | Model/statistic registry + deterministic fixtures | VERIFIED | 419 lines, substantive. Contains 14-row `invariant_model_registry`, all fixture builders, cross-method and statistic-layer fixtures. |
| `tests/testthat/test_invariants.R` | Registry-driven universal + representative-subset invariant tests | VERIFIED | 365 lines, substantive. Universal loop (CAR==cumsum(AR), boundary, monotonic), cross-method section, statistic-layer section — all wired to registry. |
| `tests/testthat/test_numerical_stability.R` | Sensitive-path guard regression tests | VERIFIED | 203 lines, substantive. Four guard blocks with positive controls; wired to real R/ source guards. |

### Key Link Verification

| From | To | Via | Status | Details |
|------|-----|-----|--------|---------|
| `invariant_model_registry` in helper-invariant-data.R | Universal identity loop in test_invariants.R | Registry loop `for (row in invariant_model_registry)` | VERIFIED | Every model row reached by the loop; GARCH/DCC guarded with `skip_if_not_installed`. A dropped row silently drops coverage (documented risk mitigated by Task 6 coverage matrix). |
| Each new guard in R/*.R | Regression test in test_numerical_stability.R | Fixture passes degenerate input -> expect_warning + is_fitted==FALSE + NA AR | VERIFIED | `rcond` guard -> Guard 1 tests (lenient/strict/positive control); overflow guard -> Guard 3 tests; bootstrap NA -> Guard 2 test; GARCH convergence -> Guard 4 tests (skip-guarded). |
| CARTest overflow guard | `.handle_degenerate` contract | `is.infinite(car) | is.nan(car)` gate (never plain NA) | VERIFIED | Guard gated on `is.infinite | is.nan` only, so it never fires on a legitimately-degenerate all-NA CAR that already warned at fit time (CONTRACT-04 preserved). Confirmed in source lines 151-161. |

### Data-Flow Trace (Level 4)

Not applicable — this phase is a pure test/guard addition phase. No UI or data-rendering components.

### Behavioral Spot-Checks

| Behavior | Command | Result | Status |
|----------|---------|--------|--------|
| test_invariants.R: 73 pass, 0 fail | `Rscript -e 'devtools::load_all("."); test_file("tests/testthat/test_invariants.R")'` | 73 passed, 4 skipped (rugarch), 0 failed | PASS |
| test_numerical_stability.R: 19 pass, 0 fail | `Rscript -e 'devtools::load_all("."); test_file("tests/testthat/test_numerical_stability.R")'` | 19 passed, 2 skipped (rugarch/rmgarch), 0 failed | PASS |
| Phase 26 golden pins unchanged | `Rscript -e 'devtools::load_all("."); test_file("tests/testthat/test_golden_values.R")'` | 70 passed, 2 skipped (GARCH/DCC), 0 failed | PASS |
| No new hard dependency | `grep -Ec "patrick|hedgehog" DESCRIPTION` | 0 | PASS |

### Requirements Coverage

| Requirement | Source Plan | Description | Status | Evidence |
|-------------|-------------|-------------|--------|----------|
| CORR-03 | 27-01-PLAN.md | Property/invariant tests across model/statistic matrix | SATISFIED | Universal identity loop (14 models), cross-method subset, statistic-layer invariants — all passing in test_invariants.R |
| CORR-04 | 27-01-PLAN.md | Numerical-stability guards with documented tolerances, no CI flakiness | SATISFIED | Four sensitive-path guards locked in test_numerical_stability.R; inline tolerance comments on every assertion; DESCRIPTION has 0 new deps |

Both CORR-03 and CORR-04 are marked Complete in REQUIREMENTS.md traceability table. No orphaned requirements for Phase 27.

### Anti-Patterns Found

| File | Line | Pattern | Severity | Impact |
|------|------|---------|----------|--------|
| — | — | None found | — | All new test files use real pipeline calls, not mocks or stubs. All guards are gated on degenerate conditions only. |

Debt-marker scan (`TBD`, `FIXME`, `XXX`) on the three new/modified files: none found.

### Human Verification Required

None. All must-haves are fully verifiable programmatically: test files exist and pass, source guards are present and correctly gated, DESCRIPTION is unchanged, golden pins are intact.

---

## Gaps Summary

No gaps. All five must-have truths are verified against the actual codebase:

- Three new test files are present, substantive, and passing.
- Three Rule-1 bug fixes (GeneralizedSignTest, CalendarTimePortfolioTest, CARTest overflow guard) are confirmed in source with the correct guards and gating logic.
- DESCRIPTION carries zero new hard dependencies.
- Phase 26 golden pins (70 tests) are intact, proving valid-input behavior unchanged.
- GARCH/DCC rows are properly skip_if_not_installed-guarded throughout.

---

_Verified: 2026-09-11T19:45:32Z_
_Verifier: Claude (gsd-verifier)_
