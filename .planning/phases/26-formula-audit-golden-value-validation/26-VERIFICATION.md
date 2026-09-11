---
phase: 26-formula-audit-golden-value-validation
verified: 2026-09-11T00:00:00Z
status: passed
score: 5/5 must-haves verified
behavior_unverified: 0
overrides_applied: 0
---

# Phase 26: Formula Audit & Golden-Value Validation Verification Report

**Phase Goal:** Every return model (13+) and test statistic (8+) is verified correct against its published academic formula, its convention choices documented in a durable shipped reference, and key statistics pinned to reference values — so a wrong number can never silently pass and a correct number can never be falsely failed by a convention mismatch. Requirements: CORR-01, CORR-02.
**Verified:** 2026-09-11
**Status:** passed
**Re-verification:** No — initial verification

## Goal Achievement

### Observable Truths

| # | Truth | Status | Evidence |
| --- | --- | --- | --- |
| 1 | Each 13+ return model and 8+ test statistic has an audited convention section in `vignettes/statistical-conventions.Rmd` citing a published source (CORR-01, SC1) | ✓ VERIFIED | 13 return-model `###` sections (Market, MarketAdjusted, ComparisonPeriodMean, FF3, FF5, Carhart4, Custom, BHAR, Volume, Volatility, RollingWindow, GARCH, DCC-GARCH) + 11 statistic sections (AR/CAR t, BHAR t, CSectT, PatellZ, BMP, Sign, GeneralizedSign, Rank, CalendarTimePortfolio, KolariPynnonen, PermutationTest stub). All 25 `###` sections carry a citation (75 year-citations total). |
| 2 | Any formula error found is fixed, the fix precedes its golden pin, and is locked by a regression test (CORR-01, SC2) | ✓ VERIFIED | 2 genuine bugs fixed in `R/multi_event_test_statistics.R`: `rank_z` (L411) and `caltime_t`/`ccaltime_t` (L563/565), `ifelse(scalar,...)`→`if(...)`. Diff at `d777113` confirms the change. Regression tests (test_golden_values.R:622, :658) pin corrected per-day values AND assert `expect_false(res$rank_z[1]==res$rank_z[2])` / `caltime_t[1]!=caltime_t[2]` so the broadcast bug cannot return. Fix + corrected pin + regression are in the same commit; the pinned value is the corrected one and distinctness is enforced. |
| 3 | Golden-value tests pin key statistics to reference constants with inline provenance, assumed conventions, and explicit tolerance (CORR-02, SC3/SC4) | ✓ VERIFIED | 23 `test_that` blocks, 72 real `expect_*` assertions. Spot-checked Patell Z block (L464): full provenance (closed-form on fixture, Patell 1976, Q_i=(m-k)/(m-k-2)), assumed-conventions comment referencing the vignette, and tolerance rationale (absolute 1e-10 for algebraic identity). No empty stubs. |
| 4 | estudy2/eventstudies appear only in the `.Rbuildignore`'d `data-raw/` derivation script, never in DESCRIPTION | ✓ VERIFIED | `grep -Ec "estudy2\|eventstudies" DESCRIPTION` = 0. `^data-raw$` in `.Rbuildignore:12`. `data-raw/derive-golden-values.R` has 4 `requireNamespace()` guards and degrades to a clear "not installed — constants pinned directly" message. |
| 5 | Behavior on valid inputs unchanged; full suite stays green | ✓ VERIFIED | Only `R/multi_event_test_statistics.R` changed in `R/` across the entire phase (`git diff --stat 9023fe0~1 d777113 -- R/`: 1 file, the 2 documented fixes). Those two sites were latent broadcast bugs (no published convention makes per-day stats identical), so the change is a genuine fix, not a valid-input change. Golden test file runs 0 failures (see below); SUMMARY reports full suite FAIL_TOTAL 0 / PASS 2429 / SKIP 31. |

**Score:** 5/5 truths verified (0 present, behavior-unverified)

### Required Artifacts

| Artifact | Expected | Status | Details |
| --- | --- | --- | --- |
| `vignettes/statistical-conventions.Rmd` | Durable CRAN-shipped convention reference | ✓ VERIFIED | 686 lines, 25 cited sections, pure ASCII (0 non-ASCII bytes), audit-log summary present. |
| `tests/testthat/test_golden_values.R` | Golden-value regression net | ✓ VERIFIED | 729 lines, 23 test blocks, 72 assertions, runs 0 failures / 2 designed skips. |
| `tests/testthat/helper-golden-data.R` | Deterministic fixtures | ✓ VERIFIED | 357 lines, 9 fixture builders feeding real R6 pipeline (`MarketModel$fit()`, `PatellZTest$compute()`, etc.). |
| `data-raw/derive-golden-values.R` | Non-shipped, estudy2-optional derivation | ✓ VERIFIED | 387 lines, 4 requireNamespace guards, sessionInfo dump, graceful degrade; `.Rbuildignore`'d. |

### Key Link Verification

| From | To | Via | Status | Details |
| --- | --- | --- | --- | --- |
| Vignette convention rows | test_golden_values.R tolerance/convention annotations | Inline "Assumed conventions (see vignette ...)" comments | ✓ WIRED | Patell block explicitly cross-references the vignette Patell section; tolerance rationale matches CONTEXT policy (1e-10 identities). |
| FEC / df / Patell-denominator choices in `R/*.R` | Documented convention row | Audit output | ✓ WIRED | Patell Q_i=(m-k)/(m-k-2) documented in vignette and pinned in test at L468/484. |

### Data-Flow Trace (Level 4)

| Artifact | Data Variable | Source | Produces Real Data | Status |
| --- | --- | --- | --- | --- |
| test_golden_values.R | golden fixtures | `golden_*_fixture()` → real `fit()`/`compute()` pipeline | ✓ (deterministic fixtures through production R6 classes) | ✓ FLOWING |

### Behavioral Spot-Checks

| Behavior | Command | Result | Status |
| --- | --- | --- | --- |
| Golden test file passes | `testthat::test_file("tests/testthat/test_golden_values.R")` | exit 0; 21 pass, 2 skip (rugarch/rmgarch absent), 0 fail | ✓ PASS |
| rank_z / caltime_t vary per day | regression assertions in golden file | passed within the 0-failure run | ✓ PASS |
| Vignette pure ASCII | `grep -cP "[^\x00-\x7F]" vignette` | 0 | ✓ PASS |
| DESCRIPTION free of estudy2/eventstudies | `grep -Ec "estudy2\|eventstudies" DESCRIPTION` | 0 | ✓ PASS |

### Requirements Coverage

| Requirement | Source Plan | Description | Status | Evidence |
| --- | --- | --- | --- | --- |
| CORR-01 | 26-01 | Formula audit + convention documentation + fix genuine errors | ✓ SATISFIED | 25 cited vignette sections; 2 bugs fixed + regression-locked. |
| CORR-02 | 26-01 | Golden-value pins with provenance + tolerance; estudy2 non-shipped | ✓ SATISFIED | 72 annotated assertions; DESCRIPTION clean; derivation script `.Rbuildignore`'d. |

### Anti-Patterns Found

| File | Line | Pattern | Severity | Impact |
| --- | --- | --- | --- | --- |
| — | — | No unreferenced debt markers in phase-modified files | ℹ️ Info | None. The "PermutationTest (unwired stub)" is honestly documented as an unwired stub, not shipped as working behavior — matches SUMMARY's honesty claim. Remaining `ifelse` uses in the file are on vector conditions (correct usage), not the fixed scalar-condition bug. |

### Human Verification Required

None. All truths verified programmatically.

### Gaps Summary

No gaps. All five roadmap success criteria are met with codebase evidence:
- Every one of the 13+ return models and 8+ statistics has both a cited vignette convention section and >=1 real golden pin (GARCH/DCC carry documented skip-guarded identity rationale, as the CONTEXT permits).
- The 2 claimed bug fixes (`ifelse`→`if` for `rank_z` and `caltime_t`/`ccaltime_t`) are real, present in the source, and locked by per-day-distinctness regression tests.
- Vignette is pure ASCII; DESCRIPTION has 0 estudy2/eventstudies mentions; derivation script is `.Rbuildignore`'d and degrades gracefully.
- Only the 2 genuine-fix sites changed in `R/` — no valid-input behavior drift; discrepancy protocol honored (fixes documented in the audit log with regression tests; everything else documented as convention).

The one deviation noted in the SUMMARY (no full `--as-cran` run inside the gate; deferred to Phase 30) is consistent with the CONTEXT/PLAN scoped-check approach and the Phase 25 baseline, and does not affect any Phase 26 success criterion.

---

_Verified: 2026-09-11_
_Verifier: Claude (gsd-verifier)_
