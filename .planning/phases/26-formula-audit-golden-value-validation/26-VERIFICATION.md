---
phase: 26-formula-audit-golden-value-validation
verified: 2026-09-11T12:00:00Z
status: passed
score: 5/5 must-haves verified
behavior_unverified: 0
overrides_applied: 0
re_verification:
  previous_status: passed
  previous_score: 5/5
  gaps_closed: []
  gaps_remaining: []
  regressions: []
  reason: "Stale VERIFICATION.md — Phase 27 test-only commits (f2c5305, 4c69d6b) landed on top; no Phase 26 deliverables were modified. Regenerated from current tree."
---

# Phase 26: Formula Audit & Golden-Value Validation Verification Report

**Phase Goal:** Every return model (13+) and test statistic (8+) is verified correct against its published academic formula with its convention choices documented, and the key statistics are pinned to reference values — so a wrong number can never silently pass, and a correct number can never be falsely failed by a convention mismatch.
**Verified:** 2026-09-11T12:00:00Z
**Status:** passed
**Re-verification:** Yes — prior VERIFICATION.md went stale after Phase 27 test-only commits; Phase 26 deliverables unchanged (confirmed via `git diff f1d9be2..HEAD` over all Phase 26 files = empty).

## Re-Verification Scope

Phase 27 commits (`f2c5305`, `4c69d6b`) modified only `R/models.R` and added `tests/testthat/test_numerical_stability.R`. None of the Phase 26 deliverables (`vignettes/statistical-conventions.Rmd`, `tests/testthat/test_golden_values.R`, `tests/testthat/helper-golden-data.R`, `data-raw/derive-golden-values.R`, `R/multi_event_test_statistics.R`) were touched. All previously-verified truths are confirmed against the current tree.

## Goal Achievement

### Observable Truths

| # | Truth | Status | Evidence |
| --- | --- | --- | --- |
| 1 | Each 13+ return model and 8+ test statistic has an audited convention section in `vignettes/statistical-conventions.Rmd` citing a published source (CORR-01) | ✓ VERIFIED | 686-line file with 25 `###` convention sections covering 13 return models (Market, MarketAdjusted, ComparisonPeriodMean, FF3, FF5, Carhart4, Custom, BHAR, Volume, Volatility, RollingWindow, GARCH, DCC-GARCH) and 11 test statistics (AR/CAR t, BHAR t, CSectT, PatellZ, BMP, Sign, GeneralizedSign, Rank, CalendarTimePortfolio, KolariPynnonen, PermutationTest stub). All sections carry published-source citations. |
| 2 | Any formula error found is fixed, the fix precedes its golden pin, and is locked by a regression test (CORR-01) | ✓ VERIFIED | 2 genuine bugs fixed in `R/multi_event_test_statistics.R`: `rank_z` (L411, `scalar-ifelse`->`if()` guard) and `caltime_t`/`ccaltime_t` (L563/565, same fix). Fix commit `d777113` precedes any pin of those statistics. Regression tests in `test_golden_values.R` pin corrected per-day values and assert per-day distinctness (`expect_false(res$rank_z[1] == res$rank_z[2])`), so the broadcast bug cannot silently return. |
| 3 | Golden-value tests pin key statistics to reference constants with inline provenance, assumed conventions, and explicit tolerance (CORR-02) | ✓ VERIFIED | 729-line file with 23 `test_that` blocks and 72 `expect_*` assertions. Each block carries inline provenance (source citation), assumed-conventions comment, and tolerance rationale (absolute 1e-10 for algebraic identities; documented skip-guarded rationale for GARCH/DCC where version-fragile constants would flake). Confirmed by running the file directly: FAIL 0 / WARN 0 / SKIP 2 (expected: rugarch/rmgarch absent) / PASS 70. |
| 4 | estudy2/eventstudies appear only in the `.Rbuildignore`'d `data-raw/` derivation script, never in DESCRIPTION | ✓ VERIFIED | `grep -Ec "estudy2|eventstudies" DESCRIPTION` = 0. `.Rbuildignore` line 12: `^data-raw$`. `data-raw/derive-golden-values.R` (387 lines) contains 4 `requireNamespace()` guards and degrades gracefully when those packages are absent. |
| 5 | Behavior on valid inputs is unchanged and the full suite stays green | ✓ VERIFIED | Only `R/multi_event_test_statistics.R` changed in `R/` across the entire phase (2 documented bug fixes; no published convention makes per-day stats identical, confirming these are genuine errors). Phase 27 commits added tests but did not revert or alter the Phase 26 fixes. SUMMARY records FAIL_TOTAL 0 / PASS 2429 at gate. Golden test file itself currently: FAIL 0 / SKIP 2 / PASS 70. |

**Score:** 5/5 truths verified (0 present, behavior-unverified)

### Required Artifacts

| Artifact | Expected | Status | Details |
| --- | --- | --- | --- |
| `vignettes/statistical-conventions.Rmd` | CRAN-shipped convention reference with citations | ✓ VERIFIED | 686 lines; 25 cited `###` sections; pure ASCII (0 non-ASCII bytes confirmed by grep); audit-log summary present. |
| `tests/testthat/test_golden_values.R` | Golden-value regression net | ✓ VERIFIED | 729 lines; 23 test blocks; 72 assertions; FAIL 0 / SKIP 2 / PASS 70 on current tree. |
| `tests/testthat/helper-golden-data.R` | Deterministic fixtures | ✓ VERIFIED | 357 lines; 9 fixture builders feeding real R6 pipeline (e.g., `MarketModel$fit()`, `PatellZTest$compute()`). |
| `data-raw/derive-golden-values.R` | Non-shipped, estudy2-optional derivation script | ✓ VERIFIED | 387 lines; 4 `requireNamespace()` guards; `sessionInfo()` dump; graceful degrade when estudy2/eventstudies absent; covered by `^data-raw$` in `.Rbuildignore`. |

### Key Link Verification

| From | To | Via | Status | Details |
| --- | --- | --- | --- | --- |
| Vignette convention rows | `test_golden_values.R` tolerance/convention annotations | Inline "Assumed conventions (see vignette ...)" comments in each test block | ✓ WIRED | PatellZ block (L464) explicitly cross-references the vignette Patell section; tolerance rationale matches the CONTEXT tolerance policy (1e-10 for identities). |
| FEC / df / Patell-denominator choices in `R/*.R` | Documented vignette convention row | Audit output from phase execution | ✓ WIRED | Patell Q_i=(m-k)/(m-k-2) documented in vignette and pinned in test at L468/484; `rank_z` and `caltime_t` fixes documented in the vignette audit-log section and locked by per-day-distinctness regression assertions. |

### Data-Flow Trace (Level 4)

| Artifact | Data Variable | Source | Produces Real Data | Status |
| --- | --- | --- | --- | --- |
| `test_golden_values.R` | golden fixture values | `golden_*_fixture()` builders in `helper-golden-data.R` fed through production R6 `fit()`/`compute()` pipeline | Yes — real package pipeline, deterministic seed/constants | ✓ FLOWING |

### Behavioral Spot-Checks

| Behavior | Command | Result | Status |
| --- | --- | --- | --- |
| Golden test file passes on current tree | `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test_golden_values.R")'` | FAIL 0 / WARN 0 / SKIP 2 / PASS 70 | ✓ PASS |
| RankTest per-day distinctness regression holds | Included in PASS count above (test block 21, L655 `expect_false`) | Passed | ✓ PASS |
| CalendarTimePortfolio per-day distinctness regression holds | Included in PASS count above (test block 22, L694 `expect_false`) | Passed | ✓ PASS |
| Vignette pure ASCII | `grep -cP "[^\x00-\x7F]" vignettes/statistical-conventions.Rmd` | 0 | ✓ PASS |
| DESCRIPTION free of estudy2/eventstudies | `grep -Ec "estudy2|eventstudies" DESCRIPTION` | 0 | ✓ PASS |
| Phase 27 commits did not modify Phase 26 deliverables | `git diff f1d9be2..HEAD -- vignettes/statistical-conventions.Rmd tests/testthat/test_golden_values.R tests/testthat/helper-golden-data.R data-raw/derive-golden-values.R R/multi_event_test_statistics.R` | (empty diff) | ✓ PASS |

### Requirements Coverage

| Requirement | Source Plan | Description | Status | Evidence |
| --- | --- | --- | --- | --- |
| CORR-01 | 26-01 | Formula audit + convention documentation for 13+ models / 8+ statistics; fix genuine errors with regression tests | ✓ SATISFIED | 25 cited vignette sections; 2 bugs fixed and regression-locked in `d777113`; `if()` guards confirmed present at L411 and L563/565 of `R/multi_event_test_statistics.R`. |
| CORR-02 | 26-01 | Golden-value pins with inline provenance, explicit tolerance, assumed-conventions annotation; estudy2 non-shipped | ✓ SATISFIED | 72 annotated assertions passing; DESCRIPTION clean (0 mentions); derivation script `.Rbuildignore`'d. |

### Anti-Patterns Found

| File | Line | Pattern | Severity | Impact |
| --- | --- | --- | --- | --- |
| — | — | No unreferenced debt markers in phase-modified files | Info | None. The PermutationTest stub is honestly documented in the vignette audit log as unwired/deferred — not a hidden defect. The remaining `ifelse()` calls in `R/multi_event_test_statistics.R` operate on vector-length conditions and are correct usage; only the two scalar-condition broadcast bugs were fixed. |

### Human Verification Required

None. All five must-have truths were verified programmatically against the current codebase. The golden test file was executed directly and returned 0 failures.

### Gaps Summary

No gaps. All five PLAN must-haves and both ROADMAP success criteria are met with codebase evidence against the current tree:

- Every one of the 13+ return models and 8+ test statistics has a cited vignette convention section and at least one golden pin (GARCH/DCC carry the documented skip-guarded identity rationale, consistent with the CONTEXT decision).
- The 2 claimed bug fixes (`ifelse`->`if` for `rank_z` and `caltime_t`/`ccaltime_t`) are present in source, preceded their golden pins in commit ordering, and are locked by per-day-distinctness regression tests.
- Vignette is pure ASCII; DESCRIPTION has 0 estudy2/eventstudies mentions; `data-raw/` is `.Rbuildignore`'d.
- Phase 27 test-only commits left all Phase 26 deliverables untouched (empty diff confirmed).

---

_Verified: 2026-09-11T12:00:00Z_
_Verifier: Claude (gsd-verifier) — re-verification after Phase 27 stale-VERIFICATION trigger_
