---
phase: 17-grounding-prose-hardening-offline-report-fallback-cran-basel
verified: 2026-09-07T00:00:00Z
status: passed
score: 9/9 must-haves verified
behavior_unverified: 0
overrides_applied: 0
---

# Phase 17: Grounding Prose Hardening + Offline Report Fallback + CRAN Baseline Verification Report

**Phase Goal:** The milestone's core-value gate lands first — the grounding guard now catches a fabricated number sitting in free-text narrative prose (not just in structured evidence[] arrays), the report narrative renders complete with no LLM provider configured (the current report_writing-is-LLM-only stop() is resolved via the rule-based offline engine), generate_report() carries a backward-compatible narrative= seam (NULL path byte-identical), and CRAN hygiene discipline for render examples/tests is established up front so no downstream phase can leak a check regression.
**Verified:** 2026-09-07
**Status:** passed
**Re-verification:** No — initial verification

## Goal Achievement

### Observable Truths

| # | Truth | Status | Evidence |
|---|-------|--------|----------|
| 1 | es_advise(diag, "report_writing", provider=NULL) returns an OfflineNarrative — no stop() (OFFLINE-01) | VERIFIED | `LLM_ONLY_TYPES` at advise.R:26 = `c("interpret","recommend_model","design_discussion")`; `KB_TYPES` at advise.R:31 includes `"report_writing"`; routing branch at advise.R:983-984 calls `.build_offline_narrative(diagnostics)`; 28/28 offline_narrative tests pass |
| 2 | The three remaining LLM-only types (interpret, recommend_model, design_discussion) still stop() with unchanged ADV-06 message when provider=NULL | VERIFIED | advise.R:965 fires on `LLM_ONLY_TYPES`; regression tests in test_offline_narrative.R and test_advise.R confirm; 103 tests pass with WARN 0 |
| 3 | OfflineNarrative carries all four section keys: exec_summary, data_methods, results, robustness | VERIFIED | `.build_offline_narrative()` in advise_offline.R:253-278 returns `structure(list(..., exec_summary=, data_methods=, results=, robustness=), class="OfflineNarrative")`; locked as Phase 18 contract |
| 4 | generate_report() accepts narrative=NULL after advice= and before ...; NULL path byte-identical to pre-Phase-17 output (REPORT-03) | VERIFIED | report.R:50 shows `narrative = NULL` formal after `advice = NULL`; report.R:137 passes it to render params; golden-file test in test_report_narrative.R Group B passes (timestamp-stripped byte identity); 19/19 report_narrative tests pass |
| 5 | A fabricated numeric literal in narrative prose absent from es_diagnostics is caught; section dropped, exactly one warning (GROUND-01/02) | VERIFIED | `.scan_prose_grounding()` at advise.R:484-511 drops whole field to "" on any ungrounded literal, emits exactly one warning; "99.99" fabricated-literal test in test_prose_grounding.R passes; 56/56 prose_grounding tests pass |
| 6 | A correctly-rounded literal (e.g. 2.35 for actual 2.3456) is KEPT — rounding-aware match (GROUND-02) | VERIFIED | `.is_grounded_literal()` at advise.R:431-467 implements rounding-aware match at literal's displayed decimal precision; spike test for 2.35 vs 2.3456 passes |
| 7 | Citation years (1997), significance constants (0.05), and structural integers (n_events) are exempt — no false-positive drops (GROUND-02) | VERIFIED | `.is_grounded_literal()` exemption chain: structural-int first, then year 1900-2100, then c(0.001,0.01,0.05,0.10); all-grounded prose zero-warning test passes |
| 8 | Fabricated-number-in-prose invariant locked by regression tests (GROUND-03) | VERIFIED | test_prose_grounding.R contains GROUND-03 test: "99.99" absent from kept sections + all-grounded zero-warning case; 56/56 pass |
| 9 | CRAN hygiene: tinytex in DESCRIPTION Suggests; render tests skip-guarded; committed cran-check-baseline.txt non-empty | VERIFIED | `grep tinytex DESCRIPTION` found in Suggests block; test_report_narrative.R Groups B/C carry `skip_on_cran()`+`skip_if_not_installed()`; cran-check-baseline.txt present, 39 lines, committed (b086016) |

**Score:** 9/9 truths verified (0 present, behavior-unverified)

### Required Artifacts

| Artifact | Expected | Status | Details |
|----------|----------|--------|---------|
| `R/advise.R` | LLM_ONLY_TYPES/KB_TYPES surgery + 4 prose grounding internals | VERIFIED | Lines 26/31 confirm type lists; lines 340/371/431/484 confirm `.extract_numeric_literals`, `.build_prose_value_registry`, `.is_grounded_literal`, `.scan_prose_grounding` |
| `R/advise_offline.R` | `.build_offline_narrative()` + 4 helpers | VERIFIED | Lines 253+ confirm function; exec_summary/data_methods/results/robustness helpers exist |
| `R/report.R` | `narrative=NULL` parameter + validation + params passthrough | VERIFIED | Line 50: formal; line 112-119: validation; line 137: params passthrough |
| `inst/rmarkdown/templates/.../skeleton.Rmd` | `narrative: NULL` in params + eval-guarded chunk | VERIFIED | Line 20: `narrative: NULL` in params; line 220: `eval=!is.null(params$narrative)` |
| `tests/testthat/test_offline_narrative.R` | 28+ tests for OFFLINE-01 including end-to-end render | VERIFIED | File exists; 28/28 pass |
| `tests/testthat/test_prose_grounding.R` | 56 tests for GROUND-01/02/03 | VERIFIED | File exists; 56/56 pass |
| `tests/testthat/test_report_narrative.R` | 19 tests for REPORT-03 golden-file + seams | VERIFIED | File exists; 19/19 pass |
| `DESCRIPTION` | tinytex in Suggests | VERIFIED | `grep tinytex DESCRIPTION` positive |
| `cran-check-baseline.txt` | Non-empty committed baseline artifact | VERIFIED | Present, 39 lines, committed b086016 |

### Key Link Verification

| From | To | Via | Status | Details |
|------|----|-----|--------|---------|
| `es_advise()` provider=NULL + task_type="report_writing" | `.build_offline_narrative()` | advise.R:983-984 routing branch | WIRED | Confirmed in grep output: `if (task_type == "report_writing") return(.build_offline_narrative(diagnostics))` |
| `generate_report()` narrative= param | `skeleton.Rmd params$narrative` | report.R:137 `narrative=narrative` in render params | WIRED | report.R line 137 confirmed |
| `.scan_prose_grounding()` | `.build_prose_value_registry()` + `.is_grounded_literal()` | advise.R:487+501 | WIRED | Internal call chain confirmed in grep |

### Behavioral Spot-Checks

| Behavior | Command | Result | Status |
|----------|---------|--------|--------|
| 28 offline_narrative tests pass | `NOT_CRAN=true Rscript -e 'testthat::test_dir(..., filter="offline_narrative")'` | 28 PASS, 0 FAIL | PASS |
| 56 prose_grounding tests pass | `NOT_CRAN=true Rscript -e 'testthat::test_dir(..., filter="prose_grounding")'` | 56 PASS, 0 FAIL | PASS |
| 19 report_narrative tests pass | `NOT_CRAN=true Rscript -e 'testthat::test_dir(..., filter="report_narrative")'` | 19 PASS, 0 FAIL | PASS |
| Combined filter | Full 3-filter run | 103 PASS, 0 FAIL, 0 SKIP, 0 WARN | PASS |

### Requirements Coverage

| Requirement | Source Plan | Description | Status | Evidence |
|-------------|------------|-------------|--------|----------|
| GROUND-01 | 17-02 | Prose scanner catches fabricated numeric literals absent from es_diagnostics | SATISFIED | `.extract_numeric_literals` + `.scan_prose_grounding` wired; 56 tests green |
| GROUND-02 | 17-02 | Fabricated literal dropped with one warning; rounded/year/constant/structural exempt | SATISFIED | `.is_grounded_literal` 5-step exemption chain; tests cover all cases |
| GROUND-03 | 17-02 | Regression tests lock fabricated-number-never-rendered invariant | SATISFIED | GROUND-03 test in test_prose_grounding.R passes |
| OFFLINE-01 | 17-01 | report_writing with provider=NULL returns complete OfflineNarrative, no stop() | SATISFIED | LLM_ONLY_TYPES/KB_TYPES surgery confirmed; routing branch confirmed; 28 tests green |
| REPORT-03 | 17-01, 17-03 | generate_report() narrative=NULL seam; NULL path byte-identical | SATISFIED | Formal confirmed; golden-file test passes; skeleton.Rmd wired |

**CRAN hygiene note:** REQUIREMENTS.md notes CRAN-01/02 are formally owned by Phase 19 but Phase 17 establishes the discipline. Phase 17 delivers: tinytex in Suggests, render tests skip-guarded, committed baseline. This is confirmed.

### Anti-Patterns Found

| File | Pattern | Severity | Impact |
|------|---------|----------|--------|
| None found | — | — | — |

No TBD/FIXME/XXX markers found in phase-modified files. No stub implementations. All prose helpers build sentences from `diag$...` values at runtime via `sprintf()` per the no-fabrication rule.

### Human Verification Required

None. All must-haves are fully verifiable programmatically. The test suite (103 tests, 0 FAIL, 0 WARN, 0 SKIP under NOT_CRAN=true) provides complete behavioral evidence including end-to-end render.

### Gaps Summary

No gaps. All 9 must-have truths are VERIFIED against the actual codebase with test evidence.

---

_Verified: 2026-09-07_
_Verifier: Claude (gsd-verifier)_
