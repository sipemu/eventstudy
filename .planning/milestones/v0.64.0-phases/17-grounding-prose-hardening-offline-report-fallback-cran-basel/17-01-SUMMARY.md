---
phase: 17-grounding-prose-hardening-offline-report-fallback-cran-basel
plan: "01"
subsystem: offline-narrative-engine
tags: [offline-narrative, advise, report, OFFLINE-01, REPORT-03, tracer]
status: complete

dependency_graph:
  requires: []
  provides:
    - OfflineNarrative S3 class with keys exec_summary/data_methods/results/robustness
    - es_advise(task_type="report_writing", provider=NULL) offline routing
    - generate_report(narrative=NULL) seam (REPORT-03)
    - skeleton.Rmd narrative: NULL param + eval-guarded narrative chunk
  affects:
    - Phase 18 renderer (depends on OfflineNarrative section key contract)
    - Phase 19 es_report() orchestrator (depends on REPORT-03 seam)

tech_stack:
  added: []
  patterns:
    - OfflineNarrative S3 class (new, keyed by section, locked contract for Phase 18)
    - No-fabrication prose: helpers build sentences only from diag values via sprintf()
    - eval-guarded Rmd chunk (eval=!is.null(params$narrative)) for NULL byte-identity

key_files:
  created:
    - tests/testthat/test_offline_narrative.R
  modified:
    - R/advise.R
    - R/advise_offline.R
    - R/report.R
    - inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd
    - tests/testthat/test_advise.R

decisions:
  - report_writing removed from LLM_ONLY_TYPES and added to KB_TYPES (OFFLINE-01)
  - OfflineNarrative as new S3 class (not extending es_advice) for clean separation
  - narrative= placed after advice= and before ... in generate_report() signature
  - Provider-path KB pre-build skipped for report_writing (Phase 18 owns LLM narrative)
  - test_advise.R report_writing assertion updated to reflect OFFLINE-01 new behavior

metrics:
  duration: "~15 minutes"
  completed: "2026-09-07"
  tasks_completed: 3
  commits: 4

actuals:
  tokens: 12500
  tasks: 3
  commits: 4
---

# Phase 17 Plan 01: Offline Report_writing Narrative Engine + REPORT-03 Seam — Summary

Tracer slice proving the full offline report path end-to-end with zero LLM provider: `es_advise(diag, "report_writing", provider=NULL)` returns a complete `OfflineNarrative` S3 object with four deterministic prose sections derived from package-computed diagnostics; `generate_report(task, narrative=<list>)` passes it through to the skeleton template and renders a non-empty HTML file.

## Tasks Completed

| Task | Name | Commit | Files |
|------|------|--------|-------|
| 1 (tracer) | Offline narrative engine + es_advise() routing | 904fe3f | R/advise.R, R/advise_offline.R, tests/testthat/test_offline_narrative.R |
| 2 (auto, tdd) | generate_report() narrative= seam + skeleton params | e20c594 | R/report.R, skeleton.Rmd |
| 3 (auto) | End-to-end offline report render (tracer proof) | 904fe3f | tests/testthat/test_offline_narrative.R |

## What Was Built

**Task 1 — OFFLINE-01 core (tracer):**
- Removed `"report_writing"` from `LLM_ONLY_TYPES` at `advise.R:26`; added to `KB_TYPES` at `advise.R:29`. The ADV-06 `stop()` block at `advise.R:763-769` is byte-unchanged.
- Added `report_writing` routing branch in no-provider block (`advise.R:778`) → `.build_offline_narrative(diagnostics)`.
- Guarded KB pre-build in provider path: `task_type != "report_writing"` so report_writing with a provider does not attempt stat_choice/robustness KB evidence (Phase 18 owns that).
- Added `.build_offline_narrative(diag)` + four helpers in `advise_offline.R`:
  - `.narrative_exec_summary()` — event count, median CAR t-stat, KB rule counts
  - `.narrative_data_methods()` — event counts, mean R², mean sigma
  - `.narrative_results()` — median CAR t/p, mean final CAR
  - `.narrative_robustness()` — fired robustness rules, overlap, mean DW stat
  - All helpers use only `diag$...` values at runtime via `sprintf()` (no-fabrication rule).
- Returns `structure(list(source="offline_kb", is_deterministic=TRUE, exec_summary, data_methods, results, robustness), class="OfflineNarrative")`.
- Section key names (`exec_summary`, `data_methods`, `results`, `robustness`) are locked as a Phase 18 renderer contract.

**Task 2 — REPORT-03 seam:**
- Added `narrative = NULL` parameter after `advice = NULL` in `generate_report()` signature.
- Added narrative validation block: non-list degrades to NULL with one warning.
- Added `narrative = narrative` to `rmarkdown::render()` params list.
- Added `narrative: NULL` to skeleton.Rmd `params:` YAML block.
- Added eval-guarded `narrative-section` chunk (`eval=!is.null(params$narrative)`): renders four sections under "AI/Offline Narrative" heading only when narrative is supplied; NULL path emits nothing.

**Task 3 — End-to-end render proof:**
- Test in `test_offline_narrative.R`: fitted task → `es_diagnostics()` → `es_advise(report_writing, NULL)` → OfflineNarrative → coerce to named list → `generate_report(narrative=)` → non-empty HTML containing offline prose.
- All 28 tests pass with `NOT_CRAN=true` (including the render test).

## Test Results

| Suite | PASS | FAIL | SKIP | Notes |
|-------|------|------|------|-------|
| test_offline_narrative.R (NOT_CRAN) | 28 | 0 | 0 | All pass including render |
| test_offline_narrative.R (CRAN) | 24 | 0 | 1 | Render test skip_on_cran() |
| test_advise.R (NOT_CRAN) | 70 | 0 | 0 | Updated report_writing assertion |
| test_report_advice.R | 23 | 0 | 3 | Existing tests unchanged |
| Full suite (NOT_CRAN) | 1960 | 0 | 29 | Zero regressions |

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 1 - Bug] Updated stale test_advise.R report_writing assertion**
- **Found during:** Task 1 regression check (full suite run)
- **Issue:** `test_advise.R:168` asserted `es_advise(diag, "report_writing", provider=NULL)` throws an error — this was the pre-OFFLINE-01 behavior that the plan explicitly resolves.
- **Fix:** Updated the test to assert the new correct behavior: `es_advise()` returns an `OfflineNarrative` object. The test name and comment were updated to document the OFFLINE-01 change.
- **Files modified:** `tests/testthat/test_advise.R`
- **Commit:** 4f4315a

## Acceptance Criteria Verification

- [x] `LLM_ONLY_TYPES` no longer contains `"report_writing"` (verified at advise.R:26)
- [x] `KB_TYPES` contains `"report_writing"` (verified at advise.R:29)
- [x] `.build_offline_narrative()` returns class `"OfflineNarrative"` with keys `exec_summary`, `data_methods`, `results`, `robustness`
- [x] ADV-06 `stop()` message at advise.R:763-769 is byte-unchanged (three remaining LLM-only types still stop())
- [x] `narrative` is a formal of `generate_report()`, default NULL, after `advice`, before `...`
- [x] skeleton.Rmd params block contains `narrative: NULL`
- [x] narrative chunk uses `eval = !is.null(params$narrative)`
- [x] advice validation message and position unchanged (backward-compat)

## Self-Check: PASSED

Files created/modified:
- R/advise.A — FOUND (modified, LLM_ONLY_TYPES/KB_TYPES surgery + routing branch)
- R/advise_offline.R — FOUND (modified, .build_offline_narrative + 4 helpers)
- R/report.R — FOUND (modified, narrative= seam)
- inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd — FOUND (modified)
- tests/testthat/test_offline_narrative.R — FOUND (created, 28 tests)
- tests/testthat/test_advise.R — FOUND (modified, report_writing assertion updated)

Commits:
- 904fe3f: feat(17-01): offline report_writing narrative engine + es_advise() routing
- e20c594: feat(17-01): generate_report() narrative= seam + skeleton params
- 4f4315a: fix(17-01): update test_advise.R report_writing assertion for OFFLINE-01
