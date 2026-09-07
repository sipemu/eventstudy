---
phase: 19-one-call-es-report-orchestrator-run-event-study-report-cran
plan: "01"
subsystem: report
tags: [es_report, orchestrator, non-mutation, offline, multi-format, REPORT-01, REPORT-04]
status: complete

dependency_graph:
  requires: []
  provides:
    - es_report()  # exported orchestrator in R/report.R
  affects:
    - R/report.R
    - tests/testthat/test_es_report.R

tech_stack:
  added: []
  patterns:
    - deep-clone-at-entry non-mutation guard (R6 clone(deep=TRUE))
    - thin-passthrough wrapper delegating to generate_report()
    - visible return (strip invisible from generate_report output)

key_files:
  created:
    - tests/testthat/test_es_report.R
  modified:
    - R/report.R

decisions:
  - es_report() is a thin passthrough: all format validation, narrative assembly,
    and rendering stay in generate_report(); es_report only adds the deep-clone
    guard and the visible return.
  - narrative=NULL passed to generate_report so narrative is assembled ONCE
    inside generate_report (NARR-01 budget preserved; no double-LLM-call risk).
  - diag harvested from clone but not passed explicitly to generate_report
    (generate_report calls es_diagnostics internally via its own WR-02 path;
    passing it separately would require a parameter not in generate_report's
    public signature).

metrics:
  duration: "138s"
  completed: "2026-09-07"
  tasks_completed: 2
  commits: 2

actuals:
  tokens: 12000
  tasks: 2
  commits: 2
---

# Phase 19 Plan 01: es_report() One-Call Orchestrator Summary

**One-liner:** Public `es_report()` wrapper that deep-clones the task, delegates narrative+render to `generate_report()`, and returns path(s) visibly.

## What Was Built

Added `es_report()` to `R/report.R` — a thin exported orchestrator over the Phase 17/18 pipeline:

1. **Task guard** — rejects non-EventStudyTask inputs with the same wording as `generate_report()`.
2. **REPORT-04 non-mutation** — `task$clone(deep = TRUE)` called before any other work; all downstream steps operate on the clone.
3. **Diagnostics harvest** — `es_diagnostics(cloned)` called from es_report; result available for future use (currently passed implicitly via generate_report's internal WR-02 path).
4. **Render delegation** — `generate_report(cloned, ..., narrative = NULL)` called once; `narrative = NULL` ensures generate_report assembles the narrative once per call (NARR-01 budget preserved).
5. **Visible return** — `paths <- generate_report(...)` captured and returned as a bare (visible) expression, stripping `generate_report`'s `invisible()` wrapper.

New test file `tests/testthat/test_es_report.R` covers:
- Structural formals and clone-guard checks (no render needed)
- REPORT-01: visible return + HTML path existence (skip_on_cran)
- REPORT-04: serialize-based snapshot identity before/after call (skip_on_cran)
- Multi-format: `format = c("html","md")` returns correct length (skip_on_cran)
- Provider passthrough: call-count stability across two invocations (skip_on_cran)
- Invalid format propagates `generate_report`'s `stop()`
- Non-ASCII gate on R/report.R

## Deviations from Plan

**1. [Rule 1 - Adjustment] diag not passed explicitly to generate_report**
- **Found during:** Task 1 implementation
- **Issue:** The plan says "harvest diagnostics then pass to generate_report", but `generate_report`'s public signature does not accept a pre-computed `diag` argument (it calls `es_diagnostics(task)` internally via the WR-02 path). Passing diag explicitly would require extending `generate_report`'s signature, which is out of scope for this plan.
- **Fix:** `es_report` harvests diag from the clone (future-proofing the composition seam), but lets `generate_report` also call `es_diagnostics` internally. The diagnostic harvest in `es_report` adds negligible cost and correctly operates on the clone.
- **Impact:** Diagnostics computed twice per call (once in es_report, once in generate_report). This is a minor redundancy, not a correctness issue.

## Self-Check

**Created files:**
- `tests/testthat/test_es_report.R` — FOUND
- `R/report.R` (modified) — FOUND

**Commits:**
- d65b6af: test(19-01) — FOUND
- 50c9d05: feat(19-01) — FOUND

## Self-Check: PASSED

## Known Stubs

None.

## Threat Flags

None — es_report adds no new network surface, auth path, or trust boundary beyond those already present in generate_report (inherited from Phase 17/18).
