---
phase: 18-multi-format-renderer-fixed-template-grounded-narrative-asse
fixed_at: 2026-09-07T13:00:00Z
review_path: .planning/phases/18-multi-format-renderer-fixed-template-grounded-narrative-asse/18-REVIEW.md
iteration: 1
findings_in_scope: 4
fixed: 4
skipped: 0
status: all_fixed
---

# Phase 18: Code Review Fix Report

**Fixed at:** 2026-09-07T13:00:00Z
**Source review:** .planning/phases/18-multi-format-renderer-fixed-template-grounded-narrative-asse/18-REVIEW.md
**Iteration:** 1

**Summary:**
- Findings in scope: 4
- Fixed: 4
- Skipped: 0

## Fixed Issues

### CR-01: `.sanitise_for_pdf()` corrupts `\textbackslash{}`

**Files modified:** `R/report_narrative.R`, `tests/testthat/test_prose_sanitiser.R`
**Commit:** d54f38a
**Applied fix:** Replaced the backslash with a placeholder token (`BSPH7F3A`) that contains no LaTeX specials before brace-escaping runs (steps 2-3), then restored it as `\textbackslash{}` after all other specials are escaped. The old approach replaced backslash with `\textbackslash{}` in step 1 and then steps 2-3 re-escaped the `{}` to `\{\}`, yielding corrupted output. Added a regression test asserting `a\b` produces `a\textbackslash{}b` (not `a\textbackslash\{\}b`).

### CR-02: `output_options = list("fig.path" = ...)` is silently ignored

**Files modified:** `R/report.R`, `inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd`, `tests/testthat/test_report_multiformat.R`
**Commit:** 1f45fe7
**Applied fix:** Removed the no-op `output_options=` argument from `rmarkdown::render()`. Added `fig_path: NULL` to `skeleton.Rmd` params block. Added `knitr::opts_chunk$set(fig.path = params$fig_path)` in the skeleton setup chunk (guarded by `!is.null(params$fig_path) && nzchar(params$fig_path)`). In `generate_report()`, set `render_params$fig_path <- file.path(output_dir, ...)` before the render call. Added tests asserting `fig_path` is declared in the skeleton and the setup chunk sets it.

### WR-01: `significance_fn` parameter declared but never invoked

**Files modified:** `R/report_narrative.R`
**Commit:** 9ddaaed
**Applied fix:** Removed the `significance_fn = .calibrate_significance` parameter from `assemble_report_narrative()` signature and its `@param` documentation line. No tests reference this parameter; verified with grep across the project.

### WR-02: `es_diagnostics(task)` called up to three times; wrong references gate

**Files modified:** `R/report.R`
**Commit:** 2a50dad
**Applied fix:** Compute `diag` once at the top of the function (before the narrative assembly block) and reuse it for narrative assembly, KB references, and `diag_for_render` (template params). Changed the references gate from `!is.null(narrative)` to `!is.null(diag)` so KB references are populated whenever diagnostics are available, even if `assemble_report_narrative()` throws.

---

## Test Results

Verification ran in the **main checkout** (workflow.use_worktrees=false).

**Affected test files (post-fix):**

| File | FAIL | WARN | SKIP | PASS |
|---|---|---|---|---|
| test_prose_sanitiser.R | 0 | 0 | 0 | 50 |
| test_report_narrative_asm.R | 0 | 3* | 0 | 73 |
| test_report_multiformat.R | 0 | 0 | 10** | 27 |

*Warnings are expected "Provider call failed" messages from the error-fallback test fixture (pre-existing behavior).
**Render tests skip with `skip_on_cran()` in this environment.

**Full suite:** fail=0 pass=2116 skip=73 (no regressions)

---

_Fixed: 2026-09-07T13:00:00Z_
_Fixer: Claude (gsd-code-fixer)_
_Iteration: 1_
