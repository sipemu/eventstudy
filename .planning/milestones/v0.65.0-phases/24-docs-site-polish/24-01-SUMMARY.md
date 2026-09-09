---
phase: 24-docs-site-polish
plan: 01
subsystem: documentation
tags: [roxygen2, family, seealso, pkgdown, docs-only]
status: complete
requires:
  - Phase 23 exported format.* / print.* S3 methods (untouched)
  - Live NAMESPACE (77 exports) as family-membership source of truth
provides:
  - "@family cross-links: 7 eventstudy- families across 74 exported analysis functions"
  - "Hub-and-spoke + sparse cross-family @seealso navigation on pipeline/advisor/export"
  - Regenerated man/*.Rd + NAMESPACE with See Also family clusters
affects:
  - pkgdown Reference index grouping (rendered in a later wave/deploy)
tech-stack:
  added: []
  patterns:
    - "@family immediately before @export; @seealso extended never replaced"
    - "single devtools::document() run after all roxygen edits"
key-files:
  created:
    - .planning/phases/24-docs-site-polish/24-01-SUMMARY.md
  modified:
    - R/execute.R
    - R/prepare_event_study.R
    - R/task.R
    - R/parameter_set.R
    - R/models.R
    - R/models_time_varying.R
    - R/return_calculation.R
    - R/single_event_test_statistics.R
    - R/multi_event_test_statistics.R
    - R/test_statistics_set.R
    - R/bootstrap.R
    - R/p_adjustment.R
    - R/cross_sectional.R
    - R/simulation.R
    - R/task_validation.R
    - R/diagnostics.R
    - R/panel_event_study.R
    - R/task_intraday.R
    - R/synthetic_control.R
    - R/plotting.R
    - R/theme.R
    - R/export.R
    - R/report.R
    - R/advise.R
    - R/es_diagnostics.R
    - R/knowledge_base.R
    - R/advise_offline.R
    - R/provider.R
    - man/ (74 Rd files regenerated)
    - NAMESPACE (unchanged content — regenerated identically)
decisions:
  - "plot_synthetic_control assigned to eventstudy-tasks (RESEARCH A2 note), giving tasks 10 members"
  - "recommend_stat / flag_robustness @family placed on the S3 generic only (methods share the Rd via @rdname)"
  - "download_* left un-familied per RESEARCH A2"
metrics:
  duration: ~18m
  completed: 2026-09-09
actuals:
  tokens: 9000
  tasks: 3
  commits: 3
---

# Phase 24 Plan 01: @family + @seealso Cross-Links Summary

Added exactly one `@family` tag to 74 exported analysis functions (7 `eventstudy-` families) plus hub-and-spoke and sparse cross-family `@seealso` bridges across the pipeline, advisor, and export functions, then regenerated `man/` + `NAMESPACE` with a single `devtools::document()` — docs-only, DESCRIPTION unchanged, full suite 2222 pass / 0 fail.

## What Was Built

**Task 1 (tracer, `49467b0`):** `@family eventstudy-pipeline` + hub-and-spoke `@seealso` on the four pipeline entry points (`run_event_study`, `fit_model`, `calculate_statistics`, `prepare_event_study`); proved `document()` regenerates the See Also cluster before broadening.

**Task 2 (`8aea5d6`):** `@family` added to the remaining 70 members across 25 files — models 15, statistics 23, tasks 10, plots 6, export 4, advisor 10, plus `EventStudyTask`/`ParameterSet` into pipeline. No `document()` yet.

**Task 3 (`775dc76`):** Cross-family `@seealso` bridges (`run_event_study`→MarketModel+ARTTest; es_advise/es_diagnostics/es_report→run_event_study; export_results→run_event_study+tidy.EventStudyTask), then one `devtools::document()` regenerating 74 Rd files + NAMESPACE.

## Family Membership (74 total, all families >= 2)

| Family | Members |
|--------|---------|
| eventstudy-pipeline | 6 |
| eventstudy-models | 15 |
| eventstudy-statistics | 23 |
| eventstudy-tasks | 10 |
| eventstudy-plots | 6 |
| eventstudy-export | 4 |
| eventstudy-advisor | 10 |

`download_stock_data`, `download_factor_data`, `download_risk_free_rate` intentionally un-familied.

## Verification

- `devtools::document()`: clean run; only pre-existing baseline roxygen warnings (in `EventStudy-package.R` @importFrom and R6 `@param`/`print` blocks — all in files this plan never touched).
- Link resolution: all 64 distinct `\link{}` targets across `man/*.Rd` resolve to a NAMESPACE export or a documented Rd alias — zero unresolved.
- Full testthat suite: **PASS=2222, FAIL=0** (WARN=4 expected, SKIP=96 On-CRAN).
- `git diff --quiet DESCRIPTION`: exit 0 (unchanged).
- No `@family` above any `format.*`/`print.*` S3 block (Pitfall 5 clean); no function block carries two `@family` (Pitfall 6 clean).

## Deviations from Plan

**1. [Discretion] plot_synthetic_control added to eventstudy-tasks**
- Plan Task 2 did not name `plot_synthetic_control`, but it is an exported plot function. RESEARCH A2 (line 174) assigns task-specific plots to `eventstudy-tasks`. Leaving it un-familied would leave an exported plot function isolated. Added to `eventstudy-tasks` → 10 members (was 9). No functional impact.
- Files: R/synthetic_control.R · Commit: 8aea5d6

Otherwise the plan executed as written.

## Out-of-Scope Observation (not actioned)

`EventStudy.Rcheck/` (339 files) is **tracked in git** from Phase 14 (`91cb2df`), contradicting the task briefing's "not tracked" description. None of this plan's three commits touched it. Removing a pre-existing tracked build-artifact directory is unrelated to this docs-only plan, so it was left in place and is flagged here for a dedicated cleanup rather than being bundled into a documentation commit.

## Self-Check: PASSED

- man/run_event_study.Rd, man/es_advise.Rd, man/export_results.Rd exist with expected `\link{}` targets — FOUND
- Commits 49467b0, 8aea5d6, 775dc76 present in `git log` — FOUND
- DESCRIPTION unchanged, NAMESPACE content unchanged — CONFIRMED
