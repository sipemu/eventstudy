---
gsd_state_version: 1.0
milestone: v0.65.0
milestone_name: Polish
status: planning
last_updated: "2026-09-08T19:06:46.242Z"
last_activity: 2026-09-08
progress:
  total_phases: 0
  completed_phases: 0
  total_plans: 0
  completed_plans: 0
  percent: 0
---

# Project State

## Project Reference

See: .planning/PROJECT.md (updated 2026-09-07)

**Core value:** Trustworthy numbers, trustworthy interpretation — the pipeline is never silently wrong, and the AI report cites only package-computed diagnostics, never fabricating a result.
**Current focus:** Planning next milestone (v0.64.0 shipped 2026-09-07)

## Current Position

Phase: Not started (defining requirements)
Plan: —
Status: Defining requirements
Last activity: 2026-09-08 — Milestone v0.65.0 started

## Milestone Roadmap (v0.64.0)

- **Phase 17: Grounding-Prose Hardening, Offline Report Fallback & CRAN Baseline** — GROUND-01..03, OFFLINE-01, REPORT-03. Extend the grounding guard to scan free-text narrative prose for numeric literals absent from `es_diagnostics()` (the milestone's core-value gate, lands first); resolve the `report_writing`-is-LLM-only `stop()` so narrative renders offline via the rule-based engine; add backward-compatible `narrative=NULL` seam to `generate_report()` (NULL path byte-identical); establish `\dontrun{}` / `skip_on_cran()` / Suggests CRAN hygiene.
- **Phase 18: Multi-Format Renderer, Fixed Template & Grounded Narrative Assembly** — NARR-01..05, FORMAT-01..04, TMPL-01/02, OFFLINE-02. Section-by-section grounded narrative assembler (one LLM call per section, independent of format count); references from KB citations; static significance calibration; fixed joint-hypothesis caveat. Fixed template (exec summary · data/methods · results · diagnostics · robustness/caveats · references), arg-toggled sections, data/methods+results auto-filled from task+diagnostics. Renders HTML/PDF/Word/Markdown; ggplot2 static for non-HTML via `knitr::is_html_output()`; per-format prose sanitiser; graceful toolchain skips; visible AI-vs-offline mode distinction.
- **Phase 19: es_report() Orchestrator, run_event_study(report=) & CRAN-Clean Release Gate** — REPORT-01/02/04, CRAN-01/02. Public `es_report()` composing study → diagnostics → advise → render, returning output path(s), deep-cloning the task (no caller mutation); additive `run_event_study(..., report=TRUE)` (defaults FALSE, byte-identical when omitted); final `R CMD check --as-cran` gate (tinytex Suggests-only, no new NOTEs/WARNINGs, suite green, NEWS v0.64.0).

Dependency order: Phase 17 (grounding + offline + CRAN discipline, blocking) → Phase 18 (renderer/template/multi-format, needs the `narrative=` seam + offline fallback + prose guard) → Phase 19 (orchestrator + convenience param + final release gate, needs the renderer).

Coverage: 22/22 v0.64.0 requirements mapped, 0 unmapped. (CRAN-01/02 established in Phase 17, formally owned + verified in Phase 19.)

## Performance Metrics

**Velocity:**

- Total plans completed (all milestones): 16
- Average duration: -
- Total execution time: 0 hours

**Recent Trend:**

- Last 5 plans: n/a (new milestone)
- Trend: n/a

*Updated after each plan completion*
**Per-Plan Metrics:**

| Plan | Duration | Tasks | Files |
|------|----------|-------|-------|
| Phase 17 P01 | 15 | 3 tasks | 6 files |
| Phase 17 P03 | 152 | 3 tasks | 3 files |
| Phase 18 P01 | 13 | 3 tasks | 4 files |
| Phase 18-multi-format-renderer-fixed-template-grounded-narrative-asse P02 | 13 | 3 tasks | 4 files |
| Phase 19-one-call-es-report-orchestrator-run-event-study-report-cran P01 | 138 | 2 tasks | 2 files |
| Phase 19 P02 | 4min | 2 tasks | 2 files |
| Phase 19-one-call-es-report-orchestrator-run-event-study-report-cran P03 | 45min | 2 tasks | 13 files |
| Phase 19.1-close-gap-ground-01-02-03-wire-prose-grounding-guard-into-ai P01 | 173 | 2 tasks | 2 files |

## Accumulated Context

### Decisions

Decisions are logged in PROJECT.md Key Decisions table.
Recent decisions affecting current work:

- One-call AI report as a new additive wrapper (`es_report()`) over `generate_report()`, not a signature change — keeps the existing renderer's NULL-advice byte-identical path intact.
- Report always renders complete offline; LLM narrative is enrichment, rule-based advice engine is the fallback (extends the v0.60.0 offline-first advisor).
- Grounding invariant carries into the report: narrative cites only computed diagnostics, enforced by the runtime guard extended to prose + regression tests (the report is the highest-visibility surface for an ungrounded number).
- Word output uses plain `rmarkdown::word_document()` (NOT officedown) to keep the package CRAN-clean with zero new transitive deps — officedown / RPTX-03 deferred.
- Multi-format (HTML/PDF/Word/Markdown) via rmarkdown output formats; PDF/Word toolchains stay optional (user-environment, not hard deps); `tinytex` added to Suggests only.
- Sensible fixed template with arg-toggled sections, no custom templating (RPTC-01 deferred).
- [Phase 17]: report_writing moved from LLM_ONLY_TYPES to KB_TYPES (OFFLINE-01); offline OfflineNarrative S3 returned without provider
- [Phase 17]: narrative=NULL seam added to generate_report() after advice=; NULL path byte-identical (REPORT-03)
- [Phase 17]: Regex alternation order longest-first prevents 4-digit year split in prose scanner
- [Phase 18]: section_hint=NULL is additive on es_advise(); NULL path byte-identical to pre-Phase-18 report_writing (backward compat lock)
- [Phase 18]: data_methods always sourced offline; exec_summary/results/robustness are LLM-narrated with per-section fallback on error
- [Phase 18]: narrative validation allows section_sources/report_mode metadata fields as non-prose elements
- [Phase 18]: appendix-section chunk name preserved in skeleton.Rmd for backward compat
- [Phase 19]: es_report() is a thin visible-return wrapper over generate_report(); deep-clones task at entry for REPORT-04 non-mutation
- [Phase 19]: Functional equality test instead of serialize() for R6 objects — R6 environments have different memory addresses between separate construction calls, making byte-level serialize() equality impossible for deep-cloned objects even when functionally identical.
- [Phase 19]: isTRUE(report) guard wraps all report machinery in run_event_study() so FALSE/omitted path is provably unreachable — additive param pattern with zero behavior change on omission.
- [Phase 19]: Field-level R6 mutation assertions instead of binary serialize() for REPORT-04 test -- R6 clone() mutates parent env serialization even with no user-visible field changes
- [Phase 19]: em-dash U+2014 replaced with -- in roxygen comments and \u2014 escape in string literals to clear CRAN non-ASCII WARNING in advise_offline.R
- [Phase 19]: Change \link{assemble_report_narrative} to plain \code{} in generate_report roxygen since that function is unexported/internal
- [Phase 19]: Wire .scan_prose_grounding() into assemble_report_narrative() at section level; fabricated numeric literals in LLM prose dropped to offline fallback with warning (GROUND-01/02/03)

### Pending Todos

- **Phase 17 (spike):** Validate numeric-literal regex + tolerance for the prose grounding scanner against real LLM output samples (rounding vs off-by-one vs fabricated) before locking the guard.
- **Phase 18 (spike):** Prose sanitisation fixtures (em-dash, smart quotes, Unicode, XML entities) per output format; verify PDF/Word render without corruption.
- **Phase 18 (spike):** Multi-format sequential render (HTML+PDF+Word) figure-directory-deletion edge case — confirm figures survive.

### Blockers/Concerns

- **GROUND-01 is the make-or-break invariant:** the existing guard only validates structured `evidence[]` arrays; the multi-section narrative adds four free-text prose fields, each a hallucination surface. Must be solid in Phase 17 before any template work.
- **OFFLINE-01 blocker:** `es_advise()` currently `stop()`s for `task_type="report_writing"` when `provider=NULL`; must be resolved in Phase 17 so a full report renders with no API key.

### Quick Tasks Completed

| # | Description | Date | Commit | Directory |
|---|-------------|------|--------|-----------|

## Deferred Items

| Category | Item | Status | Deferred At | Milestone |
|----------|------|--------|-------------|-----------|
| verification_gaps | 12/12-VERIFICATION.md (archived v0.62.0) | human_needed | 2026-09-07 | v0.64.0 |
| todos | phase12-code-review-carryforward.md | acknowledged (presence-only) | 2026-09-07 | v0.64.0 |
| Independence | INDEP-01..03: native reimplementation of did/DIDmultiplegt/rugarch | Deferred | v0.50.0 init | v2 |
| Scale | SCALE-01..03: streaming/data.table/sparse FE | Deferred | v0.50.0 init | v2 |
| Advisor Pro | PRO-01..02: RAG corpus advisor + managed hosting | Deferred | v0.60.0 roadmap | future (waitlist-gated) |
| Surfaces | SURF-01..02: MCP server + panel/intraday/synthetic diagnostics | Deferred | v0.60.0 roadmap | future |
| Docs | Package logo / hex sticker, custom homepage cards, versioned docs | Deferred | v0.62.0 roadmap | future |
| Reporting | RPTX-01: report support for panel/intraday/synthetic-control tasks | Deferred | v0.64.0 roadmap | future (needs SURF-01/02) |
| Reporting | RPTX-02: bootstrap-CI reporting | Deferred | v0.64.0 roadmap | future |
| Reporting | RPTX-03: rich Word output via officedown | Deferred | v0.64.0 roadmap | future (if demand) |
| Reporting | RPTC-01: user-supplied custom report templates | Deferred | v0.64.0 roadmap | future |

## Session Continuity

Last session: 2026-09-07T19:36:57.413Z
Stopped at: Phase 19 complete, ready to plan Phase 19.1
Resume file: None

## Operator Next Steps

- Start the next milestone with /gsd-new-milestone
