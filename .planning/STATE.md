---
gsd_state_version: 1.0
milestone: v0.65.0
milestone_name: Polish
status: planning
last_updated: "2026-09-08T20:00:00.000Z"
last_activity: 2026-09-08
progress:
  total_phases: 5
  completed_phases: 0
  total_plans: 0
  completed_plans: 0
  percent: 0
---

# Project State

## Project Reference

See: .planning/PROJECT.md (updated 2026-09-08)

**Core value:** Trustworthy numbers, trustworthy interpretation — the pipeline is never silently wrong, and the AI report cites only package-computed diagnostics. This milestone lifts the *felt* quality (brand, output, API, docs) to match that substance, without touching statistical correctness.
**Current focus:** Phase 20 — Brand & Visual Identity (roadmap created, ready to plan)

## Current Position

Phase: 20 of 24 (Brand & Visual Identity) — first phase of v0.65.0
Plan: — (not yet planned)
Status: Ready to plan
Last activity: 2026-09-08 — Roadmap created; 30/30 v0.65.0 requirements mapped across 5 phases

Progress: [░░░░░░░░░░] 0%

## Milestone Roadmap (v0.65.0 — Phases 20–24)

- **Phase 20: Brand & Visual Identity** — BRAND-01..07, CRAN-02/03/04. Logo + hex sticker (`man/figures/logo.png` <50 KB, SVG sources in `.Rbuildignore`'d `data-raw/brand/`), README badge + navbar wiring, favicon set + Open Graph card under `pkgdown/`, eventstudy.de-aligned `template.bslib` palette/typography + home card gallery/numeric badges, lifecycle `experimental`→`stable`. Owns the version bump to 0.65.0 (first commit), tarball < 1 MB assertion, and non-ASCII / no-new-check-findings guardrails.
- **Phase 21: Shared Theme & Plot Aesthetics** — VIZ-01/02/03, CRAN-01. New `R/theme.R` exporting `theme_eventstudy()` + Okabe-Ito `es_colours`; applied across ggplot2 helpers (`.plot_single_event`, `.plot_multi_event`, `plot_diagnostics`) removing hardcoded colours; plotly visuals restyled to `es_colours`; `plot_stocks()` left structurally intact. Lands the only Suggests additions (`tinytable`, `patchwork`, `ragg`, all `requireNamespace()`-guarded, no new Imports).
- **Phase 22: Report Aesthetics** — VIZ-04/05/06/07, CRAN-05. `es_report()` tables via `tinytable` (kable fallback) styled across all four formats; figure captions on every plot chunk; HTML-only `inst/rmarkdown/report.css`; per-format figure sizing (replacing global `fig.width = 10`) with `ragg`. Locks four-format render, PDF no `<script>`, and the `knitr::is_html_output()` switch by regression test. Prose sanitiser + grounding guard untouched.
- **Phase 23: API & Message Polish** — API-01..06, CRAN-06. Print methods return `invisible(x)`; `format.*` added where missing; selected `stop()`/`warning()` → classed `rlang::abort()`/`rlang::warn()`; messages name offending argument + value; `verbose=` quiet mode (byte-identical default); deprecation audit (shim-or-verified-no-op, no `lifecycle` dep). Valid-input behavior unchanged; snapshot tests cover print methods + prose sanitiser; exactly-one-warning discipline preserved.
- **Phase 24: Docs & Site Polish** — DOCS-01..04. `@family`/`@seealso` cross-links; README ecosystem section + home markers; `pkgdown::check_pkgdown()` in CI; tightened vignettes/articles (getting-started flow + cross-links), rich content stays pkgdown-only.

Dependency order: Phase 20 (brand/asset foundation + version/guardrails, blocking for site work) → Phase 21 (shared theme foundation + Suggests) → Phase 22 (report aesthetics, needs Phase 21 palette). Phase 23 (API/message) is independent — may overlap. Phase 24 (docs/site) needs Phase 20's logo — may overlap Phase 23.

Coverage: 30/30 v0.65.0 requirements mapped, 0 unmapped. CRAN-01..06 distributed to their natural verification phases (CRAN-02/03/04→20, CRAN-01→21, CRAN-05→22, CRAN-06→23).

## Performance Metrics

**Velocity:**

- Total plans completed (all milestones): 16
- Average duration: -
- Total execution time: 0 hours

**Recent Trend:**

- Last 5 plans: n/a (new milestone)
- Trend: n/a

*Updated after each plan completion*

## Accumulated Context

### Decisions

Decisions are logged in PROJECT.md Key Decisions table.
Recent decisions affecting current work:

- All four v0.65.0 polish surfaces are purely additive overlays — zero restructuring of the pipeline, advisor, or reporting backend; behavior on valid inputs must not change.
- Dependency-light stack: add only `tinytable`, `patchwork`, `ragg` to Suggests (all `requireNamespace()`-guarded); explicitly NOT adding `gt`/`kableExtra`/`flextable`/`cli`/`lifecycle`.
- Brand/site assets split by CRAN boundary: logo/hex PNG in `man/figures/` (tarball-safe, README-visible); SVG sources, favicon, OG card in `.Rbuildignore`'d `data-raw/brand/` and `pkgdown/`.
- API polish uses classed `rlang` conditions (already imported), not `cli`; `verbose=` default is byte-identical; `lifecycle` added only if a real rename appears (else API-06 is a verified no-op).

### v0.64.0 invariants that MUST NOT regress (carried into every phase)

- `.validate_grounding()` (advise.R) — drop-and-keep, single warning, never-stop; extended to report prose in Phase 19.1.
- `.handle_degenerate()` (contract.R) — exactly-one-warning per degenerate event, NA propagation, strict/lenient routing.
- `knitr::is_html_output()` switch in `skeleton.Rmd` — static/interactive selection must not move.
- `JOINT_HYPOTHESIS_CAVEAT` wording (report_narrative.R) — fixed correctness constant.
- `.sanitise_prose()` ordering (ampersand-first) — hardened; CSS/table additions must not touch it.

### Pending Todos

Research flags for planning:
- **Phase 20:** Design input — confirm eventstudy.de colour codes (bslib primary/bg/fg) with brand owner before wiring `template.bslib`.
- **Phase 22:** Multi-format rendering edge cases — CI integration tests for HTML/PDF/Word/MD render coverage.

### Blockers/Concerns

- **Design dependency (Phase 20):** eventstudy.de brand palette hex values not yet confirmed; needed for `template.bslib` + `extra.scss`. Logo/hex sticker also need visual design input (separate stream from technical build).

## Deferred Items

| Category | Item | Status | Deferred At | Milestone |
|----------|------|--------|-------------|-----------|
| verification_gaps | 12/12-VERIFICATION.md (archived v0.62.0) | human_needed | 2026-09-07 | v0.64.0 |
| todos | phase12-code-review-carryforward.md | acknowledged (presence-only) | 2026-09-07 | v0.64.0 |
| Independence | INDEP-01..03: native reimplementation of did/DIDmultiplegt/rugarch | Deferred | v0.50.0 init | v2 |
| Scale | SCALE-01..03: streaming/data.table/sparse FE | Deferred | v0.50.0 init | v2 |
| Advisor Pro | PRO-01..02: RAG corpus advisor + managed hosting | Deferred | v0.60.0 roadmap | future (waitlist-gated) |
| Surfaces | SURF-01..02: MCP server + panel/intraday/synthetic diagnostics | Deferred | v0.60.0 roadmap | future |
| Reporting | RPTX-01: report support for panel/intraday/synthetic-control tasks | Deferred | v0.64.0 roadmap | future (needs SURF-01/02) |
| Reporting | RPTX-02: bootstrap-CI reporting | Deferred | v0.64.0 roadmap | future |
| Reporting | RPTX-03: rich Word output via officedown | Deferred | v0.64.0 roadmap | future (if demand) |
| Reporting | RPTC-01: user-supplied custom report templates | Deferred | v0.64.0 roadmap | future |

## Session Continuity

Last session: 2026-09-08 20:00
Stopped at: v0.65.0 roadmap created (Phases 20–24), STATE + REQUIREMENTS traceability written
Resume file: None

## Operator Next Steps

- Review the roadmap, then plan the first phase with `/gsd-plan-phase 20`.
