---
gsd_state_version: 1.0
milestone: v0.65.0
milestone_name: Polish
current_phase: 22
current_phase_name: report-aesthetics
status: executing
stopped_at: Completed 22-01-PLAN.md
last_updated: "2026-09-08T22:47:39.044Z"
last_activity: 2026-09-09
last_activity_desc: Roadmap created; 30/30 v0.65.0 requirements mapped across 5 phases
state_head: 8f43c6898a4f77f48313b8ab1e938118a386999b
progress:
  total_phases: 5
  completed_phases: 3
  total_plans: 6
  completed_plans: 6
  percent: 60
---

# Project State

## Project Reference

See: .planning/PROJECT.md (updated 2026-09-08)

**Core value:** Trustworthy numbers, trustworthy interpretation — the pipeline is never silently wrong, and the AI report cites only package-computed diagnostics. This milestone lifts the *felt* quality (brand, output, API, docs) to match that substance, without touching statistical correctness.
**Current focus:** Phase 20 — Brand & Visual Identity (roadmap created, ready to plan)

## Current Position

Phase: 22 (report-aesthetics) — COMPLETE
Plan: 01 complete (1/1 plans in Phase 22)
Status: Phase 22 complete; ready for verification / next phase
Last activity: 2026-09-09 — Plan 22-01 complete: .report_table() tinytable/kable helper, per-format figure sizing + ragg + fig.cap, HTML-only report.css injected on the html branch only, regression net (no-script PDF, is_html_output>=3, four-format render). Suite 2311 green; R CMD check 1 baseline NOTE.

Progress: [██████░░░░] 60% (v0.65.0 phases)

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
**Per-Plan Metrics:**

| Plan | Duration | Tasks | Files |
|------|----------|-------|-------|
| Phase 20 P01 | 4m | 3 tasks | 7 files |
| Phase 20 P02 | 4 min | 3 tasks | 15 files |
| Phase 20 P03 | 3m | 3 tasks | 6 files |
| Phase 21 P01 | 8m | 4 tasks | 6 files |
| Phase 22 P01 | 18min | 4 tasks | 4 files |

## Accumulated Context

### Decisions

Decisions are logged in PROJECT.md Key Decisions table.
Recent decisions affecting current work:

- All four v0.65.0 polish surfaces are purely additive overlays — zero restructuring of the pipeline, advisor, or reporting backend; behavior on valid inputs must not change.
- Dependency-light stack: add only `tinytable`, `patchwork`, `ragg` to Suggests (all `requireNamespace()`-guarded); explicitly NOT adding `gt`/`kableExtra`/`flextable`/`cli`/`lifecycle`.
- Brand/site assets split by CRAN boundary: logo/hex PNG in `man/figures/` (tarball-safe, README-visible); SVG sources, favicon, OG card in `.Rbuildignore`'d `data-raw/brand/` and `pkgdown/`.
- API polish uses classed `rlang` conditions (already imported), not `cli`; `verbose=` default is byte-identical; `lifecycle` added only if a real rename appears (else API-06 is a verified no-op).
- [Phase 20]: Phase 20 brand assets rendered via librsvg rsvg-convert CLI (dev-time), not the rsvg/hexSticker R packages, keeping DESCRIPTION dependency-free per CRAN-02
- [Phase 20]: Phase 20 section-heading recolour uses clean-edit (removed 8 per-class rules), not !important
- [Phase 20]: CI non-ASCII guard is baseline-aware (fails only on NEW non-ASCII), preserving the package's declared UTF-8
- [Phase 21]: es_colours reference role uses brand slate #6b7280 rather than Okabe-Ito black for zero-lines
- [Phase 21]: plot_stocks uses fixed es_colours group1 for trace colour (coarse granularity acceptable)
- [Phase 22]: Report tables route through .report_table() with a byte-compatible knitr::kable fallback when tinytable is absent (.tinytable_available seam)
- [Phase 22]: report.css is injected only on the html branch of .build_output_format via system.file+nzchar; pdf/word/md never reference it

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
- **Phase 24 (from Phase 20 verify):** README prose is stale — lines ~32/34 still say "13 Return Models" / "11 Test Statistics", contradicting the new gallery badges (15+ Return Models / 12 Test Statistics). Reconcile README prose with the honest counts during docs polish.
- **Pre-existing tech debt (from Phase 21 review, out of scope):** `gridExtra::grid.arrange` used unguarded in R/plotting.R (~L351) and a stale `@return` "patchwork-style" doc. Not introduced by v0.65.0. Candidate for a future hardening ticket or Phase 23 guard pass — verify gridExtra is guarded/declared before CRAN submission.

### Blockers/Concerns

- None. (Resolved: the Phase 20 eventstudy.de palette dependency was closed during Phase 20 execution — palette #2563eb/#ffffff/#0f172a extracted from live CSS, logo/hex sticker built and verified. Phase 20 shipped and verification passed.)

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

Last session: 2026-09-08T22:47:09.659Z
Stopped at: Completed 22-01-PLAN.md
Resume file: None

## Operator Next Steps

- Review the roadmap, then plan the first phase with `/gsd-plan-phase 20`.
