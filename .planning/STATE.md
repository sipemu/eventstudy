---
gsd_state_version: 1.0
milestone: v0.63.0
milestone_name: Documentation Depth — Methods & Worked Examples
current_phase: 14
current_phase_name: Curated Per-Domain Datasets
status: planning
stopped_at: Phase 13 complete, ready to plan Phase 14
last_updated: "2026-09-05T21:04:38.669Z"
last_activity: 2026-09-05
last_activity_desc: Phase 13 complete, transitioned to Phase 14
state_head: 921448401fbdbe69cd7e49d20b8a49e86015d455
progress:
  total_phases: 6
  completed_phases: 1
  total_plans: 1
  completed_plans: 1
  percent: 17
---

# Project State

## Project Reference

See: .planning/PROJECT.md (updated 2026-09-04)

**Core value:** Trustworthy numbers, trustworthy interpretation — the pipeline is never silently wrong, and the AI advisor cites only package-computed diagnostics, never fabricating a result. This milestone makes that legible via a curated docs site.
**Current focus:** Phase 13 — Article Infrastructure & Conventions Gate

## Current Position

Phase: 14 — Curated Per-Domain Datasets
Plan: Not started
Status: Ready to plan
Last activity: 2026-09-05 — Phase 13 complete, transitioned to Phase 14

## Milestone Roadmap (v0.63.0)

- **Phase 13: Article Infrastructure & Conventions Gate** — METH-01, RENDER-03, DELIVERY-01..03. `vignettes/articles/` (`.Rbuildignore`d same commit), `_setup.Rmd` (seed + options), co-located `references.bib`, `math-rendering: katex` (plotly/MathJax fix), reusable 10-section skeleton, Learn/Methods + Gallery nav slots, 18 CRAN vignettes untouched, CI dry-run smoke test.
- **Phase 14: Curated Per-Domain Datasets** — DATA-01, DATA-02. Curated real datasets (dieselgate pattern) with reproducible `data-raw/` provenance + `DATA-SOURCES.md`, `simulate_event_study()` preferred, sized/placed to keep the CRAN tarball clean, placement recorded per dataset.
- **Phase 15: Methods Articles + Rendered Outputs** — METH-02..08, RENDER-01, RENDER-02. All 8 method-family conceptual articles (formulas, assumptions, when-to-use, references) each with build-time rendered table + plot, fully offline/`set.seed`, formula-reviewed vs primary literature + package source.
- **Phase 16: Worked-Examples Gallery + Build & Release Integrity** — GALLERY-01..03, BUILD-04..06. pyfda-style gallery card index + ≥3 cross-domain worked examples cross-linked to Methods/reference, green local build + CI deploy + `R CMD check --as-cran` (no new NOTEs/WARNINGs, unbloated tarball, suite green).

Dependency order: Phase 13 (infra gate) → Phase 14 (datasets) → Phase 15 (Methods articles, needs infra + data) → Phase 16 (gallery + final build/release gate, needs articles + data).

Coverage: 22/22 v0.63.0 requirements mapped, 0 unmapped.

## Performance Metrics

**Velocity:**

- Total plans completed (all milestones): 11
- Average duration: -
- Total execution time: 0 hours

**Recent Trend:**

- Last 5 plans: n/a
- Trend: n/a

*Updated after each plan completion*
**Per-Plan Metrics:**

| Plan | Duration | Tasks | Files |
|------|----------|-------|-------|
| Phase 11-curated-pkgdown-site-custom-theme-local-build P01 | 3777 | 3 tasks | 2 files |
| Phase 13 P01 | 45 | 3 tasks | 6 files |

## Accumulated Context

### Decisions

Decisions are logged in PROJECT.md Key Decisions table.
Recent decisions affecting current work:

- v0.62.0 is a small docs + infra milestone: two coarse phases. Phase 11 builds and verifies the curated pkgdown site locally (config + theme must exist and build before CI is meaningful); Phase 12 wires the r-lib CI deploy, repo linkage, network-safe article build, and the CRAN-clean 0.62.0 release.
- Site URL is `https://sipemu.github.io/eventstudy/`; GitHub remote is `https://github.com/sipemu/eventstudy`.
- Curated pkgdown site (grouped reference, custom homepage) over auto-generated default, matching the fdars-r reference.
- Custom Bootstrap-5 theme, no logo — professional look without artwork; logo deferred.
- CI deploy to gh-pages on push-to-main + releases (r-lib `pkgdown.yaml`); keeps the site out of the CRAN tarball via `.Rbuildignore`.
- The GitHub repo "About → Website" field is a manual operator step (CI cannot set it) — flagged in Phase 12 notes, not an automated requirement.
- [Phase 11]: pkgdown 2.2.0: all .Rd files (even non-exported) must be in reference: groups or an internal section to avoid missing-topic errors
- [Phase 11]: has_keyword('datasets') resolves dieselgate bundled dataset inclusion without export() requirement
- [Phase 11]: DESCRIPTION URL field for pkgdown site URL deferred to Phase 12 LINK-01 (scope guard)
- [Phase 13]: template.math-rendering: katex must go under template: in _pkgdown.yml (pkgdown 2.2.0 config_math_rendering reads from template.math-rendering)
- [Phase 13]: plot_event_study() returns ggplot2 in current codebase; use plotly::ggplotly() to deliver interactive plotly figures in articles

### Pending Todos

- **Phase 11:** Enumerate all 30+ exported symbols and assign each to exactly one Reference group so `build_site()` emits zero missing-topic warnings; group the 18 vignettes under Articles.
- **Phase 12:** Verify a real Actions run is green on the default branch before considering CI-03 met; confirm `.Rbuildignore` keeps the CRAN tarball byte-unchanged; confirm no new R CMD check NOTEs/WARNINGs vs v0.61.x baseline.
- **Operator step (Phase 12):** After first successful deploy, set the GitHub repo "About → Website" field to the live site URL (manual GitHub UI action).

### Blockers/Concerns

- **BUILD-02:** `data-download` (and any other network-touching) vignettes must build reproducibly in Actions — verify offline-safe/gated handling before relying on the CI docs build.

### Quick Tasks Completed

| # | Description | Date | Commit | Directory |
|---|-------------|------|--------|-----------|
| 260904-er6 | Surface v0.60.0 AI advisor in README (Features bullet + Quick Start snippet + Roadmap item) | 2026-09-04 | 6e61c3b | [260904-er6-update-readme-md-to-prominently-feature-](./quick/260904-er6-update-readme-md-to-prominently-feature-/) |
| 260904-id9 | Fix CRAN non-ASCII WARNING — escape non-ASCII in string literals of advise.R/knowledge_base.R/report.R | 2026-09-04 | 462940f | [260904-id9-fix-cran-non-ascii-warning-escape-non-as](./quick/260904-id9-fix-cran-non-ascii-warning-escape-non-as/) |
| 260904-kxy | Fix latent dplyr::lag import bug (returns silently all-zero/NA without dplyr attached) + regression test + advisor-vignette AR/CAR plots; released 0.61.1 | 2026-09-04 | 6c47339 | [260904-kxy-fix-dplyr-lag-import-bug-causing-all-na-](./quick/260904-kxy-fix-dplyr-lag-import-bug-causing-all-na-/) |
| 260904-len | Extend bundled dieselgate to 4 automakers / 2 groups (README flagship example); multi-group advisor vignette with CI bands + group CAAR comparison (VW Group CAAR −38.6% vs Other +1.3% n.s.) + mock AI-advisor block; released 0.61.2 | 2026-09-04 | fa87166 | [260904-len-multi-automaker-vignette-ci-groups-advisor](./quick/260904-len-multi-automaker-vignette-ci-groups-advisor/) |
| 260904-x88 | redesign the pkgdown site to match fdars-r | 2026-09-04 | e85ffbc | [260904-x88-redesign-the-pkgdown-site-to-match-fdars](./quick/260904-x88-redesign-the-pkgdown-site-to-match-fdars/) |

## Deferred Items

| Category | Item | Status | Deferred At | Milestone |
|----------|------|--------|-------------|-----------|
| Independence | INDEP-01..03: native reimplementation of did/DIDmultiplegt/rugarch | Deferred | v0.50.0 init | v2 |
| Scale | SCALE-01..03: streaming/data.table/sparse FE | Deferred | v0.50.0 init | v2 |
| Advisor Pro | PRO-01..02: RAG corpus advisor + managed hosting | Deferred | v0.60.0 roadmap | future (waitlist-gated) |
| Surfaces | SURF-01..02: MCP server + panel/intraday/synthetic diagnostics | Deferred | v0.60.0 roadmap | future |
| Docs | Package logo / hex sticker, custom homepage cards, versioned docs | Deferred | v0.62.0 roadmap | future |

## Session Continuity

Last session: 2026-09-05T20:55:44.058Z
Stopped at: Phase 13 complete, ready to plan Phase 14
Resume file: None

## Operator Next Steps

- Review the roadmap, then plan Phase 11 with /gsd-plan-phase 11
