# Phase 24: Docs & Site Polish - Context

**Gathered:** 2026-09-09
**Status:** Ready for planning

<domain>
## Phase Boundary

Phase 24 (the final phase) makes the documentation and pkgdown site tight and
self-navigating — WITHOUT changing any package behavior. It delivers four
docs-only outcomes: (1) `@family` + `@seealso` roxygen cross-links across the
pipeline, model, statistic, task, plotting, export, and advisor functions so the
rendered Reference index cross-navigates; (2) a CRAN-safe README Ecosystem
section placing EventStudy inside the eventstudy.de three-tool ecosystem, plus
pkgdown home markers; (3) `pkgdown::check_pkgdown()` wired into CI so broken
cross-references fail the build; (4) tightened vignettes/articles with a clear
getting-started flow and cross-links (no new CRAN vignettes — rich content stays
in `vignettes/articles/`).

Hard boundary: docs-only. No behavior change, no new Imports, no new
`R CMD check` NOTEs/WARNINGs. `@family`/`@seealso` are added in roxygen and
`man/` is regenerated via `devtools::document()` (never hand-edit Rd). All
cross-links must resolve (unresolved links break Rd checks). README stays plain
Markdown, ASCII-safe (respects Phase 20's non-ASCII CI guard); rich content
(SVGs, cards) stays pkgdown-only.

</domain>

<decisions>
## Implementation Decisions

### @family Grouping Scheme
- Define families mirroring the R6 class taxonomy / documented layers, using
  `eventstudy-`-prefixed tag names so they group cleanly in the Reference index:
  `eventstudy-pipeline`, `eventstudy-models`, `eventstudy-statistics`,
  `eventstudy-tasks`, `eventstudy-plots`, `eventstudy-export`, `eventstudy-advisor`.
- Membership (exported user-facing functions):
  - **eventstudy-pipeline** — `run_event_study`, `prepare_event_study`,
    `fit_model`, `calculate_statistics` (and the config entry points
    `EventStudyTask`, `ParameterSet` where they read as pipeline setup).
  - **eventstudy-models** — the return-model constructors/wrappers
    (Market, Market-Adjusted, Mean-Adjusted, FF3/FF5, Carhart4, GARCH,
    Rolling-Window, DCC-GARCH, BHAR, Volume, Volatility, Comparison-Period-Mean,
    Custom) as exposed to users.
  - **eventstudy-statistics** — the test-statistic entry points (AR/CAR t,
    Cross-Sectional t, Patell Z, BMP, Sign/Generalized-Sign, Rank,
    Kolari-Pynnönen, Calendar-Time Portfolio) plus `bootstrap_test`,
    `adjust_p_values`, `cross_sectional_regression`.
  - **eventstudy-tasks** — task constructors: `PanelEventStudyTask`,
    `IntradayEventStudyTask`, `SyntheticControlTask`.
  - **eventstudy-plots** — `plot_stocks`, `plot_event_study`, `plot_diagnostic`
    (and any exported plot helpers).
  - **eventstudy-export** — `export_results`, `tidy`/broom glue, `generate_report`,
    `es_report`.
  - **eventstudy-advisor** — `es_diagnostics`, `advise`, and the advisor surfaces
    already carrying `@seealso` (advise.R, advise_offline.R, knowledge_base.R).
- Final membership is verified against the live NAMESPACE at plan/execute time;
  every function gets at most one `@family`, and each family must have >= 2
  members (roxygen suppresses single-member family lists).

### @seealso Cross-Link Policy
- Hub-and-spoke on the pipeline entry points: `run_event_study`,
  `prepare_event_study`, `fit_model`, and `calculate_statistics` each `@seealso`
  the other three so the four-step flow is discoverable from any one of them.
- `@family` already generates a "See also" cluster within a family, so reserve
  explicit `@seealso` for CROSS-family bridges the family tag can't express:
  pipeline entry points → the models and statistics families; advisor functions
  (`es_diagnostics`, `advise`, `es_report`) → the pipeline; export functions →
  the pipeline. No dense N×N link webs.
- Every `@seealso`/`\link{}` target must resolve to an exported, documented
  object (verified by `pkgdown::check_pkgdown()` + `R CMD check` Rd checks).

### README Ecosystem Section + Stale-Count Reconciliation
- Add a concise **Ecosystem** section (CRAN-safe, plain Markdown, ASCII-only)
  naming the three tools — Google Sheets template · R package · WebAssembly app —
  and linking eventstudy.de. Wording/framing consistent with Phase 20's gallery:
  "Event Study Analysis Made Simple". No rich HTML/SVG/cards in README (those stay
  pkgdown-only). Placement: near the top (after the intro/badges, before or right
  after Installation) so the ecosystem framing lands early.
- RECONCILE stale counts: README lines ~32/34 currently say "13 Return Models" /
  "11 Test Statistics", contradicting Phase 20's verified honest badges. Update
  the README prose to **"15+ Return Models"** and **"12 Test Statistics"** so the
  README and the pkgdown gallery badges agree. Counts must stay TRUTHFUL against
  the actual package (the "+" keeps "15+" honest via configurations/variants).
- Add pkgdown home markers (e.g. `<!-- pkgdown-home-start/end -->` or the
  index-page fenced markers) so the pkgdown home page renders the intended
  content region; keep `home: sidebar: false` as already set in `_pkgdown.yml`.

### Vignettes / Articles + CI
- Do NOT add a new CRAN vignette. Tighten the existing `introduction.Rmd` into a
  clean getting-started flow (task → run → inspect → export/plot) and add
  cross-links between it and the deeper vignettes (`inference-robustness`,
  `factor-models-bhar`, `ai-advisor`, `result-extraction`, etc.); rich worked
  examples stay in `vignettes/articles/`.
- Ensure every vignette/article cross-links to the pkgdown Reference and to the
  next logical read, so the site is self-navigating; verify navbar/news wiring.
- Add `pkgdown::check_pkgdown()` to the existing CI workflow (the pkgdown/docs
  job) so broken cross-references and missing topics fail the build. It must pass
  green with no silent broken references.

### Claude's Discretion
- YOLO/autonomous mode: every grey area auto-accepted at the recommended answer;
  no user questions were asked.
- Exact `@family` tag spelling, per-function membership edge cases, precise README
  Ecosystem wording/placement, the exact home-marker syntax, and the specific CI
  step placement are at Claude's discretion within the constraints above.

</decisions>

<code_context>
## Existing Code Insights

### Reusable Assets
- `@family`/`@seealso` already exist in: `R/advise.R`, `R/advise_offline.R`,
  `R/knowledge_base.R`, `R/contract.R`, `R/es_diagnostics.R`, `R/theme.R`,
  `R/report.R` — establishes the tag style to mirror across the remaining
  model/statistic/task/plot/export/pipeline files.
- `_pkgdown.yml` already sets `home: sidebar: false` — home markers extend, not
  replace, this.
- Vignettes present: 19 CRAN vignettes in `vignettes/` (incl. `introduction.Rmd`,
  `gallery.Rmd`, `ai-advisor.Rmd`) + rich `vignettes/articles/` (method deep-dives
  `methods-*.Rmd`, worked `example-*.Rmd`, `_article-skeleton.Rmd`).

### Established Patterns
- Docs regenerated by roxygen2 (RoxygenNote 7.3.3) → `devtools::document()`;
  NAMESPACE + `man/*.Rd` are generated, never hand-edited.
- Phase 20 locked the honest ecosystem badge counts: "15+ Return Models ·
  12 Test Statistics · (DiD estimators)"; README must match these.
- CI already runs docs/pkgdown; add `check_pkgdown()` into that existing job.
- Non-ASCII CI guard (Phase 20) — README must stay ASCII-safe plain Markdown.

### Integration Points
- README.md — add Ecosystem section + reconcile counts + home markers.
- `_pkgdown.yml` — home markers / reference index families surface here.
- CI workflow (`.github/workflows/`) — add `pkgdown::check_pkgdown()` step.
- `R/*.R` roxygen blocks — add `@family`/`@seealso` then `devtools::document()`.

</code_context>

<specifics>
## Specific Ideas

- README currently has NO ecosystem/eventstudy.de content (grep count 0) — this
  section is net-new, not a rewrite.
- Stale prose to fix precisely: README ~line 32 "13 Return Models" → "15+ Return
  Models"; ~line 34 "11 Test Statistics" → "12 Test Statistics".
- eventstudy.de framing anchor: "Event Study Analysis Made Simple"; three tools:
  Google Sheets template · R package · WebAssembly app.
- @family tag prefix: `eventstudy-` so families sort together in the Reference
  index (e.g. `eventstudy-pipeline`, `eventstudy-models`, ...).

</specifics>

<deferred>
## Deferred Ideas

- Any behavior/API changes, new models/statistics, or new Imports — out of scope
  (docs-only phase; would violate the compatibility + CRAN constraints).
- New CRAN vignettes — explicitly excluded; rich content stays in
  `vignettes/articles/`.
- Live pkgdown CI deploy / GitHub Pages enablement — a standing operator task
  carried from prior milestones, not part of this docs-polish phase.

</deferred>
