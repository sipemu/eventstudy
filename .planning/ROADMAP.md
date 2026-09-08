# Roadmap: EventStudy — Polish

## Milestones

- ✅ **v0.50.0 Robustness Hardening** — degenerate-input contract + regression net (shipped 2026-09-02)
- ✅ **v0.60.0 Grounded AI Advisor** — offline diagnostics + grounded `es_advise()` + provider abstraction (shipped 2026-09-04)
- ✅ **v0.61.0 Advisor Vignette** — advisor vignette + bundled dieselgate dataset (shipped 2026-09-04)
- ✅ **v0.62.0 Docs Site** — curated pkgdown site + CI/CD deploy (shipped 2026-09-06)
- ✅ **v0.63.0 Docs Depth** — Methods articles + worked-examples gallery + per-domain datasets (shipped 2026-09-06)
- ✅ **v0.64.0 Automated AI Reporting** — Phases 17–19.1 (shipped 2026-09-07)
- 🚧 **v0.65.0 Polish** — Phases 20–24 (in progress)

## Phases

<details>
<summary>✅ v0.50.0 – v0.63.0 — SHIPPED</summary>

Earlier milestones are archived under `.planning/milestones/` (per-milestone `-ROADMAP.md`, `-REQUIREMENTS.md`, and `-phases/` directories). See `.planning/MILESTONES.md` for the shipped-accomplishments summary of each.

</details>

<details>
<summary>✅ v0.64.0 Automated AI Reporting (Phases 17–19.1) — SHIPPED 2026-09-07</summary>

- [x] Phase 17: Grounding-Prose Hardening, Offline Report Fallback & CRAN Baseline (3/3 plans) — GROUND-01..03, OFFLINE-01, REPORT-03
- [x] Phase 18: Multi-Format Renderer, Fixed Template & Grounded Narrative Assembly (2/2 plans) — NARR-01..05, FORMAT-01..04, TMPL-01/02, OFFLINE-02
- [x] Phase 19: es_report() Orchestrator, run_event_study(report=) & CRAN-Clean Release Gate (3/3 plans) — REPORT-01/02/04, CRAN-01/02
- [x] Phase 19.1: Close gap GROUND-01/02/03 — wire prose grounding guard into report path (1/1 plan, INSERTED) — gap closure from milestone audit

Full detail: `.planning/milestones/v0.64.0-ROADMAP.md` · requirements: `.planning/milestones/v0.64.0-REQUIREMENTS.md` · audit: `.planning/milestones/v0.64.0-MILESTONE-AUDIT.md`

</details>

### 🚧 v0.65.0 Polish (In Progress)

**Milestone Goal:** Visibly lift EventStudy's quality across brand, output, API feel, and docs — a ship-when-good polish pass that makes the package look and feel finished and aligned to the eventstudy.de ecosystem. All changes are additive: behavior on valid inputs is unchanged, existing ~2287 tests stay green, no new `R CMD check --as-cran` findings, and the v0.64.0 grounding/degenerate-input/format invariants must not regress.

**Execution Order:** Phases run in numeric order: 20 → 21 → 22 → 23 → 24.
- Phase 20 (Brand) establishes the logo/asset foundation the site work (24) depends on, and owns the version bump + tarball/non-ASCII guardrails that gate the whole milestone.
- Phase 21 (Shared Theme + Plot Aesthetics) creates `theme_eventstudy()`/`es_colours` — the shared foundation the report aesthetics (22) reuse — and lands the Suggests additions.
- Phase 22 (Report Aesthetics) depends on Phase 21's palette; owns the multi-format render guardrail.
- Phase 23 (API & Message Polish) is independent of 20–22 (can overlap); owns the valid-input-unchanged + snapshot guardrail.
- Phase 24 (Docs & Site) depends on Phase 20's logo; can overlap Phase 23.

- [ ] **Phase 20: Brand & Visual Identity** - Logo + hex sticker, favicon, OG card, eventstudy.de-aligned theme + card gallery, stable badge; version bump + tarball/non-ASCII guardrails
- [ ] **Phase 21: Shared Theme & Plot Aesthetics** - `theme_eventstudy()` + Okabe-Ito `es_colours`, applied across ggplot2 + plotly helpers; Suggests additions land guarded
- [ ] **Phase 22: Report Aesthetics** - `tinytable` report tables, figure captions, HTML-only report CSS, per-format figure sizing; four-format render locked
- [ ] **Phase 23: API & Message Polish** - Consistent print/format methods, classed rlang conditions, argument-naming messages, `verbose=` quiet mode, deprecation audit; valid-input behavior unchanged
- [ ] **Phase 24: Docs & Site Polish** - `@family`/`@seealso` cross-links, README ecosystem refresh, `check_pkgdown()` CI gate, tightened vignettes/articles

## Phase Details

### Phase 20: Brand & Visual Identity
**Goal**: EventStudy has a real visual identity — a logo and hex sticker wired into the README and pkgdown site — and the site palette/typography/card gallery reads as part of the eventstudy.de three-tool ecosystem, all while staying CRAN-clean.
**Depends on**: Nothing (first phase of milestone)
**Requirements**: BRAND-01, BRAND-02, BRAND-03, BRAND-04, BRAND-05, BRAND-06, BRAND-07, CRAN-02, CRAN-03, CRAN-04
**Success Criteria** (what must be TRUE):
  1. A logo + hex sticker exist as `man/figures/logo.png` (tarball-safe, <50 KB) with SVG sources in `.Rbuildignore`'d `data-raw/brand/`, and the logo appears in the README badge and the pkgdown navbar.
  2. A full favicon set and Open Graph social-preview card are generated and committed under `pkgdown/`, so shared links render a branded thumbnail and browser tabs show the favicon — none of it entering the CRAN tarball.
  3. The pkgdown site's palette, typography, home card gallery, and numeric badges ("15 Return Models", "12 Test Statistics", "6 DiD Estimators") are restyled to the eventstudy.de look via `template.bslib` + `pkgdown/extra.scss`, and the lifecycle badge reads `stable`.
  4. The version reads 0.65.0 in DESCRIPTION with a matching NEWS.md v0.65.0 section (landed as the first commit of execution).
  5. `R CMD check --as-cran` is clean vs baseline (no new NOTEs/WARNINGs), the built tarball is < 1 MB (asserted in CI), and no non-ASCII appears in `R/`, `man/figures/`, or `inst/`.
**Plans**: TBD
**UI hint**: yes

### Phase 21: Shared Theme & Plot Aesthetics
**Goal**: A single exported, colorblind-safe visual language (`theme_eventstudy()` + `es_colours`) governs every EventStudy plot — static ggplot2 and interactive plotly alike — replacing scattered hardcoded colours, with the plot-structure test suite still green.
**Depends on**: Phase 20
**Requirements**: VIZ-01, VIZ-02, VIZ-03, CRAN-01
**Success Criteria** (what must be TRUE):
  1. `theme_eventstudy()` and an Okabe-Ito `es_colours` palette exist in `R/theme.R`, are exported and documented, and appear in the pkgdown reference index.
  2. All ggplot2 plot helpers (`.plot_single_event`, `.plot_multi_event`, `plot_diagnostics`) render with `theme_eventstudy()` + `es_colours`; hardcoded `steelblue`/`red`/`grey40` literals are gone; `plot_stocks()` (plotly) is left structurally intact.
  3. plotly interactive visuals (hover, legend, colour) are restyled to `es_colours` for cross-plot consistency.
  4. `DESCRIPTION` adds only `tinytable`, `patchwork`, `ragg` to Suggests (each `requireNamespace()`-guarded), with no new Imports; the plot suite stays green and no colour-assertion test regresses.
**Plans**: TBD
**UI hint**: yes

### Phase 22: Report Aesthetics
**Goal**: `es_report()` output is publication-grade across all four formats — styled multi-format tables, captioned figures, polished HTML typography, and per-format figure sizing that fits page margins — without touching the prose sanitiser, grounding guard, or the `knitr::is_html_output()` switch.
**Depends on**: Phase 21 (reuses `es_colours`/theme for figure colour consistency)
**Requirements**: VIZ-04, VIZ-05, VIZ-06, VIZ-07, CRAN-05
**Success Criteria** (what must be TRUE):
  1. `es_report()` tables render via `tinytable` (with a `knitr::kable()` fallback) and are styled consistently across HTML/PDF/Word/Markdown.
  2. Every plot chunk in `skeleton.Rmd` carries a figure caption (`fig.cap`), and PDF/Word figures fit page margins via per-format sizing (replacing the global `fig.width = 10`), with `ragg` used for anti-aliased raster output.
  3. HTML report typography and table styling come from an injected `inst/rmarkdown/report.css` on the HTML branch only, leaving the prose sanitiser and grounding guard untouched.
  4. All four report formats render; the PDF contains no `<script>` tags; and the `knitr::is_html_output()` static/interactive switch stays intact — locked by a regression test.
**Plans**: TBD
**UI hint**: yes

### Phase 23: API & Message Polish
**Goal**: The package's surface feel is consistent and scriptable — print/format methods behave uniformly, errors and warnings are classed and name the offending argument, a `verbose=` flag quiets informational chatter for batch use — with valid-input behavior provably unchanged and the exactly-one-warning degenerate-input discipline preserved.
**Depends on**: Phase 20 (independent of 21–22; may overlap Phase 24)
**Requirements**: API-01, API-02, API-03, API-04, API-05, API-06, CRAN-06
**Success Criteria** (what must be TRUE):
  1. Every `print.*` method returns `invisible(x)` with consistent formatting, and a `format.*` method exists wherever a class has `print()` but no `format()` — with snapshot tests established before any change.
  2. Selected `stop()`/`warning()` call sites are migrated to classed `rlang::abort()`/`rlang::warn()` (no new dependency), errors/warnings name the offending argument and its value, and the degenerate-input contract's exactly-one-warning discipline is preserved.
  3. A `verbose=` argument quiets informational messages for scripted use, with the default byte-identical to current behavior when omitted; the deprecation audit either ships a back-compatible shim + warning for any rename or is documented as a verified no-op (no `lifecycle` dep).
  4. Behavior on valid inputs is unchanged, the full test suite is green, and snapshot tests cover the print methods plus the prose sanitiser.
**Plans**: TBD

### Phase 24: Docs & Site Polish
**Goal**: The documentation and pkgdown site are tight and self-navigating — reference pages cross-link, the README places EventStudy inside the three-tool ecosystem, broken cross-references are caught in CI, and the getting-started flow reads cleanly — with rich content staying pkgdown-only.
**Depends on**: Phase 20 (logo); may overlap Phase 23
**Requirements**: DOCS-01, DOCS-02, DOCS-03, DOCS-04
**Success Criteria** (what must be TRUE):
  1. `@family` + `@seealso` roxygen tags across pipeline/model/statistic/advisor functions make the Reference index cross-link, verifiable on the rendered site.
  2. The README carries an Ecosystem section linking the three tools (Google Sheets template · R package · WebAssembly app) and eventstudy.de, with pkgdown home markers in place.
  3. `pkgdown::check_pkgdown()` runs in CI and passes (no silent broken cross-references); navbar/news wiring is verified and rough edges are fixed.
  4. Existing vignettes/articles are tightened — getting-started flow and cross-links improved — with no new CRAN vignettes (rich content stays in `vignettes/articles/`).
**Plans**: TBD
**UI hint**: yes

## Progress

**Execution Order:** Phases execute in numeric order: 20 → 21 → 22 → 23 → 24

| Phase | Milestone | Plans Complete | Status | Completed |
|-------|-----------|----------------|--------|-----------|
| 20. Brand & Visual Identity | v0.65.0 | 0/TBD | Not started | - |
| 21. Shared Theme & Plot Aesthetics | v0.65.0 | 0/TBD | Not started | - |
| 22. Report Aesthetics | v0.65.0 | 0/TBD | Not started | - |
| 23. API & Message Polish | v0.65.0 | 0/TBD | Not started | - |
| 24. Docs & Site Polish | v0.65.0 | 0/TBD | Not started | - |
