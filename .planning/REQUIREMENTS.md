# Requirements: EventStudy v0.65.0 "Polish"

**Defined:** 2026-09-08
**Core Value:** Trustworthy numbers, trustworthy interpretation — the pipeline is never silently wrong, and the AI report cites only package-computed diagnostics. This milestone lifts the *felt* quality (brand, output, API, docs) to match that substance, without touching statistical correctness.

## v1 Requirements

Requirements for the v0.65.0 milestone. Each maps to a roadmap phase. All changes are additive; behavior on valid inputs must not change; existing ~2287 tests stay green.

### Brand & Visual Identity

- [x] **BRAND-01**: A real EventStudy logo + hex sticker exists — SVG source under `data-raw/brand/` (`.Rbuildignore`'d), optimised PNG at `man/figures/logo.png` (tarball-safe, <50 KB)
- [x] **BRAND-02**: Logo is wired into the README badge (`usethis::use_logo()` pattern) and the pkgdown navbar
- [x] **BRAND-03**: Full favicon set generated via `pkgdown::build_favicons()` and committed under `pkgdown/` (out of the CRAN tarball)
- [x] **BRAND-04**: Open Graph social-preview card configured so shared links render a branded thumbnail
- [x] **BRAND-05**: pkgdown `bslib` theme aligned to the eventstudy.de brand — palette (primary/bg/fg), typography, via `_pkgdown.yml template.bslib` + `pkgdown/extra.scss`
- [x] **BRAND-06**: pkgdown home card gallery + numeric badges ("15 Return Models", "12 Test Statistics", "6 DiD Estimators") restyled to the ecosystem look
- [x] **BRAND-07**: Lifecycle badge flipped `experimental` → `stable`; README badge row refreshed

### Report & Plot Aesthetics

- [x] **VIZ-01**: New `R/theme.R` provides `theme_eventstudy()` + an `es_colours` Okabe-Ito colorblind-safe palette (exported, documented)
- [x] **VIZ-02**: `theme_eventstudy()` + `es_colours` applied across all ggplot2 plot helpers (`.plot_single_event`, `.plot_multi_event`, `plot_diagnostics`); hardcoded `steelblue`/`red`/`grey40` removed; `plot_stocks()` (plotly) left structurally intact
- [x] **VIZ-03**: plotly interactive visuals restyled to `es_colours` (hover, legend, colour consistency)
- [x] **VIZ-04**: `es_report()` tables rendered via `tinytable` with a `knitr::kable()` fallback, styled, across HTML/PDF/Word/Markdown
- [x] **VIZ-05**: Figure captions (`fig.cap`) present on every plot chunk in `skeleton.Rmd`
- [x] **VIZ-06**: `inst/rmarkdown/report.css` typography + table styling injected on the HTML branch only (does not touch the prose sanitiser or grounding guard)
- [x] **VIZ-07**: Per-format figure sizing so PDF/Word figures fit page margins (replaces the global `fig.width = 10` that overflows PDF); `ragg` device used for anti-aliased raster output

### API & Message Polish

- [x] **API-01**: All `print.*` methods audited — return `invisible(x)`, consistent formatting; snapshot tests established *before* any change
- [x] **API-02**: `format.*` methods added where a class has `print()` but no `format()`
- [ ] **API-03**: Selective `stop()`/`warning()` calls migrated to classed `rlang::abort()`/`rlang::warn()` (rlang already imported, zero new dep); the degenerate-input contract's exactly-one-warning discipline preserved
- [ ] **API-04**: Error/warning messages name the offending argument and its value
- [ ] **API-05**: A `verbose=` argument quiets informational messages for batch/scripted use; default preserves current behavior (byte-identical when omitted)
- [ ] **API-06**: Deprecation audit — if any argument is renamed, ship a back-compatible shim with a deprecation warning; if nothing is renamed, documented as a verified no-op (no `lifecycle` dep added)

### Docs & Site Polish

- [ ] **DOCS-01**: `@family` + `@seealso` roxygen tags added across pipeline/model/statistic/advisor functions so the Reference index cross-links
- [ ] **DOCS-02**: README refreshed with an Ecosystem section linking the three tools (Google Sheets template · R package · WebAssembly app) and eventstudy.de; pkgdown home markers added
- [ ] **DOCS-03**: `pkgdown::check_pkgdown()` added to CI (catches silent broken cross-references); navbar/news wiring verified; rough edges fixed
- [ ] **DOCS-04**: Existing vignettes/articles tightened — getting-started flow and cross-links improved (no new CRAN vignettes; rich content stays in `vignettes/articles/`)

### Release Hygiene & Guardrails (cross-cutting)

- [x] **CRAN-01**: DESCRIPTION adds only `tinytable`, `patchwork`, `ragg` to Suggests (`requireNamespace()`-guarded); no new Imports
- [x] **CRAN-02**: Version bumped to 0.65.0; NEWS.md v0.65.0 section written; DESCRIPTION/NEWS consistent (bump lands as the first commit of execution)
- [x] **CRAN-03**: CRAN tarball stays lean (assert < 1 MB in CI); brand sources in `.Rbuildignore`'d dirs; `man/figures/` assets optimised
- [x] **CRAN-04**: No non-ASCII in `R/`, `man/figures/`, `inst/` (CI grep guard); no new `R CMD check --as-cran` NOTEs/WARNINGs vs baseline
- [x] **CRAN-05**: All four report formats (HTML/PDF/Word/Markdown) render; PDF contains no `<script>` tags; the `knitr::is_html_output()` static/interactive switch stays intact — locked by regression test
- [ ] **CRAN-06**: Behavior on valid inputs unchanged; full test suite green; snapshot tests cover print methods + the prose sanitiser

## Future Requirements

Deferred to a later milestone. Tracked, not in this roadmap.

### Reporting Depth

- **RPTX-01**: `es_report()` support for panel / intraday / synthetic-control tasks (gated on SURF-02 diagnostics surfaces)
- **RPTX-02**: Bootstrap-CI reporting in the report
- **RPTX-03**: Rich Word output via officedown
- **RPTC-01**: User-supplied custom report templates

### Surfaces / Commercial

- **SURF-01/02**: MCP server surface; panel/intraday/synthetic diagnostics
- **PRO-01/02**: Retrieval-corpus "Advisor Pro" + managed hosting (waitlist-gated)

## Out of Scope

Explicitly excluded for v0.65.0. Documented to prevent scope creep.

| Feature | Reason |
|---------|--------|
| Heavy table packages (`gt` ~60 deps, `kableExtra`, `flextable` 57 deps) | `tinytable` covers all four formats with zero hard deps; heavy deps break the CRAN-clean discipline |
| `cli` as a hard import | Nice message formatting, but a heavy dep for a polish pass; classed `rlang` conditions deliver the substance |
| `lifecycle` dependency (unless renames exist) | Research: zero benefit if nothing is renamed; API-06 stays a no-op unless a real rename appears |
| `_brand.yml` unified brand config | Explicit `template.bslib` in `_pkgdown.yml` is simpler for a single-site v0.65.0; revisit if the ecosystem adopts `_brand.yml` |
| A v1.0 release gate | This is an incremental, ship-when-good minor; 1.0 readiness is a separate decision |
| New statistical methods, models, or estimators | Correctness/output-feel only; no change to statistical intent |
| Changing valid-input behavior of any existing method | Additive polish only; the never-silently-wrong contract is untouched |
| Shipping rich articles/logo sources inside the CRAN tarball | Kept pkgdown-only / `.Rbuildignore`'d to keep the tarball lean and `R CMD check` fast |

## Traceability

Mapped during roadmap creation (2026-09-08). Phases 20–24 per `.planning/ROADMAP.md`.

| Requirement | Phase | Status |
|-------------|-------|--------|
| BRAND-01 | Phase 20 | Complete |
| BRAND-02 | Phase 20 | Complete |
| BRAND-03 | Phase 20 | Complete |
| BRAND-04 | Phase 20 | Complete |
| BRAND-05 | Phase 20 | Complete |
| BRAND-06 | Phase 20 | Complete |
| BRAND-07 | Phase 20 | Complete |
| CRAN-02 | Phase 20 | Complete |
| CRAN-03 | Phase 20 | Complete |
| CRAN-04 | Phase 20 | Complete |
| VIZ-01 | Phase 21 | Complete |
| VIZ-02 | Phase 21 | Complete |
| VIZ-03 | Phase 21 | Complete |
| CRAN-01 | Phase 21 | Complete |
| VIZ-04 | Phase 22 | Complete |
| VIZ-05 | Phase 22 | Complete |
| VIZ-06 | Phase 22 | Complete |
| VIZ-07 | Phase 22 | Complete |
| CRAN-05 | Phase 22 | Complete |
| API-01 | Phase 23 | Complete |
| API-02 | Phase 23 | Complete |
| API-03 | Phase 23 | Pending |
| API-04 | Phase 23 | Pending |
| API-05 | Phase 23 | Pending |
| API-06 | Phase 23 | Pending |
| CRAN-06 | Phase 23 | Pending |
| DOCS-01 | Phase 24 | Pending |
| DOCS-02 | Phase 24 | Pending |
| DOCS-03 | Phase 24 | Pending |
| DOCS-04 | Phase 24 | Pending |

**Coverage:**

- v1 requirements: 30 total (BRAND 7, VIZ 7, API 6, DOCS 4, CRAN 6)
- Mapped to phases: 30 (Phase 20: 10 · Phase 21: 4 · Phase 22: 5 · Phase 23: 7 · Phase 24: 4)
- Unmapped: 0 ✓

---
*Requirements defined: 2026-09-08*
*Last updated: 2026-09-08 after roadmap creation (Phases 20–24 mapped, 30/30 coverage)*
