# Phase 11: Curated pkgdown Site + Custom Theme (local build) - Context

**Gathered:** 2026-09-04
**Status:** Ready for planning

<domain>
## Phase Boundary

Deliver a curated, professionally-themed pkgdown site that builds cleanly on a
developer's machine — no CI yet. Every exported symbol is grouped into exactly one
Reference section, all 18 vignettes are reachable and meaningfully organized under an
Articles dropdown, the homepage renders from the README, `url:` is set to the GitHub
Pages URL, and a custom Bootstrap-5 theme (no logo) renders professionally with clean
typography, code blocks, and syntax highlighting. Scope ends at a clean local
`pkgdown::build_site()` — CI deploy, repo linkage, and release are Phase 12.

</domain>

<decisions>
## Implementation Decisions

### Theme & Visual Identity
- Accent / primary color: deep professional blue `#1C4E80` (finance-appropriate, high contrast, pairs with existing R-blue badges).
- Font pairing: body + headings **Inter**, code **JetBrains Mono** (free Google Fonts, clean and modern).
- Bootstrap base: Bootstrap 5 + **bslib variable overrides, no bootswatch** — bespoke, lightweight look configured via `template.bslib` variables.
- Code / syntax highlighting: pkgdown default clean light theme (github/arrow-style) with good contrast; no dark syntax theme.
- No logo (deferred per milestone decision) — professional look via color + type only.

### Articles Organization
- `introduction.Rmd` is the "Get Started" article, surfaced as a primary navbar item.
- 7 conceptual Article groups (pedagogical ordering intro → basic → advanced within each):
  - **Core Workflow**: result-extraction, diagnostics-validation, inference-robustness
  - **Return Models**: factor-models-bhar, time-varying-models, custom-models, volume-volatility-event-study
  - **Test Statistics**: custom-test-statistics
  - **Advanced Designs**: panel-event-study, modern-did-estimators, intraday-event-study, synthetic-control
  - **Cross-Sectional & Simulation**: cross-sectional-analysis, simulation-power-analysis
  - **AI Advisor**: ai-advisor (own group — flagship 0.60.0 feature, high visibility)
  - **Data & Reporting**: data-download, automated-reports
- All 18 vignettes must be reachable; none orphaned.

### Reference Groups (from ROADMAP success criteria — locked)
- Every exported symbol appears in exactly one grouped Reference section:
  Pipeline & tasks · Return models · Test statistics · Panel/intraday/synthetic ·
  AI advisor · Diagnostics · Cross-sectional & simulation · Export & reporting ·
  Plotting · Data & datasets.
- `build_site()` must emit zero missing-topic / orphaned-reference warnings.

### Claude's Discretion
- Exact bslib variable names/values, font import mechanism, and precise Reference
  subsection titles/ordering are at Claude's discretion, guided by the groups above.

</decisions>

<code_context>
## Existing Code Insights

### Reusable Assets
- `README.md` — rich landing content (badges, Features, Installation, examples) to render as homepage.
- 18 vignettes in `vignettes/*.Rmd` (confirmed present).
- `NEWS.md` (if present) drives News/Changelog navbar item.
- `DESCRIPTION` currently `URL: https://github.com/sipemu/eventstudy`, `Version: 0.61.3`.

### Established Patterns
- Exports declared via roxygen2 `@export`; NAMESPACE auto-generated.
- No `_pkgdown.yml` / `pkgdown/` directory exists yet — this phase creates it.
- Package uses R6 classes + snake_case functions; reference grouping must cover both exported functions and exported R6 generators.

### Integration Points
- New files: `_pkgdown.yml` (site config: url, navbar, reference groups, articles) and `pkgdown/extra.scss` or `_pkgdown.yml template.bslib` block for theme.
- `.Rbuildignore` update to keep pkgdown config out of the CRAN tarball is a Phase 12 concern (release integrity), but the config files land here.

</code_context>

<specifics>
## Specific Ideas

- Reference site aesthetic modeled on the fdars-r pkgdown site (curated, grouped, professional).
- Site URL: `https://sipemu.github.io/eventstudy/`.

</specifics>

<deferred>
## Deferred Ideas

- Package logo / hex sticker, custom homepage cards, versioned docs (deferred to future per v0.62.0 roadmap).
- CI deploy, repo "About → Website" linkage, and CRAN-clean 0.62.0 release — Phase 12.

</deferred>
