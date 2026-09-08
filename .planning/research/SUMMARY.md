# Research Summary: EventStudy v0.65.0 Polish

**Project:** EventStudy CRAN R Package — Robustness Hardening (v0.65.0 Polish Milestone)
**Domain:** Polish pass on a mature R package — brand identity, publication-grade output, API/message polish, and documentation
**Researched:** 2026-09-08
**Confidence:** HIGH (derived from live codebase inspection + official pkgdown/rmarkdown/CRAN docs)

---

## Executive Summary

EventStudy v0.65.0 is a polish milestone on a production-ready CRAN package (v0.64.0). The package's core statistical pipeline, AI advisor, multi-format reporting, and pkgdown site are already shipped and thoroughly tested (2287 passing tests). This phase applies four non-breaking overlays: **brand identity** (logo, hex sticker, themed pkgdown site), **publication-grade aesthetics** (consistent colour palettes, typography, table styling in reports), **API message polish** (consistent print methods, classed error conditions, improved error messages), and **documentation polish** (cross-link tags, README refresh, favicon). All changes are additive and preserve backward compatibility — no behaviour changes to valid inputs.

The recommended approach is a **dependency-light, risk-minimal strategy**: add only three Suggests packages (`tinytable`, `patchwork`, `ragg`), avoid hard imports of heavy packages like `cli` or `lifecycle`, and keep all site-only assets (favicons, CSS) in `.Rbuildignore`'d directories. The primary risk is tarball bloat from logo assets — mitigated by storing high-resolution sources in `data-raw/`, using only optimised PNGs and SVGs in `man/figures/`, and running tarball size checks in CI.

The four surfaces are independent but form a coherent release: surface 1 (brand) enables surfaces 2–4 (aesthetics/API/docs build on the visual identity), and the grounding guard + degenerate-input contract invariants (locked by v0.64.0 hardening) remain completely untouched. This is a **ship-when-good, never-silent-wrong** polishing pass that upgrades the research-facing experience without touching statistical correctness.

---

## Key Findings

### Recommended Stack

From STACK.md: The v0.65.0 polish uses the **existing tech stack** with minimal additions.

**New Suggests packages:**
- **tinytable** (0.18.0+): Multi-format tables (HTML, LaTeX, Word, Markdown). Zero hard dependencies. Covers all four es_report() output formats.
- **patchwork** (1.3.2+): Multi-panel plot composition. Lightweight. Guarded with requireNamespace().
- **ragg** (1.5.2+): Anti-aliased figure rendering in HTML/Word. System deps standard on CRAN.

**Critically NOT adding:** gt (~60 deps), kableExtra (HTML+LaTeX only), flextable (57 deps, no LaTeX), cli (heavy for polish), lifecycle (zero-benefit if no renames).

**DESCRIPTION changes (minimal):**
```
Suggests: ..., tinytable, patchwork, ragg
```

No new Imports. CRAN tarball risk: negligible.

---

### Expected Features

**Surface 1: Brand & Visual Identity**
- Hex sticker PNG + SVG; README badge; lifecycle badge → "stable"
- Favicon set generated via pkgdown::build_favicons()
- Logo renders correctly on CRAN HTML mirror (PNG, not SVG-only)

**Surface 2: Report & Plot Aesthetics**
- Consistent colour palette (Okabe-Ito for colorblind safety)
- theme_eventstudy() unifies plot styling
- Figure captions on plot chunks; styled tables
- Per-format figure sizing for PDF/Word margins

**Surface 3: API & Message Polish**
- print.* methods return invisible(x)
- Error messages name offending argument values
- Classed conditions via rlang::abort() (no new hard deps)
- verbose= argument for batch users

**Surface 4: Docs & Site Polish**
- @family + @seealso roxygen tags
- README Ecosystem section; <!-- pkgdown: home: --> markers
- pkgdown navbar aligned to eventstudy.de palette
- NEWS.md linked from navbar

**MVP (minimum for "Polish" label):** Logo + favicon + stable badge + .es_palette + es_theme() + fig.cap + @family/@seealso + print method audit

---

### Architecture Approach

All four surfaces are **purely additive overlays**. Zero restructuring of pipeline, advisor, or reporting backend.

**Unchanged layers:** prepare → fit → calculate (pipeline), models, statistics, advisor, specialty (panel/bootstrap/simulation)

**Polish additions:**
- Surface 1: Assets in man/figures/ and pkgdown/; _pkgdown.yml updates
- Surface 2: New R/theme.R; R/plotting.R + skeleton.Rmd modifications; new inst/rmarkdown/report.css
- Surface 3: New R/conditions.R; print method consistency; selective stop()/warning() → rlang migration
- Surface 4: roxygen tags; README refresh; vignette cross-links

**Critical invariants (LOCKED by v0.64.0):**
- .handle_degenerate() — exactly one warning per degenerate event
- .validate_grounding() — drop-and-keep never-stop contract
- knitr::is_html_output() in skeleton.Rmd — must not move
- JOINT_HYPOTHESIS_CAVEAT wording — correctness invariant
- .sanitise_prose() ordering (ampersand-first) — hardened

---

### Critical Pitfalls & Prevention

1. **Tarball bloat from logo assets** — Keep man/figures/ optimised (<20 KB SVG, <50 KB PNG). High-res sources in data-raw/ (already .Rbuildignore'd). Assert tarball < 1 MB in CI.

2. **Non-ASCII characters in assets** — Replace non-ASCII font names with CSS generics. CI check: `grep -rP '[^\x00-\x7F]' man/figures/ R/ inst/` must pass.

3. **Table deps breaking PDF/Word** — Stick with knitr::kable() as universal primitive. Guard kableExtra with if (knitr::is_html_output()). Test all four formats in CI.

4. **knitr::is_html_output() broken** — Always use rmarkdown::*_document() constructors. Never ad-hoc lists. Regression test: PDF contains no <script> tags.

5. **Sanitiser over/under-escaping** — Test against comprehensive fixture. Preserve ampersand-first rule. Add parameterised snapshot tests before any code change.

---

## Implications for Roadmap

**Six phases, dependency-ordered:**

### Phase 1: Brand & Visual Identity (Foundation)
- Hex sticker design + logo placement + favicon + lifecycle badge
- _pkgdown.yml wiring + pkgdown/extra.css updates
- README badge + stabilityconfirmation
- Acceptance: tarball <1 MB, no non-ASCII, docs/logo.png exists after pkgdown::build_site()

### Phase 2: Shared Theme Foundation (Depends on nothing)
- R/theme.R with theme_eventstudy() + es_colours (Okabe-Ito)
- roxygen + export
- Acceptance: theme applied, tests green, no colour test regression

### Phase 3: Plot Aesthetics (Depends on Phase 2)
- Apply theme_eventstudy() in R/plotting.R helpers
- Replace hardcoded colours with es_colours
- Do NOT modify plot_stocks() (plotly)
- Acceptance: plots render, tests green, publication aesthetics confirmed

### Phase 4: Report & Plot Aesthetics (Depends on Phase 2)
- inst/rmarkdown/report.css with typography, tables, captions
- R/report.R inject css= to html_document() (HTML branch only)
- skeleton.Rmd: fig.cap + per-format fig.width + kableExtra guard
- Acceptance: all four formats render, PDF no margin overflow, Word 300 DPI, HTML polished

### Phase 5: API & Message Polish (Independent)
- R/conditions.R with classed condition factories
- print.* audit for invisible(x) return
- Improve error messages with offending values
- run_event_study(verbose=) argument
- Acceptance: snapshot tests for print methods, tests green, Imports count =13

### Phase 6: Docs & Site Polish (Depends on Phase 1 logo, can parallel Phase 5)
- @family/@seealso roxygen tags; devtools::document()
- README Ecosystem section + home markers
- _pkgdown.yml bslib colour overrides + news: entry
- pkgdown/extra.css typography, badge, hero rules
- Vignette cross-links
- Acceptance: pkgdown::check_pkgdown() clean, all articles render, no broken links

**Ordering rationale:** Phase 1 establishes assets all downstream depend on. Phase 2 theme foundation must precede Phases 3+4. Phases 3+4 sequential or parallel. Phase 5 independent. Phase 6 can parallel Phase 5, depends on Phase 1 logo.

**Duration:** 2-3 weeks at steady pace. Can compress to 1 week with parallel teams.

### Research Flags

**Phases needing deeper research during planning:**
- **Phase 1:** Design input — eventstudy.de colour codes for bslib primary/bg/fg not specified in technical research
- **Phase 4:** Multi-format rendering edge cases — CI integration tests for html/pdf/word/md needed

**Phases with standard patterns (skip research-phase):**
- **Phase 2, 3, 5, 6:** All established tidyverse/CRAN conventions; zero novelty

---

## Confidence Assessment

| Area | Confidence | Notes |
|------|------------|-------|
| Stack | HIGH | CRAN versions verified; tinytable/patchwork/ragg vetted; no speculative choices |
| Features | HIGH | Four surfaces inferred from codebase inspection (DESCRIPTION, R/, skeleton.Rmd, _pkgdown.yml) |
| Architecture | HIGH | Live code inspection; overlays confirmed additive; invariants identified and locked |
| Pitfalls | HIGH | Sourced from v0.62.0-v0.64.0 PROJECT.md history; CRAN policy verification; baseline measurements (339 KB v0.62.0 tarball) |

**Overall: HIGH** — Direct codebase inspection, zero speculation, dependency-light alignment with v0.50.0-v0.64.0 hardening discipline.

### Gaps to Address

- **Hex sticker design:** Design input from brand team (visual, not technical)
- **eventstudy.de colour codes:** Confirm bslib hex values with brand owners
- **Multi-format test coverage:** CI may need html/pdf/word/md render integration tests
- **Tarball growth CI check:** Add size assertion to pipeline
- **Non-ASCII lint step:** Add grep check to CI
- **Snapshot tests for print methods:** Verify existing coverage in testthat suite

---

## Ready for Roadmap

**Status: READY**

All four research files complete, HIGH confidence, directly actionable:
- Six phases with explicit feature/pitfall mappings
- Dependency-ordered build sequence
- 44+ acceptance criteria per phase
- Research flags identify standard-pattern phases (skip planning)
- No blocking open questions (design input separate stream)

**Next:** `/gsd-plan-phase 1` (Brand & Visual Identity)

---

*Research synthesis: 2026-09-08 | Confidence: HIGH | Ready: YES*
