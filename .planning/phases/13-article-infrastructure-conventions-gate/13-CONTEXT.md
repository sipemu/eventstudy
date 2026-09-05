# Phase 13: Article Infrastructure & Conventions Gate - Context

**Gathered:** 2026-09-05
**Status:** Ready for planning
**Mode:** Smart discuss (autonomous)

<domain>
## Phase Boundary

Build the shared, reusable article infrastructure and site-config conventions
that prevent every documentation-integrity pitfall (determinism, math escaping,
citation resolution, tarball exclusion, plotly/MathJax coexistence, content
duplication) **once**, before any content article is written. Wire the
Learn/Methods track and the Gallery into the navigation, verify the whole thing
with a single smoke-test article, and leave the existing 18 CRAN vignettes
byte-unchanged and still CRAN-shipped.

Out of scope: writing the actual Methods articles (Phase 15), curating datasets
(Phase 14), the gallery worked examples (Phase 16).

</domain>

<decisions>
## Implementation Decisions

### Navigation & Delivery Structure
- Expose the conceptual method articles as a dedicated **"Methods" navbar
  dropdown** (a learning track), wired in both `navbar: structure:` and
  `navbar: components:`.
- Add a distinct **"Gallery"** navbar entry (the current `articles` component
  already targets `articles/gallery.html`); the gallery is its own surface.
- Keep the existing **18 vignettes** reachable under an "Articles" dropdown/
  listing, byte-unchanged and still CRAN-shipped — new content is strictly
  additive (DELIVERY-03).
- New rich content lives under `vignettes/articles/`; add `^vignettes/articles$`
  to `.Rbuildignore` **in the same commit** that creates the directory
  (DELIVERY-01) so articles render on the site but are absent from the CRAN
  source tarball.
- Use **one shared bibliography** at `vignettes/articles/references.bib`,
  referenced filename-only (`bibliography: references.bib`) so citation
  resolution works from the co-located file (RENDER-03 / citation pitfall).

### Article Skeleton & Determinism
- Provide a reusable `_setup.Rmd` child chunk that sets `set.seed(42)`,
  `options(scipen = 999, digits = 4)`, and knitr defaults
  (`collapse = TRUE, comment = "#>"`, echo on) so every downstream author
  inherits determinism by default (METH-01 / determinism pitfall).
- Ship a reusable **10-section article skeleton**: Title/abstract · When to use ·
  Intuition · Model & null hypothesis · Assumptions · Worked example (code) ·
  Rendered table · Rendered plot · Interpretation · References.
- Set `math-rendering: katex` in `_pkgdown.yml` to resolve the known
  pkgdown plotly/MathJax JS conflict (RENDER-03).

### Smoke-Test Article
- One smoke-test article, built from the skeleton, that proves the whole gate on
  a single page: a **KaTeX-rendered formula** (a CAR formula), a **resolved `@`
  citation** from the shared `references.bib`, and a **plotly figure** (AR/CAR
  plot), with no broken layout, no raw `[@Key]`/`$LaTeX$`, and no plotly/MathJax
  conflict.
- Drive the smoke-test article with the **bundled `dieselgate` dataset** (already
  in `data/`, offline-safe, deterministic) rather than scraped or simulated data.

### Claude's Discretion
- Exact navbar ordering, menu nesting, and label casing, provided both the
  Methods track and a Gallery entry are visible and the existing Articles/
  Reference navigation is not broken.
- Precise wording of the 10 skeleton section headers and the smoke-test prose.

</decisions>

<code_context>
## Existing Code Insights

### Reusable Assets
- `_pkgdown.yml` already exists (Phase 11): `template: bootstrap: 5`,
  `url: https://sipemu.github.io/eventstudy/`, a `navbar` with
  `structure.left: [get-started, reference, articles]`, and a fully grouped
  `reference:` index over all exported symbols.
- The `articles` navbar component already points to `articles/gallery.html`.
- Bundled `dieselgate` dataset in `data/` (Phase 9) — offline, deterministic,
  drives a valid end-to-end event study; extended to 4 automakers / 2 groups.
- 19 vignettes in `vignettes/` (18 CRAN + `gallery.Rmd`).

### Established Patterns
- `.Rbuildignore` already excludes `^_pkgdown\.yml$`, `^docs$`, `^pkgdown$`,
  `^data-raw$` — the site/build artifacts are kept out of the CRAN tarball; the
  new `^vignettes/articles$` rule follows the same pattern.
- Offline-safe / gated vignette builds are an established discipline
  (BUILD-02 blocker; `data-download` vignette must build reproducibly).

### Integration Points
- `_pkgdown.yml` `navbar.structure` + `navbar.components` — add Methods and
  Gallery entries here without disturbing get-started/reference/articles.
- `vignettes/articles/` — new directory; the skeleton + `_setup.Rmd` +
  `references.bib` + smoke-test article all live here.

</code_context>

<specifics>
## Specific Ideas

- Match the fdars-r reference look already adopted for the pkgdown site.
- The smoke-test article is a throwaway proof-of-infra: it must exercise
  KaTeX formula + resolved citation + plotly figure together on one page, since
  that triple is exactly the coexistence pitfall the gate must prevent.
- Verify via a CI dry-run rendering of the smoke-test page (success criterion 3).

</specifics>

<deferred>
## Deferred Ideas

- Actual 8 Methods articles — Phase 15.
- Curated per-domain datasets — Phase 14.
- Gallery worked examples + final build/release gate — Phase 16.

</deferred>
