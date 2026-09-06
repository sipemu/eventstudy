# Phase 16: Worked-Examples Gallery + Build & Release Integrity - Context

**Gathered:** 2026-09-06
**Status:** Ready for planning
**Mode:** Smart discuss (autonomous)

<domain>
## Phase Boundary

Deliver a cross-domain gallery of complete, rendered worked examples that make
the package tangible end-to-end (data → model fit → test statistics → plots →
written interpretation) and connect example → concept → API, then prove the
entire v0.63.0 documentation-depth addition ships clean: local pkgdown build,
CI deploy, and CRAN check all green.

In scope:
- Three end-to-end worked-example articles in `vignettes/articles/`
  (site-only, tarball-excluded): earnings, regulatory, M&A domains.
- A "Worked Examples" section added to the existing `vignettes/gallery.Rmd`
  card grid (3 domain cards), plus 3 new `man/figures/card-example-*.svg`
  thumbnails and one new `.es-examples` / `.es-tag-examples` CSS color slot.
- `_pkgdown.yml` wiring: a new "Worked Examples" Articles group listing the
  three examples (both gallery-card AND Articles-dropdown discoverability).
- Cross-links from each example to the relevant Methods articles and to the
  exported-function reference pages (example → concept → API).
- Build & release gate: local `pkgdown::build_site()` clean; CI `pkgdown.yaml`
  builds new articles offline + deploys green with no CI changes beyond
  content/config; `R CMD check --as-cran` no new NOTEs/WARNINGs vs the v0.62.0
  baseline; source tarball not bloated; testthat suite green.
- Record the v0.62.0 R CMD check baseline formally (cran-comments) as closeout.

Out of scope:
- New Methods articles or dataset creation (Phases 15 / 14 delivered these).
- Any change to the 19 top-level CRAN vignettes' *analysis* content — only
  `gallery.Rmd` (itself a top-level vignette) gains an additive HTML card
  section; the other 18 stay byte-unchanged.
- Any change to `R/`, `NAMESPACE`, `DESCRIPTION` package code, or `data/`.
- Making the Phase 12 CI pipeline live (operator step — see Deferred).

</domain>

<decisions>
## Implementation Decisions

**Both grey-area batches ACCEPTED (Accept recommended) by user, 2026-09-06.**

### Worked-example content & scope
- **Earnings** (`example-earnings.Rmd`): real data via `data("earnings_surprises")`
  (AAPL/MSFT/GOOGL beats). Market model → CAR → Patell Z + BMP → plot →
  interpretation.
- **Regulatory** (`example-regulatory.Rmd`): real data via `data("dieselgate")`,
  using the **richer two-group VW-group-vs-peer-group comparison** (shows
  idiosyncratic-shock isolation). CAAR + sign test → plot → interpretation.
- **M&A** (`example-ma.Rmd`): `simulate_event_study(seed=N)` framed as M&A deal
  announcements, leaning into the **power-analysis teaching angle** on
  clearly-labeled synthetic data (the "it's simulated" caveat becomes a
  feature — demonstrates detectability/power). Explicitly labeled synthetic.
- All three follow the `_article-skeleton.Rmd` 10-section convention, use
  `child="_setup.Rmd"` for determinism, carry NO `%\VignetteIndexEntry`
  (site-only), and live in `vignettes/articles/` (tarball-excluded).

### Gallery presentation & discoverability (both surfaces)
- Add a new **"Worked Examples" section** to `gallery.Rmd`'s `.es-gallery`
  card grid: 3 cards → `articles/example-{earnings,regulatory,ma}.html`.
- Author **3 distinct new card SVGs**: `card-example-earnings.svg`,
  `card-example-regulatory.svg`, `card-example-ma.svg` in `man/figures/`,
  matching the existing 18-card SVG style (viewBox, icon + label, ~1.5KB each).
- Add **one new CSS color slot** in `pkgdown/extra.css`:
  `.es-section-heading.es-examples` + `.es-tag-examples` (teal `#20c997`,
  distinct from the existing 7 tag colors).
- Add a **"Worked Examples" Articles group** to `_pkgdown.yml` `articles:`
  listing the three — so examples appear in BOTH the gallery card index AND
  the Articles dropdown (matches how method vignettes already appear in both).

### Cross-linking (example → concept → API) — GALLERY-03
- Each example links to the relevant Methods article(s) and to the exported
  reference pages for the functions it uses.
- Link syntax from within a `vignettes/articles/example-*.Rmd`:
  - to another article (same dir): `[Return Models](methods-return-models.html)`
  - to a reference page: `[run_event_study()](../reference/run_event_study.html)`
- Caller gotcha (from Phase 15): use the explicit `tidy.EventStudyTask(task,
  type="car")` form — the broom generic is not re-exported.

### Build & release gate
- Local: `pkgdown::build_site()` (or `build_site_github_pages`) completes with
  zero errors, zero NEW warnings, all Methods + gallery + examples included.
- CI: NO changes to `.github/workflows/pkgdown.yaml`. It runs
  `build_site_github_pages(new_process=FALSE, install=FALSE)` and auto-discovers
  new articles. Suggests deps (rugarch, did, quadprog, …) install via RSPM
  binaries on Ubuntu, so `eval=requireNamespace(...)` GARCH chunks and
  `eval=FALSE` DiD/LLM chunks build fine. Live chunks are network-free
  (`data()` / `simulate_event_study(seed=)`).
- CRAN: `^vignettes/articles` in `.Rbuildignore` already excludes all three
  new articles. The 3 new SVGs ship in `man/figures/` (~4.5KB — negligible;
  `man/figures/` is a standard CRAN location). `R CMD check --as-cran` must
  show no new NOTEs/WARNINGs vs v0.62.0 baseline; testthat stays green.

### Claude's Discretion
- Exact SVG artwork/iconography for the 3 example cards (match existing style).
- Exact `simulate_event_study()` parameters for the M&A example.
- Precise prose, section wording, and which specific reference pages each
  example cross-links (must include the core pipeline fns it actually calls).
- Exact teal shade / final tag color for `.es-examples`.

</decisions>

<code_context>
## Existing Code Insights (from Phase 16 recon, 2026-09-06)

### Gallery infrastructure — ALREADY BUILT
- `vignettes/gallery.Rmd` (7.3KB): top-level CRAN vignette
  (`%\VignetteIndexEntry{Gallery}`), pure `{=html}` passthrough (no R chunks,
  builds trivially under R CMD check). Renders to `articles/gallery.html`.
  Uses `.es-gallery` grid, `.es-gallery-item/-thumb/-title/-tags` cards, and
  `<img src="../reference/figures/card-*.svg">`. 8 sections, 18 method cards.
  NO "Worked Examples" section yet.
- `pkgdown/extra.css` (145 lines): complete `.es-` card system — grid
  (3/2/1-col responsive), card anatomy, 7 `.es-tag-*` colors + matching
  `.es-section-heading.es-*`. Missing only the `.es-examples` slot.
- `man/figures/card-*.svg`: 18 method/feature SVGs (31.7KB total). NONE for
  worked-example domains. Style: SVG viewBox, icon + label, 1.3–2.6KB each.
- Navbar (`_pkgdown.yml`): `gallery` → `articles/gallery.html` (matches);
  Methods dropdown lists all 7 Phase-15 articles.

### Worked-example articles — DO NOT EXIST
- `vignettes/articles/` holds only the 7 `methods-*.Rmd`, `_article-skeleton`,
  `_setup.Rmd`, `smoke-test.Rmd`, `references.bib`. All 3 example-*.Rmd are new.

### CI & CRAN — NO STRUCTURAL CHANGES NEEDED
- `.github/workflows/pkgdown.yaml`: `build_site_github_pages(new_process=FALSE,
  install=FALSE)`; `extra-packages: any::pkgdown, local::.`; `needs: website`.
  DESCRIPTION has NO `Config/Needs/website` — so only Imports+Suggests+pkgdown
  install (sufficient; Suggests covers rugarch/did/quadprog via RSPM).
- `.Rbuildignore` line 16: `^vignettes/articles` (anchored) — excludes all new
  articles. `gallery.Rmd` (top-level) ships to CRAN (fine; no R chunks).
- DESCRIPTION Version: `0.62.0`.

### BUILD-06 baseline (v0.62.0, from Phase 12 SUMMARY)
- 1 ERROR: "packages suggested but not available: rugarch, rmgarch, did,
  DIDmultiplegt, didimputation, DT" — ENV-ONLY (dev machine missing optional
  pkgs), not a package defect. Resolves on a full-toolchain / CI machine.
- NOTEs: New submission / archived (CRAN history); VignetteBuilder w/ no
  prebuilt index (artifact of `--no-build-vignettes`); site URL 404 (resolves
  after first Pages deploy); undefined globals `median`/`tail` (pre-existing,
  `R/es_diagnostics.R`).
- No formal `cran-comments.md` v0.62.0 section exists — Phase 16 adds one
  (GAP-7 closeout).

</code_context>

<integration>
## Integration Points

- `vignettes/articles/` — 3 new `example-*.Rmd` (site-only, tarball-excluded).
- `vignettes/gallery.Rmd` — additive "Worked Examples" HTML card section
  (top-level vignette; only additive HTML, no R chunks introduced).
- `man/figures/` — 3 new `card-example-*.svg` (ship in tarball, ~4.5KB).
- `pkgdown/extra.css` — 1 new `.es-examples` color slot (~2 lines).
- `_pkgdown.yml` — new "Worked Examples" `articles:` group (3 entries).
- `.github/workflows/pkgdown.yaml` — NO change.
- `.Rbuildignore` — NO change (`^vignettes/articles` already covers it).
- `R/`, `NAMESPACE`, `DESCRIPTION`, `data/` — NO change.
- `cran-comments.md` — add a v0.62.0/v0.63.0 baseline section (closeout).

</integration>

<deferred>
## Deferred

- Making the Phase 12 CI pipeline actually live (push to main → green Actions
  run → enable GitHub Pages → full-toolchain `R CMD check --as-cran`) is an
  OPERATOR step carried over from v0.62.0 Phase 12 (`human_needed`). BUILD-05's
  "green Actions run" can only be *confirmed* once that pipeline is live; Phase
  16 delivers the content/config that makes the workflow succeed and verifies
  the local build + workflow correctness, but the actual green-deploy
  confirmation may land as human_needed if CI is not yet enabled.
- A 4th/5th worked-example domain — out of scope (ROADMAP names three).
- Tagging/releasing v0.63.0 — milestone-complete lifecycle step, after Phase 16.

</deferred>
