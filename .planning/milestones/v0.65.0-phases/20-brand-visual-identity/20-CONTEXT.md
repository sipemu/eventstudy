# Phase 20: Brand & Visual Identity - Context

**Gathered:** 2026-09-08
**Status:** Ready for planning
**Mode:** Smart discuss (autonomous)

<domain>
## Phase Boundary

Give EventStudy a real, coherent visual identity aligned to the eventstudy.de
three-tool ecosystem (Google Sheets template · R package · WebAssembly app),
and wire it through the README and the pkgdown site — all while staying
CRAN-clean.

**In scope (BRAND-01..07, CRAN-02/03/04):**
- Logo + hex sticker: SVG source in `.Rbuildignore`'d `data-raw/brand/`, optimised
  `man/figures/logo.png` (<50 KB, tarball-safe).
- README logo/badge wiring (`usethis::use_logo()` pattern) + pkgdown navbar logo.
- Favicon set via `pkgdown::build_favicons()` under `pkgdown/` (out of tarball).
- Open Graph social-preview card.
- pkgdown `bslib` theme aligned to eventstudy.de palette + typography via
  `_pkgdown.yml template.bslib` + `pkgdown/extra.scss`.
- Home card gallery + numeric badges restyled to the ecosystem look.
- Lifecycle badge `experimental` → `stable`; README badge row refreshed.
- Version bump to 0.65.0 (first commit); NEWS.md v0.65.0 section; tarball <1 MB
  CI assertion; non-ASCII / no-new-check-findings guardrails.

**Out of scope:** plot/theme code (Phase 21), report aesthetics (Phase 22), API
polish (Phase 23), docs/vignette cross-linking (Phase 24). No change to any
statistical behavior — this phase is site/asset/metadata only, with the sole
package-code touch being the version bump and lifecycle badge.

</domain>

<decisions>
## Implementation Decisions

### Brand Palette & Theme Mechanism
- **Palette source:** Fetch the live eventstudy.de site and extract its actual CSS
  colours (primary/bg/fg/accent) to drive the theme. Confirmed qualitative
  identity: clean white/light background, dark text, neutral grays for secondary
  elements, minimal aesthetic, **no vibrant accent**. Exact hex codes live in
  eventstudy.de's linked stylesheet (not inlined in HTML) — planner/executor must
  fetch the CSS asset directly to pull precise values.
- **Theme mechanism:** Hybrid — add a **minimal `template.bslib`** (bg/fg/primary +
  fonts) for the palette on top of the existing plain-BS5 site, and **keep the
  existing `pkgdown/extra.css` fdars-style card gallery**. This satisfies BRAND-05's
  `template.bslib` wording without discarding the gallery. Palette additions may
  go in `pkgdown/extra.scss` alongside `extra.css`.
- **Typography:** Clean neutral sans matching eventstudy.de (system-ui / Inter for
  headings). No decorative fonts.
- **Bootswatch base:** None — custom bslib variables.

### Logo & Hex Sticker
- **Artwork producer:** Claude generates the artwork (proceeds autonomously; the
  brand owner can swap the SVG later).
- **Motif:** Cumulative-abnormal-return curve crossing the event line at t=0 — a
  domain-evocative mark. (Monogram "ES" was the runner-up.)
- **Hex sticker:** Build via the `hexSticker` package — logo centred, package name,
  subtle url — in the confirmed palette. `hexSticker` is a build-time/dev tool,
  NOT a package dependency (keep it out of DESCRIPTION; run in `data-raw/brand/`).
- **Assets:** SVG source under `.Rbuildignore`'d `data-raw/brand/`; optimised
  `man/figures/logo.png` <50 KB.

### Numeric Badges, Cards & Ecosystem
- **Badge counts:** Align to the eventstudy.de ecosystem badges, which read
  **"15+ Return Models · 12 Test Statistics · 6 DiD Estimators"**. Codebase-verified
  reality: 12 test-statistic classes (exact match), ~13–14 concrete return-model
  classes ("15+" honest via the plus / configurations), 5 named modern DiD
  estimators + TWFE. **Each badge must be defensible** — keep "12" exact, keep
  "15+" (the plus makes it honest), and reconcile the DiD count at plan time
  (TWFE + Sun-Abraham + Callaway-Sant'Anna + BJS + de Chaisemartin ≈ 5–6; if the
  honest count is 5, prefer the honest number and note the discrepancy for the
  brand owner rather than inflating).
- **Badge style:** eventstudy.de numeric-badge cards (large number + label).
- **Card gallery scope:** Restyle existing home cards to the ecosystem look; keep
  the existing bespoke `card-*.svg` art (do not regenerate all artwork).
- **Ecosystem framing:** Add a small strip linking the three tools
  (Sheets · R package · WASM app) on eventstudy.de.

### Version, Lifecycle & CRAN Guardrails
- **Version bump:** 0.65.0 as the first commit of execution + NEWS.md v0.65.0 section.
- **Lifecycle:** Flip `experimental` → `stable`; refresh the README badge row.
- **Favicons + OG card:** `pkgdown::build_favicons()` + OG card configured in
  `_pkgdown.yml`.
- **CRAN guards:** CI asserts tarball <1 MB; non-ASCII grep guard over
  `R/`/`man/figures/`/`inst/`; no new `R CMD check --as-cran` NOTEs/WARNINGs vs
  baseline.

</decisions>

<code_context>
## Existing Code Insights

### Reusable Assets
- `_pkgdown.yml` already: plain BS5 (`template: bootstrap: 5`, katex), `home:
  sidebar: false`, navbar [get-started, reference, articles, methods, gallery,
  examples], curated Methods/Gallery/Examples menus. DESCRIPTION `URL` +
  `BugReports` already point at github.com/sipemu/eventstudy and the pkgdown site.
- `pkgdown/extra.css` (3.4 KB) holds the fdars-style card gallery CSS — the visual
  identity today. Keep it; layer bslib palette on top.
- `man/figures/` already has ~21 bespoke `card-*.svg` thumbnails (introduction,
  factor-models, panel, intraday, synthetic-control, AI advisor, examples, etc.).
  **No `logo.png` yet** — this phase adds it.
- No `data-raw/brand/` dir yet — create it (`.Rbuildignore`'d).
- fdars-r is the design north star (repo sipemu/fdars-r): plain BS5 + hand-written
  CSS card grid + bespoke SVGs. EventStudy's site already follows this pattern.

### Established Patterns
- Brand/site assets split by CRAN boundary (logged decision): tarball-safe PNGs in
  `man/figures/`; SVG sources, favicons, OG card in `.Rbuildignore`'d
  `data-raw/brand/` and `pkgdown/`.
- `.Rbuildignore` already excludes `data-raw`, pkgdown build dirs (verify and extend).

### Integration Points
- DESCRIPTION `Version:` (currently 0.64.0) → 0.65.0; `NEWS.md` top section.
- README badge row (lifecycle + docs badges) and logo `<img>`.
- `_pkgdown.yml` `template.bslib` block (new) + `template.opengraph` (new) + navbar
  logo; `pkgdown/extra.scss` (new) + existing `pkgdown/extra.css`.
- Lifecycle badge: locate current `experimental` reference (README + any
  `@lifecycle` roxygen / lifecycle badge) and flip to `stable`.
- CI workflow(s) under `.github/workflows/` — add tarball-size + non-ASCII guards.

</code_context>

<specifics>
## Specific Ideas

- eventstudy.de is the brand anchor: "Event Study Analysis Made Simple", clean
  neutral look, card + numeric-badge layout, and (currently) **no logo** — so the
  logo EventStudy adds becomes the ecosystem's first real mark.
- eventstudy.de's own stat badges are "15+ Return Models · 12 Test Statistics ·
  6 DiD Estimators" and its MIT-licensed badge — mirror this styling on the R
  package home page for cross-tool consistency.
- Logo motif: cumulative-abnormal-return curve crossing a vertical event line at
  t=0 — reads as an event study to a domain audience.
- `hexSticker` is a dev-time tool only; it must not land in DESCRIPTION Imports or
  Suggests. Generate the hex in `data-raw/brand/` and commit the rendered PNG.

</specifics>

<deferred>
## Deferred Ideas

- Plot/theme colour system (`theme_eventstudy()`, `es_colours`) — Phase 21.
- Report table/figure aesthetics — Phase 22.
- Vignette/article cross-linking + README ecosystem prose depth — Phase 24 (this
  phase adds only the brand strip + home markers, not full docs restructuring).
- Exact eventstudy.de hex extraction from its linked CSS is an execution task, not
  a discuss decision — the approach (fetch & extract) is locked here.

</deferred>
