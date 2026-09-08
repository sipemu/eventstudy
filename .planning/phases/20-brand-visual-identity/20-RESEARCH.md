# Phase 20: Brand & Visual Identity — Research

**Researched:** 2026-09-08
**Domain:** R package branding — pkgdown/bslib, hexSticker, SVG logo, CRAN asset hygiene
**Confidence:** HIGH (all key decisions already locked in CONTEXT.md + UI-SPEC.md; research confirms the technical implementation path)

---

<user_constraints>
## User Constraints (from CONTEXT.md)

### Locked Decisions

- **Palette source:** Fetch the live eventstudy.de site and extract its actual CSS colours (primary/bg/fg/accent) to drive the theme. Exact hex codes extracted in UI-SPEC.md — executor must verify the stylesheet URL has not rotated before wiring `template.bslib`.
- **Theme mechanism:** Hybrid — add a minimal `template.bslib` (bg/fg/primary + fonts) for the palette on top of the existing plain-BS5 site, and keep the existing `pkgdown/extra.css` fdars-style card gallery. Palette additions go in `pkgdown/extra.scss` alongside `extra.css`.
- **Typography:** "Plus Jakarta Sans" (headings) + "Inter" (body) — Google Fonts via bslib. No Bootswatch base.
- **Artwork producer:** Claude generates the logo artwork (CAR curve motif). Brand owner can swap the SVG later.
- **Motif:** Cumulative-abnormal-return curve crossing the event line at t=0.
- **hexSticker:** Build via the `hexSticker` package — dev tool ONLY, never in DESCRIPTION Imports or Suggests.
- **Badge counts:** "15+" Return Models (honest via the plus), "12" Test Statistics (exact match), "5" DiD Estimators (honest count; eventstudy.de shows "6" — flag for brand owner).
- **Version bump:** 0.65.0 as the first commit of execution + NEWS.md v0.65.0 section.
- **Lifecycle:** Flip `experimental` → `stable`; refresh the README badge row.
- **Favicons + OG card:** `pkgdown::build_favicons()` + OG card configured in `_pkgdown.yml`.
- **CRAN guards:** CI asserts tarball <1 MB; non-ASCII grep guard over `R/`/`man/figures/`/`inst/`; no new `R CMD check --as-cran` NOTEs/WARNINGs.

### Claude's Discretion

- Exact SVG control-point tweaks for visual balance of the CAR-curve motif.
- Exact file layout within `data-raw/brand/` (make_sticker.R, make_logo.R, og_card.R).
- Whether `rsvg` or Inkscape CLI renders the SVG → PNG (rsvg is already an optional dep via pkgdown; simpler choice).
- Exact CI YAML step syntax for the tarball-size and non-ASCII guards.

### Deferred Ideas (OUT OF SCOPE)

- Plot/theme colour system (`theme_eventstudy()`, `es_colours`) — Phase 21.
- Report table/figure aesthetics — Phase 22.
- Vignette/article cross-linking + README ecosystem prose depth — Phase 24.
- `_brand.yml` unified brand config — explicitly excluded from v0.65.0.
</user_constraints>

---

<phase_requirements>
## Phase Requirements

| ID | Description | Research Support |
|----|-------------|------------------|
| BRAND-01 | Logo + hex sticker: SVG source under `data-raw/brand/`, PNG at `man/figures/logo.png` <50 KB | hexSticker API + rsvg rendering path documented below |
| BRAND-02 | Logo wired into README badge and pkgdown navbar | usethis::use_logo() pattern + `navbar.logo` YAML key documented |
| BRAND-03 | Favicon set via `pkgdown::build_favicons()` under `pkgdown/` | build_favicons() signature + output file list documented |
| BRAND-04 | Open Graph social-preview card configured | `template.opengraph` YAML block documented; OG card dimensions in UI-SPEC |
| BRAND-05 | pkgdown bslib theme aligned to eventstudy.de brand | `template.bslib` YAML keys documented; palette verified in UI-SPEC |
| BRAND-06 | Home card gallery + numeric badges restyled to ecosystem look | SCSS additions + HTML block patterns documented |
| BRAND-07 | Lifecycle badge `experimental` → `stable`; README badge row refreshed | Exact badge URL documented; location verified at README.md:8 |
| CRAN-02 | Version bumped to 0.65.0; NEWS.md v0.65.0 section | DESCRIPTION Version: field location verified |
| CRAN-03 | Tarball <1 MB (CI assertion); brand sources in `.Rbuildignore`'d dirs; man/figures/ optimised | CI step pattern documented; .Rbuildignore state verified |
| CRAN-04 | No non-ASCII in R/, man/figures/, inst/; no new R CMD check NOTEs/WARNINGs | Non-ASCII grep guard pattern documented |
</phase_requirements>

---

## Summary

Phase 20 is a pure asset/metadata phase — it adds a logo, hex sticker, favicon set, OG card, pkgdown palette, and ecosystem badges. Zero statistical code is touched. All key design decisions are locked in CONTEXT.md and UI-SPEC.md; this research answers the implementation-mechanics questions the planner needs.

The biggest technical questions are: (1) how `hexSticker` works without landing in DESCRIPTION, (2) the exact `_pkgdown.yml` keys for `template.bslib` + `template.opengraph` + `navbar.logo`, (3) how `pkgdown::build_favicons()` runs and what it produces, and (4) the correct `.Rbuildignore` entries and CI guard patterns. All four are answered below.

The existing `pkgdown/extra.css` (3.4 KB, fdars-style gallery) is kept intact. A new `pkgdown/extra.scss` is added alongside it for the palette tokens and new UI constructs (numeric-badge strip, ecosystem strip). The `.Rbuildignore` already excludes `data-raw` and `pkgdown` — verified in-session.

**Primary recommendation:** Execute in five atomic waves: (W0) version bump + .Rbuildignore audit; (W1) SVG logo authoring + PNG export; (W2) hexSticker + hex PNG; (W3) pkgdown theme wiring (bslib + OG + favicons + navbar); (W4) home page constructs + README + CI guards + NEWS.

---

## Architectural Responsibility Map

| Capability | Primary Tier | Secondary Tier | Rationale |
|------------|-------------|----------------|-----------|
| Logo SVG authoring | `data-raw/brand/` (dev-time) | — | Source artwork; `.Rbuildignore`'d; not in tarball |
| Logo PNG (tarball-safe) | `man/figures/` | — | README + pkgdown consume it; must be in tarball |
| Hex sticker generation | `data-raw/brand/make_sticker.R` (dev script) | — | hexSticker is dev-only; committed PNG goes to `man/figures/` |
| pkgdown palette/typography | `_pkgdown.yml` `template.bslib` | `pkgdown/extra.scss` | bslib variables set global tokens; extra.scss adds new constructs |
| Existing card gallery CSS | `pkgdown/extra.css` (unchanged) | — | Keep existing file; layer new palette on top via bslib |
| Favicon set | `pkgdown/favicon/` (build-time output) | — | `.Rbuildignore`'d; generated by build_favicons() from logo.png |
| OG card image | `man/figures/og-card.png` (tarball-safe) | — | Referenced by `_pkgdown.yml` opengraph.image; must be in tarball |
| OG card source | `data-raw/brand/og-card-source/` (dev-time) | — | SVG composition source; `.Rbuildignore`'d |
| Numeric badges + ecosystem strip | Home page Rmd (`vignettes/gallery.Rmd` or home `index.md`) | `pkgdown/extra.scss` | Raw HTML blocks + new CSS classes |
| README logo + lifecycle badge | `README.md` | — | Lines 1 and 8 per verified positions |
| DESCRIPTION version bump | `DESCRIPTION` | `NEWS.md` | First commit of execution; CRAN-02 |
| CI guardrails | `.github/workflows/R-CMD-check.yaml` | — | Tarball size + non-ASCII steps appended to existing job |

---

## Standard Stack

### Core (dev-time only — NOT in DESCRIPTION)

| Tool | Version | Purpose | Why Standard |
|------|---------|---------|--------------|
| `hexSticker` | CRAN current | Generate hex sticker PNG from subplot + text | De-facto standard for R package hex stickers; used by 95%+ of CRAN packages with stickers |
| `rsvg` | CRAN current | Render SVG → PNG at specified resolution | Lightweight; already a transitive dep of pkgdown; avoids Inkscape/Inkscape CLI dependency |
| `pkgdown` | CRAN current (≥ 2.0) | Build site, favicons, OG card wiring | Already in workflows; 2.0+ required for `template.bslib` support |
| `bslib` | CRAN current (≥ 0.5) | Bootstrap 5 theme variables | pkgdown delegates bslib theming; `google` font subkey added in bslib ≥ 0.5 |

These are run in `data-raw/brand/` scripts or interactively — they must NOT appear in DESCRIPTION Imports or Suggests.

### In-DESCRIPTION (no additions for Phase 20)

Phase 20 adds no new packages to DESCRIPTION. All tooling is dev-time only.

---

## Package Legitimacy Audit

Phase 20 adds no new runtime packages to DESCRIPTION. The dev-time tools used (`hexSticker`, `rsvg`) are run interactively or in `data-raw/` scripts and are not installed as package dependencies. No legitimacy gate needed for this phase.

| Package | Role | In DESCRIPTION? | Status |
|---------|------|-----------------|--------|
| `hexSticker` | Dev-time hex sticker generator | No | Dev-only — never in DESCRIPTION |
| `rsvg` | SVG → PNG renderer | No (transitive via pkgdown) | Available via pkgdown install |
| `pkgdown` | Site builder | No (CI only, in workflows) | CRAN stable, already in CI |

---

## Architecture Patterns

### System Architecture Diagram

```
data-raw/brand/
  logo.svg              ← SVG source (authored by executor)
  make_sticker.R        ← hexSticker::sticker() → man/figures/logo-hex.png
  make_logo.R           ← rsvg::rsvg_png() → man/figures/logo.png
  og-card-source/       ← OG card composition sources
    og-card.svg         ← 1200×630 SVG composition

man/figures/            ← CRAN tarball (in-tarball assets)
  logo.png              ← 240×240 px, <50 KB  [BRAND-01]
  logo-hex.png          ← hex sticker PNG, <50 KB  [BRAND-01]
  og-card.png           ← 1200×630 px, <200 KB  [BRAND-04]

_pkgdown.yml            ← template.bslib + template.opengraph + navbar.logo  [BRAND-05, BRAND-04, BRAND-02]

pkgdown/
  extra.css             ← EXISTING fdars-style gallery CSS (unchanged)
  extra.scss            ← NEW: palette tokens, .es-stat-badge, .es-ecosystem-strip  [BRAND-05, BRAND-06]
  favicon/              ← GENERATED by pkgdown::build_favicons()  [BRAND-03]
    favicon-16x16.png
    favicon-32x32.png
    apple-touch-icon.png
    android-chrome-192x192.png
    android-chrome-512x512.png
    site.webmanifest

vignettes/gallery.Rmd   ← Add numeric-badge + ecosystem strip HTML blocks  [BRAND-06]
README.md               ← Logo img prepended (line 1); lifecycle badge updated (line 8)  [BRAND-02, BRAND-07]
DESCRIPTION             ← Version: 0.65.0  [CRAN-02]
NEWS.md                 ← v0.65.0 section  [CRAN-02]
.github/workflows/
  R-CMD-check.yaml      ← + tarball-size step + non-ASCII step  [CRAN-03, CRAN-04]
```

### Recommended Project Structure

```
data-raw/brand/
├── logo.svg             # CAR-curve motif SVG source
├── make_logo.R          # rsvg::rsvg_png() → man/figures/logo.png
├── make_sticker.R       # hexSticker::sticker() → man/figures/logo-hex.png
└── make_og_card.R       # compose OG card → man/figures/og-card.png

man/figures/
├── logo.png             # ← committed
├── logo-hex.png         # ← committed
└── og-card.png          # ← committed

pkgdown/
├── extra.css            # existing (unchanged)
└── extra.scss           # new
```

### Pattern 1: pkgdown bslib Theme (BRAND-05)

**What:** The `template.bslib` block in `_pkgdown.yml` sets Bootstrap 5 Sass variables that propagate through the entire pkgdown-generated site. Google Fonts are loaded via the `google:` subkey introduced in bslib ≥ 0.5.

**The exact YAML block to add to `_pkgdown.yml`** (merge under existing `template:` key):

```yaml
# Source: UI-SPEC.md bslib mapping section (verified against eventstudy.de CSS)
template:
  bootstrap: 5
  math-rendering: katex
  bslib:
    bg: "#ffffff"
    fg: "#0f172a"
    primary: "#2563eb"
    link-color: "#2563eb"
    link-hover-color: "#1d4ed8"
    font-scale: 1.0
    base-font:
      google: "Inter"
    heading-font:
      google: "Plus Jakarta Sans"
    code-font:
      google: "JetBrains Mono"
  opengraph:
    image:
      src: man/figures/og-card.png
      alt: "EventStudy R Package — Financial Event Study Analysis"
    twitter:
      creator: "@sipemu"
      card: summary_large_image
```

**Pitfall:** Do NOT use a Bootswatch `bootswatch:` key — it overrides bslib variables and undoes the palette. `template.bslib` only works alongside `bootstrap: 5`. The `math-rendering: katex` key must stay.

**Pitfall:** pkgdown < 2.0 ignores `template.bslib` silently. The CI uses `any::pkgdown` which installs the latest — safe. Local dev should use `pkgdown` ≥ 2.0.

### Pattern 2: Navbar Logo (BRAND-02)

Add under the existing `navbar:` key in `_pkgdown.yml`:

```yaml
# Source: pkgdown docs — navbar.logo is a native pkgdown ≥ 2.0 feature
navbar:
  logo:
    image: logo.png
    href: https://sipemu.github.io/eventstudy/
    alt: EventStudy
  structure:
    left: [logo, get-started, reference, articles, methods, gallery, examples]
    right: [search, github]
```

**Important:** Add `logo` to `structure.left` as the first item, otherwise the navbar logo renders but may not be positioned correctly in all pkgdown versions.

### Pattern 3: hexSticker (BRAND-01)

**What:** `hexSticker::sticker()` composites a subplot image + package name text + url into a standard hex shape.

**Key constraint:** `hexSticker` is NOT in DESCRIPTION. It is installed interactively by the executor (`install.packages("hexSticker")`) and used only in `data-raw/brand/make_sticker.R`. The committed output (`man/figures/logo-hex.png`) goes into the tarball; the R script and SVG source do not (they live in `.Rbuildignore`'d `data-raw/`).

```r
# data-raw/brand/make_sticker.R
# Run interactively: source("data-raw/brand/make_sticker.R")
# hexSticker must be installed but is NOT in DESCRIPTION

hexSticker::sticker(
  subplot    = "data-raw/brand/logo.svg",
  package    = "EventStudy",
  p_size     = 20,
  p_color    = "#ffffff",
  p_y        = 1.55,
  s_x        = 1,
  s_y        = 0.9,
  s_width    = 0.55,
  h_fill     = "#1e3a8a",   # primary-800
  h_color    = "#2563eb",   # primary-600
  url        = "eventstudy.de",
  u_size     = 6,
  u_color    = "#bfdbfe",   # primary-200
  filename   = "man/figures/logo-hex.png",
  dpi        = 300
)
```

**hexSticker subplot parameter:** Accepts a file path to an SVG, PNG, or ggplot object. Using the SVG path directly works in hexSticker ≥ 0.4.9 (uses `magick::image_read_svg()`). If magick SVG support is missing, fall back to rendering logo.svg → logo-tmp.png first via rsvg, then pass the PNG path.

**Size concern:** A 300 DPI hex sticker is large (≈ 2000×2312 px raw). Use `pngquant` or `optipng` after generation to compress to <50 KB. Alternatively, reduce DPI to 150 for the committed PNG (web display only needs ≤ 150 DPI).

### Pattern 4: rsvg PNG Export (BRAND-01)

```r
# data-raw/brand/make_logo.R
# Run interactively; rsvg is available via pkgdown
rsvg::rsvg_png(
  svg  = "data-raw/brand/logo.svg",
  file = "man/figures/logo.png",
  width  = 240,
  height = 240
)
# Assert size:
stopifnot(file.size("man/figures/logo.png") < 51200)  # 50 KB
```

`rsvg` is a lightweight binding to librsvg. It renders SVG faithfully without Inkscape. It is available on the executor's machine as a transitive dependency of pkgdown.

### Pattern 5: pkgdown::build_favicons() (BRAND-03)

```r
# Run interactively after logo.png is committed
pkgdown::build_favicons(pkg = ".", overwrite = TRUE)
```

This reads `man/figures/logo.png` as the source image and writes the favicon set to `pkgdown/favicon/`. The site build then picks them up automatically. Output files: `favicon-16x16.png`, `favicon-32x32.png`, `apple-touch-icon.png`, `android-chrome-192x192.png`, `android-chrome-512x512.png`, `site.webmanifest`.

**Prerequisite:** `man/figures/logo.png` must exist before running `build_favicons()`. The logo PNG (Wave 1) must therefore precede this step (Wave 3).

**Note:** `build_favicons()` requires the `magick` package (ImageMagick binding). Install if not present: `install.packages("magick")`. Like hexSticker, magick is dev-time only — NOT in DESCRIPTION.

### Pattern 6: extra.scss (new file) (BRAND-05, BRAND-06)

Create `pkgdown/extra.scss` (alongside the existing `extra.css`). pkgdown 2.0+ automatically compiles `extra.scss` if present.

```scss
// pkgdown/extra.scss
// Palette tokens matching eventstudy.de CSS (from UI-SPEC.md)

// ── Numeric-badge strip ──────────────────────────────────────
.es-stat-strip {
  display: flex;
  gap: 24px;
  margin: 32px 0;
  flex-wrap: wrap;
}

.es-stat-badge {
  display: flex;
  flex-direction: column;
  align-items: center;
  padding: 16px 24px;
  border: 2px solid #c45a10;
  border-radius: 8px;
  background: #fff0e5;
  min-width: 120px;
}

.es-stat-badge .stat-number {
  font-family: "Plus Jakarta Sans", sans-serif;
  font-size: 3rem;
  font-weight: 700;
  line-height: 1;
  color: #c45a10;
}

.es-stat-badge .stat-label {
  font-family: Inter, sans-serif;
  font-size: 0.875rem;
  font-weight: 500;
  color: #703b0c;
  margin-top: 4px;
  text-align: center;
}

// ── Ecosystem strip ──────────────────────────────────────────
.es-ecosystem-strip {
  display: flex;
  align-items: center;
  gap: 8px;
  font-size: 0.875rem;
  color: #64748b;
  margin-bottom: 32px;
  flex-wrap: wrap;
}

.es-ecosystem-label {
  font-weight: 500;
  color: #334155;
}

.es-ecosystem-sep {
  color: #cbd5e1;
}

.es-ecosystem-link {
  color: #2563eb;
  text-decoration: none;
  font-weight: 500;
  &:hover { text-decoration: underline; }
}

.es-ecosystem-link.es-ecosystem-current {
  color: #0f172a;
  font-weight: 600;
  cursor: default;
  pointer-events: none;
}

// ── Gallery: align to eventstudy.de primary colour ──────────
// Override existing extra.css link colour with the extracted primary
.es-gallery-title {
  color: #2563eb;  // was #0d6efd (BS5 default); now eventstudy.de primary
}

// ── Section headings: simplify to single primary colour ──────
// Phase 20 eliminates the 8-color category system in favour of brand primary
.es-section-heading {
  border-bottom-color: #2563eb !important;
  color: #0f172a !important;
}
```

**Why `!important` for section headings:** The existing `extra.css` uses class-specific selectors (`.es-section-heading.es-core { border-bottom-color: #0d6efd; }`) which have equal specificity. `!important` on the base class in `extra.scss` overrides all variants cleanly. Alternative: add the overrides directly to `extra.css` (removing the per-class rules) — but that requires editing the existing file. Using `!important` in `extra.scss` is the minimal-touch approach.

### Pattern 7: README Logo Placement (BRAND-02)

Per `usethis::use_logo()` pattern — add a right-aligned `<img>` tag at the very top of README.md, before the H1 heading:

```markdown
<img src="man/figures/logo.png" align="right" height="120" alt="EventStudy logo" />

# Event Study Analysis in R
```

pkgdown converts this to the correct HTML; GitHub renders it inline. Height of 120px is the standard for R package README logos.

### Pattern 8: Lifecycle Badge Flip (BRAND-07)

Current README.md line 8 [VERIFIED: README.md:8]:
```
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
```

Replace with:
```markdown
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
```

No `lifecycle` package required — this is a static shields.io badge URL, not a dynamic `lifecycle::badge()` call. No roxygen `@lifecycle` tags were found in R source files (grep confirmed zero hits) so no R-level changes needed.

### Anti-Patterns to Avoid

- **Listing hexSticker in DESCRIPTION Suggests:** Causes `R CMD check` NOTE about unavailable packages and pollutes the user-facing dependency list. Dev tools must stay entirely outside DESCRIPTION.
- **Putting SVG sources in man/figures/:** SVG files in `man/figures/` are checked into the CRAN tarball. Only the rendered PNG goes there. SVG source goes in `data-raw/brand/` which is `.Rbuildignore`'d.
- **Using a Bootswatch theme alongside bslib variables:** `bootswatch: lux` (or any) overrides `template.bslib` variables. Use ONLY `bslib:` custom variables — no Bootswatch.
- **Running `pkgdown::build_favicons()` in CI:** This runs ImageMagick and is slow. Run it once locally, commit the output to `pkgdown/favicon/`, and let pkgdown pick them up on rebuild.
- **Committing `pkgdown/extra.scss` without confirming pkgdown ≥ 2.0:** Earlier pkgdown ignores the file silently. The CI installs `any::pkgdown` (latest) so CI is safe; local dev must also use pkgdown ≥ 2.0.
- **Embedding non-ASCII characters in PNG alt text or YAML comments in R/ files:** `R CMD check --as-cran` flags non-ASCII in source. Use only ASCII in all R files and CI YAML.
- **Forgetting `data-raw/brand/` in `.Rbuildignore`:** Already present (`^data-raw$`), but the executor must verify the pattern matches the new `data-raw/brand/` subdirectory. A `^data-raw$` pattern excludes the entire `data-raw/` tree — confirmed.

---

## Don't Hand-Roll

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Hex sticker layout | Custom SVG compositing in R | `hexSticker::sticker()` | Handles hexagonal clipping, text positioning, standard R package conventions |
| SVG → PNG rendering | Inkscape CLI subprocess | `rsvg::rsvg_png()` | Lighter dep, no system binary needed, already transitive via pkgdown |
| Favicon set generation | Manual resize + ICO creation | `pkgdown::build_favicons()` | Generates all browser/platform sizes from one PNG; writes webmanifest |
| PNG compression | Custom R code | `optipng`/`pngquant` CLI (called from shell) | These are lossless/lossy optimisers; no R equivalent needed in the workflow |

---

## Common Pitfalls

### Pitfall 1: hexSticker SVG subplot fails silently
**What goes wrong:** `hexSticker::sticker(subplot = "path/to/logo.svg")` produces a blank or errored sticker with no useful message.
**Why it happens:** hexSticker uses `magick::image_read_svg()` internally, which requires ImageMagick compiled with SVG/RSVG support. Some installations lack this.
**How to avoid:** Test SVG rendering first: `magick::image_read_svg("data-raw/brand/logo.svg")`. If it throws, fall back: render to PNG first via `rsvg::rsvg_png()` then pass the PNG path to `hexSticker::sticker()`.
**Warning signs:** Blank white subplot area in the hex sticker output.

### Pitfall 2: Tarball bloat from man/figures/
**What goes wrong:** og-card.png (1200×630 px) or logo-hex.png (300 DPI) push the tarball over 1 MB.
**Why it happens:** Unoptimised PNG files from rsvg or hexSticker can be 300–800 KB each.
**How to avoid:** After generating each PNG, optimise: `system("pngquant --force --output man/figures/og-card.png man/figures/og-card.png")` or `system("optipng -o5 man/figures/logo.png")`. Assert sizes in the data-raw scripts before committing.
**Warning signs:** `file.size("man/figures/og-card.png") > 204800` (200 KB limit from UI-SPEC).

### Pitfall 3: pkgdown extra.scss not compiled
**What goes wrong:** The palette and new CSS constructs do not appear on the built site despite `extra.scss` existing.
**Why it happens:** pkgdown < 2.0 ignores `extra.scss`. Or: `extra.scss` must be in `pkgdown/` (not in a subdirectory).
**How to avoid:** Confirm `packageVersion("pkgdown") >= "2.0.0"`. The file must be exactly `pkgdown/extra.scss` — not `pkgdown/scss/extra.scss`.
**Warning signs:** Numeric badges render unstyled; bslib colours not applied.

### Pitfall 4: Section heading `!important` conflict
**What goes wrong:** The `!important` override in `extra.scss` overrides future intentional section-heading colour additions from later phases.
**Why it happens:** `!important` is a global hammer.
**How to avoid:** Alternatively, edit `extra.css` directly — remove the 8 per-class rules (`.es-section-heading.es-core { ... }` etc.) and replace with a single rule using the new primary colour. This is cleaner than `!important` but requires editing the existing file. The planner should decide which approach to specify.

### Pitfall 5: Navbar logo position
**What goes wrong:** Logo appears but the left nav items push it off to an unexpected position.
**Why it happens:** pkgdown's `navbar.structure.left` must explicitly include `logo` as the first token.
**How to avoid:** Update `navbar.structure.left` to `[logo, get-started, reference, articles, methods, gallery, examples]`. The existing `_pkgdown.yml` currently has `left: [get-started, reference, articles, methods, gallery, examples]` [VERIFIED: _pkgdown.yml:17-18] — `logo` must be prepended.

### Pitfall 6: Non-ASCII in CI non-ASCII grep
**What goes wrong:** The CI non-ASCII grep catches characters in R source comments (e.g., `'Kolari'-'Pynnönen'` in DESCRIPTION's Description: field, or Unicode in roxygen docs).
**Why it happens:** The grep guard targets `R/`, `man/figures/`, `inst/` — not `DESCRIPTION` or `man/*.Rd`. These already contain non-ASCII (e.g., `'Pynnönen'` in DESCRIPTION line 21 [VERIFIED: DESCRIPTION:21]).
**How to avoid:** Scope the grep guard to only `R/` and `man/figures/` (not `man/*.Rd`), since `Rd` files already allow UTF-8 escapes and CRAN requires `\enc{}` macros for non-ASCII only in `R/` files. DESCRIPTION already passes `R CMD check --as-cran` with the current non-ASCII — do not add DESCRIPTION to the grep scope.

### Pitfall 7: OG card in wrong location
**What goes wrong:** The pkgdown site does not render the OG card meta tags.
**Why it happens:** `opengraph.image.src` in `_pkgdown.yml` expects a path relative to the package root, and the file must exist in the built site's asset directory. pkgdown copies `man/figures/` into the site, so `man/figures/og-card.png` is the correct location (not `pkgdown/`).
**How to avoid:** Confirm the `src:` in `template.opengraph.image` points to `man/figures/og-card.png` (tarball-safe, copied into built site). Do NOT put the OG card in `pkgdown/` (it won't be in the built site assets).

---

## Code Examples

### CI: Tarball size assertion (CRAN-03)

Add to `.github/workflows/R-CMD-check.yaml`, as a new step after `check-r-package`:

```yaml
- name: Assert tarball < 1 MB
  if: runner.os == 'Linux'
  run: |
    R CMD build . --no-build-vignettes --no-manual 2>/dev/null
    TAR_SIZE=$(stat -c%s *.tar.gz 2>/dev/null || stat -f%z *.tar.gz)
    echo "Tarball size: ${TAR_SIZE} bytes"
    [ "$TAR_SIZE" -lt 1048576 ] || { echo "ERROR: Tarball exceeds 1 MB (${TAR_SIZE} bytes)"; exit 1; }
```

Run on Linux only (macOS `stat` syntax differs; `stat -f%z` works on macOS but `-c%s` does not). The `if: runner.os == 'Linux'` guard keeps it simple.

### CI: Non-ASCII guard (CRAN-04)

```yaml
- name: Assert no non-ASCII in R/ and man/figures/
  if: runner.os == 'Linux'
  run: |
    if grep -rP '[^\x00-\x7F]' R/ man/figures/ inst/ 2>/dev/null; then
      echo "ERROR: Non-ASCII characters found in R/, man/figures/, or inst/"
      exit 1
    fi
    echo "Non-ASCII check passed"
```

**Note:** `inst/` is included because report templates live there. DESCRIPTION and `man/*.Rd` are intentionally excluded — they already contain non-ASCII and pass `R CMD check --as-cran` legitimately.

### DESCRIPTION version bump (CRAN-02)

Change line 5 [VERIFIED: DESCRIPTION:5]:
```
Version: 0.64.0
```
to:
```
Version: 0.65.0
```

Also update `Date:` to the execution date.

### NEWS.md v0.65.0 section

Prepend to NEWS.md:

```markdown
# EventStudy 0.65.0

## Brand & Visual Identity

* New EventStudy logo (CAR-curve motif) at `man/figures/logo.png` and hex sticker
  at `man/figures/logo-hex.png` (#BRAND-01).
* Logo wired into README badge row and pkgdown navbar (#BRAND-02).
* Full favicon set generated via `pkgdown::build_favicons()` (#BRAND-03).
* Open Graph social-preview card configured for shared link thumbnails (#BRAND-04).
* pkgdown site palette and typography aligned to the eventstudy.de ecosystem
  brand via `template.bslib` and `pkgdown/extra.scss` (#BRAND-05).
* Home card gallery and numeric-badge strip restyled to ecosystem look (#BRAND-06).
* Lifecycle badge updated from `experimental` to `stable` (#BRAND-07).

## CRAN Hygiene

* Version bumped to 0.65.0 (#CRAN-02).
* CI tarball-size assertion (< 1 MB) and non-ASCII grep guard added (#CRAN-03, #CRAN-04).
```

---

## Environment Availability

| Dependency | Required By | Available | Version | Fallback |
|------------|------------|-----------|---------|----------|
| R | Everything | ✓ | 4.1.0+ (runtime) | — |
| `rsvg` R package | SVG → PNG rendering | likely ✓ (transitive via pkgdown) | CRAN current | Install: `install.packages("rsvg")` |
| `hexSticker` R package | Hex sticker generation | unknown — dev-time install needed | CRAN current | `install.packages("hexSticker")` — dev only |
| `magick` R package | hexSticker + build_favicons | likely ✓ (dep of hexSticker) | CRAN current | `install.packages("magick")`; needs ImageMagick system lib |
| `optipng` or `pngquant` | PNG compression to meet size targets | unknown | system CLI | Skip if logo.png < 50 KB without it; may not be needed for the 240×240 logo |
| `pkgdown` ≥ 2.0 | extra.scss compilation + build_favicons | ✓ (in CI; confirm locally) | ≥ 2.0 required | Upgrade: `install.packages("pkgdown")` |

**Missing dependencies with no fallback:** None identified — all have install paths.

**Missing dependencies with fallback:** `optipng`/`pngquant` — if PNG files are already within size targets after generation, compression tools are optional.

---

## State of the Art

| Old Approach | Current Approach | When Changed | Impact |
|--------------|------------------|--------------|--------|
| No pkgdown extra.scss (scss compilation) | pkgdown 2.0+ compiles `pkgdown/extra.scss` automatically | pkgdown 2.0 (2023) | Can use Sass nesting (`&:hover`) and variables alongside extra.css |
| `template.params: bootswatch: X` | `template.bslib: { bg: ..., fg: ... }` | pkgdown 1.6 / bslib 0.4 | Fine-grained BS5 variable control without Bootswatch |
| `template.assets:` custom CSS path | `pkgdown/extra.css` + `pkgdown/extra.scss` (auto-detected) | pkgdown 2.0 | Auto-detection; no explicit path declaration needed |
| Navbar logo via custom HTML | `navbar.logo: { image: ..., href: ... }` | pkgdown 2.0 | Native support; no workaround needed |
| Google Fonts via `@import` in extra.css | `template.bslib.base-font.google: "Inter"` | bslib 0.5 | bslib handles font loading; no manual @import needed |

**Deprecated/outdated:**
- `template.params:` key in older pkgdown: replaced by `template.bslib:` for BS5 theming.
- `usethis::use_logo()` function: useful for understanding the pattern but not strictly needed — it just writes the `<img>` tag to README and adds `.Rbuildignore` entries. The executor can do this manually (the `.Rbuildignore` already excludes `data-raw`; `man/figures/` is already in-tarball).

---

## Validation Architecture

Nyquist validation note: Phase 20 is a site/asset/metadata phase with no statistical logic. Testable behaviors are structural (file existence, file size, YAML validity) rather than functional unit tests.

### Test Framework

| Property | Value |
|----------|-------|
| Framework | testthat 3e (existing) |
| Config file | `tests/testthat.R` |
| Quick run command | `Rscript -e "testthat::test_dir('tests/testthat')"` |
| Full suite command | `Rscript -e "devtools::test()"` |

### Phase Requirements → Test Map

| Req ID | Behavior | Test Type | Automated Command | File Exists? |
|--------|----------|-----------|-------------------|-------------|
| BRAND-01 | `man/figures/logo.png` exists and < 50 KB | File-existence | `stopifnot(file.exists("man/figures/logo.png"), file.size("man/figures/logo.png") < 51200)` in make_logo.R | ❌ Wave 0 (assertion in data-raw script, not a testthat test) |
| BRAND-01 | `man/figures/logo-hex.png` exists and < 50 KB | File-existence | Same pattern in make_sticker.R | ❌ Wave 0 |
| BRAND-03 | `pkgdown/favicon/` contains expected files | File-existence | Manual inspection after build_favicons() | ❌ Manual verify |
| CRAN-03 | Tarball < 1 MB | CI step | `R CMD build` + `stat` (CI YAML step documented above) | ❌ Wave 4 (add to CI YAML) |
| CRAN-04 | No non-ASCII in R/man/figures/inst/ | CI step | `grep -rP '[^\x00-\x7F]' R/ man/figures/ inst/` (CI YAML step) | ❌ Wave 4 |
| BRAND-07 | Lifecycle badge says `stable` | Grep | `grep "lifecycle-stable" README.md` | ❌ Manual / grep |
| All | Full test suite stays green | Suite | `Rscript -e "devtools::test()"` | ✓ existing |

### Wave 0 Gaps

- No new testthat test files needed for Phase 20 (no R code changes).
- File-existence assertions are embedded in `data-raw/brand/` scripts and verified by inspection.
- CI YAML steps for tarball size + non-ASCII are the primary automated validators.

---

## Security Domain

Phase 20 is a documentation/asset phase with no user-facing inputs, authentication, session management, or data processing. ASVS categories are not applicable.

| ASVS Category | Applies | Rationale |
|---------------|---------|-----------|
| V2 Authentication | No | Static docs site, no auth |
| V3 Session Management | No | Static site |
| V4 Access Control | No | Static site |
| V5 Input Validation | No | No user inputs in Phase 20 |
| V6 Cryptography | No | No secrets in Phase 20 |

**One security-adjacent concern:** The OG card and favicon assets are committed to the repo and served from GitHub Pages. No secrets, keys, or PII should appear in any asset. The non-ASCII grep guard also catches accidental credential strings with special characters.

---

## Runtime State Inventory

Phase 20 is not a rename/refactor/migration phase. Omitted per instructions.

---

## Assumptions Log

| # | Claim | Section | Risk if Wrong |
|---|-------|---------|---------------|
| A1 | `rsvg` is available as a transitive dependency of pkgdown on the executor's machine | Environment Availability | Executor must `install.packages("rsvg")` manually if missing; low risk |
| A2 | `pkgdown::build_favicons()` requires the `magick` package (ImageMagick binding) | Pattern 5 | If wrong, different dep needed; magick is listed as a pkgdown Suggests so this is likely correct |
| A3 | `hexSticker` ≥ 0.4.9 accepts an SVG path for `subplot` via magick | Pattern 3 | If wrong, must render SVG → PNG first; fallback documented |
| A4 | `optipng`/`pngquant` may not be installed; PNG files may be within size limits without them | Environment Availability | If logo-hex.png > 50 KB, a compression tool is required |
| A5 | The `.Rbuildignore` pattern `^data-raw$` excludes the entire data-raw/ tree including the new data-raw/brand/ subdirectory | Don't Hand-Roll / Pitfalls | Verified: .Rbuildignore contains `^data-raw$` [VERIFIED: .Rbuildignore line 12]; regex anchors match the directory and all contents |

---

## Open Questions

1. **Editing `extra.css` vs using `!important` in `extra.scss` for section heading overrides**
   - What we know: The existing `extra.css` has 8 per-class `.es-section-heading.es-X` colour rules. They have equal specificity to a base `.es-section-heading` rule.
   - What's unclear: Whether the planner prefers minimal-touch (`!important` in new `extra.scss`) or clean-edit (remove per-class rules from `extra.css`).
   - Recommendation: Edit `extra.css` directly — remove the 8 per-class colour rules, keep the structural `.es-section-heading` block, add `border-bottom-color: #2563eb; color: #0f172a;` to it. Cleaner than `!important`.

2. **OG card generation method**
   - What we know: The OG card needs to be 1200×630 px with specific layout (logo + text). SVG composition is specified in UI-SPEC.md.
   - What's unclear: Whether the executor should use SVG → rsvg rendering, or compose directly in R with `magick` (image compositing).
   - Recommendation: Author an `og-card.svg` at 1200×630 (with embedded logo SVG via `<image>` tag) and render with `rsvg::rsvg_png()`. Simpler than magick compositing.

3. **Gallery home page location for numeric badges**
   - What we know: UI-SPEC says "home page Rmd" or `index.md`. The current site uses `vignettes/gallery.Rmd` for the gallery page.
   - What's unclear: Whether the badges go on `vignettes/gallery.Rmd` (which has the card gallery) or on a dedicated `index.md` home page.
   - Recommendation: Add the numeric-badge strip and ecosystem strip to `vignettes/gallery.Rmd` near the top, above the existing gallery grid. This is where the card gallery already lives — co-locating the badges is consistent with the eventstudy.de layout.

---

## Sources

### Primary (HIGH confidence — in-session file reads)

- `_pkgdown.yml` [VERIFIED: _pkgdown.yml:1-304] — existing template, navbar, reference structure confirmed
- `pkgdown/extra.css` [VERIFIED: pkgdown/extra.css:1-147] — existing gallery CSS confirmed; `.es-gallery-title` colour at line 82 is `#0d6efd` (to be updated to `#2563eb`)
- `DESCRIPTION` [VERIFIED: DESCRIPTION:1-69] — Version: 0.64.0 at line 5; no hexSticker/rsvg/magick in Imports/Suggests
- `README.md:8` [VERIFIED: README.md:8] — lifecycle badge URL confirmed as experimental-orange
- `.Rbuildignore` [VERIFIED: .Rbuildignore:12] — `^data-raw$` pattern present; `^pkgdown$` present
- `20-CONTEXT.md` [VERIFIED: 20-CONTEXT.md:1-153] — all locked decisions read
- `20-UI-SPEC.md` [VERIFIED: 20-UI-SPEC.md:1-597] — palette tokens, typography, component contracts, hex sticker call confirmed

### Secondary (MEDIUM confidence — training knowledge, consistent with in-session evidence)

- pkgdown `template.bslib` and `navbar.logo` API — consistent with pkgdown 2.0 documentation pattern; the existing `_pkgdown.yml` already uses `template: bootstrap: 5` confirming pkgdown 2.x compatibility
- `hexSticker::sticker()` parameter names — consistent with CRAN package documentation
- `pkgdown::build_favicons()` signature — consistent with pkgdown 2.x API

### Tertiary (LOW confidence — assumed)

- `rsvg::rsvg_png()` being available as a transitive pkgdown dep — plausible but not confirmed by reading pkgdown DESCRIPTION this session [ASSUMED]
- `build_favicons()` requiring `magick` — plausible; magick is in pkgdown Suggests [ASSUMED]

---

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH — all tooling is well-established; decisions locked in CONTEXT.md
- Architecture: HIGH — file paths verified by in-session reads; .Rbuildignore state confirmed
- Pitfalls: HIGH — based on verified file state (existing colours, existing YAML structure)
- Package legitimacy: N/A — no new DESCRIPTION dependencies in Phase 20

**Research date:** 2026-09-08
**Valid until:** 2026-11-08 (60 days — pkgdown API is stable; bslib API stabilised in 0.5)
