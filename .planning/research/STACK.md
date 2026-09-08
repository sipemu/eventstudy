# Stack Research

**Domain:** R package polish — brand identity, publication-grade output, and pkgdown theming for EventStudy v0.65.0
**Researched:** 2026-09-08
**Confidence:** MEDIUM (CRAN pages verified for all versions; pkgdown/bslib theming verified via official docs; table package comparison cross-checked via multiple sources; hex sticker workflow verified via hexSticker CRAN + usethis docs)

> **Supersedes** the v0.64.0 STACK.md (multi-format report rendering). The v0.64.0 decisions (rmarkdown, knitr, tinytex, pandoc guard pattern) remain in force and are not repeated here. This file covers only what is NEW for v0.65.0 polish work.

---

## Context: What Already Exists

The following are already in EventStudy's `Imports` or `Suggests` and must not be re-added:

| Package | Status | Role |
|---------|--------|------|
| `ggplot2` | **Imports** | All plot functions — `plot_event_study()`, `plot_stocks()`, `plot_diagnostic()` |
| `plotly` | **Imports** | Interactive plots |
| `rmarkdown` | Suggests | `generate_report()` / `es_report()` renderer |
| `knitr` | Suggests | Vignette builder + report code chunks |
| `tinytex` | Suggests | PDF toolchain detection |
| `DT` | Suggests | Interactive HTML tables (existing, unused in reports) |

`scales` is a transitive dependency of `ggplot2` and is already present on any machine that has EventStudy installed — no DESCRIPTION entry needed.

The pkgdown site already uses Bootstrap 5 (`template: bootstrap: 5`) and has a custom `_pkgdown.yml` with grouped reference, articles nav, and navbar. There is currently no logo or hex sticker. The `pkgdown/` directory is `.Rbuildignore`d per the v0.62.0 decisions.

---

## Area A: Brand Identity — Logo + Hex Sticker

### A1. hexSticker (dev-only, never in DESCRIPTION)

| Attribute | Value |
|-----------|-------|
| Package | `hexSticker` |
| CRAN version | **0.5.1** (2026-01-21) |
| R requirement | >= 3.4.0 |
| Hard imports | ggimage, ggplot2, grDevices, hexbin, rlang, showtext, sysfonts |
| Suggests | magick (optional, for post-processing) |
| System requirements | None (NeedsCompilation: no) |
| Classification | **Dev-only** — never add to EventStudy DESCRIPTION |

**Why hexSticker:** The `sticker()` function accepts a ggplot2 object, base plot, or image file as the subplot; the filename extension determines whether PNG or SVG is emitted. SVG output is supported by passing `filename = "logo.svg"`. It handles font loading via sysfonts/showtext internally, so custom Google Fonts or local fonts work without separate setup. No ImageMagick required — `magick` is Suggests-only.

**Hex design workflow:**
1. Write `inst/logo/logo.R` — a reproducible R script that calls `hexSticker::sticker()`.
2. Output to `man/figures/logo.png` (181×209 px — the CRAN-acceptable standard) and optionally `man/figures/logo.svg` (preferred for build_favicons).
3. Run `usethis::use_logo("man/figures/logo.png")` once: it scales the image, generates the README markdown badge snippet, and adds `pkgdown/` to `.Rbuildignore`.
4. Run `pkgdown::build_favicons()` once (requires network — calls realfavicongenerator.net API): stores a full favicon set in `pkgdown/favicon/`; `init_site()` copies it on each rebuild.

**CRAN tarball implications:**
- `man/figures/logo.png` at 181×209 px is ~10–30 KB — CRAN-acceptable, no NOTE.
- `inst/logo/logo.R` is source code, fine to include.
- `pkgdown/favicon/` is `.Rbuildignore`d — stays out of tarball.
- The large print-quality SVG/PNG (if generated) should live in `inst/logo/` or `tools/`, not `man/figures/`.

### A2. usethis (dev-only)

| Attribute | Value |
|-----------|-------|
| Package | `usethis` |
| Role | `use_logo()` — places logo, generates README img tag, manages `.Rbuildignore` |
| Classification | **Dev-only** — already used for package setup, no new DESCRIPTION entry |

### A3. svglite (dev-only, optional)

| Attribute | Value |
|-----------|-------|
| Package | `svglite` |
| CRAN version | **2.2.2** (2025-10-21) |
| Role | SVG graphics device for generating vector ggplot2 output for the hex sticker or site figures |
| Classification | **Dev-only** — only needed when authoring the logo, not shipped in the package |

Use `svglite::svglite("logo.svg")` if hexSticker's built-in SVG output needs font embedding control that `showtext` doesn't provide. In practice, hexSticker's own SVG output (via `filename = "logo.svg"`) is sufficient.

---

## Area B: Publication-Grade Tables in es_report()

### B1. Recommendation: tinytable as Suggests

**Use `tinytable` for all result/diagnostics tables in `es_report()` output across all four formats (HTML, LaTeX/PDF, Word, Markdown).**

| Attribute | Value |
|-----------|-------|
| Package | `tinytable` |
| CRAN version | **0.18.0** (2026-08-20) |
| R requirement | >= 4.1.0 |
| Hard imports | methods (base R only) |
| Suggests (all optional) | 21 packages including knitr, rmarkdown, ggplot2, tinytex, webshot2 |
| Formats supported | HTML, LaTeX, Markdown, Word, PNG, PDF, Typst |
| System requirements | None |
| Classification | **Suggests** — add to EventStudy DESCRIPTION Suggests; guard with `requireNamespace("tinytable", quietly = TRUE)` |

**Why tinytable over alternatives:**

| Criterion | tinytable | gt | kableExtra | flextable |
|-----------|-----------|-----|------------|-----------|
| HTML | Yes | Yes (best) | Yes | Yes |
| LaTeX/PDF | Yes (tabularray) | Yes (as_latex) | Yes | No |
| Word | Yes | Yes (as_word) | No | Yes (best) |
| Markdown | Yes | No | No | No |
| Hard deps | 1 (methods) | 17 | ~10 | 13 |
| Total transitive deps | ~0 | ~60 | ~48 | ~57 |
| Maintenance trajectory | Active, growing | Active | Slowing | Active |
| CRAN tarball risk | None | High | Medium | High |

tinytable was explicitly designed for R package developers who need to avoid dependency chains and upstream breaking changes. It covers all four es_report() output formats from a single API with zero mandatory dependencies. gt is more powerful for HTML-only scenarios but brings ~60 transitive deps; its Word output is available but not its primary strength. kableExtra is HTML+LaTeX only and its regex-based architecture is acknowledged as hard to maintain. flextable has no LaTeX output.

**Integration point:** In `R/report.R` `generate_report()`, replace `knitr::kable()` calls (if any) with `tinytable::tt()` wrapped in a `requireNamespace("tinytable")` guard. The table output automatically adapts to the active knitr output format.

**Fallback:** When tinytable is not installed, fall back to `knitr::kable()` (already available via the existing knitr Suggests). This maintains the offline-first principle.

```r
# Pattern for es_report() table output
.render_table <- function(df, caption = NULL) {
  if (requireNamespace("tinytable", quietly = TRUE)) {
    tinytable::tt(df, caption = caption)
  } else {
    knitr::kable(df, caption = caption)
  }
}
```

### B2. Do NOT add gt, kableExtra, or flextable

| Avoid | Why | Use Instead |
|-------|-----|-------------|
| `gt` in Suggests | ~60 transitive deps; heavy; Word/LaTeX not primary strengths | `tinytable` |
| `kableExtra` in Suggests | HTML+LaTeX only; ~48 deps; maintenance slowing; no Word | `tinytable` + knitr::kable() fallback |
| `flextable` in Suggests | No LaTeX; 57 deps; only adds value for Word-specific rich formatting not needed here | `tinytable` for Word tables |
| `officer` in Suggests | Only needed as a flextable companion for Word; tinytable handles Word natively | Not needed |

---

## Area C: ggplot2 Publication Figures

### C1. Custom theme function (zero new deps — built on existing ggplot2 Imports)

Create `theme_eventstudy()` in `R/plotting.R` or a new `R/theme.R`. This adds zero dependencies — ggplot2 is already a hard Import.

```r
#' @export
theme_eventstudy <- function(base_size = 11, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
  ggplot2::theme(
    panel.grid.minor  = ggplot2::element_blank(),
    panel.grid.major  = ggplot2::element_line(colour = "#e5e7eb", linewidth = 0.4),
    axis.line         = ggplot2::element_line(colour = "#6b7280", linewidth = 0.5),
    plot.title        = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.1)),
    plot.subtitle     = ggplot2::element_text(colour = "#6b7280"),
    legend.position   = "bottom",
    strip.text        = ggplot2::element_text(face = "bold")
  )
}
```

Apply via `ggplot2::theme_set(theme_eventstudy())` at the top of each report template.

### C2. scales (already a transitive dep — no new DESCRIPTION entry)

`scales` 1.4.0 (2025-04-24) is already present as a transitive dependency of ggplot2. No DESCRIPTION change needed. Use for:

- `scales::label_percent(accuracy = 0.01)` on CAR/AAR y-axes
- `scales::label_comma()` for volume axes
- `scales::percent_format()` for return axes
- `scales::hue_pal()` / `viridis_pal()` for consistent color schemes

### C3. patchwork (add to Suggests)

| Attribute | Value |
|-----------|-------|
| Package | `patchwork` |
| CRAN version | **1.3.2** (2025-08-25) |
| Hard imports | ggplot2 >= 3.0.0, gtable, grid, rlang, cli, farver |
| Suggests | ragg, gt, etc. (all optional) |
| System requirements | None |
| Classification | **Suggests** — add to EventStudy DESCRIPTION Suggests; guard with `requireNamespace("patchwork", quietly = TRUE)` |

**Why patchwork:** For multi-panel report figures (e.g., AAR time series + CAR distribution side by side in the report). The `+` operator and `plot_layout()` are idiomatic and well-understood. Pure R, no system deps, lightweight transitive footprint. Guarded as Suggests since most users of the basic pipeline don't need multi-panel composition.

**Integration point:** In the report Rmd template, when building the results/diagnostics section composite figure:

```r
if (requireNamespace("patchwork", quietly = TRUE)) {
  p1 + p2 + patchwork::plot_layout(ncol = 2) +
    patchwork::plot_annotation(tag_levels = "A")
}
```

### C4. ragg (add to Suggests — for report knitr device)

| Attribute | Value |
|-----------|-------|
| Package | `ragg` |
| CRAN version | **1.5.2** (2026-03-23) |
| Hard imports | systemfonts >= 1.0.3, textshaping >= 0.3.0 |
| System requirements | freetype2, libpng, libtiff, libjpeg, libwebp |
| Classification | **Suggests** — add to EventStudy DESCRIPTION Suggests |

**Why ragg:** When present, ragg provides anti-aliased text rendering and proper font access in PNG figures inside reports. `ggsave()` uses ragg automatically if installed. For the report Rmd template, set `knitr::opts_chunk$set(dev = "ragg_png")` when ragg is available — this improves all figure output quality. pkgdown already imports ragg directly, so it will always be present in the site build environment.

**Important limitation:** ragg does not support PDF output — PDF figures in rmarkdown PDF reports use the standard `cairo_pdf` or `pdf` device. The ragg Suggests entry only affects HTML/Word/Markdown report figure quality.

**System dep note:** ragg requires freetype2, libpng, libtiff, libjpeg, libwebp at the system level. On most Linux/macOS developer machines these are already present. On CRAN check systems they are present. This is an acceptable Suggests classification because CRAN does not require Suggests deps to build cleanly on all systems — failure to install ragg simply means better font rendering is unavailable, not that the package fails.

**Integration point:** In the report Rmd template header:

```r
if (requireNamespace("ragg", quietly = TRUE)) {
  knitr::opts_chunk$set(dev = "ragg_png", dpi = 150)
}
```

### C5. systemfonts (Suggests, indirect — via ragg)

| Attribute | Value |
|-----------|-------|
| Package | `systemfonts` |
| CRAN version | **1.3.2** (2026-03-05) |
| Hard imports | base64enc, grid, jsonlite, lifecycle, tools, utils |
| Classification | **Do not add to DESCRIPTION directly** — it arrives as a hard import of ragg; if ragg is in Suggests, systemfonts is available whenever ragg is |

No direct DESCRIPTION entry needed. If the package theme function needs a named font family, use `base_family = ""` (system default) rather than hardcoding a font name that may not be installed. Font selection is a report-template-level concern, not a package dep concern.

### C6. ggthemes (do NOT add — unnecessary dep)

ggthemes 5.2.0 provides `theme_economist()`, `theme_wsj()` etc. These are aesthetically appealing for financial data but add a dependency for style-only benefit. The custom `theme_eventstudy()` function (zero deps, built on ggplot2) is the correct approach for a branded package theme. **Do not add ggthemes.**

---

## Area D: pkgdown Site Theming

All pkgdown configuration lives in `_pkgdown.yml` and `pkgdown/extra.scss`. Nothing in this area touches the CRAN tarball (pkgdown/ is `.Rbuildignore`d). No new DESCRIPTION entries are needed for any of the following.

### D1. pkgdown (dev-only, already present)

| Attribute | Value |
|-----------|-------|
| Package | `pkgdown` |
| CRAN version | **2.2.1** (2026-07-07) |
| Hard imports | bslib >= 0.5.1, ragg, rmarkdown >= 2.27, xml2 |
| System requirements | pandoc >= 2.10.1 |
| Classification | **Dev-only** — already in use; no DESCRIPTION change |

### D2. Logo + Favicon wiring in pkgdown

Place the generated hex logo at `man/figures/logo.png` (or `man/figures/logo.svg`). pkgdown 2.2.1 auto-discovers either filename and places it in the navbar. No `_pkgdown.yml` configuration needed for logo placement — the file location is the convention.

For favicons, run once during site setup (requires network):

```r
pkgdown::build_favicons()  # calls realfavicongenerator.net API
# Stores pkgdown/favicon/ — already .Rbuildignore'd
```

`init_site()` copies the favicon set on each rebuild. `build_favicons(overwrite = TRUE)` to regenerate after a logo change.

### D3. bslib + _pkgdown.yml theming to match eventstudy.de

bslib 0.12.0 (2026-08-04) is the engine behind pkgdown Bootstrap 5 theming. The package author never imports bslib in EventStudy's DESCRIPTION — bslib is consumed by pkgdown, which is already dev-only.

**Recommended `_pkgdown.yml` template section** (extending the existing Bootstrap 5 config):

```yaml
template:
  bootstrap: 5
  math-rendering: katex
  bslib:
    # Match eventstudy.de neutral palette (clean, card-based)
    primary:        "#2563eb"   # main action color — adjust to match site
    bg:             "#ffffff"
    fg:             "#111827"
    border-radius:  "0.375rem"
    # Typography — Google Fonts for web consistency
    base_font:      {google: "Inter"}
    heading_font:   {google: "Inter"}
    code_font:      {google: "JetBrains Mono"}
  light-switch: true            # dark/light toggle in navbar
```

**Navbar color** is set via:

```yaml
navbar:
  bg: light     # or: dark | primary | secondary
  type: light
```

**Fine-grained overrides** go in `pkgdown/extra.scss` (compiled into main CSS, can reference Sass variables):

```scss
// Card gallery styling to match eventstudy.de numeric badge cards
.card-badge {
  font-size: 2rem;
  font-weight: 700;
  color: $primary;
}
.section-cards {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 1rem;
}
```

Use `pkgdown/extra.scss` (not `pkgdown/extra.css`) because SCSS is compiled into the main CSS and can reference `$primary`, `$body-bg` and other bslib variables — giving consistent brand colors without hardcoding hex values.

### D4. _brand.yml (optional — use if cross-tool identity is a priority)

bslib 0.12.0 supports a `_brand.yml` file for unified cross-tool theming (pkgdown, Shiny, R Markdown). For EventStudy's scope, this is optional: the eventstudy.de brand is managed by the website team (not this R package), and maintaining a separate `_brand.yml` adds overhead without benefit unless the package site is expected to stay in exact pixel-perfect alignment with the external site.

**If used**, wire via:

```yaml
# _pkgdown.yml
template:
  bslib:
    version: 5
    brand: pkgdown/_brand.yml  # path relative to package root
```

`_brand.yml` supports `colors`, `typography` (fonts), and `logos` (small/medium/large variants). It does NOT replace `template.bslib` settings — it is a layer that feeds into them.

**Verdict:** Skip `_brand.yml` for v0.65.0. Use explicit `template.bslib` settings in `_pkgdown.yml` instead — simpler, fully version-controlled in one file, no ambiguity about which file takes precedence.

---

## Summary: DESCRIPTION Changes Required

| Package | Change | Classification | Reason |
|---------|--------|----------------|--------|
| `tinytable` | Add to Suggests | Suggests | Multi-format tables in es_report(); zero hard deps; `requireNamespace()`-guarded |
| `patchwork` | Add to Suggests | Suggests | Multi-panel publication figures in reports; lightweight; guarded |
| `ragg` | Add to Suggests | Suggests | Anti-aliased figures in HTML/Word/MD reports; guarded; improves visual quality |
| `hexSticker` | Do NOT add | Dev-only | Logo authoring tool; never a package dep |
| `usethis` | Do NOT add | Dev-only | Already used for package setup; not a dep |
| `svglite` | Do NOT add | Dev-only | Only needed during logo SVG authoring |
| `bslib` | Do NOT add | Dev-only via pkgdown | pkgdown consumes it; package DESCRIPTION never needs it |
| `systemfonts` | Do NOT add | Transitive via ragg | Arrives automatically when ragg is installed |
| `scales` | Do NOT add | Transitive via ggplot2 | Already present on any EventStudy installation |
| `gt` | Do NOT add | Rejected | ~60 transitive deps; tinytable covers all needed formats better for a Suggests dep |
| `kableExtra` | Do NOT add | Rejected | HTML+LaTeX only; maintenance slowing; tinytable is strictly better |
| `flextable` | Do NOT add | Rejected | 57 deps; no LaTeX; tinytable covers Word tables adequately |
| `ggthemes` | Do NOT add | Rejected | Style-only; custom theme_eventstudy() is zero-dep and brand-aligned |
| `officer` | Do NOT add | Rejected | Needed only as a flextable companion; not required |

```
# Minimal DESCRIPTION Suggests additions for v0.65.0:
Suggests:
    ...existing entries...,
    tinytable,
    patchwork,
    ragg
```

---

## Development Tooling (not in DESCRIPTION)

| Tool | Purpose | Install |
|------|---------|---------|
| `hexSticker` 0.5.1 | Generate hex logo PNG/SVG | `install.packages("hexSticker")` |
| `usethis` | Wire logo into README + .Rbuildignore | `install.packages("usethis")` |
| `svglite` 2.2.2 | SVG output device if needed for logo work | `install.packages("svglite")` |
| `pkgdown` 2.2.1 | Build docs site; `build_favicons()` | `install.packages("pkgdown")` |
| `bslib` 0.12.0 | Consumed by pkgdown for Bootstrap 5 theming | Installed automatically with pkgdown |

---

## Alternatives Considered

| Recommended | Alternative | Why Not |
|-------------|-------------|---------|
| `tinytable` (Suggests) | `gt` (Suggests) | ~60 transitive deps — too heavy for a Suggests table package; tinytable covers all four formats |
| `tinytable` (Suggests) | `kableExtra` (Suggests) | HTML+LaTeX only; no Word; maintenance slowing |
| `tinytable` (Suggests) | `flextable` (Suggests) | 57 deps; no LaTeX; only better for rich Word-only scenarios not needed here |
| `theme_eventstudy()` in R/ | `ggthemes` (Suggests) | Style-only dep; a custom function built on existing ggplot2 Import is zero-dep and fully brand-controlled |
| `pkgdown/extra.scss` | `pkgdown/extra.css` | SCSS compiles into main CSS and can reference bslib Sass variables — far more powerful for brand alignment |
| `template.bslib` in _pkgdown.yml | `_brand.yml` | Simpler; single-file; no cross-tool identity requirement for the R package site at this stage |
| `hexSticker` (dev-only) | Manual SVG in Inkscape/Figma | Reproducible R code is version-controllable and regenerable; design tool assets are binary blobs |
| `man/figures/logo.png` | Top-level `logo.png` | `man/figures/` is the standard location per Writing R Extensions; pkgdown and roxygen2 both discover it there |

---

## Version Compatibility

| Package | Version Used | Compatible With | Notes |
|---------|-------------|-----------------|-------|
| `tinytable` | >= 0.18.0 | R >= 4.1.0 | Zero hard deps; no version-sensitive API surface |
| `patchwork` | >= 1.3.0 | ggplot2 >= 3.0.0 | `+` operator and `plot_layout()` stable across this range |
| `ragg` | >= 1.5.0 | systemfonts >= 1.0.3, textshaping >= 0.3.0 | System lib requirements (freetype2 etc.) standard on CRAN check servers |
| `hexSticker` | >= 0.5.1 | R >= 3.4.0 | Dev-only; showtext/sysfonts bundled as imports |
| `pkgdown` | >= 2.2.1 | bslib >= 0.5.1, pandoc >= 2.10.1 | `build_favicons()` requires network; logo auto-discovery from 2.x |
| `bslib` | >= 0.12.0 | Bootstrap 5 | brand.yml support added in 0.12.0 (if used) |

---

## Sources

- CRAN hexSticker — version 0.5.1, 2026-01-21, imports/suggests verified (LOW/web): https://cran.r-project.org/web/packages/hexSticker/index.html
- hexSticker sticker() function — filename-extension-driven SVG/PNG output confirmed (LOW/web): https://rdrr.io/cran/hexSticker/man/sticker.html
- usethis use_logo() reference — man/figures/logo.png convention, README wiring, .Rbuildignore management (LOW/web): https://usethis.r-lib.org/reference/use_logo.html
- pkgdown build_home — logo auto-discovery at man/figures/logo.png (LOW/web): https://pkgdown.r-lib.org/reference/build_home.html
- pkgdown build_favicons — realfavicongenerator.net API, pkgdown/favicon/ storage, SVG preferred (LOW/web): https://pkgdown.r-lib.org/reference/build_favicons.html
- pkgdown Customise — extra.css vs extra.scss, template.bslib options, bootswatch, light-switch (LOW/web): https://pkgdown.r-lib.org/articles/customise.html
- CRAN pkgdown — version 2.2.1, 2026-07-07, bslib >= 0.5.1 import confirmed (LOW/web): https://cran.r-project.org/web/packages/pkgdown/index.html
- CRAN tinytable — version 0.18.0, 2026-08-20, imports: methods only (LOW/web): https://cran.r-project.org/web/packages/tinytable/index.html
- tinytable alternatives comparison — tinytable vs gt vs kableExtra vs flextable (LOW/web): https://vincentarelbundock.github.io/tinytable/vignettes/alternatives.html
- CRAN gt — version 1.3.0, 2026-01-22, 17 imports + 18 suggests (LOW/web): https://cran.r-project.org/web/packages/gt/refman/gt.html
- CRAN flextable — version 0.10.0, 2026-07-07, 13 imports confirmed (LOW/web): https://cran.r-project.org/web/packages/flextable/index.html
- CRAN patchwork — version 1.3.2, 2025-08-25, ggplot2 >= 3.0.0 import (LOW/web): https://cran.r-project.org/web/packages/patchwork/index.html
- CRAN ragg — version 1.5.2, 2026-03-23, system requirements freetype2/libpng/libtiff/libjpeg/libwebp (LOW/web): https://cran.r-project.org/package=ragg
- CRAN systemfonts — version 1.3.2, 2026-03-05 (LOW/web): https://cran.r-project.org/web/packages/systemfonts/index.html
- CRAN scales — version 1.4.0, 2025-04-24 (LOW/web): https://cran.r-project.org/web/packages/scales/index.html
- Tidyverse fonts blog — ragg + systemfonts 2025 canonical workflow, textshaping, PDF limitation (LOW/web): https://tidyverse.org/blog/2025/05/fonts-in-r/
- CRAN svglite — version 2.2.2, 2025-10-21 (LOW/web): https://cran.r-project.org/package=svglite
- CRAN bslib — version 0.12.0, 2026-08-04, brand.yml support (LOW/web): https://cran.r-project.org/web/packages/bslib/index.html
- bslib brand.yml article — _brand.yml fields (colors, fonts, logos), pkgdown integration via template.bslib.brand (LOW/web): https://rstudio.github.io/bslib/articles/brand-yml/index.html
- Nan Xiao hex sticker blog — inst/logo/logo.R source pattern, man/figures/logo.png output, usethis wiring (LOW/web): https://nanx.me/blog/post/rebranding-r-packages-with-hexagon-stickers/

---
*Stack research for: EventStudy v0.65.0 Polish — brand identity, publication output, pkgdown theming*
*Researched: 2026-09-08*
