---
phase: 20-brand-visual-identity
plan: 02
subsystem: pkgdown-site-theme
tags: [pkgdown, bslib, branding, favicon, opengraph]
status: complete
requires:
  - "man/figures/logo.png (from 20-01)"
  - "data-raw/brand/logo.svg (from 20-01)"
provides:
  - "_pkgdown.yml template.bslib palette + Google fonts"
  - "_pkgdown.yml navbar.logo + logo-first structure.left"
  - "_pkgdown.yml template.opengraph block"
  - "pkgdown/favicon/* full favicon set + site.webmanifest"
  - "man/figures/og-card.png (1200x630 OG social card)"
  - "data-raw/brand/og-card.svg + make_og_card.R (OG card source)"
affects:
  - "20-03 (home page / README styles on top of this site theme)"
tech-stack:
  added: []
  patterns:
    - "pkgdown::build_favicons() via realfavicongenerator.net API (pkgdown 2.2.0)"
    - "rsvg-convert CLI for SVG->PNG asset rendering (no R rsvg/magick dependency)"
key-files:
  created:
    - data-raw/brand/og-card.svg
    - data-raw/brand/make_og_card.R
    - man/figures/og-card.png
    - pkgdown/favicon/favicon.ico
    - pkgdown/favicon/favicon.svg
    - pkgdown/favicon/favicon-16x16.png
    - pkgdown/favicon/favicon-32x32.png
    - pkgdown/favicon/favicon-96x96.png
    - pkgdown/favicon/apple-touch-icon.png
    - pkgdown/favicon/android-chrome-192x192.png
    - pkgdown/favicon/android-chrome-512x512.png
    - pkgdown/favicon/web-app-manifest-192x192.png
    - pkgdown/favicon/web-app-manifest-512x512.png
    - pkgdown/favicon/site.webmanifest
  modified:
    - _pkgdown.yml
decisions:
  - "Generated legacy-named favicon sizes (favicon-16x16/32x32, android-chrome-192/512) from logo.svg because pkgdown 2.2.0's realfavicongenerator API now emits favicon.svg + favicon-96x96 + web-app-manifest-* instead of the older names the plan/UI-SPEC verify against"
  - "OG card rendered via rsvg-convert CLI (R rsvg package absent); no dependency added to DESCRIPTION"
  - "No PNG optimiser needed: card rendered at 26 KB, far under the 200 KB budget"
metrics:
  duration: "~4 min"
  completed: "2026-09-08"
actuals:
  tokens: 9000
  tasks: 3
  commits: 3
---

# Phase 20 Plan 02: pkgdown Site Theme, Favicons & OG Card Summary

Wired the eventstudy.de brand into the pkgdown site metadata: a `template.bslib`
palette (#2563eb primary, #ffffff bg, #0f172a fg) with Inter / Plus Jakarta Sans /
JetBrains Mono Google fonts, a navbar logo placed first in `structure.left`, the
full favicon set generated from `man/figures/logo.png`, and a 1200x630 Open Graph
social card wired via `template.opengraph`. No new dependency was added; all asset
rendering used dev-time tooling (`pkgdown::build_favicons()`, `rsvg-convert`).

## What Was Built

- **Task 1 (`73d4778`)** — `_pkgdown.yml` `template.bslib` block (bg/fg/primary,
  link + link-hover colors, font-scale 1.0, base/heading/code Google fonts) merged
  under the existing `bootstrap: 5` + `math-rendering: katex`, with no `bootswatch`
  key so the bslib variables stay authoritative. Added `navbar.logo` (logo.png ->
  site root, alt "EventStudy") and prepended `logo` to `structure.left`.
- **Task 2 (`daccad4`)** — `pkgdown::build_favicons(overwrite = TRUE)` generated the
  favicon set from `man/figures/logo.png` into `pkgdown/favicon/`. The whole set is
  excluded from the CRAN tarball via the existing `.Rbuildignore` `^pkgdown$` rule.
  CI (`R-CMD-check.yaml`) does not call `build_favicons`.
- **Task 3 (`44ea9b9`)** — `data-raw/brand/og-card.svg` (1200x630: white canvas,
  8px #2563eb top strip, CAR-curve logo mark centred in the left zone, headline /
  subheading / footer in the right zone) plus `data-raw/brand/make_og_card.R` which
  renders it to `man/figures/og-card.png` and asserts `< 204800` bytes. Added the
  `template.opengraph` block (image.src `man/figures/og-card.png`, alt text,
  twitter creator `@sipemu` + `summary_large_image`).

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 3 - Blocking / upstream drift] pkgdown 2.2.0 favicon filenames differ from plan**
- **Found during:** Task 2
- **Issue:** The plan's `<verify>` and the UI-SPEC expected the older pkgdown
  favicon names (`favicon-16x16.png`, `favicon-32x32.png`, `android-chrome-192x192.png`,
  `android-chrome-512x512.png`). pkgdown 2.2.0's realfavicongenerator.net API now
  emits `favicon.svg`, `favicon-96x96.png`, `favicon.ico`, `apple-touch-icon.png`,
  `web-app-manifest-192x192.png`, `web-app-manifest-512x512.png`, `site.webmanifest`
  instead. The plan's verify (`test -f pkgdown/favicon/favicon-32x32.png`) would fail
  against the current tool output. This is upstream tool drift, which the milestone
  constraints say to wrap gracefully rather than fix upstream.
- **Fix:** Kept the authoritative `build_favicons()` output in full, then rendered the
  four legacy-named sizes from `data-raw/brand/logo.svg` via `rsvg-convert` so both the
  modern pkgdown 2.2.0 set and the plan/UI-SPEC-named files are present. No fake
  artifact: every PNG is a real render of the brand mark. No dependency added.
- **Files modified:** pkgdown/favicon/ (added favicon-16x16.png, favicon-32x32.png,
  android-chrome-192x192.png, android-chrome-512x512.png)
- **Commit:** `daccad4`

**2. [Rule 3 - Blocking] OG card render tool substitution**
- **Found during:** Task 3
- **Issue:** The plan prescribes `rsvg::rsvg_png()` but the R `rsvg` package is not
  installed, and the plan forbids adding it to DESCRIPTION. optipng/pngquant (the
  suggested optimisers) are also absent.
- **Fix:** `make_og_card.R` prefers the `rsvg` R package when present and otherwise
  falls back to the `rsvg-convert` CLI (the same tool used for logo.png in 20-01),
  with an optimiser cascade (pngquant -> optipng -> magick) that only fires if the
  render exceeds 200 KB. The card rendered at 26,794 bytes, so no optimiser ran.
- **Files modified:** data-raw/brand/make_og_card.R
- **Commit:** `44ea9b9`

## Authentication Gates

`pkgdown::build_favicons()` performs an outbound request to
`realfavicongenerator.net` (sending the brand logo, which contains no secrets or PII
- matches threat register T-20-02 disposition "accept"). No credentials required;
this is normal `build_favicons` flow, not an auth gate.

## Verification

Full `<verification>` block re-run at plan end, all green:
- `_pkgdown.yml`: `template.bslib.primary == "#2563eb"`, `bg == "#ffffff"`,
  `fg == "#0f172a"`, `base-font.google == "Inter"`,
  `heading-font.google == "Plus Jakarta Sans"`, `navbar.structure.left[[1]] == "logo"`,
  `template.opengraph.image.src == "man/figures/og-card.png"`, no `bootswatch` key.
- `pkgdown/favicon/` contains the full favicon set + `site.webmanifest`.
- `man/figures/og-card.png` exists at 1200x630, 26,794 bytes (< 200 KB).
- DESCRIPTION unchanged (Version 0.65.0); no pkgdown/rsvg/magick/hexSticker in deps.

## Known Stubs

None. All assets are real rendered artifacts.

## Self-Check: PASSED

All created/modified files exist (_pkgdown.yml, man/figures/og-card.png,
pkgdown/favicon/site.webmanifest + favicon-32x32.png + apple-touch-icon.png,
data-raw/brand/og-card.svg, data-raw/brand/make_og_card.R) and all three task
commits (73d4778, daccad4, 44ea9b9) are present in git history.
