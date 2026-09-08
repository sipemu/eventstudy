---
phase: 20-brand-visual-identity
plan: 01
subsystem: brand-assets
tags: [brand, logo, hex-sticker, cran, version-bump, pkgdown]
requires: []
provides:
  - man/figures/logo.png
  - man/figures/logo-hex.png
  - DESCRIPTION@0.65.0
affects:
  - downstream Phase 20 plans (favicons, navbar, OG card, README) consume logo.png
tech-stack:
  added: []
  patterns:
    - "SVG source in .Rbuildignore'd data-raw/brand/ -> rendered PNG in man/figures/"
    - "dev-time rendering via librsvg CLI (rsvg-convert), zero DESCRIPTION dependencies"
key-files:
  created:
    - data-raw/brand/logo.svg
    - data-raw/brand/make_logo.R
    - data-raw/brand/make_sticker.R
    - man/figures/logo.png
    - man/figures/logo-hex.png
  modified:
    - DESCRIPTION
    - NEWS.md
decisions:
  - "Rendered assets with the librsvg rsvg-convert CLI instead of the rsvg/hexSticker R packages, because those packages are not installed and CRAN-02 forbids adding any dependency; scripts document the canonical hexSticker path as primary with a CLI fallback."
  - "Hex sticker built as a single deterministic SVG composited by rsvg-convert (pointy-top hexagon, primary-800 fill, primary-600 border) rather than hexSticker::sticker(), keeping the artifact reproducible with no R dependency."
  - "Logo mark strokes recoloured white/light-blue inside the hex for contrast against the deep-blue fill, faithful to the CAR-curve motif."
metrics:
  duration: "~4 min"
  completed: "2026-09-08"
status: complete
actuals:
  tokens: 6500
  tasks: 3
  commits: 3
---

# Phase 20 Plan 01: Brand Assets (version bump + logo + hex sticker) Summary

Version bumped to 0.65.0 with a matching NEWS section, and the EventStudy brand mark (CAR-curve-crossing-event-line SVG) authored and rendered to a 5 KB tarball-safe `logo.png` plus a 25 KB deep-blue hex sticker `logo-hex.png` — all with zero DESCRIPTION dependencies.

## What Landed

- **Task 1 (a029aca):** `DESCRIPTION` Version 0.64.0 -> 0.65.0, Date -> 2026-09-08; `NEWS.md` prepended with a `# EventStudy 0.65.0` section (Brand & Visual Identity + CRAN Hygiene bullets, ASCII-only). No Imports/Suggests changes.
- **Task 2 (5ac03b8):** `data-raw/brand/logo.svg` (100x100 CAR curve crossing the event line at t=0, dashed baseline, t=0 dot, white rounded background) + `data-raw/brand/make_logo.R` (rsvg / rsvg-convert render at 240x240 with a <50 KB guard). Rendered `man/figures/logo.png` at 5265 bytes.
- **Task 3 (c195293):** `data-raw/brand/make_sticker.R` (canonical `hexSticker::sticker()` call with the Pitfall-1 SVG-subplot guard as the primary path, plus a librsvg CLI fallback that composites an equivalent hexagon from the same SVG source) -> `man/figures/logo-hex.png` at 25312 bytes.

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 3 - Blocking] Rendering R packages not installed**
- **Found during:** Task 2 and Task 3
- **Issue:** The `rsvg`, `magick`, and `hexSticker` R packages named in the plan/UI-SPEC are not installed in this environment, and CRAN-02 forbids adding any dependency to DESCRIPTION.
- **Fix:** Rendered both PNGs with the librsvg `rsvg-convert` CLI (present at `/usr/bin/rsvg-convert`), a pure dev-time tool with no R-package footprint. The committed scripts document the canonical R-package path as primary (`rsvg::rsvg_png`, `hexSticker::sticker()` with the Pitfall-1 guard) and fall back to the CLI when the packages are absent, so the assets regenerate on any machine with librsvg and without adding a dependency.
- **Files modified:** data-raw/brand/make_logo.R, data-raw/brand/make_sticker.R
- **Commits:** 5ac03b8, c195293

This is a mechanism substitution, not a spec deviation: the produced artifacts match the UI-SPEC geometry and palette, and the plan explicitly authorised "produce the asset by the best available means" if a rendering tool is unavailable, without adding to DESCRIPTION.

## Verification

- `grep '^Version:' DESCRIPTION` -> `Version: 0.65.0`; first NEWS.md line -> `# EventStudy 0.65.0`. PASS
- `man/figures/logo.png` 5265 bytes (< 51200); `man/figures/logo-hex.png` 25312 bytes (< 51200). PASS
- `grep -iE 'hexSticker|rsvg|magick' DESCRIPTION` returns nothing. PASS
- `.Rbuildignore` `^data-raw$` covers `data-raw/brand/` (SVG + scripts excluded from tarball). PASS
- Both PNGs visually confirmed: logo shows the CAR curve crossing the event line at t=0 on white; hex shows the mark in white on primary-800 fill with primary-600 border, "EventStudy" and "eventstudy.de" text.

## Known Stubs

None.

## Threat Flags

None. T-20-01 (dep surface) mitigated — DESCRIPTION grep gate green. T-20-02 (PNG info disclosure) accepted per plan; assets are pure brand artwork.

## Self-Check: PASSED
- FOUND: data-raw/brand/logo.svg, data-raw/brand/make_logo.R, data-raw/brand/make_sticker.R
- FOUND: man/figures/logo.png, man/figures/logo-hex.png
- FOUND commits: a029aca, 5ac03b8, c195293
