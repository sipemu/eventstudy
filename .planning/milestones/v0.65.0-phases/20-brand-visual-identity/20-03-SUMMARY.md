---
phase: "20"
plan: "03"
subsystem: brand-visual-identity
tags: [pkgdown, scss, branding, readme, ci, cran-hygiene]
requires:
  - "20-01 (man/figures/logo.png)"
  - "20-02 (bslib fonts/palette; README/OG surfaces reference logo.png)"
provides:
  - "pkgdown home/gallery numeric-badge strip + ecosystem strip"
  - "single-primary section headings + gallery-title recolour (#2563eb)"
  - "README logo img + stable lifecycle badge"
  - "CI guards: sub-1 MB tarball assertion + baseline-aware non-ASCII guard"
affects:
  - pkgdown/extra.scss
  - pkgdown/extra.css
  - vignettes/gallery.Rmd
  - README.md
  - .github/workflows/R-CMD-check.yaml
  - .github/non-ascii-baseline.txt
tech-stack:
  added: []
  patterns:
    - "pkgdown 2.0+ auto-compiles pkgdown/extra.scss (must be at that exact path)"
    - "flex-wrap: wrap on strips for narrow-viewport reflow"
    - "baseline-diff CI guard: fail on NEW non-ASCII only, not declared UTF-8"
key-files:
  created:
    - pkgdown/extra.scss
    - .github/non-ascii-baseline.txt
  modified:
    - pkgdown/extra.css
    - vignettes/gallery.Rmd
    - README.md
    - .github/workflows/R-CMD-check.yaml
decisions:
  - "Section-heading recolour uses the clean-edit approach (removed the 8 per-class rules) rather than the !important hammer, per RESEARCH Open Question 1 recommendation."
  - "Ecosystem-strip separators use ASCII '.' instead of the UI-SPEC middle-dot, honoring the plan's ASCII-only copy mandate."
  - "Non-ASCII CI guard made baseline-aware (Rule 1 fix): a literal grep over R/ would have failed CI on the package's deliberate, Encoding: UTF-8-declared non-ASCII (author names, math symbols, SVG glyphs)."
metrics:
  duration: "3m"
  completed: "2026-09-08"
actuals:
  tokens: 4700
  tasks: 3
  commits: 3
status: complete
---

# Phase 20 Plan 03: Brand Visual Surfaces & CRAN Guards Summary

Populated and styled the visible eventstudy.de brand surfaces (numeric-badge strip, ecosystem strip, primary-blue section headings/gallery titles, README logo + stable badge) and locked the release-hygiene CI guards (sub-1 MB tarball assertion, baseline-aware non-ASCII source guard) — all without adding a single DESCRIPTION dependency or touching any statistical R code.

## What Was Built

### Task 1 — extra.scss + section-heading/gallery recolour (commit bc46f8a)
- Created `pkgdown/extra.scss` (exact path for pkgdown 2.0+ auto-compile) with `.es-stat-strip`, `.es-stat-badge` (`.stat-number` / `.stat-label` children), and `.es-ecosystem-strip` (`.es-ecosystem-label` / `.es-ecosystem-sep` / `.es-ecosystem-link` / `.es-ecosystem-current`) rules per the UI-SPEC: accent `#c45a10` badge border+number, `#fff0e5` background, `#703b0c` label, Plus Jakarta Sans 3rem number, Inter 0.875rem label. Both strips declare `flex-wrap: wrap` for narrow-viewport reflow.
- In `pkgdown/extra.css`: removed the 8 per-class `.es-section-heading.es-*` colour declarations, set the base `.es-section-heading` to `border-bottom-color: #2563eb; color: #0f172a`, and changed `.es-gallery-title` from `#0d6efd` to `#2563eb`. The multi-colour `.es-tag` category badges are left unchanged.

### Task 2 — gallery.Rmd numeric-badge + ecosystem HTML (commit 91100f0)
- Inserted, above the Core Workflow grid in the first raw-HTML block of `vignettes/gallery.Rmd`, the `.es-stat-strip` (15+ Return Models / 12 Test Statistics / 5 DiD Estimators) and the `.es-ecosystem-strip` linking Google Sheets Template, R Package (`.es-ecosystem-current`, non-linked bold), and WebAssembly App.
- Used the honest DiD count of **5** (codebase reality), not the eventstudy.de "6". The existing gallery grid below is untouched.

### Task 3 — README logo/badge + CI guards (commit a88be01)
- `README.md`: prepended `<img src="man/figures/logo.png" align="right" height="120" alt="EventStudy logo" />` before the H1, and flipped the lifecycle badge from `experimental-orange` to `stable-brightgreen`. All other badges unchanged.
- `.github/workflows/R-CMD-check.yaml`: appended two `if: runner.os == 'Linux'` steps after `check-r-package` — (1) "Assert tarball under 1 MB" via `R CMD build . --no-build-vignettes --no-manual` + `stat -c%s`, failing at `>= 1048576` bytes; (2) a non-ASCII guard over `R/ man/figures/ inst/` using `grep -rnP '[^\x00-\x7F]'`.

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 1 - Bug] Made the CI non-ASCII guard baseline-aware to avoid failing on valid inputs**
- **Found during:** Task 3
- **Issue:** The plan's literal non-ASCII step (`grep -rP '[^\x00-\x7F]' R/ man/figures/ inst/ && exit 1`) would fail CI immediately on `main`: `R/` and `man/figures/` already contain 106 lines of deliberate non-ASCII (em-dashes in roxygen comments, author names like `Pynnonen`, math symbols `beta`/`sigma`/superscript-2, SVG glyphs). The package declares `Encoding: UTF-8`, so this content is legitimate and passes `R CMD check --as-cran`. A guard that reddens CI on valid current inputs violates the milestone constraint "Behavior on valid inputs must not change / existing tests stay green" and the threat model's actual intent (catch *stray/injected* non-ASCII, e.g. homoglyph credential strings — not the package's own declared UTF-8).
- **Fix:** Kept the `grep -rP '[^\x00-\x7F]'` guard over the exact `R/ man/figures/ inst/` scope (so the `x00-` pattern and scope acceptance criteria hold), but made it diff against a committed baseline `.github/non-ascii-baseline.txt` of the existing legitimate UTF-8. The step now fails only when a **new** non-ASCII line appears outside the baseline. Simulated locally: current tree matches baseline, step passes.
- **Files modified:** `.github/workflows/R-CMD-check.yaml`, `.github/non-ascii-baseline.txt` (new)
- **Commit:** a88be01

### Minor spec adaptations (ASCII-only mandate)
- Ecosystem-strip separators use ASCII `.` instead of the UI-SPEC middle-dot (`·`), per the plan's explicit "Keep all copy ASCII-only" instruction. Same visual role, no non-ASCII in the vignette.
- SCSS/CSS comments I added use plain ASCII (`--`, `-`) rather than box-drawing/em-dash characters.

## Constraints Honored
- **No DESCRIPTION dependency added** — verified `pkgdown|rsvg|magick|hexSticker` absent from DESCRIPTION Imports/Suggests.
- **No statistical code or tests touched** — the diff spans only `pkgdown/`, `vignettes/gallery.Rmd`, `README.md`, and `.github/`; the 400+ tests are unaffected by construction.
- **`.github/` and `README.md` are `.Rbuildignore`'d** — the baseline file and README's own non-ASCII do not enter the CRAN tarball.

## Known Stubs
None.

## Threat Flags
None — the plan's threat register (T-20-04 tarball bloat, T-20-05 non-ASCII injection) is exactly what the two CI guards mitigate; no new trust-boundary surface introduced.

## Self-Check: PASSED
All created/modified files present on disk (pkgdown/extra.scss, .github/non-ascii-baseline.txt, vignettes/gallery.Rmd, README.md, .github/workflows/R-CMD-check.yaml, 20-03-SUMMARY.md) and all three task commits (bc46f8a, 91100f0, a88be01) exist in the git log.
