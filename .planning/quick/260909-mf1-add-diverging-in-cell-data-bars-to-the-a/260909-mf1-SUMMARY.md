---
phase: quick-260909-mf1
plan: "01"
subsystem: vignettes/docs
tags: [tinytable, data-bars, visualization, pkgdown, aar, caar]
status: complete
completed: "2026-09-09"
duration: "~45m"
actuals:
  tokens: 9000
  tasks: 3
  commits: 3
key_files:
  modified:
    - vignettes/articles/_setup.Rmd
    - vignettes/introduction.Rmd
    - docs/articles/introduction.html
    - docs/articles/methods-test-statistics.html
    - docs/articles/ (all article HTMLs rebuilt)
decisions:
  - "Bar width scaled proportionally: 50% track = max-abs value, so the largest bar fills half the track width and all others are scaled relative."
  - "Zero value and NA/non-finite: render centerline-only cell (no colored bar) so no zero-width bar artifacts appear."
  - "Bar cols now character post-replacement: align computation runs on x_out so they naturally get 'l' alignment — no special overriding needed."
  - "Fallback (knitr::kable) branch unchanged: no bars in non-HTML output, consistent with prior behavior."
---

# Phase quick-260909-mf1 Plan 01: Add Diverging In-Cell Data Bars to aar/caar Tables — Summary

## One-Liner

Added diverging in-cell HTML data bars (green right = positive, red left = negative) to `es_tt()` for `aar`/`caar` columns, synced both vignette copies to byte-identical, and rebuilt the pkgdown site — making sign and magnitude of abnormal returns scannable at a glance in every docs table.

## Tasks Completed

| # | Task | Commit | Files |
|---|------|--------|-------|
| 1 | Make es_tt() in _setup.Rmd bar-aware | 3aa0579 | vignettes/articles/_setup.Rmd |
| 2 | Sync identical es_tt() into introduction.Rmd | 78461e8 | vignettes/introduction.Rmd |
| 3 | Rebuild pkgdown site, verify bar markup in HTML | 4d0a442 | docs/ (24 files) |

## What Was Built

`es_tt()` now accepts `bar_cols = c("aar", "caar")` (default). For each column in `bar_cols` that exists in the input frame as a numeric column:

1. Compute `m = max(abs(col), na.rm = TRUE)`. Skip bars if `m` is 0/NA/non-finite.
2. For each row value `val`: build an HTML cell containing a 70px track div with a 1px gray centerline at 50%, a colored inner bar positioned `left:50%` (green `#16a34a`) for positive or `right:50%` (red `#dc2626`) for negative, scaled to `50 * abs(val) / m` percent of the track. NA/zero values render the centerline only.
3. The formatted number appears alongside in a tabular-nums span.
4. Replaced columns become character; `align` is recomputed on the modified frame so they get `"l"` (acceptable — bar layout controls visual alignment).

Both `vignettes/articles/_setup.Rmd` and `vignettes/introduction.Rmd` carry byte-identical `es_tt()` bodies. The `knitr::kable()` fallback branch is untouched. No new package dependencies added.

## Verification

- ASCII-clean check: passed (no non-ASCII bytes in either vignette source)
- Bar render check: `#dc2626` (red) and `#16a34a` (green) present in tinytable HTML output; `right:50` anchor confirmed
- Body identity check: `identical(ex("_setup.Rmd"), ex("introduction.Rmd"))` is `TRUE`
- Rebuilt HTML check: both `docs/articles/introduction.html` and `docs/articles/methods-test-statistics.html` contain bar markup; 84 instances of `right:50|left:50` anchors in introduction.html

## Deviations from Plan

None — plan executed exactly as written.

## Self-Check: PASSED

- vignettes/articles/_setup.Rmd: FOUND
- vignettes/introduction.Rmd: FOUND
- docs/articles/introduction.html: FOUND (contains #dc2626, #16a34a)
- docs/articles/methods-test-statistics.html: FOUND (contains #dc2626, #16a34a)
- Commits: 3aa0579, 78461e8, 4d0a442 all present in git log
