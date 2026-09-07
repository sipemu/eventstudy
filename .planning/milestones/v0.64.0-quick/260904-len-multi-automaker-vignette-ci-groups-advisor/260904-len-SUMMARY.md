---
task_id: 260904-len
title: Multi-automaker dieselgate dataset + CI bands + group CAAR plot + mock advisor
phase: quick
plan: 260904-len
subsystem: data, vignette, release
tags: [dieselgate, multi-group, caar, ci-bands, vignette, 0.61.2]
status: complete
metrics:
  duration: "~45 minutes"
  completed: "2026-09-04"
  commits: 3
  tasks: 3
key-files:
  created: []
  modified:
    - data-raw/dieselgate.R
    - data/dieselgate.rda
    - R/data-dieselgate.R
    - man/dieselgate.Rd
    - vignettes/ai-advisor.Rmd
    - DESCRIPTION
    - NEWS.md
decisions:
  - "Use default ParameterSet (already includes MultiEventStatisticsSet with CSectT) — adding CSectTTest again causes duplicate-column error"
  - "CI ribbons built from caar/caar_t ratio (se = |caar/caar_t|) — consistent with .plot_multi_event() internal approach"
  - "CAAR comparison uses dplyr::bind_rows of both groups and a single ggplot facet-free overlay — cleaner than two separate plot_event_study() calls"
actuals:
  tasks: 3
  commits: 3
---

# Quick Task 260904-len: Multi-automaker dieselgate dataset + CI bands + group CAAR plot

## One-liner

Extended `dieselgate` to 4 German automakers / 2 groups (VW Group vs Other),
rewrote advisor vignette as multi-group walkthrough with CI bands and group CAAR
comparison, released as 0.61.2.

## What was built

### 1. Extended `dieselgate` dataset (commit 9d609fb)

- `data-raw/dieselgate.R`: fetches VOW.DE, PAH3.DE, BMW.DE, MBG.DE + ^GDAXI
  (2014-06-01 to 2015-11-01) using `download_stock_data()` (tidyquant backend)
- 4-row request tibble: `event_id` 1-4, groups "VW Group" (1-2) / "Other" (3-4)
- `event_id = 1` remains VOW.DE for backward compat (`get_ar(1L)`, `get_car(1L)`)
- Regenerated `data/dieselgate.rda` (1440 firm rows + 360 index rows, 9 KB)
- Updated `R/data-dieselgate.R` roxygen doc → `man/dieselgate.Rd` regenerated

### 2. Vignette rewrite (commit 0a03165)

- `vignettes/ai-advisor.Rmd` fully rewritten:
  - Title updated: "Multi-Automaker Walkthrough" (VignetteIndexEntry aligned)
  - 4-firm pipeline with default `ParameterSet$new()` (CSectT included by default)
  - VW single-firm: AR/CAR + CI band via `plot_event_study(type="car", event_id=1L, confidence_level=0.95)`
  - Multi-group CAAR comparison: ggplot2 overlay with per-group CI ribbons (base64 PNG, no htmlwidgets)
  - Updated mock advisor block: grounded in actual multi-group numbers
- Vignette builds fully offline (EVENTSTUDY_NO_NETWORK=1)

### 3. Release 0.61.2 (commit fa87166)

- `DESCRIPTION Version: 0.61.2`
- `NEWS.md` 0.61.2 entry covering all four changes

## Key multi-group numbers (from live pipeline run)

| Metric | Value |
|--------|-------|
| VW (event_id=1) AR day+1 | -17.4% |
| VW (event_id=1) AR day+2 | -12.7% |
| VW (event_id=1) CAR at +10 | -35.5% |
| VW Group CAAR at +10 | -38.6% |
| VW Group CAAR t-stat at +10 | -12.6 |
| Other CAAR at +10 | +1.3% |
| Other CAAR t-stat at +10 | 0.34 (n.s.) |
| Median estimation R-squared | 0.736 |
| Shapiro-Wilk p-value | 0.004 |

## Vignette verification

| Check | Result |
|-------|--------|
| Bare NA cells in output | 0 (grep match was base64 image data) |
| Embedded base64 PNGs | 3 (AR bar chart, VW CAR with CI, group CAAR comparison) |
| CI bands visible | Yes (geom_ribbon for both CAR and CAAR plots) |
| Mock advisor block present | Yes (labelled "BEGIN static captured LLM example") |
| htmlwidget/plotly blobs | 0 |
| Builds offline (no API key) | Yes |
| VignetteIndexEntry matches YAML | Yes |

## Test suite

- PASS: 1931, FAIL: 0, WARN: 1 (pre-existing), SKIP: 29 (optional packages)

## R CMD check vs baseline

No new NOTEs or WARNINGs introduced. Results match documented baseline:
- 2 NOTEs: "Package was archived on CRAN"; `median`/`tail` undefined-globals
- 2 WARNINGs: missing inst/doc (pre-existing — vignettes not pre-built in source)
- 0 ERRORs
- .rda size: 9 KB (well within CRAN limits)

## Commits

| Hash | Message |
|------|---------|
| 9d609fb | data(quick-260904-len): extend dieselgate to 4 automakers / 2 groups |
| 0a03165 | vignette(quick-260904-len): multi-group walkthrough + CI bands + group CAAR plot + mock advisor |
| fa87166 | chore(release): bump to 0.61.2 + NEWS entry |

## Deviations from plan

None — plan executed exactly as specified.

**Implicit discovery:** The installed package was v0.50.0 (system install).
The source (v0.61.1) was installed into a temp lib for all verification runs.
This is expected behavior — not a deviation.

**Implicit discovery:** Adding `CSectTTest$new()` to a fresh `MultiEventStatisticsSet`
on top of `ParameterSet$new()` causes a duplicate-column error in `calculate_statistics()`,
because `MultiEventStatisticsSet` already includes `CSectTTest$new()` by default. The
vignette uses `ParameterSet$new()` directly (no manual test addition), which is correct.

## Self-Check: PASSED

- data/dieselgate.rda: FOUND (9265 bytes)
- man/dieselgate.Rd: FOUND (reflects 4 firms/2 groups)
- vignettes/ai-advisor.Rmd: FOUND (212 lines)
- Commits 9d609fb, 0a03165, fa87166: FOUND in git log
- Vignette HTML: 3 embedded PNGs, 0 bare NA cells, 0 htmlwidgets
