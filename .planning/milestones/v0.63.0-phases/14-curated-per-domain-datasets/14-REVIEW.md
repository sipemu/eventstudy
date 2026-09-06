---
phase: 14-curated-per-domain-datasets
reviewed: 2026-09-05T23:55:00Z
depth: deep
files_reviewed: 5
files_reviewed_list:
  - data-raw/earnings_surprises.R
  - data/earnings_surprises.rda
  - R/data-earnings-surprises.R
  - man/earnings_surprises.Rd
  - data-raw/DATA-SOURCES.md
findings:
  critical: 0
  warning: 1
  info: 1
  total: 2
status: findings
---

# Phase 14: Code Review Report

**Reviewed:** 2026-09-05T23:55:00Z
**Depth:** deep
**Files Reviewed:** 5
**Status:** findings

## Summary

Phase 14 delivers the `earnings_surprises` frozen dataset following the dieselgate pattern. The wiring is structurally sound: the frozen object matches the roxygen `@format` description in every measurable field (813 firm rows, 271 index rows, 3-row request tibble, correct 9-column layout, `%d.%m.%Y` date strings, correct `meta` slot names). CRAN safety gates pass. One warning-level factual mislabeling in comments and one info-level documentation imprecision are the only actionable findings.

---

## Verdict on the −0.0006 CAAR at Day 0

**Not a bug. Defensible real data.**

All three earnings releases (AAPL May 4, MSFT/GOOGL Apr 25, 2023) were after-hours announcements. Day 0 captures the closing price on the announcement date itself — before the market has digested the news. The actual price reaction lands at day +1 and later:

- MSFT raw return Apr 25→26: +7.24% vs S&P −0.38% → AR day+1 ≈ +7.6%
- AAPL raw return May 4→5: +4.69% → strong positive AR at day+1
- GOOGL reaction spread to Apr 27 (+3.74%)

The full pipeline confirms: CAAR at day 0 = −0.0006 (flat), CAAR at day +5 = **+0.0377** (t = 2.35, significant). The roxygen doc's claim that the panel produces "a positive cumulative average abnormal return (CAAR) over the event window" is correct — it refers to the full [−5, +5] window, not the day-0 snapshot. The event dates (2023-04-25 for MSFT/GOOGL, 2023-05-04 for AAPL) are factually correct and in `%d.%m.%Y` format. No wiring bug.

---

## Warnings

### WR-01: AAPL Fiscal Quarter Mislabeled in data-raw Script (Factual Error in Comments)

**File:** `data-raw/earnings_surprises.R:13,31`
**Issue:** The inline comments label the AAPL May 4, 2023 earnings report as "Q1 FY2023". Apple's fiscal year runs October–September. FY2023 Q1 covers October–December 2022 and was reported on **February 2, 2023**. The May 4, 2023 report covers fiscal **Q2 FY2023** (January–March 2023). A reader trying to reproduce the dataset or cross-reference external sources will find the wrong quarter if they search for "AAPL Q1 FY2023".

The event date itself (2023-05-04) is correct. The `meta$note` line 87 also perpetuates this via the "Q1 2023" umbrella label for AAPL specifically — though that is calendar-year framing and less precise than "Q2 FY2023".

**Fix:** In `data-raw/earnings_surprises.R`:
```r
# Line 13 — change:
#   2023-05-04 -- AAPL Q1 FY2023 earnings (beat consensus ~8%)
# to:
#   2023-05-04 -- AAPL Q2 FY2023 earnings (Jan-Mar 2023 period; beat consensus ~8%)

# Line 31 — change:
event_dates  <- c("04.05.2023",   # AAPL -- Q2 FY2023 earnings, 2023-05-04 in dd.mm.yyyy
                  "25.04.2023",   # MSFT -- Q3 FY2023 earnings, 2023-04-25
                  "25.04.2023")   # GOOGL -- Q1 CY2023 earnings, 2023-04-25
```

The roxygen doc (`R/data-earnings-surprises.R`) uses "Q1 2023" without "FY" for AAPL, which can be read as calendar-Q1-period framing and is less wrong, but a parenthetical clarification would remove ambiguity: `Apple Inc. reported on 2023-05-04 (Q2 FY2023, covering the Jan–Mar 2023 calendar-Q1 period)`.

---

## Info

### IN-01: em-Dashes in .R and .Rd Are UTF-8 — Pre-Existing Package-Wide Pattern

**File:** `R/data-earnings-surprises.R:46-48`, `man/earnings_surprises.Rd:62-64`
**Issue:** Lines 46–48 of the roxygen source and lines 62–64 of the generated `.Rd` contain the UTF-8 em-dash character (U+2014: —). Neither file has an explicit `\encoding{UTF-8}` declaration in the `.Rd` header.

This is not a new issue introduced by Phase 14. The pre-existing `R/data-dieselgate.R` and `man/dieselgate.Rd` use identical em-dashes in the same structural positions and the package already declares `Encoding: UTF-8` in `DESCRIPTION`. With `roxygen2 >= 7.x` and `Encoding: UTF-8` in `DESCRIPTION`, `R CMD check` accepts UTF-8 in `.Rd` files without a per-file `\encoding{}` tag. The Phase 14 files merely replicate the established pattern. No action required unless a future CRAN toolchain change flags it.

**Fix (if ever needed):** Replace `—` with `\enc{—}{-}` in the `\item` lines, or add `\encoding{UTF-8}` as the first line of the `.Rd` — but do so package-wide (including `dieselgate.Rd`) rather than selectively.

---

_Reviewed: 2026-09-05T23:55:00Z_
_Reviewer: Claude (gsd-code-reviewer)_
_Depth: deep_
