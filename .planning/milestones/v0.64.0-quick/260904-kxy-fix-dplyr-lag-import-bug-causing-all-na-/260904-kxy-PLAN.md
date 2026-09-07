---
phase: quick
plan: 260904-kxy
type: fix
autonomous: true
requirements: []
subsystem: return-calculation
tags: [correctness, namespace, vignette, cran]
---

# Quick 260904-kxy: Fix dplyr::lag import bug (all-NA AR/CAR without dplyr attached)

## Objective

`R/return_calculation.R` calls bare `lag(price)`. `dplyr::lag` is NOT imported in
the EventStudy NAMESPACE, so `lag` resolves to `stats::lag`, which no-ops on a plain
numeric vector. Every return computes as 0 (including row 1 = 0 instead of NA);
zero-variance returns trip the MarketModel variance guard, so ALL abnormal returns /
CARs come back NA. Silent whenever `dplyr` is not attached. Tests/audit run with
dplyr attached (masking `stats::lag`), so they never caught it; the
`vignettes/ai-advisor.Rmd` build (`library(EventStudy)` only) exposes it as all-NA.

## Context

- Root cause confirmed: installed namespace `identical(get("lag", ns), stats::lag) == TRUE`.
- `task_intraday.R` already uses `dplyr::lag` explicitly — import addition won't change it.
- `get_ar(1L)` -> `relative_index, abnormal_returns`; `get_car(1L)` adds `car`.

## Tasks

### Task 1 (auto): Import dplyr::lag + regenerate NAMESPACE
Add `lag` to `@importFrom dplyr ...` in `R/EventStudy-package.R`; run `roxygen2::roxygenise()`;
confirm `importFrom(dplyr,lag)` in NAMESPACE. Bump DESCRIPTION 0.61.0 -> 0.61.1 + NEWS.md.

### Task 2 (auto): Regression test
`tests/testthat/test-return-calculation-lag.R`: lock namespace resolution
(`identical(get("lag", asNamespace("EventStudy")), dplyr::lag)` and not stats::lag) +
behavioral assertions (row 1 NA, price c(100,110) -> simple return 0.1). Full suite green.

### Task 3 (auto): Vignette AR/CAR plots
Add offline plot chunks to `vignettes/ai-advisor.Rmd` (AR bar across [-10,+10] with day 0
marked; CAR path to ~-35%). ggplot2/base R, `requireNamespace()`-guarded, deterministic.

## Verification
- `roxygen2::roxygenise()` clean; NAMESPACE has `importFrom(dplyr,lag)`.
- Reinstall to temp lib; run pipeline WITHOUT dplyr attached -> AR day+1 ~ -0.174,
  day+2 ~ -0.127, CAR[-10,+10] ~ -0.355, 0 NAs.
- `devtools::test()` 0 failures.
- Vignette rebuilds offline, NA-free, plots render.
- `R CMD check --as-cran` no new NOTEs/WARNINGs vs baseline.

## Output
Fix + test + vignette + release bump (0.61.1) committed atomically; SUMMARY.md written.
