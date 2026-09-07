---
phase: quick
plan: 260904-kxy
subsystem: return-calculation
tags: [correctness, namespace, vignette, cran, release]
status: complete
requires: []
provides: [dplyr-lag-import, return-calc-regression-test, advisor-vignette-plots]
affects: [R/EventStudy-package.R, NAMESPACE, R/return_calculation.R (behavior), vignettes/ai-advisor.Rmd]
tech-stack:
  added: []
  patterns: [namespace-import-lock-test]
key-files:
  created:
    - tests/testthat/test-return-calculation-lag.R
  modified:
    - R/EventStudy-package.R
    - NAMESPACE
    - DESCRIPTION
    - NEWS.md
    - vignettes/ai-advisor.Rmd
decisions:
  - "Added `lag` to the existing `@importFrom dplyr` line rather than a new line — avoids the pre-existing multi-line @importFrom roxygen warning pattern and keeps the dplyr import consolidated."
  - "Used ggplot2 (a hard dependency) for the vignette plots with a requireNamespace()-guarded base R fallback, keeping the build offline and dependency-clean."
metrics:
  duration: ~9m
  completed: 2026-09-04
actuals:
  tokens: 9000
  tasks: 3
  commits: 4
---

# Quick 260904-kxy: Fix dplyr::lag import bug (all-NA AR/CAR without dplyr attached) Summary

Imported `dplyr::lag` into the EventStudy namespace so `SimpleReturn`/`LogReturn` stop silently returning all-zero (then all-NA) results when `dplyr` is not attached; locked it with a namespace-resolution regression test; and added offline AR/CAR plots to the advisor vignette. Released as 0.61.1.

## What Changed

- **Root cause (Task 1):** `R/return_calculation.R` called bare `lag(price)`. `dplyr::lag` was absent from the NAMESPACE, so `lag` resolved to `stats::lag` (a no-op on a plain numeric vector) whenever `dplyr` was not on the search path. Every return computed as `0` (row 1 as `0` instead of `NA`), the zero-variance series tripped the MarketModel variance guard, and all AR/CAR came back NA. Added `lag` to `@importFrom dplyr ...` in `R/EventStudy-package.R`, regenerated `NAMESPACE` (`importFrom(dplyr,lag)` now present). `task_intraday.R` already used `dplyr::lag` explicitly, so it is unaffected. No behavior change for callers who had `dplyr` attached.
- **Regression net (Task 2):** New `tests/testthat/test-return-calculation-lag.R` asserts `identical(get("lag", asNamespace("EventStudy")), dplyr::lag)` and `!identical(..., stats::lag)` — directly locking the import — plus behavioral checks (`SimpleReturn`/`LogReturn` row 1 = NA, `c(100,110,99)` → simple returns `0.1`, `-0.1`; log returns match `log(ratio)`).
- **Vignette (Task 3):** `vignettes/ai-advisor.Rmd` gains two offline plot chunks after the AR/CAR numbers — an abnormal-return bar plot across `[-10,+10]` with the event day highlighted, and the cumulative-abnormal-return path sliding to ~-35%. ggplot2 (hard dep) with a `requireNamespace()`-guarded base R fallback; deterministic, no network/API.
- **Release:** DESCRIPTION `0.61.0 → 0.61.1`; NEWS.md `# EventStudy 0.61.1` entry describing the fix and the vignette plots.

## Verified Results

- `roxygen2::roxygenise()` regenerated NAMESPACE; `importFrom(dplyr,lag)` present. (Pre-existing roxygen warnings about a separate multi-line `@importFrom stats` and undocumented `print` params are unrelated and not introduced here.)
- **End-to-end WITHOUT dplyr attached** (installed to temp lib): `lag == dplyr::lag` TRUE, `dplyr attached: FALSE`, AR day+1 = **-0.174**, day+2 = **-0.127**, CAR[+10] = **-0.35515**, **AR NA count 0**. (Pre-fix: all NA.)
- **Full test suite** via `devtools::test()`: **FAIL 0 | PASS 1931 | SKIP 29 | WARN 1**. The 29 skips are optional Suggests (rmgarch/did/DIDmultiplegt/didimputation/rmarkdown) absent in the sandbox; the 1 warning is a pre-existing rank-deficient-design edge case, not from this change. The new test file passes (10 assertions).
- **Vignette rebuild** (offline, `EVENTSTUDY_NO_NETWORK=1`, keys unset): renders successfully, 2 plot chunks emit 2 embedded base64 images, and **no NA in the AR/CAR data output** (the only literal `NA` substrings are inside the two `<img aria-label>` attributes).
- **`R CMD check --as-cran`** (`_R_CHECK_FORCE_SUGGESTS_=false`): **Status: 2 NOTEs, 0 WARNINGs, 0 ERRORs** — exactly the documented baseline: (1) "Package was archived on CRAN", (2) `median`/`tail` undefined-globals. No new NOTEs/WARNINGs vs the v0.60.0/0.61.0 baseline; the non-ASCII WARNING remains fixed.

## Commits

- `6c47339` fix(returns): import dplyr::lag so returns are correct without dplyr attached (quick-260904-kxy)
- `46497bb` test(returns): lock dplyr::lag namespace resolution + non-zero returns
- `2bb3109` docs(vignette): add AR and CAR plots to advisor vignette
- `f9d0d5f` chore(release): 0.61.1 + NEWS

## Deviations from Plan

None — plan executed as written. Note on process: an initial full-suite run via `test_dir()` against the *installed* package reported spurious "could not find function `.resolve_degenerate_mode`" errors because internal (non-exported) functions are not visible on the search path that way; re-running via the canonical `devtools::test()` (which uses `load_all` to expose internals) showed the true result — 0 failures. No code affected.

## Self-Check: PASSED

- FOUND: tests/testthat/test-return-calculation-lag.R
- FOUND: commit 6c47339, 46497bb, 2bb3109, f9d0d5f
- NAMESPACE contains `importFrom(dplyr,lag)`
