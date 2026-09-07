---
phase: 17-grounding-prose-hardening-offline-report-fallback-cran-basel
plan: "02"
subsystem: prose-grounding-scanner
tags: [grounding, prose, advise, GROUND-01, GROUND-02, GROUND-03]
status: complete

dependency_graph:
  requires:
    - 17-01 (OfflineNarrative engine + REPORT-03 seam)
  provides:
    - .extract_numeric_literals() — numeric literal extractor from free-text prose
    - .build_prose_value_registry() — diagnostics value registry for prose scanning
    - .is_grounded_literal() — exemption + tolerance grounding check
    - .scan_prose_grounding() — drop-and-keep prose scanner with single-warning discipline
    - tests/testthat/test_prose_grounding.R — GROUND-01/02/03 regression lock (56 tests)
  affects:
    - Phase 18 section assembler (will invoke .scan_prose_grounding on LLM narrative)
    - Any future prose-emitting path in advise.R

tech_stack:
  added: []
  patterns:
    - Numeric literal regex with longest-alternative-first to avoid 4-digit number splits
    - Vector-to-mean summarization matching .validate_grounding() verbatim
    - Exemption order: structural-int, year(1900-2100), significance-constant, tolerance, rounding-aware
    - Single-warning discipline: one warning per scan regardless of how many sections dropped

key_files:
  created:
    - tests/testthat/test_prose_grounding.R
  modified:
    - R/advise.R

decisions:
  - Regex alternation order: `\d{1,3}(?:,\d{3})+|\d+` (thousands-with-commas FIRST, plain digits second) — avoids 4-digit number like "1997" splitting to "199"+"7"
  - Year exemption threshold 1900-2100 (integer only) covers all citation years without false-positiving on valid event counts
  - Significance constants c(0.001, 0.01, 0.05, 0.10) exempt — these are always statistical thresholds, never computed results
  - Rounding-aware match at literal's displayed decimal precision (format + nchar) reuses abs_tol for the rounding comparison
  - expect_warning() in testthat 3e returns the condition, not the expression value — capture result in outer variable

metrics:
  duration: "~6 minutes"
  completed: "2026-09-07"
  tasks_completed: 3
  commits: 2

actuals:
  tokens: 8500
  tasks: 3
  commits: 2
---

# Phase 17 Plan 02: Prose Grounding Scanner — Summary

Four internal functions locking the GROUND-01/02/03 invariant: a fabricated numeric literal in free-text narrative prose is caught, its section dropped, and the unverified number never emitted into rendered output — with exactly one warning per scan and zero false-positive drops on correctly-grounded prose.

## Tasks Completed

| Task | Name | Commit | Files |
|------|------|--------|-------|
| 1 (auto) | Spike — .extract_numeric_literals() contract (RED) | bc7a38d | tests/testthat/test_prose_grounding.R |
| 2 (auto, tdd) | Prose grounding scanner internals in R/advise.R (GREEN) | e103972 | R/advise.R, tests/testthat/test_prose_grounding.R |
| 3 (auto) | GROUND-03 report-path regression lock | e103972 | tests/testthat/test_prose_grounding.R |

## What Was Built

**Task 1 — Spike (RED):**
- Created `tests/testthat/test_prose_grounding.R` with the extractor contract spike block.
- Pinned seven `.extract_numeric_literals()` sample cases: decimal (2.35/0.001), integer+decimal (42/0.87), citation year (1997), percentage (12.5%), range (0.30/0.70), thousands-separator (1,234.56 → 1234.56), scientific notation (2.5e-4).
- Also included all GREEN-phase tests (registry, exemptions, scanner behavior, GROUND-03) so the full file is written once and the RED→GREEN transition is clean.
- All tests RED until Task 2 implemented the internals (confirmed by test output).

**Task 2 — Implementation (GREEN) — four @noRd internals added to R/advise.R after `.validate_grounding()`:**

1. `.extract_numeric_literals(text)`: perl regex with correct alternation order (`\d{1,3}(?:,\d{3})+|\d+` — thousands pattern with commas FIRST, plain-digit fallback second). This avoids the "1997" → "199"+"7" split that occurs when `\d{1,3}` greedily matches without a following comma and leaves the tail digit. Strips thousands separators with `gsub(",", "", ..., fixed=TRUE)`.

2. `.build_prose_value_registry(diag)`: flattens all six `estimation_window` and five `event_window` vector fields to `mean(na.rm=TRUE)` — identical summarization to `.validate_grounding()` at advise.R:258-260. Adds two `cross_sectional` scalars directly. Returns `list(scalars=<finite numeric>, structural_ints=<integer event counts>)`.

3. `.is_grounded_literal(lit, scalars, structural_ints, abs_tol, rel_tol)`: five-step exemption chain in priority order:
   - Structural integer (event-count integers from meta/cross_sectional)
   - Year integer in 1900-2100 (citation years)
   - Universal significance constants c(0.001, 0.01, 0.05, 0.10)
   - Direct tolerance match: `max(abs_tol, rel_tol*abs(v))` — verbatim from advise.R:279
   - Rounding-aware: `abs(lit - round(v, dec_places)) <= abs_tol` where `dec_places` is the literal's displayed decimal precision

4. `.scan_prose_grounding(prose_fields, diag, abs_tol=, rel_tol=)`: iterates named prose fields, drops whole field to "" if any literal is ungrounded, emits exactly ONE warning via `sprintf("Prose grounding guard: %d section(s) dropped ...")` when `n_drop > 0`. Returns `list(sections=, n_dropped=)`.

**Task 3 — GROUND-03 regression lock:**
- `GROUND-03: fabricated literal ... never present in kept sections`: 4-field prose with `exec_summary` containing "99.99" → exactly 1 warning, exec_summary = "", "99.99" absent from `paste(unlist(sections))`.
- `GROUND-03: all-grounded prose passes with zero warnings`: prose built from real diag values (5 events, 0.45 R², 0.01 sigma, 0.025 IQR, 1997 year, 0.05 threshold) → 0 warnings, 0 drops, all sections retained.

## Test Results

| Suite | PASS | FAIL | SKIP | Notes |
|-------|------|------|------|-------|
| test_prose_grounding.R | 56 | 0 | 0 | All GROUND-01/02/03 tests green |
| Full suite | 1946 | 0 | 58 | Zero regressions; 58 pre-existing CRAN skips |

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 1 - Bug] Regex alternation order: 4-digit number split**
- **Found during:** Task 2 RED→GREEN iteration (first run showed 1997 → [199, 7])
- **Issue:** Research.md regex `(?:\\d{1,3}(?:,\\d{3})*|\\d+)` puts the 1-3-digit alternative first with `*` (zero or more commas), so "1997" matches "199" (1-3 digits, zero commas) then "7" as a separate match.
- **Fix:** Changed `*` to `+` so the thousands-separator alternative only matches when commas ARE present (`(?:\\d{1,3}(?:,\\d{3})+|\\d+)`). The plain `\\d+` fallback then matches "1997" in one shot.
- **Files modified:** R/advise.R (`.extract_numeric_literals`)
- **Commit:** e103972

**2. [Rule 1 - Bug] `expect_warning()` return value in testthat 3e**
- **Found during:** Task 2 test run (two failures: result$sections$exec_summary NULL)
- **Issue:** In testthat 3e, `result <- expect_warning(expr)` captures the warning condition, not the expression result. Plan showed pattern that assumed it captured the return value.
- **Fix:** Capture result in outer variable: `result <- NULL; expect_warning(result <- expr, regexp=...)`.
- **Files modified:** tests/testthat/test_prose_grounding.R
- **Commit:** e103972

## Acceptance Criteria Verification

- [x] GROUND-01: `.extract_numeric_literals()` passes all 9 spike assertions (decimal, year, %, range, thousands, sci-notation, empty)
- [x] GROUND-01: `.build_prose_value_registry()` returns finite scalars (vectors to mean) + structural_ints
- [x] GROUND-02: fabricated literal "99.99" → section dropped + exactly one warning
- [x] GROUND-02: rounded literal "2.35" for actual 2.3456 → KEPT (rounding-aware match)
- [x] GROUND-02: year 1997 → exempt (no false positive)
- [x] GROUND-02: significance constants 0.001/0.01/0.05/0.10 → exempt
- [x] GROUND-02: structural integer (n_events=5) → exempt
- [x] GROUND-03: "99.99" absent from any kept section
- [x] GROUND-03: all-grounded prose passes with zero warnings (Pitfall 1 guard)
- [x] Single-warning discipline: one warning per scan regardless of sections dropped
- [x] Tolerance formula `max(abs_tol, rel_tol*abs(v))` reused verbatim from advise.R:279
- [x] Exemption order: structural-int, year, significance-constant, tolerance, rounding

## Known Stubs

None. All four scanner internals are fully implemented. Integration into the Phase 18 section assembler is deferred to Phase 18 (as specified in the plan: "Phase 18 wires the scanner into the section assembler").

## Self-Check: PASSED

Files:
- R/advise.R — FOUND (four @noRd internals added after .validate_grounding)
- tests/testthat/test_prose_grounding.R — FOUND (56 tests, 0 FAIL)

Commits:
- bc7a38d: test(17-02): spike block for .extract_numeric_literals (RED, GROUND-01)
- e103972: feat(17-02): prose grounding scanner internals + GROUND-01/02/03 regression tests
