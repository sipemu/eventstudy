---
phase: 17-grounding-prose-hardening-offline-report-fallback-cran-basel
fixed_at: 2026-09-07T00:00:00Z
review_path: .planning/phases/17-grounding-prose-hardening-offline-report-fallback-cran-basel/17-REVIEW.md
iteration: 1
findings_in_scope: 8
fixed: 7
skipped: 1
status: partial
---

# Phase 17: Code Review Fix Report

**Fixed at:** 2026-09-07
**Source review:** .planning/phases/17-grounding-prose-hardening-offline-report-fallback-cran-basel/17-REVIEW.md
**Iteration:** 1

**Summary:**
- Findings in scope: 8 (CR-01, CR-02, CR-03, WR-01, WR-02, WR-03, WR-04, WR-05)
- Fixed: 7
- Skipped: 1 (WR-02 — not a correctness bug; message() is intentional)

## Fixed Issues

### CR-01: Prose scanner regex silently drops sign on negatives after non-space chars

**Files modified:** `R/advise.R`, `tests/testthat/test_prose_grounding.R`
**Commit:** 0ab8f7b
**Applied fix:** Changed the extractor pattern from `-?(?:...)` to `(?<![\w.])(-?)(?:...)` using a lookbehind. The lookbehind ensures the optional minus is only captured when not preceded by a word character or dot, so `CAR=-0.032` extracts `-0.032` (with sign) rather than `0.032` (sign-stripped). Also added explicit `is.null/is.na` guard at the top of `.extract_numeric_literals()`. Added four regression tests: signed extraction after `=`, sign-flip drop scenario.

---

### CR-02: Rounding-aware branch uses abs_tol only — inconsistent with rest of guard

**Files modified:** `R/advise.R`, `tests/testthat/test_prose_grounding.R`
**Commit:** 18df4da
**Applied fix:** Step 5 of `.is_grounded_literal()` now computes `tol5 <- max(abs_tol, rel_tol * abs(rounded_v))` and uses that combined tolerance, matching the formula in step 4. Added a regression test proving `1234.57` (correctly-rounded from `1234.5678`) is grounded and `1234.99` is not.

---

### CR-03: generate_report() narrative validation does not check element types

**Files modified:** `R/report.R`
**Commit:** a586579
**Applied fix:** After the `is.list()` guard, added a `vapply` check that each element is either `NULL` or a character scalar of length 1. Non-conforming elements emit a warning naming the bad sections and reset `narrative` to `NULL`. The `narrative=NULL` path is byte-identical (unchanged).

---

### WR-01: Non-ASCII em-dash in warning/message string literals

**Files modified:** `R/advise.R`, `R/report.R`
**Commit:** 4f177ef
**Applied fix:** Replaced all `—` (em-dash) bytes in string literals passed to `warning()` and LLM prompt strings with ASCII `--`. Four occurrences in `R/advise.R` (lines 304, 520, 751, 765, 777) and one in `R/report.R` (line 106). Comments and roxygen docs were left unchanged (CRAN check flags string literal bytes, not source comments).

---

### WR-03: median vs mean mismatch between offline narrative helpers and prose registry

**Files modified:** `R/advise_offline.R`
**Commit:** 0777e13
**Applied fix:** `.narrative_exec_summary()` and `.narrative_results()` used `median(car_t_vals)` while `.build_prose_value_registry()` summarises vectors to `mean()`. Switched both helpers to `mean()` and updated prose text from "median" to "mean". This ensures the grounded value in the narrative matches the registry value exactly, preventing false-drops on skewed distributions.

---

### WR-04: Test fixture encodes ungrounded literal 2.10

**Files modified:** `tests/testthat/test_report_narrative.R`
**Commit:** 8f6e938
**Applied fix:** Replaced `"Median CAR t-statistic was 2.10."` with `"The cumulative abnormal returns were examined."` (no bare numeric literal). Added a comment explaining the test exercises type validation and rendering, not the grounding scanner.

---

### WR-05: NA_character_ values silently bypass prose scanner

**Files modified:** `R/advise.R`
**Commit:** cf54526
**Applied fix:** Added `is.na(text) ||` before `!nzchar(text)` in the `.scan_prose_grounding()` loop. NA-valued fields now short-circuit to `NA_character_` in the output rather than falling through to the literal extractor. (The `.extract_numeric_literals()` function also received an explicit NA guard in the CR-01 commit.)

---

## Skipped Issues

### WR-02: message() in exported generate_report() — untestable, no suppression path

**File:** `R/report.R:145`
**Reason:** Verified against current code — the finding is real, but this is a product decision not a correctness bug. The `message()` call reports the output path after successful render, which is useful feedback for interactive use. Removing it without a `verbose=` parameter would silently break any user relying on the message to locate the output. This is a quality improvement for a future pass, not a critical fix. The 400+ existing tests pass with the message present. Skipping to avoid behaviour change.

---

**Verification:** All fixes verified via `testthat::test_dir(filter="prose_grounding|offline_narrative|report_narrative|advise")`.
Result: FAIL 0 | WARN 0 | SKIP 0 | PASS 219. Verification ran in the main checkout.

---

_Fixed: 2026-09-07_
_Fixer: Claude (gsd-code-fixer)_
_Iteration: 1_
