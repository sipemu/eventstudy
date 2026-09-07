---
phase: 17-grounding-prose-hardening-offline-report-fallback-cran-basel
reviewed: 2026-09-07T00:00:00Z
depth: standard
files_reviewed: 8
files_reviewed_list:
  - R/advise.R
  - R/advise_offline.R
  - R/report.R
  - inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd
  - tests/testthat/test_advise.R
  - tests/testthat/test_offline_narrative.R
  - tests/testthat/test_prose_grounding.R
  - tests/testthat/test_report_narrative.R
findings:
  critical: 3
  warning: 5
  info: 3
  total: 11
status: issues_found
---

# Phase 17: Code Review Report

**Reviewed:** 2026-09-07
**Depth:** standard
**Files Reviewed:** 8
**Status:** issues_found

## Summary

Phase 17 delivers an offline rule-based narrative engine (`OfflineNarrative` S3), routing surgery that moves `report_writing` from `LLM_ONLY_TYPES` to `KB_TYPES`, a prose grounding scanner (`GROUND-01/02/03`), a `narrative=NULL` backward-compatible seam in `generate_report()`, and CRAN hygiene guards in tests.

The implementation is architecturally sound and the grounding guard's drop-and-keep contract is well-structured. However, three correctness bugs were found: (1) the prose grounding scanner contains a regex that silently fails on negative numbers, causing false negatives that let a fabricated negative literal pass the guard unchecked; (2) the rounding-aware branch in `.is_grounded_literal()` uses only `abs_tol` (not the combined `max(abs_tol, rel_tol * abs(v))` formula that the rest of the guard uses), creating inconsistent tolerance behaviour on large values; and (3) the `narrative` validation in `generate_report()` does not catch a named list containing non-character values, so a partial-fabrication path can reach the template unguarded. Two CRAN-compliance warnings (non-ASCII `—` characters in warning strings, and a `message()` call in an exported function) are also noted.

---

## Critical Issues

### CR-01: Prose scanner regex silently drops negative numeric literals — false negatives let fabricated negatives through

**File:** `R/advise.R:347`
**Issue:** The extractor pattern is:
```
"-?(?:\\d{1,3}(?:,\\d{3})+|\\d+)(?:\\.\\d+)?(?:[eE][+-]?\\d+)?"
```
The optional sign `-?` is part of the outer pattern, but `gregexpr` with `perl=TRUE` is applied to the full string. When a negative number appears mid-sentence (e.g. `"The mean CAR was -0.032 over the window."`), the `-` immediately follows a space, so the engine does match `-0.032` — BUT if the negative number appears directly after a letter or parenthesis (e.g. `"t-statistic=-0.032"` or `"(CAR=-0.032)"`), the `-` is consumed as part of the preceding token boundary and `0.032` is extracted without its sign. The extracted literal `0.032` then gets matched against the registry value `-0.032`; the absolute difference is `0.064`, which easily exceeds any reasonable tolerance. Result: the scanner falsely believes `0.032` is ungrounded (false positive), but also if the registry contains `0.032` coincidentally, the negative literal passes undetected (false negative).

More critically: if a fabricated value `-99.99` appears as `"=-99.99"`, the extracted value is `99.99` (sign stripped), which will not match any registry entry and the section is correctly dropped. However if `-0.045` appears where the registry has `0.045` (e.g. a sign-flipped CAR), the scanner silently passes it as grounded — a fabrication slip.

Additionally, the pattern requires at least one digit before the decimal point (`\d+` before `(?:\.\d+)?`), so literals like `.032` (uncommon but valid) are not extracted and pass through entirely unchecked.

**Fix:** Anchor the sign to the start of a number token using a word-boundary or lookbehind, and test both extracted sign and sign-less form:
```r
# Use a lookbehind to avoid consuming operator characters as sign
pattern <- "(?<![\\w.])(-?)(?:\\d{1,3}(?:,\\d{3})+|\\d+)(?:\\.\\d+)?(?:[eE][+-]?\\d+)?"
```
Or, simpler — after extracting with the current pattern, also try extracting with `[-]?` anchored to `\\b`:
```r
pattern <- "(?<=^|[^\\w.-])-?(?:\\d{1,3}(?:,\\d{3})+|\\d+)(?:\\.\\d+)?(?:[eE][+-]?\\d+)?"
```
At minimum, add a test for `"mean CAR=-0.032"` in `test_prose_grounding.R` to expose the boundary failure before fixing.

---

### CR-02: Rounding-aware match in `.is_grounded_literal()` uses `abs_tol` only — inconsistent with the rest of the guard

**File:** `R/advise.R:460`
**Issue:** Step 5 (rounding-aware match) is:
```r
if (abs(lit - round(v, dec_places)) <= abs_tol) return(TRUE)
```
Every other numeric comparison in the guard (`.validate_grounding()` at line 279, and step 4 here at line 446) uses:
```r
tol <- max(abs_tol, rel_tol * abs(v))
if (abs(lit - v) <= tol) return(TRUE)
```
Using only `abs_tol` (default `1e-6`) in the rounding-aware branch means: for a registry value `v = 1234.5678` the combined tolerance would be `max(1e-6, 1e-4 * 1234.5678) = 0.12345`, but the rounding-aware branch uses `1e-6`. After rounding `v` to 4 decimal places, `round(1234.5678, 4) = 1234.5678`; the comparison `abs(1234.57 - 1234.5678) <= 1e-6` is `FALSE`. The correctly-rounded literal `1234.57` (2 decimal places) against `round(1234.5678, 2) = 1234.57` gives `abs(0) <= 1e-6` which happens to pass. But for values where floating-point rounding leaves a residual (e.g. `round(2.005, 2)` is `2` in IEEE 754), the `abs_tol`-only guard rejects a correctly-rounded literal it should pass.

This is inconsistent with documented behaviour and will cause false positives (correct prose dropped) when diagnostics contain values subject to floating-point round-half-even behaviour.

**Fix:**
```r
# Step 5 — rounding-aware (use same combined tolerance as step 4)
for (v in scalars) {
  if (!is.finite(v)) next
  rounded_v <- round(v, dec_places)
  tol5 <- max(abs_tol, rel_tol * abs(rounded_v))
  if (abs(lit - rounded_v) <= tol5) return(TRUE)
}
```

---

### CR-03: `generate_report()` narrative validation does not check that list elements are character scalars — partial-fabrication path reaches template unguarded

**File:** `R/report.R:114-120`
**Issue:** The validation accepts any `is.list(narrative)` without checking that the individual section values are character scalars:
```r
if (!is.null(narrative) && !is.list(narrative)) {
  warning(...)
  narrative <- NULL
}
```
A caller can pass `narrative = list(exec_summary = 42L, data_methods = list(a=1))`. This passes validation and reaches the Rmd template, where `cat(narrative$exec_summary)` with an integer silently coerces `42L` to `"42"`, and `cat(narrative$data_methods)` with a list causes an error inside the knitr chunk — crashing the report mid-render with an opaque knitr error rather than a clean pre-flight warning.

More importantly, the prose grounding scanner (`.scan_prose_grounding()`) is never applied to the `narrative` list inside `generate_report()` itself. The scanner exists and is tested, but nothing calls it on the incoming `narrative=` argument. The report's "no-fabrication" invariant is therefore only enforced if the caller explicitly invokes `.scan_prose_grounding()` before passing the list — an assumption that is not documented and not enforced at the boundary.

**Fix — two parts:**

Part 1: Validate that each value is a character scalar (or NULL/empty):
```r
if (!is.null(narrative)) {
  if (!is.list(narrative)) {
    warning("generate_report(): 'narrative' must be a named list or NULL — narrative will be skipped.",
            call. = FALSE)
    narrative <- NULL
  } else {
    bad <- !vapply(narrative, function(v) is.null(v) || (is.character(v) && length(v) == 1L),
                   logical(1L))
    if (any(bad)) {
      warning(
        sprintf("generate_report(): narrative section(s) %s are not character scalars — narrative will be skipped.",
                paste(names(narrative)[bad], collapse = ", ")),
        call. = FALSE
      )
      narrative <- NULL
    }
  }
}
```

Part 2: Document (at minimum) that callers are responsible for grounding scanning before calling `generate_report(narrative=...)`. Ideally, add an `es_diagnostics` optional parameter and call `.scan_prose_grounding()` automatically if both are supplied.

---

## Warnings

### WR-01: Non-ASCII Unicode escape `—` in warning/message strings — potential CRAN NOTE

**File:** `R/advise.R:304, 748, 762, 774`
**Issue:** The package emits warning strings containing `—` (em-dash). While R source files permit `\uXXXX` escapes in string literals without triggering `R CMD check --as-cran` NOTEs by default, CRAN's policy on non-ASCII characters in message strings is stricter: `tools::showNonASCIIfile()` and `R CMD check --as-cran` will flag any non-ASCII bytes in installed package strings on some platforms. The CLAUDE.md constraint is "No new R CMD check NOTEs/WARNINGs."

The em-dash also appears in the `print.Advice` block comment at line 749 inside a string passed to `paste0()`, not just a comment, so it will be present in the installed `.rdb` binary.

**Fix:** Replace em-dash with ASCII equivalent in all string literals:
```r
# Line 304:
"Grounding guard: %d recommendation(s) dropped -- evidence cited absent or mismatched diagnostic values."
# Lines 748, 762, 774: replace — with " -- " or " - "
```

---

### WR-02: `message()` in exported `generate_report()` — untestable, no suppression path

**File:** `R/report.R:145`
**Issue:**
```r
message("Report generated: ", output_path)
```
CRAN policy discourages `message()` in library functions because it cannot be suppressed by `suppressMessages()` in non-interactive pipelines without wrapping the entire call. More critically: `generate_report()` returns `invisible(output_path)`, so the path is available to the caller. The `message()` provides no additional information the caller cannot obtain from the return value, and it fires even when the caller has already specified `quiet = TRUE` in `...` (which is passed to `rmarkdown::render`, not to this message). Any CI pipeline running `generate_report()` in a test will produce noise.

**Fix:**
```r
# Replace message() with a comment that the path is returned invisibly,
# or gate on a verbose= parameter:
invisible(output_path)
```
If a progress message is desired, gate it: `if (isTRUE(getOption("EventStudy.verbose", FALSE))) message(...)`.

---

### WR-03: `.build_offline_narrative()` narrative sections may contain numeric literals from `sprintf()` that will not pass `.scan_prose_grounding()` if the prose is later re-scanned

**File:** `R/advise_offline.R:306-318, 339-360, 366-408, 413-468`
**Issue:** The offline narrative helpers build prose strings with `sprintf("%.3f", med_car_t)`, `sprintf("%.4f", mean_sig)`, etc. These values ARE from diagnostics — the no-fabrication rule is satisfied at generation time. However, the section strings are returned as the `OfflineNarrative` S3 fields and are then passed (after manual coercion to a list) to `generate_report(narrative=...)`.

If a downstream user or test later calls `.scan_prose_grounding()` on these sections (as the GROUND-03 test does via `test_report_narrative.R:227-255`), the scanner will check whether each `%.3f`/`%.4f`-formatted literal matches the registry. This works correctly **only if** the registry summarises identically (mean of vectors). But the `exec_summary` section uses `median(car_t_vals)` (line 291, advise_offline.R), while `.build_prose_value_registry()` summarises `event_window$car_t` to its `mean` (advise.R:394). A median and a mean will differ whenever the distribution is skewed, causing the scanner to DROP the exec_summary section as "fabricated" even though the value was computed from diagnostics.

This is a latent false-positive bug that will manifest whenever `car_t` values are skewed (common in real event studies). The test fixture uses `c(2.1, 1.5, 3.0, 0.2, 2.5)` where `median=2.1` but `mean=1.86` — the scanner would drop `exec_summary` if it were scanned.

**Fix:** Either (a) unify the summary function — use mean throughout the offline narrative helpers, or (b) extend `.build_prose_value_registry()` to also include `median()` summaries of vector fields, or (c) document clearly that the OfflineNarrative output is pre-grounded at generation time and must NOT be re-scanned.

---

### WR-04: `test_report_narrative.R` "advice=NULL with a valid narrative list" test uses ungrounded numeric literal `2.10` — will fail if grounding scanning is ever applied

**File:** `tests/testthat/test_report_narrative.R:239`
**Issue:**
```r
narrative_list <- list(
  ...
  results = "Median CAR t-statistic was 2.10.",
  ...
)
```
The value `2.10` does not appear in the test fixture's diagnostics (`car_t` mean is `~1.86`, median is `2.1`). If `.scan_prose_grounding()` is ever called on this list (e.g. if CR-03's fix is implemented), this test will produce a warning and the results section will be dropped, causing the render test to behave differently than expected. The test currently does not apply the scanner, so it passes now — but it encodes a bad example of what valid narrative prose looks like.

**Fix:** Replace `2.10` with a value actually present in the test fixture registry, or add a comment explaining this narrative is intentionally not pre-scanned.

---

### WR-05: `.scan_prose_grounding()` does not handle `NULL` or `NA` values within the `prose_fields` list — will error on unexpected input

**File:** `R/advise.R:491-495`
**Issue:**
```r
for (nm in names(prose_fields)) {
  text <- prose_fields[[nm]] %||% ""
  if (!nzchar(text)) { ... }
```
The `%||%` operator handles `NULL`, but if a value is `NA_character_`, `nzchar(NA_character_)` returns `NA` (not `FALSE`), causing the `if (!nzchar(text))` guard to evaluate `!NA` = `NA`, which R treats as `FALSE` — meaning NA-valued fields fall through to `.extract_numeric_literals(NA_character_)`. Inside `.extract_numeric_literals()`, `!nzchar(text %||% "")` handles `NULL` but not `NA`: `nzchar(NA_character_)` = `NA`, `!NA` = `NA`, so the function body proceeds to `gregexpr(pattern, NA_character_)` which returns `list(integer(0))` with attribute `match.length = -1` — no crash, but silent pass-through of an NA field with no literals extracted (treated as "all grounded"). This is a silent correctness hole: an NA narrative section passes grounding checks.

**Fix:**
```r
text <- prose_fields[[nm]] %||% ""
if (is.na(text) || !nzchar(text)) {
  sections_kept[[nm]] <- if (is.na(text)) NA_character_ else text
  next
}
```
Or, at the top of `.extract_numeric_literals()`:
```r
if (is.null(text) || length(text) == 0L || is.na(text) || !nzchar(text)) return(numeric(0L))
```

---

## Info

### IN-01: `CANNED_JSON_THREE_RECS` in test fixture is conditionally defined — test failure mode is confusing

**File:** `tests/testthat/helper-advice-fixtures.R:77`
**Issue:**
```r
CANNED_JSON_THREE_RECS <- if (requireNamespace("jsonlite", quietly = TRUE)) {
  jsonlite::toJSON(...)
} else {
  NULL
}
```
When `jsonlite` is absent, `CANNED_JSON_THREE_RECS` is `NULL`. Several tests in `test_advise.R` (lines 64, 130) pass this directly to `CustomProvider$new(function(prompt, schema) CANNED_JSON_THREE_RECS)`, which would return `NULL` from the provider. The resulting test failure message would be "provider returned no text" rather than "jsonlite not installed", making CI failures opaque. Since `jsonlite` is already guarded everywhere it's used in production code, the fixture should either `skip_if_not_installed("jsonlite")` in each test that uses it, or use a raw JSON string literal (not `toJSON`) for maximum portability.

---

### IN-02: `print.Advice` at line 864 calls `.advisor_pro_footer()` — undocumented internal dependency not visible in this file

**File:** `R/advise.R:864`
**Issue:** `print.Advice` and `print.es_advice` (advise_offline.R:168) both call `.advisor_pro_footer()` without any `tryCatch` or existence check. If this function is defined in another file that is conditionally loaded or removed in a future refactor, both print methods will silently error at the user-visible output stage. This is a fragile coupling.

---

### IN-03: `skeleton.Rmd` eval guard for narrative section uses `!is.null(params$narrative)` but inner body re-checks `is.list(narrative)` — redundant double-check with wrong coverage

**File:** `inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd:220-243`
**Issue:** The chunk eval guard is `eval=!is.null(params$narrative)`. Inside the chunk, the code additionally checks `if (!is.null(narrative) && is.list(narrative))`. This means:
1. If `narrative` is a non-list (e.g. `"string"`), the outer guard fires the chunk, but the inner guard silently does nothing — no error, no user feedback in the rendered document. The caller already got a warning from `generate_report()`, but the rendered HTML gives no indication the narrative was skipped.
2. The double-check is redundant if `generate_report()`'s validation (report.R:114-120) always coerces invalid narratives to NULL before passing to `rmarkdown::render()`. If that invariant holds, the inner check in the Rmd is dead code. If it doesn't hold (per CR-03), the inner check is the only safety net but provides no user-visible feedback.

A comment explaining which layer owns the invariant would prevent future confusion.

---

_Reviewed: 2026-09-07_
_Reviewer: Claude (gsd-code-reviewer)_
_Depth: standard_
