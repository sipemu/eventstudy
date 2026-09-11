---
phase: 28-api-stabilization-signature-lock
reviewed: 2026-09-12T00:00:00Z
depth: standard
files_reviewed: 7
files_reviewed_list:
  - R/deprecation.R
  - R/shape_contracts.R
  - R/execute.R
  - R/plotting.R
  - NAMESPACE
  - tests/testthat/test-deprecation.R
  - tests/testthat/test-shape-contracts.R
  - tests/testthat/test-api-snapshot.R
findings:
  critical: 0
  warning: 3
  info: 1
  total: 4
status: issues_found
---

# Phase 28: Code Review Report

**Reviewed:** 2026-09-12
**Depth:** standard
**Files Reviewed:** 8
**Status:** issues_found

## Summary

Phase 28 introduces a deprecation lifecycle helper (`.deprecate_arg()`), an option-gated return-shape contract system (`R/shape_contracts.R`), a `do_sample`→`sample_symbols` rename shim in `plot_stocks()`, and a structural API snapshot test. The architecture is sound and the core invariants (default-off, never-stop, lifecycle stays Suggests) are correctly honoured. Three warnings require attention before the next CRAN submission; none are correctness regressions on valid inputs.

## Warnings

### WR-01: `.deprecate_arg()` emits TWO warnings per call when `lifecycle` is installed

**File:** `R/deprecation.R:60-71`

**Issue:** The function unconditionally calls `.Deprecated(msg = msg)` (line 60) which emits one `deprecation` warning, then — when `lifecycle` is installed — also calls `lifecycle::deprecate_warn()` (line 66) which emits a second, independent warning. A user with `lifecycle` installed sees two warning messages per deprecated call. The policy stated in the roxygen block ("emit exactly one deprecation warning per call") and in the test comment ("exactly one deprecation warning per call") is violated in that common environment.

The test `DEPR-01` only asserts `length(w) >= 1L` (line 89 of `test-deprecation.R`), which passes whether one or two warnings are emitted — the "exactly one" claim in the comment on line 11 is not enforced by the assertion.

**Fix:** Gate the `.Deprecated()` call on `!requireNamespace("lifecycle", quietly = TRUE)`, so only one path fires:

```r
.deprecate_arg <- function(old, new, value, fn = NULL, when = "0.66.0") {
  msg <- paste0(
    if (!is.null(fn)) paste0("In `", fn, "()`: ") else "",
    "argument `", old, "` is deprecated as of EventStudy ", when, "; ",
    "please use `", new, "` instead."
  )

  if (requireNamespace("lifecycle", quietly = TRUE)) {
    lifecycle::deprecate_warn(
      when  = when,
      what  = paste0(if (!is.null(fn)) paste0(fn, "(") else "", old, if (!is.null(fn)) ")" else ""),
      with  = paste0(if (!is.null(fn)) paste0(fn, "(") else "", new, if (!is.null(fn)) ")" else "")
    )
  } else {
    .Deprecated(msg = msg)
  }

  invisible(value)
}
```

Also tighten the test assertion to `expect_equal(length(w), 1L)` to lock the invariant.

---

### WR-02: Shape-contract hook samples only `[[1]]` — multi-group tasks silently unchecked

**File:** `R/execute.R:165-169` (single-event hook) and `204-209` (multi-event hook)

**Issue:** When a task contains multiple groups or event IDs, `task$data_tbl[[sn]]` is a list column with one tibble per row. The hook only inspects `stat_col[[1]]` — the first row's tibble — and silently skips all subsequent rows. A shape drift introduced in row 2..N would go undetected with the contract enabled. The same pattern repeats for `task$aar_caar_tbl[[sn]][[1]]`. Given the stated goal ("lock column names and types of result tibbles against accidental structural drift"), checking only the first element undermines the contract's reliability when used in CI.

**Fix:** Iterate over all list-column elements, passing a context suffix that identifies the row:

```r
if (.resolve_shape_contract_mode()) {
  stat_names_se <- names(stats_tbl)
  for (sn in stat_names_se) {
    stat_col <- task$data_tbl[[sn]]
    if (!is.null(stat_col)) {
      for (i in seq_along(stat_col)) {
        .check_single_event_shape(stat_col[[i]], sn,
          context_suffix = paste0("row=", i))
      }
    }
  }
}
```

Apply the same change to the multi-event hook (lines 204-209).

---

### WR-03: `context_suffix` in `.check_single_event_shape()` / `.check_aar_caar_shape()` is never passed from the execute hooks

**File:** `R/execute.R:167, 207` and `R/shape_contracts.R:188-207`

**Issue:** Both dispatcher functions accept a `context_suffix` parameter intended to identify the event/group in warning messages (e.g., `"ART [event_id=E1]"`). The execute.R hooks always call them without that argument (`context_suffix = ""`), so any shape warning emitted when the contract is enabled only says `"ART"` with no identifying context. When multiple events exist the user cannot tell which row/event has the structural drift. This is a usability defect for the diagnostic tool's primary audience (CI maintainers).

**Fix:** Pass a meaningful suffix. At minimum, pass the row index (see WR-02 fix above). Better, join `task$data_tbl$event_id[[i]]` or `task$data_tbl$firm_symbol[[i]]` as the suffix.

---

## Info

### IN-01: `test-deprecation.R` DEPR-01 "exactly once" comment contradicts the `>= 1L` assertion

**File:** `tests/testthat/test-deprecation.R:11, 89`

**Issue:** The file header documents the contract as "exactly one deprecation warning per call" but line 89 asserts `expect_true(length(w) >= 1L)`, which allows two or more warnings. This makes the test non-falsifiable with respect to the exact-count invariant and masks the double-warning bug described in WR-01.

**Fix:** After resolving WR-01, change the assertion to:

```r
expect_equal(length(w), 1L)
```

---

_Reviewed: 2026-09-12_
_Reviewer: Claude (gsd-code-reviewer)_
_Depth: standard_
