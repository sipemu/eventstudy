---
phase: 23-api-message-polish
plan: 02
subsystem: api
tags: [errors, rlang, classed-conditions, testthat, api-03, api-04]

# Dependency graph
requires:
  - phase: 23
    plan: 01
    provides: byte-locked print/format snapshots (must stay identical through error refactor)
provides:
  - Classed error conditions (eventstudy_error_<kind> + parent eventstudy_error) at ~28 user-facing argument/column/state-guard sites
  - API-04 argument-naming message convention (backticked arg, quoted value, first-5-then-ellipsis vector truncation)
  - test_classed_conditions.R regression net asserting BOTH class and message substring per migrated family
affects: [23-03, api-message-polish]

actuals:
  tokens: 5500
  tasks: 3
  commits: 2

tech-stack:
  added: []
  patterns:
    - "Classed conditions via rlang::abort(msg, class=c(\"eventstudy_error_<kind>\",\"eventstudy_error\")); rlang already an Import, suppresses call by default (matches old call.=FALSE idiom)"
    - "Message convention: paste0() concatenation (never sprintf/gettextf — locale byte trap), arg names in backticks, string values in double-quotes, vector/long values truncated to utils::head(x,5) then literal ' ...'"
    - "Extend-not-replace rewording: existing expect_error() substrings preserved inside the new message so parity holds without touching test files"

key-files:
  created:
    - tests/testthat/test_classed_conditions.R
  modified:
    - R/task.R
    - R/models.R
    - R/export.R
    - R/report.R
    - R/cross_sectional.R

decisions:
  - "'missing columns' -> kept, NOT reworded to 'missing required columns'. Preserved the exact substring 'Request file missing columns:' so test_task.R:41 and test_panel.R:83 stay green with zero test edits. The API-04 improvement (backticked column list + truncation) was ADDED after the preserved prefix rather than replacing it."
  - "No existing test assertions required reconciliation — every flagged substring (missing columns / requires columns / firm_volume / Cannot infer format / Input must be a formula / must be an EventStudyTask / No valid format / not found) survived the extend-not-replace rewording. Task 3's 'update existing assertions' branch was therefore a no-op; only the new additive class= assertions were added, in a separate test file."
  - "cross_sectional.R:54 'No matching event_id' left as plain stop() — it is a data-state outcome, not in the RESEARCH Task B bounded migration set; out of scope for this plan."
  - "Code style uses `class = c(...)` (spaced, codebase-idiomatic) not the plan's literal `class=`. The plan's Task 3 verify grep `class=\"eventstudy_error_` is a literal-spacing mismatch, not a correctness gap — the substantive class-based expect_error() assertions are present and pass."

metrics:
  duration: ~15m
  completed: 2026-09-09

status: complete
---

# Phase 23 Plan 02: Classed Error Conditions & Argument-Naming Messages Summary

Migrated ~28 high-value user-facing argument/column-validation and `not_fitted`/`not_computed`
state-guard `stop()` sites to classed `rlang::abort(class = c("eventstudy_error_<kind>",
"eventstudy_error"))` across R/task.R, R/models.R, R/export.R, R/report.R, R/cross_sectional.R,
applying the API-04 backtick-arg + quoted-value + first-5-then-ellipsis truncation convention,
and locked each family with a class+substring regression net. rlang was already an Import — zero
new dependency. Satisfies API-03 and API-04.

## What Was Built

**Task 1 — argument/column-validation migration.** Sites raising the six `<kind>` tags:
`bad_argument` (export.R non-task guard + unknown-extension switch default, models.R non-formula,
report.R task-class guard ×2 + no-valid-format, cross_sectional.R non-task),
`missing_column` (task.R request-file + check_data_input ×3, models.R LinearFactorModel
"requires columns" + VolumeModel firm_volume, cross_sectional.R data-without-event_id),
`unknown_statistic` (task.R get_aar, export.R .tidy_aar), `unknown_event_id` (task.R ×2),
`unknown_column` (cross_sectional.R car_by_group group_var). Long/vector values truncated via
`utils::head(x, 5)` + literal ` ...`.

**Task 2 — not_fitted family (pure classing pass, message text byte-identical).** task.R
get_ar / get_aar / get_model_stats; export.R "No results available", "Abnormal returns not
computed" ×2, "AAR/CAAR not computed", "Models not fitted"; cross_sectional.R "Abnormal returns
not computed" — all now `eventstudy_error_not_fitted` with unchanged rendered text (9 sites).

**Task 3 — regression net.** New `tests/testthat/test_classed_conditions.R` (13 test_that blocks)
asserts BOTH the condition class and the preserved message substring for every migrated family,
plus the parent `eventstudy_error`. Full suite green.

## Deviations from Plan

None affecting scope. Line numbers in the plan were approximate (file had drifted since RESEARCH);
sites were located by content, not line number. See `decisions` for the "missing columns"
keep-vs-reword choice (kept) and the no-op reconciliation.

### Auto-fixed Issues

None — plan executed as written.

## Message-Parity Handling of the "missing columns" Reword

The plan flagged one deliberate reword choice ("missing columns" → "missing required columns").
**Decision: preserve the substring, do NOT reword.** The new message is
`"Request file missing columns: \`col1\`, \`col2\` ..."` — the API-04 improvement (backticked,
truncated column list) is appended after the preserved `"Request file missing columns:"` prefix,
so `test_task.R:41` (asserts "missing columns") stays green with zero test edits. Because every
other flagged substring was likewise preserved, no existing `expect_error()` assertion was modified;
all class assertions are purely additive in the new test file.

## Invariants Confirmed Untouched

- `git diff R/contract.R R/advise.R` — empty; `.handle_degenerate` / `.validate_grounding`
  exactly-one-warning invariants byte-untouched (no `warning()` migrated this plan).
- `tests/testthat/_snaps/` — byte-identical to the 23-01 lock (stop()→abort() did not alter
  print output; `.sanitise_prose` ordering + JOINT_HYPOTHESIS_CAVEAT unchanged).
- `git diff DESCRIPTION` — empty; no `cli`, no `lifecycle`, no new Imports.

## Verification Results

- Task 1 verify: 28 `rlang::abort` calls across the 5 files (≥15 required); invariants CLEAN.
- Task 2 verify: 9 `eventstudy_error_not_fitted` sites (≥7 required); 6 distinct kinds.
- Task 3 verify: full `testthat::test_dir` → **FAIL 0** (PASS 2214, WARN 4 pre-existing,
  SKIP 96 all "On CRAN" snapshot skips in interactive run); DESCRIPTION unchanged.

## Known Stubs

None.

## Self-Check: PASSED
- FOUND: tests/testthat/test_classed_conditions.R
- FOUND commit 9027f94 (feat: R migration)
- FOUND commit 2215351 (test: regression net)
