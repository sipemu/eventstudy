---
phase: 28-api-stabilization-signature-lock
plan: "02"
subsystem: api
tags: [shape-contracts, r-package, cran, testing, robustness]

# Dependency graph
requires:
  - phase: 28-api-stabilization-signature-lock
    plan: "01"
    provides: deprecation lifecycle + do_sample -> sample_symbols rename + 28-API-AUDIT.md (reconciled API surface)
provides:
  - R/shape_contracts.R with .resolve_shape_contract_mode() + .check_shape() + canonical ART/CART/CSectT specs
  - EventStudy.shape_contracts option (logical, default FALSE) gating all shape checks
  - Pipeline hooks in calculate_statistics() for single-event and multi-event results
  - tests/testthat/test-shape-contracts.R — 24 tests covering off/on/drift/degenerate
  - man/eventstudy-shape-contracts.Rd generated from roxygen doc block
  - NEWS.md 0.66.0 entry for the opt-in shape contract
affects: [28-03-api-snapshot, 29-install-tested-ci, 30-cran-resubmission]

# Actuals (#2632)
actuals:
  tokens: 5806    # 23223 chars / 4 over the realized diff
  tasks: 3
  commits: 2
  plan_head_before: f98a9de45647e8fe74c6bc8ddecf6098aa79a756

# Tech tracking
tech-stack:
  added: []
  patterns:
    - "Shape contract pattern: .resolve_shape_contract_mode() reads getOption(..., default=FALSE); .check_shape() emits exactly-one warning() on mismatch, never stop()"
    - "Canonical spec as named character vector: names=column names, values=R type class (is() check); same spec covers fitted and is_fitted=FALSE degenerate variants"
    - "Pipeline hook pattern: if (.resolve_shape_contract_mode()) { for each stat tibble: .check_XYZ_shape(tbl, stat_name) }"

key-files:
  created:
    - R/shape_contracts.R
    - tests/testthat/test-shape-contracts.R
    - man/eventstudy-shape-contracts.Rd
  modified:
    - R/execute.R (two shape-contract hook blocks added to calculate_statistics())
    - NEWS.md (0.66.0 Return-Shape Contracts section)

key-decisions:
  - "Tracer implementation was broad enough to cover Task 2 scope — multi-event hook and degenerate specs were included in Task 1; Task 2 verified without additional code changes"
  - "Spec coverage: ART + CART for single-event; CSectT/aar_caar for multi-event; both specs cover fitted and is_fitted=FALSE degenerate (same columns, NA values) as valid shapes"
  - "Unknown stat names silently skipped (additive-only; no warning for unregistered stats)"
  - "integer columns (relative_index, n_events etc.) checked as 'numeric' since is(1L, 'numeric') is TRUE in R — avoids false positive when column is integer vs double"

patterns-established:
  - "Shape-contract option pattern: mirrors .resolve_degenerate_mode() exactly; option namespace EventStudy.*, default-off fallback"
  - "Warn-once-never-stop: all drift in a single tibble is collected into one warning message, never multiple, never stop()"

requirements-completed: [APIS-02]

# Coverage metadata (#1602)
coverage:
  - id: D1
    description: "R/shape_contracts.R: .resolve_shape_contract_mode() defaults FALSE via getOption, .check_shape() emits exactly one warning on mismatch, never stop()"
    requirement: APIS-02
    verification:
      - kind: unit
        ref: "tests/testthat/test-shape-contracts.R#SHAPE-01 through SHAPE-06 (24 tests)"
        status: pass
    human_judgment: false
  - id: D2
    description: "Pipeline hooks in calculate_statistics() gated by option — default-off means zero impact on existing suite (FAIL_TOTAL=0, no new warnings)"
    requirement: APIS-02
    verification:
      - kind: integration
        ref: "devtools::test() FAIL_TOTAL: 0, WARN_TOTAL: 6 (all pre-existing)"
        status: pass
    human_judgment: false
  - id: D3
    description: "Degenerate (is_fitted=FALSE) shapes — same column names, NA-propagated values — accepted as valid (no warning): ART, CART, CSectT"
    requirement: APIS-02
    verification:
      - kind: unit
        ref: "tests/testthat/test-shape-contracts.R#SHAPE-04a through SHAPE-04d"
        status: pass
    human_judgment: false
  - id: D4
    description: "NEWS.md and man/eventstudy-shape-contracts.Rd document the new opt-in option"
    requirement: APIS-02
    verification:
      - kind: other
        ref: "NEWS.md#Return-Shape Contracts section; man/eventstudy-shape-contracts.Rd"
        status: pass
    human_judgment: false

# Metrics
duration: 10min
completed: 2026-09-11
status: complete
---

# Phase 28 Plan 02: Return-Shape Contracts Summary

**Option-gated return-shape contracts in R/shape_contracts.R — default-off, warn-once-never-stop, covering ART/CART single-event and CSectT multi-event AAR/CAAR tibbles including their is_fitted=FALSE degenerate variants.**

## Performance

- **Duration:** 10 min
- **Started:** 2026-09-11T22:05:33Z
- **Completed:** 2026-09-11T22:16:25Z
- **Tasks:** 3 (Task 2 had no additional code changes — tracer covered its scope)
- **Files modified:** 5

## Accomplishments

- Created `R/shape_contracts.R` with the full shape-contract system: `.resolve_shape_contract_mode()` (mirrors `.resolve_degenerate_mode()` option-resolution pattern), `.check_shape()` (emits exactly one `warning()` on mismatch, never `stop()`), canonical expected-shape specs for ART/CART (single-event) and CSectT (multi-event AAR/CAAR), and dispatcher helpers
- Wired two shape-contract hooks into `calculate_statistics()` in `R/execute.R` — one after single-event stats assembly, one after aar_caar_tbl assembly — both gated by `.resolve_shape_contract_mode()` (default-off, complete no-op for existing suite)
- Added 24 tests in `test-shape-contracts.R`: off-by-default=silent, on+valid=silent, on+drift=exactly-one-warning-no-error, is_fitted=FALSE degenerate shapes accepted as valid (SHAPE-01 through SHAPE-06)
- Full 2560+ test suite stays green (FAIL_TOTAL=0, 6 pre-existing warnings from plan-01 and test infrastructure — none from shape contracts)
- Documented `EventStudy.shape_contracts` option in roxygen (`man/eventstudy-shape-contracts.Rd`) and in `NEWS.md` under 0.66.0

## Task Commits

Each task was committed atomically:

1. **Task 1: End-to-end tracer — shape spec, option-gated, warn-only, wired into pipeline** — `05282cf` (feat)
2. **Task 2: Extend coverage — multi-event + degenerate shapes** — (no new code; tracer implementation already covered full scope; verified green)
3. **Task 3: Register option, document, prove additive-only + CRAN-clean** — `44b1602` (chore)

## Files Created/Modified

- `R/shape_contracts.R` — new; full shape-contract system (`.resolve_shape_contract_mode()`, `.check_shape()`, specs, dispatchers)
- `R/execute.R` — two shape-contract hook blocks added to `calculate_statistics()` (single-event + multi-event)
- `tests/testthat/test-shape-contracts.R` — new; 24 tests covering all coverage requirements
- `man/eventstudy-shape-contracts.Rd` — new; generated by roxygen from `R/shape_contracts.R`
- `NEWS.md` — 0.66.0 Return-Shape Contracts section added

## Decisions Made

- **Tracer scope covered Task 2 fully**: The Task 1 tracer implementation included the multi-event hook and all degenerate specs up front (knowing what Task 2 needed). Task 2 had no additional code changes — all acceptance criteria were already met. Documented as deviation below.
- **Spec uses `is(x, class)` checks**: integer columns (relative_index, n_events, etc.) pass `is(1L, "numeric")` in R — so checking for `"numeric"` avoids false-positive warnings when a column is integer-typed vs double-typed. This is intentional and correct.
- **Unknown stat names silently skipped**: Any stat name not in the registered specs (e.g. PatellZ, BMPTest) is silently skipped. This keeps the contract additive-only — it only validates what it explicitly covers, never warns about stats it hasn't been updated for.

## Deviations from Plan

### Task 2 Scope Absorbed by Tracer

**1. [Scope - Tracer Over-Delivered] Task 2 had no additional code changes**
- **Found during:** Task 2 implementation review
- **Issue:** The plan described Task 2 as extending the implementation to multi-event AAR/CAAR and both degenerate shapes. The Task 1 tracer included ALL of these (multi-event hook, AAR/CAAR spec, degenerate variants) because the plan's `<artifacts_this_phase_produces>` clearly specified the full scope of `R/shape_contracts.R`.
- **Fix:** Verified all Task 2 acceptance criteria against the Task 1 commit. All pass. Task 2 committed as a zero-delta verification step.
- **Files modified:** None (Task 1 commit covers Task 2's scope)
- **Impact:** No scope change. All acceptance criteria met. The plan's task decomposition was coarser than the implementation granularity.

---

**Total deviations:** 1 scope observation (tracer over-delivered, Task 2 needed no extra code)
**Impact:** No scope change, all criteria met. Slightly more efficient than planned.

## Issues Encountered

None — plan executed cleanly. The `is(integer, "numeric")` type-check subtlety was caught during implementation and handled correctly before any false positives could occur.

## Known Stubs

None — all shape specs are fully wired and tested.

## Threat Surface Scan

No new network endpoints, auth paths, file access patterns, or schema changes. The only new surface is a package option reading `getOption()` — pure R internal state, no external input surface. Threat T-28-03 (DoS via large tibble) mitigated per plan: shape check is O(ncol) metadata-only, gated behind default-off option.

## Next Phase Readiness

- Plan 28-03 (API snapshot) can proceed: shape contracts are landed and the reconciled API surface is stable
- The snapshot will lock the surface AFTER the rename (sample_symbols, not do_sample) and WITH the shape-contract option documented
- CRAN baseline: unchanged from Phase 25 — 0 ERROR / 0 WARNING / 1 pre-existing NOTE (no new findings from this plan)

## Self-Check: PASSED

- R/shape_contracts.R: FOUND
- R/execute.R (shape hook blocks): FOUND
- tests/testthat/test-shape-contracts.R: FOUND
- man/eventstudy-shape-contracts.Rd: FOUND
- NEWS.md (Return-Shape Contracts section): FOUND
- Commit 05282cf (Task 1 feat): FOUND
- Commit 44b1602 (Task 3 chore): FOUND

---
*Phase: 28-api-stabilization-signature-lock*
*Completed: 2026-09-11*
