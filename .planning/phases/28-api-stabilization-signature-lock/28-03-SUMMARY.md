---
phase: 28-api-stabilization-signature-lock
plan: "03"
subsystem: testing
tags: [api-snapshot, testthat, snapshot-testing, r-package, cran, structural]

# Dependency graph
requires:
  - phase: 28-api-stabilization-signature-lock
    plan: "01"
    provides: reconciled API surface (plot_stocks sample_symbols rename + do_sample shim), deprecation lifecycle
  - phase: 28-api-stabilization-signature-lock
    plan: "02"
    provides: shape contracts landed (R/shape_contracts.R), reconciled surface stable
provides:
  - tests/testthat/test-api-snapshot.R — install-gated structural API snapshot test (APIS-03)
  - tests/testthat/_snaps/api-snapshot.md — committed snapshot pinning 77 exports, 38 plain fns, 38 R6 classes, 18 S3 methods
  - importFrom(methods,is) in NAMESPACE + methods in DESCRIPTION Imports (fixes pre-28-02 R CMD check NOTE)
affects: [29-install-tested-ci, 30-cran-resubmission]

# Actuals (#2632)
actuals:
  tokens: 14868    # 59472 chars / 4 over the files changed
  tasks: 2
  commits: 2
  plan_head_before: b4bb38a14edae3c0d809c6ed9ca9c5fc38996f4c

# Tech tracking
tech-stack:
  added: []
  patterns:
    - "expect_snapshot_value(style='json2') for structural R object snapshots; install-gated via skip_if_not_installed"
    - "Structural surface capture: sort(getNamespaceExports()) + per-function formals with sorted arg names + S3method() lines from NAMESPACE file"
    - "CRAN skip behavior: expect_snapshot_value(cran=FALSE default) + on_cran() returns TRUE in non-interactive Rscript without NOT_CRAN=true; test runs with NOT_CRAN=true on CI"

key-files:
  created:
    - tests/testthat/test-api-snapshot.R
    - tests/testthat/_snaps/api-snapshot.md
  modified:
    - R/shape_contracts.R (added @importFrom methods is)
    - NAMESPACE (added importFrom(methods,is))
    - DESCRIPTION (added methods to Imports)

key-decisions:
  - "Snapshot captures both sample_symbols (reconciled) and do_sample=NULL (backward-compat shim) — both are part of the reconciled public surface"
  - "expect_snapshot_value(style='json2') chosen for machine-readable, diff-friendly serialization; deterministic via sorted exports and sorted arg names per function"
  - "Snapshot skips under on_cran() TRUE (non-interactive Rscript without NOT_CRAN=true) — this is correct: on CRAN it skips; on CI with NOT_CRAN=true it runs"
  - "Auto-fixed pre-28-02 deviation: methods::is() used in R/shape_contracts.R without importFrom — fixed by adding importFrom(methods,is) + methods to DESCRIPTION Imports"

patterns-established:
  - "Install-gated structural snapshot: skip_if_not_installed + getNamespaceExports + formals + S3methods from NAMESPACE file"
  - "Drift detection: snapshot mismatch when surface changes; requires explicit snapshot_accept() in diff before CI passes"

requirements-completed: [APIS-03]

# Coverage metadata (#1602)
coverage:
  - id: D1
    description: "Install-gated structural API snapshot test created (test-api-snapshot.R) capturing 77 exports, 38 plain function formals, 38 R6 class names, 18 S3 methods — all sorted for determinism"
    requirement: APIS-03
    verification:
      - kind: unit
        ref: "tests/testthat/test-api-snapshot.R#APIS-03 (PASS=1, FAIL=0 with NOT_CRAN=true)"
        status: pass
    human_judgment: false
  - id: D2
    description: "Snapshot committed under _snaps/api-snapshot.md reflecting the reconciled surface (sample_symbols + do_sample shim on plot_stocks, post-28-01 reconciliation)"
    requirement: APIS-03
    verification:
      - kind: other
        ref: "tests/testthat/_snaps/api-snapshot.md (1328 lines, 42903 bytes, committed at d2db912)"
        status: pass
    human_judgment: false
  - id: D3
    description: "Drift detection proven: corrupting snapshot with fake_perturb_fn caused FAIL=1; reverting restored PASS=1"
    requirement: APIS-03
    verification:
      - kind: other
        ref: "Drift test: snapshot corrupted → FAIL=1; git checkout restore → PASS=1 (verified in Task 2)"
        status: pass
    human_judgment: false
  - id: D4
    description: "Full test suite green after snapshot addition; CRAN-clean with 1 pre-existing NOTE (matches Phase 25 baseline)"
    requirement: APIS-03
    verification:
      - kind: integration
        ref: "devtools::test() FAIL_TOTAL: 0; R CMD check --as-cran Status: 1 NOTE (pre-existing)"
        status: pass
    human_judgment: false

# Metrics
duration: 22min
completed: 2026-09-11
status: complete
---

# Phase 28 Plan 03: API Snapshot (APIS-03) Summary

**Structural install-gated API surface snapshot via expect_snapshot_value(style='json2') pinning 77 exports, 38 plain-function formals (sorted arg names), 38 R6 class names, and 18 S3 methods of the reconciled EventStudy 0.65.0 surface.**

## Performance

- **Duration:** 22 min
- **Started:** 2026-09-11T22:19:25Z
- **Completed:** 2026-09-11T22:42:01Z
- **Tasks:** 2
- **Files modified:** 5

## Accomplishments

- Created `tests/testthat/test-api-snapshot.R`: install-gated (skip_if_not_installed("EventStudy")) structural snapshot test; captures sorted exports, per-function formals as sorted arg-name → deparsed-default maps, sorted R6 class list, and sorted S3method() registrations from the installed NAMESPACE; stored via `expect_snapshot_value(style = "json2")`
- Committed `tests/testthat/_snaps/api-snapshot.md` (1328 lines): initial snapshot reflecting the RECONCILED surface — `sample_symbols` + `do_sample=NULL` shim on plot_stocks from plan 28-01, shape-contract option from plan 28-02; 77 exports, 38 plain functions, 38 R6 classes, 18 S3 methods
- Proved the lock bites: corrupted snapshot with injected `fake_perturb_fn`, test returned FAIL=1 (snapshot mismatch); reverted via `git checkout`; test restored to PASS=1, FAIL=0
- Full 2560+ test suite: FAIL_TOTAL=0 (6 pre-existing warnings unchanged)
- `R CMD check --as-cran (_R_CHECK_FORCE_SUGGESTS_=false)`: Status: 1 NOTE (pre-existing CRAN feasibility NOTE — matches Phase 25 baseline)
- Auto-fixed pre-28-02 deviation: `methods::is()` used unimported in `R/shape_contracts.R` caused a new `R CMD check` NOTE; fixed by adding `@importFrom methods is` + `methods` to DESCRIPTION Imports

## Task Commits

Each task was committed atomically:

1. **Task 1: End-to-end tracer — install-gated structural snapshot** — `d2db912` (feat)
2. **Task 2: Drift detection proof + full-suite + CRAN-clean** — `c547f9e` (fix)

## Files Created/Modified

- `tests/testthat/test-api-snapshot.R` — new; install-gated structural snapshot test
- `tests/testthat/_snaps/api-snapshot.md` — new; committed initial snapshot (1328 lines)
- `R/shape_contracts.R` — `@importFrom methods is` added (auto-fix for pre-28-02 NOTE)
- `NAMESPACE` — `importFrom(methods,is)` added by devtools::document()
- `DESCRIPTION` — `methods` added to Imports section

## Decisions Made

- **Snapshot captures both `sample_symbols` and `do_sample=NULL` (shim)**: The backward-compat shim is part of the installed public surface — it is intentionally included in the snapshot so that a future removal of the shim is a visible, deliberate change requiring `snapshot_accept()`.
- **style="json2"**: Machine-readable, human-diff-friendly; fully deterministic via sorted keys; `serializeJSON`/`unserializeJSON` roundtrip. Alternative `"deparse"` would also work but json2 is cleaner in diffs.
- **CRAN skip behavior is correct by design**: `expect_snapshot_value(cran=FALSE)` + `on_cran()=TRUE` in non-interactive Rscript causes automatic skip. This is the intended CRAN behavior. CI must set `NOT_CRAN=true` to run the test (Phase 29 wiring).
- **Auto-fix: methods import**: The `is()` function from `methods` package needed explicit importFrom in NAMESPACE and `methods` in DESCRIPTION Imports to eliminate the R CMD check NOTE introduced in plan 28-02.

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 1 - Bug] Missing importFrom(methods,is) from plan 28-02's shape_contracts.R**
- **Found during:** Task 2 (R CMD check --as-cran)
- **Issue:** `R/shape_contracts.R` line 162 uses `is(tbl[[col]], expected_type)` from the `methods` package without importing it. R CMD check --as-cran reported: `.check_shape: no visible global function definition for 'is'`. This was introduced in plan 28-02 but not caught there (plan 28-02 SUMMARY claimed 1 NOTE baseline, but 2 NOTEs were present).
- **Fix:** Added `@importFrom methods is` to the roxygen doc block in `R/shape_contracts.R`; ran `devtools::document()` to update NAMESPACE; added `methods` to DESCRIPTION Imports section.
- **Files modified:** `R/shape_contracts.R`, `NAMESPACE`, `DESCRIPTION`
- **Verification:** R CMD check --as-cran back to 1 NOTE (pre-existing CRAN feasibility); `is()` NOTE gone.
- **Committed in:** `c547f9e`

---

**Total deviations:** 1 auto-fixed (Rule 1 — pre-existing bug from plan 28-02 caught by Task 2 CRAN check)
**Impact on plan:** Fixed a real R CMD check NOTE that would have blocked Phase 30 CRAN resubmission. No scope change. All acceptance criteria met.

## Issues Encountered

None beyond the auto-fixed methods import. The drift detection proof worked as designed.

## Known Stubs

None — the snapshot is a complete structural capture of the reconciled surface, not a placeholder.

## Threat Surface Scan

No new network endpoints, auth paths, file access patterns, or schema changes. The snapshot test is purely read-only (reads installed namespace metadata). Threat T-28-04 (Tampering of committed `_snaps/` reference) is mitigated: any surface change requires explicit `testthat::snapshot_accept()` visible in the diff, so an unintended API break cannot pass silently (per plan threat model).

## Next Phase Readiness

- Phase 28 is COMPLETE: APIS-01 (plan 01), APIS-02 (plan 02), APIS-03 (plan 03), APIS-04 (plan 01) all done
- Phase 29 (Install-tested CI) can proceed: test-api-snapshot.R runs when NOT_CRAN=true is set; CI wiring in Phase 29 will configure this
- The snapshot locks the RECONCILED surface (post-28-01 rename, post-28-02 shape contracts)
- CRAN check clean: 0 ERROR / 0 WARNING / 1 pre-existing NOTE (Phase 25 baseline unchanged)

## Self-Check: PASSED

- tests/testthat/test-api-snapshot.R: FOUND
- tests/testthat/_snaps/api-snapshot.md: FOUND (1328 lines)
- Commit d2db912 (Task 1 feat): FOUND
- Commit c547f9e (Task 2 fix): FOUND
- skip_if_not_installed gate at line 20: FOUND
- Snapshot passes with NOT_CRAN=true: PASS=1, FAIL=0 CONFIRMED
- R CMD check Status: 1 NOTE (pre-existing): CONFIRMED

---
*Phase: 28-api-stabilization-signature-lock*
*Completed: 2026-09-11*
