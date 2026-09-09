---
phase: 23-api-message-polish
plan: 01
subsystem: api
tags: [s3, print, format, snapshot, testthat, roxygen]

# Dependency graph
requires:
  - phase: 22
    provides: v0.64.0 print methods + prose sanitiser invariants being locked
provides:
  - Snapshot regression net for all 6 print.* methods (byte-locked baseline)
  - Snapshot lock for the 4 prose-sanitiser variants (ampersand-first ordering)
  - format.* S3 methods for all 6 classes returning a character vector
  - Uniform thin print.* delegating via cat(format(x), sep="\n"); invisible(x)
affects: [23-02, 23-03, api-message-polish]

actuals:
  tokens: 9000
  tasks: 3
  commits: 3

tech-stack:
  added: []
  patterns:
    - "print/format split: format.<Class> builds the char vector via utils::capture.output of the original cat() body; print.<Class> is cat(format(x), sep=\"\\n\"); invisible(x)"
    - "Snapshot-first refactor gate: commit expect_snapshot baseline against unmodified code, then require git diff --quiet on the snapshot file after the refactor"

key-files:
  created:
    - tests/testthat/test-print-snapshots.R
    - tests/testthat/test-prose-sanitise-snapshots.R
    - tests/testthat/_snaps/print-snapshots.md
    - tests/testthat/_snaps/prose-sanitise-snapshots.md
    - man/format.Advice.Rd (+ 5 more format.*.Rd)
  modified:
    - R/advise.R
    - R/task.R
    - R/advise_offline.R
    - R/cross_sectional.R
    - R/es_diagnostics.R
    - R/simulation.R
    - NAMESPACE

key-decisions:
  - "Implemented format.* via utils::capture.output of the original cat() body rather than manually translating each cat() into paste()/vector elements — this is byte-exact by construction (reproduces cat's multi-arg space separation and trailing-newline behaviour) and keeps every rounding/width/wording detail identical."
  - "Advisor Pro footer emission moved INTO format.Advice/format.es_advice (captured inside capture.output; a no-op unless the opt-in option is set) and is NOT re-called in print.* — prevents the double-emit pitfall."

patterns-established:
  - "print/format split pattern for R6/S3 result objects: format returns lines, print delegates and returns invisible(x)"
  - "Snapshot baseline committed before a refactor as the byte-identical guard (git diff --quiet on _snaps)"

requirements-completed: [API-01, API-02]

coverage:
  - id: D1
    description: "All 6 print.* methods return invisible(x) uniformly (API-01)"
    requirement: "API-01"
    verification:
      - kind: unit
        ref: "tests/testthat/test-print-snapshots.R#print.* snapshots (8 fixtures, both branches)"
        status: pass
    human_judgment: false
  - id: D2
    description: "A format.* method exists and is exported for all 6 classes; print delegates to it (API-02)"
    requirement: "API-02"
    verification:
      - kind: unit
        ref: "grep 'S3method(format,' NAMESPACE == 6 && print bodies == cat(format(x), sep=\"\\n\"); invisible(x)"
        status: pass
    human_judgment: false
  - id: D3
    description: "Console output of print() on all 6 classes is byte-identical to pre-refactor code (snapshot-locked)"
    verification:
      - kind: unit
        ref: "git diff --quiet tests/testthat/_snaps/print-snapshots.md after refactor"
        status: pass
    human_judgment: false
  - id: D4
    description: "Prose sanitiser (.sanitise_prose + 3 variants) output snapshot-locked and unchanged"
    verification:
      - kind: unit
        ref: "tests/testthat/test-prose-sanitise-snapshots.R#.sanitise_* snapshots"
        status: pass
    human_judgment: false

# Metrics
duration: 8min
completed: 2026-09-09
status: complete
---

# Phase 23 Plan 01: Snapshot-locked print/format split Summary

**Committed a byte-locked snapshot net for all 6 print methods + 4 prose-sanitiser variants against unmodified code, then split each print.* into an exported format.* (character vector via capture.output) + a thin delegating print.*, proven byte-identical by the committed snapshots.**

## Performance

- **Duration:** 8 min
- **Started:** 2026-09-09T07:31:15+02:00
- **Completed:** 2026-09-09T07:39:27+02:00
- **Tasks:** 3
- **Files modified:** 19 (6 R/ + 2 test + 2 _snaps + NAMESPACE + 8 man/)

## Accomplishments
- Snapshot baseline for all 6 print methods (EventStudySummary, es_diagnostics, es_simulation, es_cross_sectional, Advice, es_advice) — both fitted and degenerate/zero-rules branches — captured against UNMODIFIED code and committed first (the byte-identical guard for the whole phase).
- Snapshot lock for the prose sanitiser (.sanitise_universal, .sanitise_for_pdf, .sanitise_for_word, .sanitise_prose dispatcher) preserving the LOCKED ampersand-first ordering.
- 6 exported format.* methods returning a character vector; all 6 print.* reduced to `cat(format(x), sep="\n"); invisible(x)` (API-01, API-02).
- NAMESPACE registers all 6 S3method(format,*) (purely additive; no exports removed); full testthat suite green (2324 pass, 0 fail).

## Task Commits

1. **Task 1: Snapshot baseline** - `4f74f94` (test)
2. **Task 2: Add format.* + thin print.*** - `531a23f` (feat)
3. **Task 3: devtools::document()** - `a05d441` (docs)

_Note: the print/format snapshots are byte-identical across Task 2 and Task 3 (git diff --quiet passed at each gate)._

## Files Created/Modified
- `tests/testthat/test-print-snapshots.R` - 8 snapshot fixtures for the 6 print methods (ASCII-only, hand-assembled, deterministic)
- `tests/testthat/test-prose-sanitise-snapshots.R` - snapshot lock for the 4 sanitiser variants
- `tests/testthat/_snaps/print-snapshots.md`, `_snaps/prose-sanitise-snapshots.md` - committed byte baselines
- `R/task.R`, `R/es_diagnostics.R`, `R/simulation.R`, `R/cross_sectional.R`, `R/advise.R`, `R/advise_offline.R` - added format.<Class>, reduced print.<Class> to delegate
- `NAMESPACE` + `man/format.*.Rd` - regenerated by devtools::document()

## Decisions Made
- Used `utils::capture.output({ <original cat body> })` for each format.* rather than manually rewriting cat() calls into a vector. Verified via a round-trip test that `cat(capture.output(cat(...)), sep="\n")` reproduces the original cat output byte-for-byte (including cat's multi-arg space separation and the trailing-newline discipline the plan flagged as Pitfall 1). This makes the refactor byte-exact by construction and required zero snapshot updates.
- Advisor Pro footer emission moved into format.Advice/format.es_advice (captured; silent no-op by default) and removed from print.* to avoid double-emit (RESEARCH Pitfall 2).

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 1 - Bug] Corrected snapshot artifact filenames in verify commands**
- **Found during:** Task 1
- **Issue:** The plan's frontmatter artifacts and Task 1/2 verify commands referenced `tests/testthat/_snaps/test-print-snapshots.md`, but testthat drops the `test-` prefix and writes `_snaps/print-snapshots.md` / `_snaps/prose-sanitise-snapshots.md`. The literal `test -f` in the verify string would never pass.
- **Fix:** Used the actual testthat-generated filenames (`print-snapshots.md`, `prose-sanitise-snapshots.md`) for the snapshot assets and byte-identity gate. The functional guarantee (byte-locked snapshots, git diff --quiet after refactor) is fully satisfied.
- **Files modified:** none (naming only; the committed snapshot files follow testthat convention)
- **Committed in:** `4f74f94`

**2. [Rule 3 - Blocking] Snapshot generation required NOT_CRAN=true**
- **Found during:** Task 1
- **Issue:** `expect_snapshot` skips on CRAN by default; running `testthat::test_file` outside `R CMD check` was treated as CRAN, so the first run SKIPped all snapshots and wrote no `_snaps/`.
- **Fix:** Set `NOT_CRAN=true` in the R invocation so snapshots actually run and write. This is an environment flag, not a code/test change.
- **Committed in:** `4f74f94`

**3. [Rule 1 - Bug] Repaired orphaned roxygen blocks from the print/format split**
- **Found during:** Task 2/3
- **Issue:** For advise.R, es_diagnostics.R, advise_offline.R the format.* method was inserted where the original print.* roxygen block sat, leaving the old print roxygen stacked above format.* and print.* with only `@export`. document() then deleted print.Advice.Rd (undocumented).
- **Fix:** Removed the stale/duplicated roxygen above each format.*, gave each print.* its own minimal roxygen block. Re-ran document(); print.Advice.Rd and print.es_advice.Rd regenerate cleanly, no man/ deletions.
- **Files modified:** R/advise.R, R/advise_offline.R, R/es_diagnostics.R
- **Committed in:** `531a23f` / `a05d441`

**4. [Rule 3 - Blocking, out-of-scope cleanup] Removed transient rmarkdown render logs**
- **Found during:** Task 3 (after running the full suite)
- **Issue:** The report-template test emitted stray `inst/.../skeleton/file*.log` render artifacts into the working tree.
- **Fix:** Deleted the transient logs so the doc commit tree was clean. These are not tracked and not authored by this task.
- **Committed in:** n/a (removed, not committed)

---

**Total deviations:** 4 (2 blocking-environment/naming, 2 bug — all necessary for correctness). No scope creep; no behaviour change on valid inputs.
**Impact on plan:** Snapshot filenames follow testthat convention; the byte-identical guarantee, format.* exports, and NAMESPACE registration are all satisfied exactly.

## Issues Encountered
None beyond the deviations above. Full suite: 2324 pass, 0 fail, 4 pre-existing warnings, 29 skips (optional packages / graceful-degradation paths — unchanged).

## Known Stubs
None.

## User Setup Required
None - no external service configuration required.

## Next Phase Readiness
- The committed snapshot net is the byte-identical guard for 23-02 and 23-03: any later message/API change that alters console output will fail these snapshots deliberately.
- No new Imports, no cli/lifecycle; DESCRIPTION unchanged across the entire plan. R CMD check baseline unchanged (median/tail globals NOTE only, already declared).

## Self-Check: PASSED

All created files exist on disk (2 test files, 2 _snaps, 6 format.*.Rd, SUMMARY) and all 3 task commits (`4f74f94`, `531a23f`, `a05d441`) are present in git history.

---
*Phase: 23-api-message-polish*
*Completed: 2026-09-09*
