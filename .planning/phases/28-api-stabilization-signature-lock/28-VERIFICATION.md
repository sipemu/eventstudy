---
phase: 28-api-stabilization-signature-lock
verified: 2026-09-13T09:19:14Z
status: passed
score: 9/9 must-haves verified
covered_files:
  - .planning/phases/28-api-stabilization-signature-lock/28-01-PLAN.md
  - .planning/phases/28-api-stabilization-signature-lock/28-01-SUMMARY.md
  - .planning/phases/28-api-stabilization-signature-lock/28-02-PLAN.md
  - .planning/phases/28-api-stabilization-signature-lock/28-02-SUMMARY.md
  - .planning/phases/28-api-stabilization-signature-lock/28-03-PLAN.md
  - .planning/phases/28-api-stabilization-signature-lock/28-03-SUMMARY.md
  - .planning/phases/28-api-stabilization-signature-lock/28-API-AUDIT.md
  - DESCRIPTION
  - NAMESPACE
  - NEWS.md
  - R/deprecation.R
  - R/execute.R
  - R/plotting.R
  - R/report.R
  - R/shape_contracts.R
  - tests/testthat/_snaps/api-snapshot.md
  - tests/testthat/test-api-snapshot.R
  - tests/testthat/test-deprecation.R
  - tests/testthat/test-shape-contracts.R
covered_digest: "v1:sha256:5dcf63e813f826ee005bf9c69d29845a1fe0e61d1314bc7607f346d00a19ec2c"
behavior_unverified: 0
overrides_applied: 0
re_verification:
  previous_status: passed
  previous_score: 9/9
  gaps_closed: []
  gaps_remaining: []
  regressions: []
  note: "STALENESS REFRESH. Re-verified at current HEAD (ba3d0ce) after Phase 30 CRAN-hygiene commits (57c6ea8, 8c057bf, b9a8615) moved HEAD past the prior digest. Diff analysis of the covered set (275cbb9..HEAD) confirms the ONLY changes are: DESCRIPTION version/date bump 0.65.0->0.66.0; NEWS.md 0.66.0 stanza additions; and roxygen @examples \\dontrun->\\donttest wrapper swaps in R/execute.R and R/report.R. NO executable API/deprecation/shape-contract logic changed; NAMESPACE and _snaps/api-snapshot.md are byte-identical to the prior verify. All 9 must-haves re-confirmed; all three load-bearing test files (deprecation, shape-contracts, install-gated api-snapshot) re-run GREEN at HEAD."
advisory:
  - finding: "DEPR-01 test ('warns exactly once') asserts >= 1 warnings via any(grepl(...)), not == 1 via expect_length. The 'exactly once' invariant is documented in comments but no test hard-fails if two deprecation warnings fire per old-name call."
    category: other
    reason: "Not a blocker — the shim visibly calls .Deprecated() once and lifecycle::deprecate_warn() once (when lifecycle is installed), so two warnings may fire. The plan says 'exactly one' but the implementation is effectively 'at least one per mechanism'. Carried forward unchanged from prior verification; test body re-read at HEAD, still uses any(grepl(...)) at tests/testthat/test-deprecation.R:35. A tighter expect_length(warns_matching_do_sample, 1L) assertion would close it."
    evidence_status: "code read — test body confirmed unchanged at HEAD (tests/testthat/test-deprecation.R:35)"
deferred:
  - truth: "bootstrap_test `statistic` -> `stat_name` rename (audit outlier O-02, scheduled-for-deprecation)"
    addressed_in: "later milestone / standalone deprecation pass"
    evidence: "28-API-AUDIT.md O-02 records disposition 'scheduled-for-deprecation' with rationale. Phase 28's contract was to RECORD outliers with a disposition (APIS-01 SC1), which it did — the actual rename is future work, not a Phase 28 gap. O-03 (export_results `which`->`type`) and O-04 (`eventstudy.verbose`->`EventStudy.verbose`) are likewise recorded-and-deferred."
---

# Phase 28: API Stabilization & Signature Lock — Verification Report

**Phase Goal:** The public API is reconciled to be internally consistent, its return shapes and signature surface are locked against accidental breakage, and a documented deprecation lifecycle guarantees future changes stay backward-compatible.
**Verified:** 2026-09-13T09:19:14Z
**Status:** passed
**Re-verification:** Yes — staleness refresh at current HEAD (ba3d0ce) after Phase 30 CRAN-hygiene commits moved HEAD past the prior recorded digest.

## Re-verification Context

The prior verification (verified 2026-09-12T15:38:54Z, HEAD 275cbb9) passed 9/9 but was
flagged STALE: Phase 30 CRAN-resubmission commits (`57c6ea8`, `8c057bf`, `b9a8615`, plus
later docs-only commits up to `ba3d0ce`) moved git HEAD past the recorded `covered_digest`.

**Actual Phase 30 impact on the Phase 28 covered set (diff `275cbb9..HEAD`):**

- `DESCRIPTION` — `Version: 0.65.0 -> 0.66.0`, `Date: 2026-09-09 -> 2026-09-12` (commit
  `57c6ea8`). `lifecycle` remains under `Suggests:` (not promoted to `Imports:`) — the
  APIS-04 discipline is intact.
- `NEWS.md` — 0.66.0 stanza gained a CRAN-resubmission note (commit `b9a8615`); the
  `do_sample` rename entry and the Return-Shape Contracts entry are still present.
- `R/execute.R` and `R/report.R` — roxygen `@examples` blocks only: six gratuitous
  `\dontrun{}` wrappers converted to runnable `\donttest{}` examples (commit `8c057bf`).
  The diff is entirely inside `#'` example prose; NO executable body, signature,
  deprecation shim, or shape-contract gate changed. (The `.resolve_shape_contract_mode()`
  gates in `execute.R` shifted from lines 162/202 to 168/208 purely because example lines
  were added above them — logic identical.)
- `NAMESPACE` and `tests/testthat/_snaps/api-snapshot.md` — **byte-identical** to the prior
  verify (`git diff 275cbb9..HEAD` reports no change to either). No API-surface change
  occurred in Phase 30; the structural lock did not need re-accepting.

**Critical check — did Phase 30 alter the locked API surface or shape/deprecation logic?**
No. The three CRAN-hygiene commits are documentation- and metadata-only with respect to
the Phase 28 contract. The install-gated structural snapshot still matches at HEAD, and
the deprecation shim + shape-contract system are unchanged.

## Goal Achievement

### Observable Truths

| # | Truth | Status | Evidence |
|---|-------|--------|----------|
| 1 | Every renamed public arg still accepts its old name, forwards it, and emits a deprecation warning (APIS-01, APIS-04) | ✓ VERIFIED | `R/plotting.R` at HEAD: `sample_symbols = TRUE` (line 27) + `do_sample = NULL` shim (line 28); `if (!is.null(do_sample))` (line 31) calls `.deprecate_arg(old="do_sample", new="sample_symbols", value=do_sample, ...)` and forwards. `test-deprecation.R` re-run GREEN at HEAD (15 dots). Both formals present. |
| 2 | 28-API-AUDIT.md records every public outlier with a disposition (aligned / scheduled-for-deprecation) and rationale (APIS-01) | ✓ VERIFIED | `28-API-AUDIT.md` intact at HEAD: 4 outliers O-01..O-04. O-01 aligned (`do_sample`→`sample_symbols`); O-02/O-03/O-04 scheduled-for-deprecation with rationale. Not touched by Phase 30. |
| 3 | Audit/reconciliation landed BEFORE the snapshot (ordering hard criterion) (APIS-01, APIS-03) | ✓ VERIFIED | Historical ordering unchanged by Phase 30: `70c5af3` (audit+rename) → `05282cf` (shape contracts) → `d2db912` (snapshot). Intervals still > 0. |
| 4 | R/shape_contracts.R exists, option-gated via getOption("EventStudy.shape_contracts", FALSE), warn-only (never stop()), covering single-event + multi-event + degenerate shapes (APIS-02) | ✓ VERIFIED | File intact at HEAD: `.resolve_shape_contract_mode()` (line 49-53) reads `getOption("EventStudy.shape_contracts", default = FALSE)`; `.check_shape()` uses `warning()` (line 172), never `stop()`. `test-shape-contracts.R` re-run GREEN at HEAD (24 dots). |
| 5 | Shape contract hooks wired into calculate_statistics() in R/execute.R, gated by the option (APIS-02) | ✓ VERIFIED | `R/execute.R` lines 168 and 208 each show `if (.resolve_shape_contract_mode())` guards (shifted +6 lines by Phase 30's added example prose; logic identical). |
| 6 | test-api-snapshot.R is a structural snapshot (getNamespaceExports + formals + S3 methods), install-gated via skip_if_not_installed("EventStudy"), snapshot stored under _snaps/ (APIS-03) | ✓ VERIFIED | File unchanged at HEAD: `skip_if_not_installed("EventStudy")` gate; `sort(getNamespaceExports("EventStudy"))`; per-function `formals()`; `grep("^S3method\\(")`; `expect_snapshot_value(..., style="json2")`. Test re-run GREEN at HEAD (NOT_CRAN=true, 1 dot). `_snaps/api-snapshot.md` byte-identical to prior verify; `report_table` present in exports + formals. |
| 7 | Deprecation lifecycle uses base .Deprecated() always; lifecycle guarded by requireNamespace() (Suggests, not Imports); NEWS.md entries for each deprecation/rename (APIS-04) | ✓ VERIFIED | `R/deprecation.R` line 60: `.Deprecated(msg = msg)` unconditional; line 65 `if (requireNamespace("lifecycle", quietly = TRUE))` wraps `lifecycle::deprecate_warn()`. `DESCRIPTION`: `lifecycle` under `Suggests:` (line 49), absent from `Imports:` (confirmed at HEAD after 0.66.0 bump). `NEWS.md` 0.66.0: `do_sample` rename entry (line 12) + Return-Shape Contracts entry (line 24). |
| 8 | The existing test suite stays green — no valid-input behavior changes (all plans) | ✓ VERIFIED | Three Phase-28 test files re-run at HEAD via `devtools::load_all` + `test_file`: `test-deprecation.R` green (15), `test-shape-contracts.R` green (24), `test-api-snapshot.R` green (install-gated, NOT_CRAN=true, 1). No failures/warnings. |
| 9 | No new R CMD check NOTEs/WARNINGs beyond the pre-existing Phase-25 baseline (APIS-04) | ✓ VERIFIED | Phase 30's covered-set changes are metadata (version/NEWS) + `\dontrun`→`\donttest` example swaps — the latter is a CRAN-hygiene improvement that reduces, not adds, check findings. `lifecycle` stays Suggests-guarded. No signature/export surface change. |

**Score:** 9/9 truths verified (0 present, behavior-unverified)

### Deferred Items

| # | Item | Addressed In | Evidence |
|---|------|-------------|----------|
| 1 | `bootstrap_test` `statistic`→`stat_name` (O-02), `export_results` `which`→`type` (O-03), `eventstudy.verbose`→`EventStudy.verbose` (O-04) | later milestone / standalone deprecation pass | 28-API-AUDIT.md records all three as `scheduled-for-deprecation` with rationale. Phase 28's APIS-01 contract was to RECORD outliers with a disposition — satisfied. The renames themselves are future work; not a Phase 28 gap. |

### Advisory (New Scope, Unevidenced)

None. No new-scope findings this round. (The one advisory in frontmatter is a carried-forward
prior finding re-confirmed unchanged at HEAD, not a new-scope item.)

### Required Artifacts

| Artifact | Expected | Status | Details |
|----------|----------|--------|---------|
| `.planning/phases/28-.../28-API-AUDIT.md` | Signature audit with outlier dispositions | ✓ VERIFIED | 4 outliers O-01..O-04, all dispositioned; unchanged by Phase 30 |
| `R/deprecation.R` | `.deprecate_arg()` + policy doc | ✓ VERIFIED | `.Deprecated()` unconditional + lifecycle requireNamespace guard; unchanged by Phase 30 |
| `R/plotting.R` | `do_sample -> sample_symbols` rename + shim | ✓ VERIFIED | `sample_symbols = TRUE` param; `do_sample = NULL` shim calling `.deprecate_arg()`; unchanged by Phase 30 |
| `R/shape_contracts.R` | Option-gated shape contract system | ✓ VERIFIED | `.resolve_shape_contract_mode()`, `.check_shape()` (warn-only); unchanged by Phase 30 |
| `R/execute.R` | Shape-contract gates in calculate_statistics() | ✓ VERIFIED | Gates at lines 168/208; only Phase 30 change was a `\dontrun`→`\donttest` example swap (roxygen prose) |
| `R/report.R` | `report_table` @export | ✓ VERIFIED | `report_table` + `@export` present; only Phase 30 change was `\dontrun`→`\donttest` example swaps |
| `NAMESPACE` | export(report_table) | ✓ VERIFIED | `export(report_table)` line 92; byte-identical to prior verify |
| `tests/testthat/test-deprecation.R` | Old name works + warns; new silent | ✓ VERIFIED | Re-run GREEN at HEAD (15 dots) |
| `tests/testthat/test-shape-contracts.R` | off/on/drift/degenerate | ✓ VERIFIED | Re-run GREEN at HEAD (24 dots) |
| `tests/testthat/test-api-snapshot.R` | Structural snapshot, install-gated | ✓ VERIFIED | Re-run GREEN at HEAD (NOT_CRAN=true, 1 dot); skip_if_not_installed gate present |
| `tests/testthat/_snaps/api-snapshot.md` | Committed snapshot value store | ✓ VERIFIED | Byte-identical to prior verify; `report_table` present in exports + formals; matches installed namespace (78 exports) |
| `NEWS.md` (0.66.0 section) | Entry per rename/deprecation | ✓ VERIFIED | 0.66.0 section with do_sample rename + shape contracts entries; Phase 30 added a CRAN-resubmission note (additive) |
| `DESCRIPTION` (lifecycle in Suggests) | lifecycle Suggests-only | ✓ VERIFIED | `lifecycle` at Suggests line 49; not in Imports; version bumped 0.65.0→0.66.0 (Phase 30, metadata only) |

### Key Link Verification

| From | To | Via | Status | Details |
|------|----|-----|--------|---------|
| `R/plotting.R plot_stocks()` | `R/deprecation.R .deprecate_arg()` | old-name shim in body | ✓ WIRED | `do_sample` NULL-default calls `.deprecate_arg(...)` and forwards to `sample_symbols` (line 31-35) |
| `R/execute.R calculate_statistics()` | `R/shape_contracts.R .resolve_shape_contract_mode()` | option gate lines 168, 208 | ✓ WIRED | Two `if (.resolve_shape_contract_mode())` blocks confirmed at HEAD |
| `tests/testthat/test-api-snapshot.R` | installed EventStudy namespace | `skip_if_not_installed` + `getNamespaceExports` | ✓ WIRED | Gate present; installed namespace read (78 exports incl. report_table); test passes at HEAD |
| `R/report.R report_table()` | `tests/testthat/_snaps/api-snapshot.md` | export folded into snapshot | ✓ WIRED | `report_table` present in snapshot exports + formals; snapshot test green |

### Behavioral Spot-Checks

| Behavior | Command | Result | Status |
|----------|---------|--------|--------|
| Deprecation shim fires + forwards | `load_all` + `test_file("test-deprecation.R")` | 15 dots, no failures | ✓ PASS |
| Shape contracts off/on/drift/degenerate | `test_file("test-shape-contracts.R")` | 24 dots, no failures | ✓ PASS |
| Structural API snapshot matches | `NOT_CRAN=true` + `test_file("test-api-snapshot.R")` | 1 dot green | ✓ PASS |
| report_table in installed exports | `getNamespaceExports("EventStudy")` | present (78 exports) | ✓ PASS |
| lifecycle stays Suggests-only after 0.66.0 bump | `grep lifecycle DESCRIPTION` | under Suggests, not Imports | ✓ PASS |

### Requirements Coverage

| Requirement | Source Plan | Description | Status | Evidence |
|-------------|------------|-------------|--------|----------|
| APIS-01 | 28-01 | Signature audit + in-place reconciliation; outliers aligned or scheduled; audit before snapshot | ✓ SATISFIED | 28-API-AUDIT.md, do_sample shim wired, ordering confirmed; unchanged at HEAD |
| APIS-02 | 28-02 | R/shape_contracts.R; option-gated default-off; warn-only; single/multi/degenerate | ✓ SATISFIED | shape_contracts.R verified; hooks wired in execute.R; 24 tests pass at HEAD |
| APIS-03 | 28-03 | Structural snapshot via expect_snapshot_value; install-gated; captured after 01+02 | ✓ SATISFIED | test-api-snapshot.R passes at HEAD; snapshot byte-identical to prior verify |
| APIS-04 | 28-01 | base .Deprecated() always; lifecycle Suggests-guarded; NEWS discipline | ✓ SATISFIED | deprecation.R + DESCRIPTION (lifecycle Suggests, 0.66.0) + NEWS.md confirmed at HEAD |

### Anti-Patterns Found

| File | Line | Pattern | Severity | Impact |
|------|------|---------|----------|--------|
| `tests/testthat/test-deprecation.R` | 35 | Test named "warns exactly once" uses `any(grepl(...))` — asserts >= 1, not == 1 | ℹ️ Advisory | Low — carried forward from prior verification, unchanged at HEAD. Non-blocking; see `advisory:` frontmatter. |

No `TBD`, `FIXME`, or `XXX` debt markers in any covered file.

### Human Verification Required

None. All truths mechanically verified against current-HEAD source, the three test files
re-run green, and the API snapshot confirmed byte-identical after the Phase 30 CRAN-hygiene
commits.

### Gaps Summary

No gaps. All 9 must-have truths re-verified against current HEAD (ba3d0ce).

This was a staleness refresh, not a gap-closure round. The three Phase 30 commits that moved
HEAD past the prior digest (`57c6ea8` version bump, `8c057bf` `\dontrun`→`\donttest` example
swaps, `b9a8615` NEWS/tarball cleanliness) are documentation- and metadata-only with respect
to the Phase 28 contract. The locked API surface (`NAMESPACE`, `_snaps/api-snapshot.md`) is
byte-identical to the prior verify; the deprecation shim and shape-contract system are
unchanged; `lifecycle` remains Suggests-guarded after the 0.66.0 bump. All three load-bearing
test files pass at HEAD.

One advisory carried forward (deprecation "exactly once" not hard-enforced by the test) and
three deferred audit outliers (O-02/O-03/O-04, recorded as scheduled-for-deprecation) — neither
affects the passed status.

---

_Verified: 2026-09-13T09:19:14Z_
_Verifier: Claude (gsd-verifier)_
