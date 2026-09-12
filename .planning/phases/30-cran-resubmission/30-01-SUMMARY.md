---
phase: 30-cran-resubmission
plan: 01
subsystem: infra
tags: [cran, release-engineering, rcmdcheck, win-builder, roxygen, dontrun, donttest, cran-comments]

# Dependency graph
requires:
  - phase: 29-install-tested-ci
    provides: install-tested multi-OS CI (rcmdcheck against installed package + forced-Suggests leg) used as the cover-letter's multi-platform evidence
  - phase: 28-api-stabilization
    provides: existing NEWS.md 0.66.0 stanza (extended, not replaced)
provides:
  - EventStudy_0.66.0.tar.gz that passes _R_CHECK_FORCE_SUGGESTS_=false R CMD check --as-cran clean (0 errors, 0 warnings, 1 explainable NOTE)
  - CRAN example-policy compliance (6 gratuitous \dontrun -> self-contained \donttest; 3 provider examples remain \dontrun)
  - dispatched win-builder R-devel + R-release checks (emailed async to sm@data-zoo.de)
  - fully rewritten cran-comments.md with archival acknowledgment, rewrite framing, local results, and pending win-builder blocks
affects: [30-02, cran-submission, release]

# Actuals
actuals:
  tokens: 7512
  tasks: 4
  commits: 4

# Tech tracking
tech-stack:
  added: []
  patterns:
    - "Self-contained \\donttest{} examples via bundled dieselgate dataset + tempdir()-only output (CRAN-compliant, runs clean under --run-donttest)"
    - "es_advise example split: offline KB path \\donttest{}, LLM provider path \\dontrun{}"

key-files:
  created:
    - .planning/phases/30-cran-resubmission/30-01-SUMMARY.md
  modified:
    - DESCRIPTION
    - NEWS.md
    - cran-comments.md
    - R/report.R
    - R/execute.R
    - R/advise.R
    - R/advise_offline.R
    - R/task_intraday.R
    - man/es_report.Rd
    - man/generate_report.Rd
    - man/run_event_study.Rd
    - man/es_advise.Rd
    - man/recommend_stat.Rd
    - man/flag_robustness.Rd
    - man/nonparametric_intraday_test.Rd
    - .planning/config.json

key-decisions:
  - "Set git.allow_default_branch_commits:true in .planning/config.json — orchestrator directed running on the main working tree (branching_strategy:none), consistent with the entire prior GSD phase history committing directly to main."
  - "Converted \\dontrun examples were made SELF-CONTAINED (bundled dieselgate task; synthetic intraday grid) rather than only re-wrapped — required because --as-cran runs \\donttest and undefined my_task/est_data/event_data would ERROR (Rule 1/3 fix)."
  - "es_advise offline/LLM split WAS cleanly achievable: offline KB block -> \\donttest{}, LLM provider block -> \\dontrun{}."
  - "cran-comments.md source-size claim uses the SOURCE tarball (~1.9 MB, well under 5 MB); the 7.3 MB reported by --as-cran is INSTALLED size dominated by pre-built vignette HTML in inst/doc/ — clarified in the letter."

patterns-established:
  - "CRAN example compliance: slow/data-dependent-but-safe examples use \\donttest{} with bundled data + tempdir() writes; only genuine network+credentials examples use \\dontrun{}."

requirements-completed: [CRAN-01, CRAN-02, CRAN-03, CRAN-04]

coverage:
  - id: D1
    description: "EventStudy_0.66.0.tar.gz passes --as-cran with 0 errors / 0 warnings / 1 explainable NOTE (archived-package incoming feasibility)"
    requirement: CRAN-01
    verification:
      - kind: other
        ref: "_R_CHECK_FORCE_SUGGESTS_=false R CMD check --as-cran --no-manual EventStudy_0.66.0.tar.gz -> Status: 1 NOTE"
        status: pass
    human_judgment: false
  - id: D2
    description: "Six gratuitous \\dontrun converted to self-contained \\donttest; exactly 3 provider \\dontrun + 1 es_advise LLM \\dontrun remain; examples run clean under --run-donttest"
    requirement: CRAN-04
    verification:
      - kind: other
        ref: "grep -c dontrun R/provider.R == 3; total R/*.R dontrun == 4; check 'checking examples with --run-donttest ... OK'"
        status: pass
    human_judgment: false
  - id: D3
    description: "Built tarball carries zero transient skeleton *.log; full suite green (FAIL 0 | ERR 0 | SKIP 37 | PASS 2561)"
    requirement: CRAN-01
    verification:
      - kind: other
        ref: "tar tzf EventStudy_0.66.0.tar.gz | grep -c '.log$' == 0; devtools::test() -> FAIL 0 ERR 0"
        status: pass
    human_judgment: false
  - id: D4
    description: "win-builder R-devel + R-release dispatched to sm@data-zoo.de"
    requirement: CRAN-02
    verification:
      - kind: other
        ref: "devtools::check_win_devel() + check_win_release() -> 'Check <sm@data-zoo.de> for the results in 15-30 mins'"
        status: pass
    human_judgment: false
  - id: D5
    description: "cran-comments.md rewritten: archival ack (2024-04-20) + exact reason + ground-up-rewrite framing + local results + pending win-builder blocks; no stale v0.5x content"
    requirement: CRAN-03
    verification:
      - kind: other
        ref: "ARCHIVE_ACK=3, REASON=2, REWRITE=1, WINDEV=2, WINREL=1, STALE=0"
        status: pass
    human_judgment: false

# Metrics
duration: 22min
completed: 2026-09-12
status: complete
plan_head_before: 736526ea9ae5ea654c30166de089c277559037d5
---

# Phase 30 Plan 01: CRAN Resubmission (Autonomous Half) Summary

**EventStudy v0.66.0 brought to submission-ready: clean --as-cran on the built tarball (0/0/1), 6 gratuitous \dontrun converted to self-contained \donttest, win-builder dispatched, and cran-comments.md rewritten with an honest 2024-04-20 archival acknowledgment.**

## Performance

- **Duration:** ~22 min
- **Started:** 2026-09-12
- **Completed:** 2026-09-12
- **Tasks:** 4
- **Files modified:** 16 (2 docs/version, 5 R sources, 7 man/*.Rd, cran-comments.md, config.json)

## Accomplishments
- DESCRIPTION bumped 0.65.0 -> 0.66.0 (Date 2026-09-12); the version -> document -> build -> --as-cran toolchain proven end-to-end on the built tarball.
- `EventStudy_0.66.0.tar.gz` passes `_R_CHECK_FORCE_SUGGESTS_=false R CMD check --as-cran --no-manual`: **0 errors, 0 warnings, 1 NOTE** (the expected archived-package incoming-feasibility NOTE). Suggests-absent packages degrade to an INFO line, not a NOTE.
- Six gratuitous `\dontrun{}` example blocks converted to `\donttest{}` and made self-contained (bundled `dieselgate` task; inline synthetic intraday grid); the three genuine provider network+credentials examples remain `\dontrun{}`. All examples run clean under `--run-donttest`.
- Built tarball carries **zero** transient `skeleton/*.log` files; full suite green: **FAIL 0 | WARN 6 | SKIP 37 | PASS 2561** (skips are optional-Suggests-absent guards).
- win-builder R-devel + R-release **dispatched** to sm@data-zoo.de (results emailed async; Plan 02 pastes them).
- `cran-comments.md` fully rewritten (stale v0.5x content removed): Resubmission section (archival 2024-04-20, exact reason, ground-up-rewrite framing), filled local check-results block, pending win-builder placeholders, test-suite counts, multi-platform (Phase 29 CI) and examples-policy notes.

## Task Commits

Each task was committed atomically:

1. **Task 1 (tracer): bump to 0.66.0 + prove one clean --as-cran gate** - `57c6ea8` (chore)
2. **Task 2: convert 6 gratuitous \dontrun to \donttest** - `8c057bf` (fix)
3. **Task 3: tarball cleanliness + suite green + NEWS 0.66.0** - `b9a8615` (docs)
4. **Task 4: dispatch win-builder + rewrite cran-comments.md** - `27416f6` (docs)

_Plan metadata commit follows this SUMMARY._

## Exact --as-cran NOTE text

The single NOTE (verbatim):

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Simon Mueller <sm@data-zoo.de>'

New submission

Package was archived on CRAN

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2024-04-20 as issues were not corrected
    despite reminders.
```

`Status: 1 NOTE` (0 errors, 0 warnings). Optional Suggests (`rugarch`, `rmgarch`, `did`, `DIDmultiplegt`, `didimputation`, `DT`) appear as an INFO line, not a NOTE, because of `_R_CHECK_FORCE_SUGGESTS_=false`.

## Test pass/skip counts

`[ FAIL 0 | WARN 6 | SKIP 37 | PASS 2561 ]` on Linux R 4.6.1. Skips are optional-Suggests-not-installed guards (each behind `requireNamespace()`); no skip masks a failure.

## es_advise offline/LLM split

**Achieved cleanly.** The offline KB path (`EventStudyTask` from `dieselgate` -> `es_diagnostics` -> `es_advise(diag, task_type = "recommend_stat")`) is wrapped in `\donttest{}` with no network/credentials. The LLM-grounded path (`provider("openai")` + `es_advise(..., provider = p)`) is a separate `\dontrun{}` block. No network call leaks into the `\donttest{}` block.

## win-builder dispatch status

**Completed (online).** Both `devtools::check_win_devel()` and `devtools::check_win_release()` built and uploaded `EventStudy_0.66.0.tar.gz` to win-builder; each returned "Check <sm@data-zoo.de> for the results in 15-30 mins". Results are emailed asynchronously and are to be pasted into the two `[PENDING win-builder email -- filled in Plan 02]` blocks in `cran-comments.md` by the operator/Plan 02.

## Decisions Made
- **Committing on `main`:** the orchestrator explicitly directed running on the main working tree (`workflow.use_worktrees=false`, `git.branching_strategy=none`) and the entire prior GSD phase history commits directly to `main`. Set `git.allow_default_branch_commits:true` in `.planning/config.json` to authorize the directed commits through the pre-commit protected-branch assertion. (See Deviations.)
- **Source vs installed size in the cover letter:** used the ~1.9 MB *source* tarball figure; clarified that the 7.3 MB installed size is dominated by pre-built vignette HTML in `inst/doc/`.

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 1/3 - Blocking] Made converted \donttest examples self-contained**
- **Found during:** Task 2 (dontrun -> donttest conversion)
- **Issue:** Simply re-wrapping `\dontrun{}` -> `\donttest{}` caused `R CMD check --as-cran` to ERROR under `--run-donttest`, because the example bodies referenced undefined objects (`my_task`, `est_data`, `event_data`) that `\dontrun{}` had previously prevented from executing. `Status: 1 ERROR, 1 NOTE` after the naive conversion.
- **Fix:** Rewrote all six example bodies to be self-contained — building `task` from the bundled `dieselgate` dataset (matching the canonical runnable example in `R/data-dieselgate.R`), and constructing an inline synthetic intraday grid for `nonparametric_intraday_test`. Report examples write to `file.path(tempdir(), ...)` only (CRAN tempdir policy). Verified render runs in ~3s to tempdir.
- **Files modified:** R/report.R, R/execute.R, R/advise.R, R/advise_offline.R, R/task_intraday.R (+ regenerated man/*.Rd)
- **Verification:** `checking examples with --run-donttest ... OK`; `Status: 1 NOTE` (archived-package only); PROVIDER_DONTRUN=3, total R/*.R dontrun=4.
- **Committed in:** `8c057bf` (Task 2 commit)

**2. [Rule 3 - Config] Authorized main-branch commits**
- **Found during:** Task 1 (first commit)
- **Issue:** `main` is flagged protected by `git.base-branch --is-protected`, but the orchestrator explicitly directed committing atomically on the main working tree with `branching_strategy=none`.
- **Fix:** Added `"allow_default_branch_commits": true` to the `git` block of `.planning/config.json` — the documented override the pre-commit assertion honors.
- **Files modified:** .planning/config.json
- **Verification:** pre-commit assertion passes; four task commits landed on `main`.
- **Committed in:** included with the final plan-metadata commit.

---

**Total deviations:** 2 auto-fixed (1 blocking correctness fix, 1 directed config authorization)
**Impact on plan:** The self-contained-example fix was necessary for correctness — the plan's own guard rails require no gratuitous `\dontrun` AND a clean check, which is only satisfiable when the `\donttest{}` bodies are actually runnable. No scope creep; the STALE `median`/`tail` "undefined globals" pitfall was correctly NOT touched (already qualified, confirmed by the clean check).

## Issues Encountered
- Running `devtools::test()` regenerates transient `skeleton/*.log` files in the source tree; these are `.Rbuildignore`d (verified: 0 in the tarball) and were cleaned from the working tree. Cosmetic only.
- Pre-existing roxygen `document()` warnings (e.g. `print(...) is not documented`, `PermutationTest.Rd` skipped) are unchanged from prior phases and do not affect the `--as-cran` result (Rd files check OK). Out of scope; not fixed.

## User Setup Required
None - no external service configuration required. (win-builder result paste + `submit_cran()` are Plan 02 operator steps.)

## Next Phase Readiness
- Package is submission-ready pending: (1) the win-builder R-devel/R-release emails arriving at sm@data-zoo.de, pasted into the two PENDING blocks in `cran-comments.md`; (2) the operator running `devtools::submit_cran()` and confirming the CRAN acknowledgment email (CRAN-05).
- All preconditions for the Plan 02 handoff runbook are met: clean local --as-cran, rewritten cover letter, DESCRIPTION 0.66.0, NEWS 0.66.0 stanza complete, suite green.

## Self-Check: PASSED

All claimed files exist (30-01-SUMMARY.md, EventStudy_0.66.0.tar.gz, cran-comments.md, NEWS.md, DESCRIPTION) and all four task commits are present in git history (57c6ea8, 8c057bf, b9a8615, 27416f6).

---
*Phase: 30-cran-resubmission*
*Completed: 2026-09-12*
