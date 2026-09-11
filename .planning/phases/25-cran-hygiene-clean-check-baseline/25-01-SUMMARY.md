---
phase: 25-cran-hygiene-clean-check-baseline
plan: 01
subsystem: infra
tags: [cran, r-cmd-check, non-ascii, roxygen, namespace, rbuildignore, hygiene]

# Dependency graph
requires:
  - phase: 24-and-earlier
    provides: the R package sources, CI non-ASCII guard, DESCRIPTION Suggests block
provides:
  - Clean R CMD check --as-cran baseline (0 ERROR, 0 WARNING; only the pre-existing new-submission NOTE)
  - Pure-ASCII R/*.R sources + refreshed man/figures-only CI non-ASCII baseline
  - Namespace-qualified median/tail (no undefined-globals NOTE)
  - Removed stale tarball + tar.gz/scratch ignore rules (no non-standard-file NOTE)
  - Audited optional-package call sites (all requireNamespace-guarded + in Suggests)
affects: [26-formula-audit, 27-property-tests, 28-api-lock, 29-install-tested-ci, 30-cran-resubmission]

# Actuals
actuals:
  tokens: 21000
  tasks: 6
  commits: 8

# Tech tracking
tech-stack:
  added: []
  patterns:
    - "Non-ASCII sweep: escape in code comments/strings, transliterate in roxygen #' prose"
    - "Call-site pkg::fn qualification for base-package globals (stats::median, utils::tail)"

key-files:
  created: []
  modified:
    - R/es_diagnostics.R
    - R/advise.R
    - R/models.R
    - R/provider.R
    - R/knowledge_base.R
    - R/multi_event_test_statistics.R
    - R/single_event_test_statistics.R
    - R/plotting.R
    - .github/non-ascii-baseline.txt
    - .Rbuildignore
    - .gitignore
    - man/*.Rd (regenerated)

key-decisions:
  - "median/tail cleared via call-site stats::/utils:: qualification (no @importFrom, NAMESPACE unchanged)"
  - "Non-ASCII in # code comments -> inert \\uXXXX escapes; in #' roxygen prose -> ASCII transliteration (\\uXXXX breaks Rd)"
  - "zoo::index accepted as transitively guarded (only reachable via requireNamespace(quantmod) branch; quantmod hard-imports zoo)"

patterns-established:
  - "Pattern: after any non-ASCII escape pass, run R CMD build and grep for 'unknown macro \\u' before trusting the fix"
  - "Pattern: scratch/ workspace is .Rbuildignore'd + .gitignore'd so tracer artifacts never trip the top-level-files NOTE"

requirements-completed: [HYG-01, HYG-02, HYG-03, HYG-04]

coverage:
  - id: D1
    description: "HYG-01: every R/*.R non-ASCII byte removed; CI non-ASCII baseline refreshed to man/figures-only and byte-matches the guard; no non-ASCII WARNING"
    requirement: "HYG-01"
    verification:
      - kind: automated
        ref: "grep -rlP '[^\\x00-\\x7F]' R/ | grep -c . == 0"
        status: pass
      - kind: automated
        ref: "diff <(grep -rnP '[^\\x00-\\x7F]' R/ man/figures/ inst/ | sort) .github/non-ascii-baseline.txt (empty)"
        status: pass
      - kind: integration
        ref: "R CMD check --as-cran: 'checking code files for non-ASCII characters ... OK'"
        status: pass
    human_judgment: false
  - id: D2
    description: "HYG-02: median/tail namespace-qualified (stats::median, utils::tail); no undefined-globals NOTE"
    requirement: "HYG-02"
    verification:
      - kind: automated
        ref: "grep -nE '(^|[^:._[:alnum:]])(median|tail)\\(' R/es_diagnostics.R | grep -vE 'stats::median|utils::tail|lower.tail' (empty)"
        status: pass
      - kind: integration
        ref: "R CMD check --as-cran: 'checking R code for possible problems ... OK' (no visible global function definition for median/tail)"
        status: pass
    human_judgment: false
  - id: D3
    description: "HYG-03: stale EventStudy_0.62.0.tar.gz removed; tar.gz + scratch ignore rules added; no non-standard-file NOTE"
    requirement: "HYG-03"
    verification:
      - kind: automated
        ref: "git ls-files | grep -c 'tar.gz' == 0; .Rbuildignore + .gitignore carry tarball/scratch rules"
        status: pass
      - kind: integration
        ref: "R CMD check --as-cran: 'checking top-level files ... OK'"
        status: pass
    human_judgment: false
  - id: D4
    description: "HYG-04: all optional-package call sites requireNamespace-guarded + in Suggests; stale plot_diagnostics @return doc corrected to gridExtra"
    requirement: "HYG-04"
    verification:
      - kind: automated
        ref: "grep 'requireNamespace(\"gridExtra\"' R/plotting.R; ! grep 'patchwork-style' R/plotting.R; gridExtra in DESCRIPTION Suggests"
        status: pass
      - kind: manual_procedural
        ref: "audit table of 16 optional packages: all guarded (zoo transitively) + declared"
        status: pass
    human_judgment: false
  - id: D5
    description: "Full ~2359-test suite green; behavior on valid inputs unchanged"
    verification:
      - kind: unit
        ref: "devtools::test_local(): FAIL 0, PASS 2359, SKIP 29 (all optional-pkg-absent), WARN 4 (pre-existing degradation-path)"
        status: pass
    human_judgment: false

# Metrics
duration: ~2h active (wall-clock spanned background vignette builds + check runs)
completed: 2026-09-11
status: complete
---

# Phase 25 Plan 01: CRAN Hygiene & Clean Check Baseline Summary

**Cleared all three known CRAN findings (non-ASCII WARNING, undefined-globals NOTE, non-standard-file NOTE) and audited optional-package guards, leaving `R CMD check --as-cran` at 0 ERROR / 0 WARNING / 1 pre-existing new-submission NOTE with the full 2359-test suite green.**

## Performance

- **Duration:** ~2h active (wall-clock longer; spanned background vignette build + multiple full-check runs)
- **Started:** 2026-09-10T20:40:56Z
- **Completed:** 2026-09-11T05:31:01Z
- **Tasks:** 6
- **Files modified:** 41 tracked (16 R sources, 20 man/*.Rd, .github/non-ascii-baseline.txt, .Rbuildignore, .gitignore; 1 file deleted)

## Accomplishments
- HYG-01: 105 non-ASCII bytes across 16 R files removed; roxygen prose transliterated to ASCII, code-comment glyphs escaped; CI baseline refreshed to the 5 man/figures SVG lines only and byte-matches the guard.
- HYG-02: 9 `median`/`tail` call sites in `R/es_diagnostics.R` qualified to `stats::median` / `utils::tail`; NAMESPACE unchanged (call-site route, no `@importFrom`).
- HYG-03: stale `EventStudy_0.62.0.tar.gz` removed; `.Rbuildignore`/`.gitignore` gained tar.gz + scratch rules; `checking top-level files ... OK`.
- HYG-04: optional-package audit — all 16 Suggests-package `pkg::` call sites are `requireNamespace()`-guarded (zoo transitively via the quantmod branch) and declared in Suggests; no new guards needed; stale `plot_diagnostics` `@return` "patchwork-style" doc corrected to "gridExtra 2x2 grid layout".
- Verified `R CMD check --as-cran` on a properly-built tarball: 0 ERROR, 0 WARNING, 1 NOTE (pre-existing CRAN incoming feasibility: new submission / archived / no prebuilt vignette index — outside this phase's scope). Full suite: 2359 passed, 0 failed.

## Task Commits

1. **Task 2: HYG-02 namespace-qualify median/tail** - `751ba99` (fix)
2. **Task 3: HYG-01 escape non-ASCII bytes in R/** - `491d6c3` (fix)
3. **Task 4: HYG-01 refresh non-ASCII baseline** - `4536941` (chore)
4. **Task 5: HYG-03 remove tarball + ignore rules** - `5d86d71` (chore)
5. **Task 6: HYG-04 audit guards + fix @return doc** - `6092a5e` (docs)
6. **Deviation (Rule 3): ignore scratch/ workspace** - `bc1246c` (chore)
7. **Deviation (Rule 1): transliterate \\uXXXX in roxygen prose** - `2dd8404` (fix)

_Task 1 was a `type="tracer"` that only proved the gate (three findings observed) and changed no source — no commit (nothing to commit); its scratch note lives in the git-ignored `scratch/`._

## Files Created/Modified
- `R/es_diagnostics.R` - stats::median / utils::tail qualification + non-ASCII escapes/transliterations
- `R/advise.R`, `R/models.R`, `R/provider.R`, `R/knowledge_base.R`, `R/multi_event_test_statistics.R`, `R/single_event_test_statistics.R`, `R/advisor_pro.R`, `R/contract.R`, `R/parameter_set.R`, `R/execute.R`, `R/synthetic_control.R`, `R/models_time_varying.R`, `R/data-dieselgate.R`, `R/data-earnings-surprises.R`, `R/EventStudy-package.R` - non-ASCII removed (escaped in code comments, transliterated in roxygen prose)
- `R/plotting.R` - `@return` doc corrected to gridExtra 2x2 grid layout
- `man/*.Rd` (20 files) - regenerated by roxygen after doc/prose edits
- `.github/non-ascii-baseline.txt` - refreshed to 5 man/figures SVG lines only
- `.Rbuildignore` - added `^EventStudy_.*\.tar\.gz$` and `^scratch$`
- `.gitignore` - added `*.tar.gz` and `scratch/`
- `EventStudy_0.62.0.tar.gz` - DELETED (stale build artifact)

## Optional-Package Audit (HYG-04 record)

| Package | File(s) | Guarded? | In Suggests? | Notes |
|---------|---------|----------|--------------|-------|
| gridExtra | plotting.R | yes (L351) | yes | already guarded; verified only |
| openxlsx | export.R | yes | yes | |
| rugarch | models.R, models_time_varying.R | yes | yes | |
| rmgarch | models_time_varying.R | yes | yes | |
| sandwich | models.R, cross_sectional.R, panel_event_study.R | yes | yes | |
| quadprog | synthetic_control.R | yes | yes | |
| did / DIDmultiplegt / didimputation | panel_event_study.R | yes | yes | |
| tidyquant / quantmod | data_download.R | yes | yes | |
| zoo | data_download.R | transitive | yes | `zoo::index` only inside `else if (requireNamespace("quantmod"))`; quantmod hard-imports zoo |
| httr2 / jsonlite | provider.R, advise.R | yes | yes | |
| tinytex / tinytable | report.R | yes | yes | |

**Result:** every optional-package call site already guarded and declared. No new guards or Suggests entries required (the expected outcome).

## Decisions Made
- HYG-02 used call-site `stats::`/`utils::` qualification rather than `@importFrom` — unambiguous, needs no globalVariables entry, and leaves NAMESPACE untouched.
- Non-ASCII handling is line-class-dependent: `\uXXXX` escapes in `#` code comments (inert, never reach Rd) but ASCII transliteration in `#'` roxygen prose (roxygen copies literal text into Rd, and Rd misreads `\u` as an unknown macro).
- `zoo` accepted as transitively guarded rather than adding a redundant `requireNamespace("zoo")` — the call is unreachable unless quantmod (a hard zoo importer) is present.

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 1 - Bug] `\uXXXX` in roxygen prose produced "unknown macro '\u'" Rd build warnings**
- **Found during:** Verification (R CMD build after Task 6)
- **Issue:** Task 3 escaped non-ASCII bytes to `\uXXXX` uniformly. In `#'` roxygen comments the escape text is copied verbatim into the generated `.Rd`; Rd does NOT interpret `\uXXXX`, so `R CMD build` emitted `Warning: ... unknown macro '\u'` — which would become a new check WARNING (violating the no-new-WARNINGs constraint). The plan's note that comment escapes are "inert" holds for `#` code comments but not for `#'` roxygen prose.
- **Fix:** Transliterated `\uXXXX` on `#'` lines only (em-dash -> `--`, right-quote -> `'`, o-diaeresis -> `oe`; 54 occurrences across 12 files); regenerated man/*.Rd. Code-comment `#` escapes left inert.
- **Files modified:** 12 R sources + regenerated man/*.Rd
- **Verification:** `R CMD build .` emits no `unknown macro '\u'`; `grep "#'.*\uXXXX" R/` empty; R/ stays pure ASCII.
- **Committed in:** `2dd8404`

**2. [Rule 3 - Blocking] Task 1 tracer's `scratch/` directory tripped the top-level-files NOTE**
- **Found during:** Verification (R CMD check --as-cran)
- **Issue:** After removing the stale tarball (HYG-03), `checking top-level files` still emitted the non-standard-file NOTE — now pointing at `scratch/`, the tracer workspace created in Task 1 (repo root is not git-ignored for `scratch/`).
- **Fix:** Added `^scratch$` to `.Rbuildignore` and `scratch/` to `.gitignore`; scratch/ stays untracked and excluded from the tarball.
- **Files modified:** `.Rbuildignore`, `.gitignore`
- **Verification:** `checking top-level files ... OK`; `tar tzf` shows scratch excluded from the build.
- **Committed in:** `bc1246c`

---

**Total deviations:** 2 auto-fixed (1 Rule 1 bug, 1 Rule 3 blocking).
**Impact on plan:** Both fixes were required to actually satisfy the phase's own verification gate (no new WARNING; top-level files clean). No scope creep — both are direct consequences of tasks in this plan.

## Issues Encountered
- `R CMD check --as-cran` requires 6 Suggests packages (rugarch, rmgarch, did, DIDmultiplegt, didimputation, DT) not installed locally; without them the dependency check ERRORs early. Ran the check with `_R_CHECK_FORCE_SUGGESTS_=false` (as the ERROR text itself instructs) to reach completion — an environment limitation, not a package defect.
- The full check times out (>10 min) rendering all 19 vignettes. Confirmed code-level findings with `--no-tests --no-examples`, ran the full suite separately via `devtools::test_local()` (green), and confirmed the vignette-file checks pass on a properly-built (with-vignettes) tarball.

## Threat Flags
None — no new network endpoints, auth paths, file-access patterns, or schema changes introduced. All edits are comment escapes, namespace qualification, doc text, and ignore rules.

## Known Stubs
None.

## User Setup Required
None - no external service configuration required.

## Next Phase Readiness
- Clean `R CMD check --as-cran` baseline is established (0 ERROR / 0 WARNING; only the expected new-submission NOTE). Every downstream v0.66.0 phase (26 formula audit, 27 property tests, 28 API lock, 29 install-tested CI, 30 resubmission) can now diff meaningfully against this baseline.
- No blockers. Note the pre-existing CRAN-incoming NOTE (archived package, no prebuilt vignette index) is expected for a resubmission and is addressed by the resubmission phase, not here.

## Self-Check: PASSED

- SUMMARY.md present on disk.
- All 7 task/deviation commits present in git history (751ba99, 491d6c3, 4536941, 5d86d71, 6092a5e, bc1246c, 2dd8404).

---
*Phase: 25-cran-hygiene-clean-check-baseline*
*Completed: 2026-09-11*
