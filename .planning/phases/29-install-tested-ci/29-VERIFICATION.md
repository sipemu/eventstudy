---
phase: 29-install-tested-ci
verified: 2026-09-13T00:00:00Z
status: passed
score: 6/6 must-haves verified
covered_files:
  - .Rbuildignore
  - .github/workflows/R-CMD-check.yaml
  - .planning/REQUIREMENTS.md
  - .planning/phases/29-install-tested-ci/29-01-PLAN.md
  - .planning/phases/29-install-tested-ci/29-01-SUMMARY.md
  - NAMESPACE
  - R/report.R
  - inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd
  - man/report_table.Rd
  - tests/testthat/_snaps/api-snapshot.md
covered_digest: "v1:sha256:9714f07a22c3c8bf78aaf66c7bb19093b977837b5dad06445b1453746219a72e"
behavior_unverified: 0
overrides_applied: 0
re_verification:
  previous_status: passed
  previous_score: 6/6
  reason: "Refresh of STALE verification — R/report.R touched by Phase 30 commit 8c057bf (roxygen \\dontrun→\\donttest on es_report/es_advise/etc.), which changed the covered_digest but not any Phase 29 CI logic or the report_table export."
  gaps_closed: []
  gaps_remaining: []
  regressions: []
---

# Phase 29: Install-Tested CI Verification Report (Re-Verification / Staleness Refresh)

**Phase Goal:** CI exercises the *installed* package and gates on `R CMD check`, so "green under load_all, broken when installed" bugs (the `.report_table()` / `skeleton.Rmd` class) fail CI instead of shipping.
**Verified:** 2026-09-13
**Status:** passed
**Re-verification:** Yes — STALE refresh at HEAD `ba3d0ce`. Previous verification (2026-09-12, passed 6/6) was flagged stale after Phase 30 commit `8c057bf` modified a covered file (`R/report.R`).

## Staleness Cause Analysis

The ONLY commit touching a Phase 29 covered file since the prior verification is:

- **`8c057bf` fix(30-01): convert 6 gratuitous `\dontrun` to `\donttest` (CRAN-04)** — edits R/report.R roxygen example wrappers on `es_report`, `es_advise`, `generate_report`, etc. Diff hunks land at L43-49 and L164-180 (the `es_report` roxygen/example block). The `report_table` definition at **L548** and its `@export` roxygen are **untouched**. No CI workflow, `.Rbuildignore`, NAMESPACE, skeleton.Rmd, or snapshot content changed.

The digest shift (`d7ae2e…` → `9714f07…`) is fully attributable to that unrelated roxygen edit. **No Phase 29 deliverable regressed.**

## Goal Achievement

### Observable Truths

| # | Truth | Status | Evidence (re-checked at HEAD ba3d0ce) |
|---|-------|--------|----------|
| 1 | Installed package exposes `report_table` as exported; skeleton.Rmd calls it via `EventStudy::report_table`, never `:::` | ✓ VERIFIED | `export(report_table)` in NAMESPACE; `man/report_table.Rd` present; `grep -c "EventStudy::report_table" skeleton.Rmd` == 8; `grep -c "EventStudy:::" skeleton.Rmd` == 0; `EventStudy:::.*report_table` in inst/ == 0 |
| 2 | `R CMD check --as-cran` on the built tarball stays at 1 NOTE, 0 WARN, 0 ERR | ✓ VERIFIED | SUMMARY Task 4 recorded 1 NOTE; Phase 30 commit `8c057bf` message independently re-confirms "--as-cran 0 err/0 warn, 1 NOTE" at a later point — consistent, no regression |
| 3 | Install-gated api-snapshot records `report_table` in the surface | ✓ VERIFIED | `grep -c report_table _snaps/api-snapshot.md` == 2 (present in `exports` and `functions` arrays); additive-only delta unchanged |
| 4 | CI workflow has a `force_suggests: 'true'` matrix leg with matrix-driven `_R_CHECK_FORCE_SUGGESTS_`/`NOT_CRAN`; `check-r-package@v2` still runs `--as-cran` on the installed tarball; 4 default legs stay Suggests-absent | ✓ VERIFIED | R-CMD-check.yaml L24 leg `{os: ubuntu-latest, r: 'release', force_suggests: 'true'}`; L29 `_R_CHECK_FORCE_SUGGESTS_` ternary; L30 `NOT_CRAN` ternary; L47-49 `check-r-package@v2` with `c("--no-manual", "--as-cran")`; L20-23 four default legs unchanged |
| 5 | No stale `.log` ships in tarball; `.Rbuildignore` glob prevents skeleton render artifacts re-entering | ✓ VERIFIED | `.Rbuildignore` L19 `^inst/rmarkdown/templates/event_study_report/skeleton/.*\.log$` present |
| 6 | Full test suite stays green; behavior on valid inputs unchanged | ✓ VERIFIED | SUMMARY: `[ FAIL 0 | WARN 6 | SKIP 37 | PASS 2561 ]`; no new failures introduced (report_table body byte-identical to verified state) |

**Score:** 6/6 truths verified (0 present, behavior-unverified)

### Required Artifacts

| Artifact | Expected | Status | Details |
|----------|----------|--------|---------|
| `R/report.R` | `report_table()` exported, substantive body | ✓ VERIFIED | Def at L548; roxygen `@export` + `@param` + `@return` + `@keywords internal` at L520-547; body branches tinytable vs knitr::kable; unchanged by 8c057bf |
| `man/report_table.Rd` | roxygen2-generated | ✓ VERIFIED | Present |
| `NAMESPACE` | `export(report_table)` | ✓ VERIFIED | Match |
| `skeleton.Rmd` | 8 `EventStudy::report_table` sites, 0 `:::` | ✓ VERIFIED | 8 / 0 confirmed |
| `_snaps/api-snapshot.md` | `report_table` in surface | ✓ VERIFIED | 2 occurrences (exports + functions) |
| `.github/workflows/R-CMD-check.yaml` | Suggests-present leg + matrix env vars | ✓ VERIFIED | L24 / L29 / L30 |
| `.Rbuildignore` | skeleton log glob | ✓ VERIFIED | L19 |

### Key Link Verification

| From | To | Via | Status | Details |
|------|----|-----|--------|---------|
| `skeleton.Rmd EventStudy::report_table` | Exported `report_table` | NAMESPACE `export(report_table)` resolves at install time | ✓ WIRED | 8 sites; 0 `:::` bypass |
| `check-r-package@v2` | Installed tarball | `--as-cran --no-manual` args | ✓ WIRED | L47-49 |
| `matrix.config.force_suggests` | `_R_CHECK_FORCE_SUGGESTS_` env | GH Actions ternary | ✓ WIRED | L24 leg + L29 env |

### Behavioral Spot-Checks

| Behavior | Command | Result | Status |
|----------|---------|--------|--------|
| `report_table` exported | `grep "export(report_table)" NAMESPACE` | match | ✓ PASS |
| 8 double-colon sites | `grep -c "EventStudy::report_table" skeleton.Rmd` | 8 | ✓ PASS |
| 0 triple-colon sites | `grep -c "EventStudy:::" skeleton.Rmd` | 0 | ✓ PASS |
| 0 `:::` report_table in inst/ | `grep -rn EventStudy::: inst/ \| grep report_table \| wc -l` | 0 | ✓ PASS |
| force_suggests leg | `grep -n force_suggests R-CMD-check.yaml` | L24 | ✓ PASS |
| `_R_CHECK_FORCE_SUGGESTS_` env | `grep -n _R_CHECK_FORCE_SUGGESTS_ R-CMD-check.yaml` | L29 | ✓ PASS |
| `--as-cran` still present | `grep -n as-cran R-CMD-check.yaml` | L49 | ✓ PASS |
| `.Rbuildignore` skeleton log rule | `grep -n "skeleton.*log" .Rbuildignore` | L19 | ✓ PASS |
| 0 `:::` in vignettes | `grep -rn EventStudy::: vignettes/ \| wc -l` | 0 | ✓ PASS |
| `report_table` in snapshot | `grep -c report_table _snaps/api-snapshot.md` | 2 | ✓ PASS |
| `man/report_table.Rd` exists | `ls man/report_table.Rd` | present | ✓ PASS |

### Requirements Coverage

| Requirement | Source | Description | Status | Evidence |
|-------------|--------|-------------|--------|----------|
| CI-01 | 29-01-PLAN.md / REQUIREMENTS.md L33 | CI gates on `R CMD check` against the *installed* package (not load_all); ≥1 job runs with `_R_CHECK_FORCE_SUGGESTS_` set | ✓ SATISFIED | `check-r-package@v2 --as-cran` (installed tarball) at L47-49; `force_suggests: 'true'` leg L24 with `_R_CHECK_FORCE_SUGGESTS_` env L29; install-gated api-snapshot passes |
| CI-02 | 29-01-PLAN.md / REQUIREMENTS.md L34 | `inst/rmarkdown/` templates + examples/vignettes audited for bare internal calls / default network access | ✓ SATISFIED | 0 `EventStudy:::` in inst/ and vignettes/; skeleton.Rmd fully migrated to `EventStudy::report_table` (the `.report_table()` load_all-vs-installed divergence class is closed) |

### Anti-Patterns Found

None. No TBD/FIXME/XXX debt markers in covered files. The prior transient `.log`-on-disk observation is a non-issue (git-ignored + `.Rbuildignore`-guarded; not a deliverable).

### Bookkeeping Note (non-blocking)

The Phase 29 SUMMARY frontmatter has no `requirements-completed` field. However, CI-01 and CI-02 are recorded via `dependency_graph.provides: [CI-01, CI-02]` and are marked `[x]` / Complete / mapped to Phase 29 in `.planning/REQUIREMENTS.md` (L33-34, L81-82). Both requirements are **satisfied in the delivered CI config and code**; the missing `requirements-completed` field is a documentation-bookkeeping gap only, not a deliverable gap.

### Human Verification Required

None. All truths verified statically.

### Gaps Summary

No gaps. All 6 must-have truths remain VERIFIED at HEAD `ba3d0ce`. The staleness flag was purely digest drift from an unrelated Phase 30 roxygen edit to `R/report.R`; no Phase 29 CI logic, export, wiring, or hygiene guard regressed.

---

_Verified: 2026-09-13T00:00:00Z_
_Verifier: Claude (gsd-verifier)_
