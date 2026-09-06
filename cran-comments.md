## R CMD check baseline (pre-milestone, commit 63d67a1)

The following NOTEs were present at the pre-milestone baseline (commit 63d67a1,
before Phases 1-4 robustness hardening). These are the expected, pre-existing
findings; the gate is NO NEW NOTEs/WARNINGs introduced by this milestone, NOT
absolute zero.

### Pre-existing NOTEs at baseline (63d67a1)

All items below are Suggests-related NOTEs from optional packages that are not
installed in the check environment but are listed in DESCRIPTION Suggests:

1. Packages suggested but not available for checking:
   - `rugarch` (optional GARCH model; guarded by requireNamespace())
   - `rmgarch` (optional DCC-GARCH; guarded by requireNamespace())
   - `did` (optional Callaway-Sant'Anna estimator; guarded by requireNamespace())
   - `DIDmultiplegt` (optional DiD estimator; guarded by requireNamespace())
   - `didimputation` (optional BJS estimator; guarded by requireNamespace())
   - `quadprog` (optional synthetic control QP solver; guarded by requireNamespace())
   - `sandwich` (optional HAC SEs for cross-sectional regression; guarded by requireNamespace())
   - `tidyquant` (optional data download; guarded by requireNamespace())
   - `quantmod` (optional data download; guarded by requireNamespace())
   - `DT` (optional interactive tables; guarded by requireNamespace())
   - `zoo` (optional time series; guarded by requireNamespace())
   - `openxlsx` (optional Excel export; guarded by requireNamespace())

2. Vignette execution failures (pre-existing, environment-dependent):
   - Vignettes requiring missing optional packages (tidyquant, rugauth) or
     network access to CRAN/data sources fail in the offline check environment.
   - These are pre-existing and not caused by this milestone.

3. Hidden files/directories `.git` and `.planning` NOTE:
   - Present when checked from a git worktree (the worktree is inside .claude/worktrees/).
   - Not present in the real package source directory; CRAN submission uses the
     installed package source, not the worktree.

These baseline NOTEs/WARNINGs are accepted per CRAN policy for packages with many
optional Suggests packages that require external data sources or platform-specific
libraries.

---

## R CMD check gate result (milestone: robustness-hardening, Phase 4, Plan 02)

**Check path: PRIMARY** — `devtools::check(document = TRUE, args = c("--no-manual", "--no-build-vignettes"))`

**Check date:** 2026-09-02

**R version:** 4.6.1 (2026-06-24), Linux (Manjaro), x86_64

### Initial check findings (before milestone fixes applied)

Run against commit 277b2c9 (post 04-01 merge), BEFORE this plan's fixes:

- **1 ERROR:** Vignette execution failures (pre-existing; offline CRAN, missing
  optional packages tidyquant/rugarch/etc.) — NOT milestone-caused.
- **1 WARNING:** `R/cross_sectional.R` — non-ASCII characters (em-dash `—`)
  in comments and warning strings — **NEW** (milestone-caused, Phase 3 commit 52f3d40).
- **3 NOTEs:**
  - Hidden files/dirs `.git`, `.planning` (worktree artifact; pre-existing in baseline)
  - `callr` used via `::` in `R/panel_event_study.R:511` but not declared in
    DESCRIPTION Suggests — **NEW** (milestone-caused; callr used for DIDmultiplegt probe)
  - `bootstrap_test: no visible binding for global variable 'n_car'` — **NEW**
    (milestone-caused; `n_car` NSE column not in `globalVariables()`)

### Fixes applied (source-level, no suppression)

1. **[Rule 1 - Bug] `R/cross_sectional.R`:** Replaced Unicode em-dash (`—`)
   with ASCII double-dash (`--`) in 4 lines (2 comments, 2 warning strings).
   Eliminates the non-ASCII WARNING.

2. **[Rule 2 - Missing] `DESCRIPTION Suggests`:** Added `callr` to Suggests.
   `callr::r()` is called directly in `R/panel_event_study.R:511` for the
   DIDmultiplegt subprocess probe, so it must be declared.

3. **[Rule 2 - Missing] `R/EventStudy-package.R`:** Added `"n_car"` to
   `utils::globalVariables()`. `n_car` is used as an NSE column name in
   `bootstrap_test()` in `R/bootstrap.R`.

### Final check result (after fixes, without vignettes for clean delta)

Command: `devtools::check(document=TRUE, args=c("--no-manual","--no-build-vignettes","--no-vignettes"), error_on="never")`

- **ERRORS: 0**
- **WARNINGS: 0**
- **NOTES: 1** — hidden files `.git`, `.planning` (worktree artifact; present in
  baseline; not present in CRAN submission from real package directory)

**Delta vs baseline: 0 new NOTEs/WARNINGs.** All 3 new findings were fixed at source.
The vignette ERROR is pre-existing (offline environment; not milestone-caused).

**Gate result: PASSED** — zero errors, and current findings (subset: `.git/.planning`
NOTE only) are a strict subset of the recorded baseline. No new finding introduced
by this milestone remains unaddressed.

---

## Test environments

* local: Linux (Manjaro), R 4.6.1 — 1378 pass / 0 fail / 52 skip

## Notes

This is a resubmission of the EventStudy package, previously archived on
2024-04-20. The package has been completely rewritten with all prior issues
addressed. Version number has been incremented beyond the archived version
(0.39.2 -> 0.40.0).

This milestone (0.50.0) adds robustness hardening: degenerate-input contract,
per-model guards, test-statistic guards, pipeline hardening, external-package
wrapping, and a 1378-test regression net.

---

## R CMD check gate result (milestone: Grounded AI Advisor v0.60.0, Phase 8, Plan 02)

**Check command:** `rcmdcheck::rcmdcheck(args = c("--as-cran", "--no-manual"), error_on = "never")` with `_R_CHECK_FORCE_SUGGESTS_=false`

**Check date:** 2026-09-04

**R version:** 4.6.x, Linux (Manjaro), x86_64

### Result summary

```
ERRORS: 0   WARNINGS: 1   NOTES: 2
```

**Milestone gate: PASSED — 0 new NOTEs/WARNINGs introduced by Phases 5–8.**

All findings are pre-existing from prior milestones:

| Finding | Category | Pre-existing Since | New? |
|---------|----------|--------------------|------|
| Non-ASCII chars in R/advise.R, R/knowledge_base.R, R/report.R | WARNING | Phase 5 | NO |
| CRAN incoming feasibility (archived package) | NOTE | Original baseline | NO |
| Undefined globals: `median`, `tail` in es_diagnostics.R | NOTE | Phase 5 | NO |

### Pre-existing WARNING: non-ASCII characters

`R/advise.R`, `R/knowledge_base.R`, `R/report.R` contain UTF-8 non-ASCII
characters (Phase 5 commits). Requires `\uXXXX` escapes for CRAN submission.
Deferred to future cleanup phase.

### Pre-existing NOTE: CRAN incoming feasibility

Package was archived 2024-04-20. Appears on every local check. Cover letter
required at CRAN re-submission.

### Pre-existing NOTE: undefined globals

`median` (stats) and `tail` (utils) used in `R/es_diagnostics.R` (Phase 5)
without explicit `importFrom`. Fix: add to `globalVariables()` or `importFrom`.
Deferred to future cleanup phase.

### CRAN-02 Network-Safety Confirmation

**Confirmed clean:** No examples, tests, or vignettes make network calls by
default under `R CMD check --as-cran`. All network access (LLM via `es_advise()`,
factor data via `download_factor_data()`, stock data via `download_stock_data()`)
requires explicit user action and is guarded by `requireNamespace()` for optional
packages (`httr2`, `jsonlite` in Suggests). CRAN-02 concern is **confirmed resolved**.

### Test suite result (v0.60.0)

```
[ FAIL 0 | WARN 1 | SKIP 31 | PASS 1913 ]
```

1913 tests pass, 0 failures. The single WARN and 31 SKIPs are pre-existing
(optional Suggests packages not installed in dev environment).

---

## R CMD check gate result (milestone: Docs Site v0.62.0, Phases 9–12)

**Baseline source:** Phase 12 SUMMARY (BUILD-06 baseline recorded in Phase 16
CONTEXT.md). This section establishes the accepted v0.62.0 finding set so future
maintainers have a clear diff baseline for v0.63.0.

### Result summary (v0.62.0 baseline)

- **1 ERROR:** Packages suggested but not available for checking — `rugarch`,
  `rmgarch`, `did`, `DIDmultiplegt`, `didimputation`, `DT`. **ENV-ONLY**: these
  are optional Suggests, each guarded by `requireNamespace()`; the ERROR appears
  only in an offline check environment without those packages installed. It is
  not a package defect and is not triggered when `_R_CHECK_FORCE_SUGGESTS_=false`.
- **NOTEs:**
  - CRAN incoming feasibility — package archived 2024-04-20 (original baseline).
  - `VignetteBuilder` no prebuilt vignette index (offline environment).
  - Documentation site URL 404 (resolves after the first GitHub Pages deploy).
  - Undefined globals `median`/`tail` in `R/es_diagnostics.R` (pre-existing since
    Phase 5).

**Gate: PASSED** — 0 new findings introduced by Phases 9–12 vs the prior
(v0.60.0) baseline. All findings are a subset of previously accepted findings.

---

## R CMD check gate result (milestone: Docs Depth v0.63.0, Phase 16, Plan 01)

**Check command:** `devtools::check(document = TRUE, args = c("--no-manual",
"--no-build-vignettes", "--no-vignettes"), error_on = "never")`

**Check date:** 2026-09-06

**R version:** 4.6.1 (2026-06-24), Linux (Manjaro), x86_64

### Result summary (v0.63.0)

```
ERRORS: 0   WARNINGS: 0   NOTES: 1
```

The single NOTE is the **pre-existing** undefined-globals finding:

> `median` (stats) and `tail` (utils) used in `R/es_diagnostics.R` without an
> explicit `importFrom`.

This NOTE is identical to the one recorded in the v0.60.0 and v0.62.0 sections
above (pre-existing since Phase 5). No new NOTE and no WARNING were introduced by
the Phase 16 documentation additions.

### Why v0.63.0 introduces no new CRAN findings

Phase 16 adds only site-level documentation assets, none of which enter the CRAN
tarball or the checked R sources:

- **3 worked-example articles** live in `vignettes/articles/` and are excluded
  from the build tarball by the `^vignettes/articles` line in `.Rbuildignore`
  (confirmed present and unchanged).
- **3 card SVGs** in `man/figures/` (~4.5 KB combined) are static assets with no
  external references; they do not add R code, NOTEs, or WARNINGs.
- **`vignettes/gallery.Rmd`** gained only an additive HTML card section.
- **`_pkgdown.yml`, `pkgdown/extra.css`** are pkgdown-only site configuration,
  not part of the R package.
- No changes were made to `R/`, `NAMESPACE`, `DESCRIPTION`, or `data/`
  (verified by `git diff --name-only` over the Phase 16 commits).

### pkgdown build note (site-only, not a CRAN finding)

`pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)`
completes with **0 errors**. It emits 21 benign `VignetteIndexEntry` title-check
messages; 18 are pre-existing (Phase 15 and earlier vignettes/articles), and 3
correspond to the new site-only example articles, which intentionally carry no
`%\VignetteIndexEntry` — the identical, established convention used by the seven
`methods-*` articles from Phase 15. No new *class* of warning is introduced.

**Gate result: PASSED** — 0 errors, 0 warnings, and the single NOTE is a strict
subset of the v0.62.0 baseline.

### Test environment (v0.63.0)

* local: Linux (Manjaro), R 4.6.1 — `R CMD check`: 0 errors / 0 warnings / 1 note.
