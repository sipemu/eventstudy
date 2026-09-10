# Architecture Research

**Domain:** CRAN R package — correctness hardening, API locking, install-tested CI, resubmission
**Researched:** 2026-09-10
**Confidence:** HIGH (based on direct inspection of existing codebase, not general-domain inference)

---

## How the Four Thrusts Integrate with the Existing Architecture

This is a brownfield integration study. All findings are grounded in direct file inspection of
the current codebase at v0.65.0.

---

## Thrust 1: Correctness of Results — Golden/Reference Fixtures

### Where fixtures live now

The package already has a working fixtures pattern:

```
tests/testthat/
  fixtures/                            <- EXISTING, committed to git
    contract05_baseline.rds            <- MarketModel golden (CONTRACT-05)
    contract05_bhar_baseline.rds
    contract05_carhart4_baseline.rds
    contract05_comparisonperiod_baseline.rds
    contract05_custom_baseline.rds
    contract05_ff3_baseline.rds
    contract05_ff5_baseline.rds
    contract05_linearfactor_baseline.rds
    contract05_marketadjusted_baseline.rds
    contract05_rollingwindow_baseline.rds
    contract05_volatility_baseline.rds
    contract05_volume_baseline.rds
```

The existing `test_contract.R` at line 281 demonstrates the exact load pattern:

```r
baseline <- readRDS(testthat::test_path("fixtures", "contract05_baseline.rds"))
```

`testthat::test_path()` resolves correctly whether tests run from the package root or the
testthat directory. Combined with committed .rds files, this is fully offline and CI-safe.

### New golden fixtures for v0.66.0

For reference-value validation against published examples (estudy2, eventstudies) and
formula-level checks, follow the same pattern. Extend, do not create a parallel system.

**New fixture naming convention** (extend existing naming under `fixtures/`):

```
tests/testthat/fixtures/
  golden_market_model_ols.rds         # AR/CAR from MacKinlay (1997) Table 1 hand-calc
  golden_patell_z.rds                 # Patell (1976) example values
  golden_bmp_test.rds                 # Boehmer-Musumeci-Poulsen worked example
  golden_fama_french_3.rds            # FF3 benchmark against estudy2 output
  golden_csect_t.rds                  # CSectT reference values
```

**No data-raw provenance needed** for test-only fixtures: these are computed reference values
(not real market datasets), so `data-raw/` is not appropriate. For fixtures derived from a
published reference implementation (estudy2, eventstudies), capture the provenance in
`data-raw/ref_<name>.R` — a short R script showing the reference package call that produced
the expected numbers. This is the existing `data-raw/dieselgate.R` pattern from v0.61.0.

**Offline/CI-safe guarantee:** Committed .rds files + `testthat::test_path()` = fully offline,
no `skip_on_cran()` required. The existing CI (`R-CMD-check.yaml`) runs all tests on
ubuntu/macos/windows-release plus ubuntu-devel, so all golden tests run on every push.

---

## Thrust 2: Stable API — Snapshot Tests Over the Export Surface

### The export surface (measured at v0.65.0)

From `NAMESPACE`:
- **77 `export()` entries** (R6 classes + functions)
- **19 `S3method()` entries** across flag_robustness, format, print, recommend_stat

Total exposed surface: ~96 symbols.

### Where the API snapshot file lives

Use the existing `_snaps/` directory with a dedicated test file:

```
tests/testthat/
  test-api-snapshot.R                  <- NEW: drives the snapshot
  _snaps/
    api-snapshot.md                    <- NEW: auto-created by expect_snapshot()
```

testthat 3e maps `test-api-snapshot.R` to `_snaps/api-snapshot.md` automatically. The
`-` prefix convention in the existing `test-print-snapshots.R` -> `_snaps/print-snapshots.md`
already proves this works in the project.

### How to capture the full export surface

```r
# test-api-snapshot.R
test_that("API surface snapshot -- exports", {
  exports <- sort(getNamespaceExports("EventStudy"))
  expect_snapshot(exports)
})

test_that("API surface snapshot -- formals for each exported function", {
  exports <- getNamespaceExports("EventStudy")
  fns     <- exports[vapply(exports, function(nm) {
    is.function(get(nm, envir = asNamespace("EventStudy")))
  }, logical(1))]
  sig_list <- lapply(sort(fns), function(nm) {
    f <- get(nm, envir = asNamespace("EventStudy"))
    list(name = nm, args = names(formals(f)))
  })
  expect_snapshot(sig_list)
})

test_that("API surface snapshot -- S3 method registry from NAMESPACE", {
  ns_text  <- readLines(system.file("NAMESPACE", package = "EventStudy"))
  s3_lines <- sort(ns_text[grepl("^S3method", ns_text)])
  expect_snapshot(s3_lines)
})
```

**Why `expect_snapshot()` not `.rds`:** The API surface is structured text (function names,
arg names). `expect_snapshot()` diffs are human-readable in PR reviews. The existing
`test-print-snapshots.R` pattern is proof this works well in the project.

**Critical constraint:** `getNamespaceExports()` only returns meaningful results for an
**installed** package, not a `load_all()` session. This is the same class of bug as the
`skeleton.Rmd` `.report_table()` incident mentioned in PROJECT.md. The API snapshot test
must run under `R CMD check` (which installs the package first). Add a guard:

```r
skip_if_not_installed("EventStudy")
```

This makes the test a no-op in raw `devtools::test()` sessions and fully active under
`R CMD check`.

### Signature audit output

A separate `test-signature-consistency.R` asserts invariants derived from the audit (e.g.,
all exported functions that take a `task` arg have it in position 1) as regular
`expect_true`/`expect_equal` tests -- not snapshots -- so CI fails on violation without
needing a snapshot update.

---

## Thrust 3: Return-Shape Contracts — Relationship to R/contract.R

### What R/contract.R provides now

`R/contract.R` (v0.50.0) owns three things:
1. `.resolve_degenerate_mode()` -- ParameterSet field -> lenient/strict resolution
2. `.finite_residual_df()` -- residual df utility
3. `.handle_degenerate()` -- degenerate-input condition handler (warn/stop + sets `is_fitted=FALSE`)

The contract covers the **input** side: what to do when estimation data is degenerate.

### Where return-shape contracts fit

Return-shape contracts cover the **output** side: what column names, types, and shapes each
pipeline stage promises to downstream code. These are orthogonal to degenerate-input handling.
A shape contract fires regardless -- even an all-NA tibble must still have the correct columns.

**Recommendation: sibling, not extension of `R/contract.R`.**

Rationale: `R/contract.R` is scoped to model-fitting degenerate conditions. Mixing in
output-shape validation would confuse its documented purpose and make the degenerate-input
contract harder to audit at a glance.

**New file: `R/shape_contracts.R`**

```r
# R/shape_contracts.R
#
# Return-shape contracts: assert that pipeline outputs have the expected
# column names, types, and shapes.
#
# Activated only when options("EventStudy.check_shapes") is TRUE
# (default FALSE in production; tests set TRUE via withr::local_options()).

.assert_shape <- function(tbl, expected_cols, context = "") {
  if (!isTRUE(getOption("EventStudy.check_shapes", FALSE))) return(invisible(tbl))
  missing <- setdiff(names(expected_cols), names(tbl))
  if (length(missing) > 0L) {
    stop(context, ": missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(tbl)
}
```

**Key design decisions:**
- Shape checks are opt-in via `options("EventStudy.check_shapes")`, default OFF.
  Zero performance overhead in production. Tests enable it via `withr::local_options()`.
- Violations raise `stop()` (always a code bug, not a data condition), contrasting
  with the lenient/strict degenerate-input path.

**Shape constants to define** (tibbles downstream code relies on):

| Output tibble | Required columns | Source |
|--------------|------------------|--------|
| `abnormal_returns()` return | `abnormal_returns`, `relative_index`, `event_window`, `estimation_window` | ModelBase subclasses |
| `fit_model()` nested `model` col | `event_id`, `firm_symbol`, `model` | `R/execute.R` |
| `calculate_statistics()` single-event | `event_id`, `firm_symbol`, `relative_index`, stat col(s) | stat classes |
| `calculate_statistics()` multi-event (CSectT) | `relative_index`, `aar`, `caar` | multi-event stats |
| `tidy()` output | `event_id`, `firm_symbol`, `relative_index`, `abnormal_returns`, `car` | `R/export.R` |

**Connection back to R/contract.R:** When `is_fitted = FALSE`, `abnormal_returns()` must
still return a tibble with the correct shape (all `abnormal_returns = NA_real_`, but the
column must be present and correctly typed). The shape contract test explicitly covers this
case, closing the gap between the two contracts.

**Test placement:** `tests/testthat/test-shape-contracts.R` (NEW). No existing test files
need modification.

---

## Thrust 4a: Install-Tested CI

### What the existing CI already does

`.github/workflows/R-CMD-check.yaml` already runs:
- `r-lib/actions/check-r-package@v2` with `args: 'c("--no-manual", "--as-cran")'`
- Matrix: ubuntu-latest (release), macos-latest (release), windows-latest (release),
  ubuntu-latest (devel)
- Tarball size guard (<1 MB), non-ASCII baseline guard, coverage (covr)

`R CMD check` **installs** the package before running tests. The existing CI is already
install-tested. **No new workflow file is strictly required.**

### The actual gap

The `skeleton.Rmd` `.report_table()` bug (PROJECT.md) happened because `devtools::test()`
(which uses `load_all()`) does not catch cases where code in templates or vignettes calls
package-internal symbols that are only accessible after installation.

The fix for v0.66.0:
1. The API snapshot test (`test-api-snapshot.R`) with `skip_if_not_installed("EventStudy")`
   will only run under `R CMD check`, making it an install-gated test by construction.
2. For vignette smoke coverage, keep the existing `--as-cran` flag (it builds vignettes).
3. Add a minimal smoke-test step to the existing `R-CMD-check.yaml`:

```yaml
- name: Smoke-test installed package import
  if: runner.os == 'Linux'
  run: |
    Rscript -e "library(EventStudy); stopifnot(is.function(run_event_study))"
```

### Optional: r-hub multi-platform (pre-submission only)

For CRAN resubmission validation, run r-hub manually:
- `devtools::check_win_devel()` for Windows devel
- `rhub::check_for_cran()` for additional platforms

Document results in `cran-comments.md`. Only add `.github/workflows/rhub.yaml` if automated
weekly pre-submission gates are desired (not required for v0.66.0).

---

## Thrust 4b: CRAN Resubmission — Pre-existing Issues to Fix

These are in `cran-comments.md` as deferred issues that block CRAN acceptance:

| Issue | Location | Fix |
|-------|----------|-----|
| Bare `median`/`tail` (undefined globals NOTE) | `R/es_diagnostics.R` | Add to `globalVariables()` in `R/EventStudy-package.R` |
| Non-ASCII in R sources (WARNING) | `R/advise.R`, `R/knowledge_base.R`, `R/report.R` | Replace with `\uXXXX` escapes; refresh `.github/non-ascii-baseline.txt` |
| Stale tarball in project root | `EventStudy_0.62.0.tar.gz` | Delete; add `^EventStudy_.*\\.tar\\.gz$` to `.Rbuildignore` |
| Cover letter for archived package | `cran-comments.md` | New v0.66.0 section acknowledging archival + changes |
| `gridExtra` guard | `R/plotting.R` | Already correctly `requireNamespace()`-guarded; no change needed |

The non-ASCII issue is the most important: it generates a WARNING (not just a NOTE), which is
a direct CRAN submission blocker.

---

## Deprecation Shim Architecture

### Decision

From REQUIREMENTS.md API-06 and v0.65.0 scoping: `lifecycle` dependency rejected unless
actual renames exist. For v0.66.0, use base `R` `.Deprecated()` wrapped in a thin shim.

### New file: `R/deprecation.R`

```r
# R/deprecation.R
#
# .es_deprecate() -- thin deprecation shim using base R .Deprecated().
# No new package dependency required.
#
# Raises a warning of class c("deprecatedWarning", "warning") with fields
# old, new, package -- catchable programmatically by tests.

.es_deprecate <- function(old, new = NULL, pkg = "EventStudy") {
  .Deprecated(old = old, new = new, package = pkg)
}
```

**Threading through R6 `initialize()` for deprecated args:**

```r
initialize = function(old_arg = NULL, new_arg = NULL, ...) {
  if (!is.null(old_arg)) {
    .es_deprecate("old_arg", "new_arg")
    new_arg <- old_arg
  }
  # rest of init using new_arg
}
```

**Threading through exported functions:** Same pattern at function entry, before computation.

---

## Integration Map: New vs Modified Files

### New files

| File | Purpose | Thrust |
|------|---------|--------|
| `tests/testthat/fixtures/golden_*.rds` | Reference-value golden fixtures per model/stat | Correctness |
| `data-raw/ref_*.R` | Provenance scripts for reference-implementation baselines | Correctness |
| `tests/testthat/test-golden-values.R` | Load golden fixtures, assert 1e-8 tolerance | Correctness |
| `tests/testthat/test-formula-audit.R` | Formula-level tests (OLS, Patell, CAR cumsum) | Correctness |
| `tests/testthat/test-numerical-stability.R` | Long-window CAR, GARCH, bootstrap stability | Correctness |
| `R/shape_contracts.R` | `.assert_shape()` + shape constant definitions | Return-shape |
| `tests/testthat/test-shape-contracts.R` | Shape contract tests via withr::local_options | Return-shape |
| `tests/testthat/test-api-snapshot.R` | expect_snapshot() over full export surface | API snapshot |
| `tests/testthat/_snaps/api-snapshot.md` | Auto-generated on first run, then committed | API snapshot |
| `tests/testthat/test-signature-consistency.R` | Invariant assertions from signature audit | API snapshot |
| `R/deprecation.R` | `.es_deprecate()` shim via base `.Deprecated()` | Deprecation |

### Modified files

| File | Modification | Thrust |
|------|-------------|--------|
| `R/EventStudy-package.R` | Add `"median"`, `"tail"` to `globalVariables()` | CRAN gate |
| `R/es_diagnostics.R` | Remove bare `median`/`tail` calls (or add importFrom) | CRAN gate |
| `R/advise.R`, `R/knowledge_base.R`, `R/report.R` | Replace non-ASCII with `\uXXXX` | CRAN gate |
| `.github/non-ascii-baseline.txt` | Refresh after removing non-ASCII from R/ sources | CRAN gate |
| `cran-comments.md` | New v0.66.0 section: cover letter, platform results | CRAN gate |
| `DESCRIPTION` | Version bump to 0.66.0 | Release |
| `NEWS.md` | v0.66.0 section | Release |
| `.github/workflows/R-CMD-check.yaml` | Optional: add smoke-test step | CI |
| `R/contract.R` | No changes -- sibling relationship preserved | -- |

---

## Suggested Build Order

Cross-thrust dependencies drive this sequence:

```
Phase A: CRAN hygiene fixes  (unblocks --as-cran for all subsequent check runs)
  - Fix bare median/tail in es_diagnostics.R
  - Fix non-ASCII in advise.R / knowledge_base.R / report.R
  - Refresh .github/non-ascii-baseline.txt
  - Delete EventStudy_0.62.0.tar.gz

Phase B: Formula audit + golden fixture capture  (findings feed golden tests)
  - Audit each model/stat formula against statistical reference
  - Run estudy2 / eventstudies on reference examples, capture output
  - Commit golden_*.rds to tests/testthat/fixtures/
  - Write test-golden-values.R loading those fixtures

Phase C: Edge/property + numerical stability tests
  - CAR = cumsum(AR) property tests
  - Cross-method consistency tests
  - Boundary windows (event window = 1 day, min estimation obs)
  - GARCH/bootstrap long-window numerical stability

Phase D: Return-shape contracts  (after B/C reveal any shape gaps)
  - Write R/shape_contracts.R with .assert_shape() + constants
  - Write test-shape-contracts.R
  - Verify degenerate-NA case satisfies shape contract

Phase E: API snapshot + signature audit + deprecation policy
  - establish expect_snapshot() baseline (requires installed pkg, run under R CMD check)
  - conduct signature audit (formals inconsistencies, naming outliers)
  - write test-signature-consistency.R
  - document deprecation policy; write R/deprecation.R
  - back-compat wiring for any renamed args found in audit

Phase F: CRAN resubmission
  - devtools::check_win_devel() + rhub::check_for_cran()
  - Update cran-comments.md with cover letter + platform results
  - Submit to CRAN
```

**Why this order:**
- Phase A first: non-ASCII WARNING and undefined-globals NOTE contaminate every subsequent
  `R CMD check` run; fix them before any check-gated work.
- Formula audit (Phase B) must precede golden fixture capture: golden values must reflect
  the corrected formula, not a bug. If a bug is found, the fixture must be captured after
  the fix.
- Return-shape contracts (Phase D) sit after correctness tests because the formula audit
  may reveal shape gaps (missing columns in edge cases) the contracts should then lock.
- API snapshot (Phase E) must be last before CRAN: it depends on the installed package and
  must reflect the final post-audit state of all signatures. Capturing before the signature
  audit produces a snapshot that immediately needs updating.
- CRAN resubmission (Phase F) is gated on all prior phases being complete and green.

---

## Anti-Patterns to Avoid

### Anti-Pattern 1: Parallel fixture directories

**What to avoid:** Creating `tests/testthat/reference/` or `tests/testthat/golden/` alongside
the existing `fixtures/` directory.

**Why it's wrong:** 12 committed .rds files with `contract05_*` naming already establish the
convention. A parallel directory fragments fixture management.

**Do this instead:** Extend `fixtures/` with `golden_*.rds` naming to distinguish
reference-value golden tests from the existing degenerate-input baselines.

### Anti-Pattern 2: Hard-coded numeric literals in golden tests

**What to avoid:** `expect_equal(m$statistics$beta, 0.9823456)`

**Why it's wrong:** Already documented in `test_contract.R` comment: "The test MUST NOT
hard-code numeric literals -- the .rds IS the reference." Hard-coded values differ by
platform/R version and can mask real regressions.

**Do this instead:** Capture once into .rds, load with `readRDS(testthat::test_path(...))`,
compare with `expect_equal(tolerance = 1e-8)`.

### Anti-Pattern 3: Extending R/contract.R with return-shape logic

**What to avoid:** Adding `.assert_shape()` or shape constant declarations to `R/contract.R`.

**Why it's wrong:** `R/contract.R` has a clear, documented, single responsibility: degenerate-
input handling on the input side of the pipeline. Adding output-shape validation mixes concerns
and makes the degenerate-input contract harder to audit.

**Do this instead:** Create `R/shape_contracts.R` as a sibling. The two files share the
invariant that even degenerate outputs must have correct shape, which is tested in
`test-shape-contracts.R`.

### Anti-Pattern 4: API snapshot under devtools::test() only

**What to avoid:** Running `getNamespaceExports("EventStudy")` in `devtools::test()` and
treating the result as the installed package surface.

**Why it's wrong:** `getNamespaceExports()` in a `load_all()` session reflects the in-memory
loaded state, not the installed NAMESPACE file. This is the same class of bug as the
`skeleton.Rmd` incident. The snapshot would pass locally but fail to catch installation-time
discrepancies.

**Do this instead:** Wrap with `skip_if_not_installed("EventStudy")`. The test is then a
no-op in `devtools::test()` and active under `R CMD check`.

---

## Sources

- Direct inspection of `R/contract.R`, `NAMESPACE`, `.github/workflows/R-CMD-check.yaml`,
  `cran-comments.md`, `tests/testthat/fixtures/`, `tests/testthat/_snaps/`,
  `tests/testthat/test_contract_matrix.R`, `tests/testthat/test_contract.R`,
  `tests/testthat/test_classed_conditions.R`, `tests/testthat/test-print-snapshots.R`,
  `R/inform.R`, `R/models.R`, `DESCRIPTION`, `.planning/REQUIREMENTS.md`,
  `.planning/PROJECT.md` at v0.65.0
- [testthat snapshot tests documentation](https://testthat.r-lib.org/articles/snapshotting.html)
- [R Packages (2e) — Releasing to CRAN](https://r-pkgs.org/release.html)
- [R Packages (2e) — Lifecycle](https://r-pkgs.org/lifecycle.html)
- [Base R .Deprecated](https://stat.ethz.ch/R-manual/R-devel/library/base/html/Deprecated.html)

---
*Architecture research for: EventStudy v0.66.0 Stabilization & CRAN Resubmission*
*Researched: 2026-09-10*
*Confidence: HIGH -- all integration points grounded in direct inspection of v0.65.0 codebase*
