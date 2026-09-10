# Stack Research

**Domain:** R package hardening — correctness testing, API stability, install-tested CI, CRAN resubmission
**Researched:** 2026-09-10
**Confidence:** MEDIUM (all version numbers verified via CRAN index; reference-package archived status cross-checked)

---

## Scope Constraint

This file covers only the **new tooling** needed for v0.66.0. The existing production stack (R6, distributional, plotly, ggplot2, dplyr/tidyr/tibble/purrr/stringr, rlang, testthat 3e, roxygen2, withr, httr2, jsonlite, etc.) is already in DESCRIPTION and is not re-recommended here. **No new hard Imports may be added.** All new tooling is either dev-only (Suggests) or CI-only (workflow files, not in DESCRIPTION at all).

---

## Thrust A — Golden / Reference-Value Testing

### Reference Implementations: estudy2 and eventstudies

**Critical finding: both reference packages are archived on CRAN.**

| Package | Last CRAN Version | Archived | Reason |
|---------|------------------|----------|--------|
| `estudy2` | 0.10.0 | 2022-09-04 | Policy violation |
| `eventstudies` | 1.2.2 | 2021-12-05 | Check problems not corrected |

Because both are archived, neither can be listed as a `Suggests` dep in DESCRIPTION — CRAN policy requires all `Suggests` be available on CRAN. They must be treated as **source-level reference material only**, not runtime deps.

#### estudy2 as a Golden Source

**Suitability: GOOD for Patell Z and BMP cross-checks; MEDIUM for market model alpha/beta.**

estudy2 implements the single-index market model (`apply_market_model(returns, regressors, model="sim", method="ols")`), Patell (1976) standardized-residual Z, Brown-Warner (1980/1985), and the Boehmer-Musumeci-Poulsen BMP test. Its formulas are academically grounded and match the standard references (MacKinlay 1997, Patell 1976, Boehmer et al. 1991).

**Convention differences vs EventStudy:**
- estudy2 returns flat `data.frame` with columns `date`, `weekday`, `percentage`, `mean`, `pt_stat`, `pt_signif`, `bh_stat`, `bh_signif`. EventStudy returns nested tibbles with columns named `ar_t`, `car_t`, `patell_z`, `bmp_stat`. Column names differ; the *values* are what to compare.
- estudy2 expects `zoo` objects for returns; EventStudy uses nested tibbles. A thin adapter is needed to feed the same raw data through both.
- estudy2 uses C++ (Rcpp) internally for return calculations; EventStudy uses pure R. For OLS-based statistics the results should agree to at least `1e-6` absolute tolerance.
- estudy2 CAR parametric functions (`car_parametric_tests`, `car_brown_warner_1985`, `car_lamb`) return columns `car_mean`, `statistic`, `number_of_days` — not cumulative per-day AR. The cross-check is: EventStudy's CAR at window end == estudy2's `car_mean * number_of_days` for mean-based statistics, or direct sum for accumulation-based.

**How to use as golden source (dev-only, not a DESCRIPTION dep):**
Install estudy2 from GitHub (`remotes::install_github("irudnyts/estudy2")`) in a local dev script only. Hard-code the resulting golden numeric values as `dput()`-derived constants in test files. Tests run without estudy2 installed; the golden derivation is a one-time manual step documented in `data-raw/golden/README.md`.

#### eventstudies as a Golden Source

**Suitability: LOW for numeric cross-validation; MEDIUM for pipeline-level smoke checks.**

eventstudies focuses on the estimation/event pipeline using `zoo` objects and the `eventstudy()` function. It has less parametric-test coverage than estudy2 and uses different data conventions (zoo-based vs tibble-based). Its primary value is as a worked-example reference (bundled datasets `SplitDates`, `StockPriceReturns`) rather than formula-level cross-checks. Skip for numeric golden values; prefer estudy2 or published paper tables instead.

#### Published-Paper Golden Values (Preferred Primary Source)

The highest-confidence golden source is numeric tables from the original papers — they are unambiguously stable and require no runtime dep:
- **MacKinlay (1997)**, "Event Studies in Economics and Finance", *Journal of Economic Literature* 35(1): Table 4 reports market model estimation results and CAR values.
- **Patell (1976)**, "Corporate Forecasts of Earnings Per Share": Table 1 reports standardized abnormal returns.
- **Boehmer, Musumeci, Poulsen (1991)**: Table 2 reports BMP statistics.

These hard-coded numeric constants run offline everywhere and are the most defensible golden source.

### Tooling for Golden/Numerical Tests

No new packages beyond existing testthat + withr are required. The idioms are:

```r
# Golden constant derived once from a reference run, never recomputed at test time
GOLDEN_CAR_MARKET_MODEL <- c(-0.0032, 0.0015, -0.0071)  # dput() of reference values

test_that("market model CAR matches MacKinlay 1997 Table 4", {
  result <- run_pipeline_on_fixture()
  expect_equal(result$car, GOLDEN_CAR_MARKET_MODEL, tolerance = 1e-6)
})
```

`expect_equal(tolerance = 1e-6)` is appropriate for OLS-based market model results cross-checked against a C++/reference implementation. Use `tolerance = 1e-4` when cross-checking against hand-computed paper tables (rounding from 4 decimal places). For GARCH-based results, use `tolerance = 1e-4` due to optimizer variance across platforms.

---

## Thrust B — API Signature Snapshot Testing

### testthat 3.3.2 (already in Suggests — no new package required)

`expect_snapshot()` in testthat 3e locks function output, error messages, print method output, and serialized R objects. It is the right tool for API surface tests.

**Idioms for locking the public API surface:**

```r
# Lock argument names and defaults for a function
test_that("fit_model() signature is stable", {
  expect_snapshot(cat(deparse(formals(fit_model))))
})

# Lock return column names and types for a tibble-returning function
test_that("calculate_statistics() return shape is stable", {
  result <- run_pipeline_with_fixture()
  expect_snapshot(names(result))
  expect_snapshot(vapply(result, class, character(1)))
})

# Lock all package exports (snapshot the sorted export list)
test_that("package exports are stable", {
  expect_snapshot(sort(getNamespaceExports("EventStudy")))
})

# Lock S3 method list
test_that("EventStudyTask S3 methods are stable", {
  expect_snapshot(
    grep("^(print|tidy|summary)\\.EventStudyTask",
         getNamespaceExports("EventStudy"), value = TRUE)
  )
})
```

Snapshots live in `tests/testthat/_snaps/` and are committed to git. Diffs surface via waldo. Update with `testthat::snapshot_accept()` or `TESTTHAT_SNAPSHOT_UPDATE=true` env var.

### waldo 0.6.2 (add to DESCRIPTION Suggests)

waldo is testthat's underlying comparison engine — already used implicitly when `expect_equal()` fails. Adding it explicitly to Suggests (a) documents the dependency and (b) allows direct use of `waldo::compare()` in diagnostic helpers for snapshot diffs. It is lightweight (deps: cli, diffobj, glue, rlang, methods).

| Package | Version | CRAN? | Classification |
|---------|---------|-------|----------------|
| `waldo` | 0.6.2 | Yes (2025-07-11) | Suggests (dev-only) |

---

## Thrust C — Deprecation / Lifecycle

### lifecycle 1.0.5 (add to DESCRIPTION Suggests)

lifecycle provides the standard tidyverse/r-lib deprecation infrastructure. rlang (already in Imports) re-exports lifecycle internals, but for calling `lifecycle::deprecate_warn()` directly in package R code, lifecycle itself must be in Suggests and called via `lifecycle::` — or imported with `@importFrom lifecycle deprecate_warn`. Because deprecation warnings fire in production code paths (not just tests), lifecycle belongs in Suggests rather than CI-only.

**Key functions:**

```r
# Soft deprecation — message once per session (suits internal or API-unused fns)
lifecycle::deprecate_soft("0.66.0", "old_fun()", "new_fun()")

# Warn deprecation — warning once per 8 hours (suits public API with real users)
lifecycle::deprecate_warn("0.66.0", "old_fun(arg=)", "new_fun(new_arg=)")

# Hard deprecation — always errors (use only after 2+ release deprecation cycles)
lifecycle::deprecate_stop("1.0.0", "old_fun()")
```

**Roxygen badge:**

```r
#' @description
#' `r lifecycle::badge("deprecated")`
#' `old_fun()` is deprecated. Use [new_fun()] instead.
```

**Policy for EventStudy v0.66.0:** Use `deprecate_warn()` for renamed public arguments — warns without breaking. Never call `deprecate_stop()` in v0.66.0; that requires a 1.0 boundary or a prior deprecation cycle. Add `@importFrom lifecycle deprecate_warn` to `EventStudy-package.R` if calling it in more than one file; otherwise prefix every call with `lifecycle::`.

| Package | Version | CRAN? | Classification |
|---------|---------|-------|----------------|
| `lifecycle` | 1.0.5 | Yes (2026-01-08) | Suggests (used in production code) |

---

## Thrust D — Property-Based and Numerical-Tolerance Testing

### testthat 3.3.2 (already in Suggests) — numerical tolerances

The existing `expect_equal(tolerance=)` idiom covers all numerical-stability tests without new packages:
- `CAR == cumsum(AR)` invariant: `expect_equal(result$car, cumsum(result$ar), tolerance = 1e-10)`
- Cross-method consistency: `expect_equal(log_ret_approx, simple_ret, tolerance = 0.01)` (order-of-magnitude)
- Boundary window edge: standard `expect_equal` on AR at first/last window slot vs NA guard

### hedgehog 0.2 (add to DESCRIPTION Suggests)

hedgehog is the foundational property-based testing framework for R — random generators with automatic counterexample shrinking. Use it for invariants that are infeasible to enumerate exhaustively (e.g., `CAR == cumsum(AR)` holds for any AR vector of any length and sign pattern).

```r
library(hedgehog)
test_that("CAR equals cumsum(AR) for all valid AR vectors", {
  forall(
    gen.c(gen.double(from = -0.1, to = 0.1), from = 1, to = 50),
    function(ar) expect_equal(cumsum(ar)[length(ar)], sum(ar), tolerance = 1e-12)
  )
})
```

hedgehog was updated November 2025 and is CRAN-clean with no heavyweight deps.

Do not add `quickcheck` (0.1.3, last updated October 2023) — it is a thin testthat wrapper over hedgehog that has not been maintained. Use hedgehog directly.

### patrick 0.3.1 (add to DESCRIPTION Suggests)

patrick enables table-driven (parameterized) tests — exactly what a "cross-model/cross-statistic consistency matrix" requires. It avoids copy-pasted test blocks for each model/statistic/window combination.

```r
with_parameters_test_that("market model {model_type} returns non-NA AR on valid data", {
  result <- run_pipeline(model = model_type, data = fixture_data)
  expect_false(any(is.na(result$ar)))
}, cases(
  model_type = list("market_model", "mean_adjusted", "market_adjusted")
))
```

Google-maintained; integrates fully with `devtools::test()` and `R CMD check`. Requires only testthat as a dep.

| Package | Version | CRAN? | Classification |
|---------|---------|-------|----------------|
| `hedgehog` | 0.2 | Yes (2025-11-03) | Suggests (dev-only) |
| `patrick` | 0.3.1 | Yes (2025-12-02) | Suggests (dev-only) |
| `quickcheck` | 0.1.3 | Yes (stale) | **Do not add** — use hedgehog directly |

---

## Thrust E — Install-Tested CI

### The Problem

`devtools::load_all()` patches around NAMESPACE issues and missing `importFrom` declarations. `R CMD check` on the *installed* package catches what `load_all()` hides. The existing cran-comments.md records exactly this class of bug: bare `median`/`tail` in `es_diagnostics.R` not declared in `importFrom`.

### rcmdcheck 1.4.0 (CI-only — do not add to DESCRIPTION)

rcmdcheck provides programmatic R CMD check from R. Key function: `rcmdcheck::rcmdcheck(path = ".", args = c("--as-cran"), error_on = "warning")`. Returns a structured object with `$errors`, `$warnings`, `$notes`. It is already available via devtools; the r-lib/actions `check-r-package@v2` step calls it internally. Do not add it to DESCRIPTION.

### r-lib/actions check-standard.yaml (CI-only — workflow file only, not in DESCRIPTION)

Add `.github/workflows/check-standard.yaml` via `usethis::use_github_action("check-standard")`. This workflow:
- Runs `R CMD check` via rcmdcheck on **Ubuntu + macOS + Windows** against **r-release, r-devel, r-oldrel-1**
- Uses `r-lib/actions/setup-r@v2` (installs R), `setup-r-dependencies@v2` (installs deps via pak), `check-r-package@v2` (runs rcmdcheck)
- Installs the package as a proper tarball — not `load_all()` — so `importFrom` gaps surface immediately
- Gates on no new ERRORs or WARNINGs under `--as-cran`

**Recommended workflow matrix:**

```yaml
strategy:
  matrix:
    config:
      - {os: ubuntu-latest,  r: 'release'}
      - {os: ubuntu-latest,  r: 'devel', http-user-agent: 'release'}
      - {os: ubuntu-latest,  r: 'oldrel-1'}
      - {os: macos-latest,   r: 'release'}
      - {os: windows-latest, r: 'release'}
env:
  _R_CHECK_FORCE_SUGGESTS_: false
```

Set `_R_CHECK_FORCE_SUGGESTS_=false` to match the existing local check baseline (optional Suggests like rugarch/did are not installed in CI and must not cause ERRORs).

No DESCRIPTION entry needed for any r-lib/actions component.

---

## Thrust F — CRAN Resubmission Toolchain

### devtools 2.5.2 (already dev dependency — not in DESCRIPTION)

The CRAN submission flow using devtools:

1. `devtools::check(remote = TRUE, manual = TRUE)` — local final check with manual pages built
2. `devtools::check_win_devel()` — submits to CRAN win-builder r-devel; results via email (~30 min)
3. `devtools::check_win_release()` — submits to win-builder r-release
4. `devtools::check_mac_release()` — submits to CRAN's M1 macOS builder
5. `rhub::rhub_check(platforms = c("linux", "macos", "windows"))` — r-hub cross-platform gate
6. `devtools::submit_cran()` — posts tarball to CRAN web form, attaches `cran-comments.md`

### rhub 2.0.1 (dev-only tool — not in DESCRIPTION)

Use rhub v2; the legacy `rhub::check_for_cran()` is **defunct** as of v2. The new flow:

```r
# One-time setup (commits a workflow file to the repo):
rhub::rhub_setup()     # adds .github/workflows/rhub.yaml; commit and push

# Run checks on demand:
rhub::rhub_check(platforms = c("linux", "macos", "windows"))

# Without GitHub — uses shared R Consortium runners (public, slower):
rhub::rc_submit()
```

Results appear in GitHub Actions, not email. rhub v2 uses binary packages so dependency installation is fast. Available on 20+ platforms including clang-asan, valgrind, and intel variants.

### cran-comments.md resubmission structure (no new package)

For an archived package, the cover letter must explicitly address each prior finding. Required structure for EventStudy (archived 2024-04-20):

```
## Resubmission

This is a resubmission. The package was archived on 2024-04-20.

### Changes since archival

- Non-ASCII characters in R/advise.R, R/knowledge_base.R, R/report.R replaced
  with \uXXXX Unicode escapes to eliminate the non-ASCII WARNING
- Undefined globals `median` and `tail` in R/es_diagnostics.R added to
  importFrom(stats, median) / importFrom(utils, tail) to eliminate the NOTE
- [List any other findings from the archival-time check result]

### R CMD check results

── R CMD check results ─────────── EventStudy 0.66.0 ────
Duration: ...

0 errors | 0 warnings | 0 notes

## Test environments

* local: Linux (Manjaro), R 4.6.1 — N pass / 0 fail / N skip
* win-builder (r-devel): 0 errors, 0 warnings, 0 notes
* win-builder (r-release): 0 errors, 0 warnings, 0 notes
* R-hub: linux / macos / windows — 0 errors, 0 warnings, 0 notes
```

CRAN reviewers for archived packages look for: (a) explicit acknowledgment of the archival reason, (b) evidence all prior findings are fixed, (c) clean `--as-cran` results on at least two platforms.

---

## Complete Addition Summary

### DESCRIPTION Suggests — add these four packages

```
lifecycle,
waldo,
patrick,
hedgehog,
```

### CI-only (workflow files, not in DESCRIPTION)

| Tool | Version | How to add |
|------|---------|------------|
| r-lib/actions check-standard | v2 | `usethis::use_github_action("check-standard")` |
| r-lib/actions check-package | v2 | invoked by check-standard |

### Dev-only tools (install locally, not in DESCRIPTION)

| Tool | Version | How to use |
|------|---------|------------|
| `devtools` | 2.5.2 | `check_win_devel()`, `submit_cran()` |
| `rhub` | 2.0.1 | `rhub_setup()`, `rhub_check()` |

---

## What NOT to Add

| Avoid | Why | Use Instead |
|-------|-----|-------------|
| `estudy2` in Suggests | Archived on CRAN 2022-09-04 — CRAN will reject | Install from GitHub locally; hard-code golden values in test constants |
| `eventstudies` in Suggests | Archived on CRAN 2021-12-05 | Use estudy2 or published paper tables |
| `quickcheck` | No updates since 2023; thin wrapper over hedgehog | Use `hedgehog` directly |
| `vdiffr` | Plot snapshot testing not in this milestone's scope | Revisit if visual regression tests become a goal |
| Any new hard Imports | CRAN discipline — all tooling is test/dev-only | Suggests-guard everything |
| `lintr` / `styler` | Code style audit is not a v0.66.0 goal | Defer to a future "code quality" milestone |

---

## Version Compatibility Notes

| Pair | Status | Notes |
|------|--------|-------|
| testthat 3.3.2 + patrick 0.3.1 | Compatible | patrick requires testthat >= 3.0.0 |
| testthat 3.3.2 + hedgehog 0.2 | Compatible | hedgehog integrates with test_that() |
| lifecycle 1.0.5 + rlang (Imports) | Compatible | rlang re-exports lifecycle internals; no conflict |
| rhub 2.0.1 + R >= 4.0 | Compatible | EventStudy requires R >= 4.1 — superset |
| r-lib/actions v2 + R 4.6.x | Compatible | v2 branch supports R 4.x release/devel |
| `_R_CHECK_FORCE_SUGGESTS_=false` + baseline | Required | Keeps optional-Suggests NOT-installed check from becoming ERRORs in CI |

---

## Sources

- CRAN package index pages for estudy2, eventstudies, lifecycle, rcmdcheck, rhub, devtools, waldo, patrick, hedgehog, quickcheck, testthat — versions confirmed (MEDIUM confidence, cross-checked via webfetch + websearch)
- [R-hub v2 announcement (April 2024)](https://blog.r-hub.io/2024/04/11/rhub2/) — rhub v2 architecture (MEDIUM)
- [r-lib/actions examples README](https://github.com/r-lib/actions/blob/v2-branch/examples/README.md) — check-standard.yaml coverage matrix (MEDIUM)
- [R Packages 2e, Chapter 22: Releasing to CRAN](https://r-pkgs.org/release.html) — submission flow and cran-comments.md format (MEDIUM)
- [testthat snapshotting article](https://testthat.r-lib.org/articles/snapshotting.html) — expect_snapshot idioms (MEDIUM)
- [lifecycle stages article](https://lifecycle.r-lib.org/articles/stages.html) — deprecate_warn/soft function signatures (MEDIUM)
- [estudy2 parametric_tests.R source via rdrr.io](https://rdrr.io/cran/estudy2/src/R/car_parametric_tests.R) — Patell/BMP formulas and return column names (LOW — archived package)
- [estudy2 intro vignette](https://irudnyts.github.io/estudy2/articles/estudy2-intro.html) — function signatures and workflow (LOW — archived package)

---
*Stack research for: EventStudy v0.66.0 Stabilization & CRAN Resubmission*
*Researched: 2026-09-10*
