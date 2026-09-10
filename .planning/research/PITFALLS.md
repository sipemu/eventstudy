# Pitfalls Research

**Domain:** R package hardening + archived-CRAN resubmission (EventStudy v0.66.0)
**Researched:** 2026-09-10
**Confidence:** HIGH — grounded in direct codebase inspection, prior `cran-comments.md` findings, and `R CMD check` history across milestones v0.50.0–v0.65.0.

---

## Critical Pitfalls

### Pitfall 1: Reference-implementation convention mismatch causes false "our numbers are wrong" panic

**What goes wrong:**
Golden values from estudy2, eventstudies, or academic worked examples disagree with EventStudy output on numerically identical data. The implementation is *correct* — but a convention differs silently. The team spends hours debugging a formula that is right, or — worse — "fixes" it to match the reference and introduces a real regression.

**Why it happens:**
Event study literature has multiple valid conventions that are not labelled on results:

- **Return type:** estudy2 uses log returns throughout; EventStudy supports both log and simple via `ParameterSet$return_calculation`. If the reference used log returns and you feed simple returns, CAAR values diverge by the log-approximation error (~0.5% per day for typical equity returns, compounding across a window).
- **Forecast-error correction (FEC):** The standard MacKinlay (1997) market-model sigma for the CAR t-statistic includes a correction for out-of-sample prediction uncertainty: `sigma * sqrt(1 + 1/L + (Rm - mean_Rm)^2 / sum((Rm - mean_Rm)^2))`. Some references omit this term (using raw estimation-window sigma) for simplicity. EventStudy computes the full FEC via `calculate_forecast_error_correction()`. A comparison against an FEC-omitting reference shows CAR t-stats that differ by O(1/sqrt(L)) even on identical returns.
- **Degrees of freedom:** MarketModel uses `df.residual` from `lm()` (n_valid - 2). ComparisonPeriodMeanAdjustedModel uses `sum(!is.na(residuals)) - 1` (no slope). A reference that applies market-model df to every model type produces divergent p-values.
- **p-value sidedness:** EventStudy computes two-sided p-values consistently (`lower.tail = FALSE) * 2`). Some older tools (EVENTSTUDY.xls, certain Stata modules) report one-sided. A "two-sided" comparison against a one-sided reference will show p-values that are off by exactly 2x.
- **Market-adjusted vs market-model:** Market-adjusted AR = r_firm - r_index (no regression; sigma = sd of estimation-window differences; df = n-1). Market-model AR = r_firm - (alpha + beta * r_index) (OLS; sigma = residual std; df = n-2). These are different models. Comparing market-model t-stats to market-adjusted t-stats from a reference produces systematic divergence, especially in Patell Z which uses per-event standardization.
- **Patell Z denominator:** The Patell (1976) standardization divides by sigma times a scaling factor that includes the event-window FEC. The CRAN eventstudies package implements a simplified version without FEC scaling. This produces SAR values roughly 1-3% larger from eventstudies on typical 120-day estimation windows.

**How to avoid:**
Before writing a golden-value test, document three conventions for each reference: (a) return type, (b) FEC included/omitted, (c) df formula. Run the reference on the same data *with the same conventions*, not just the same raw prices. Each golden test must carry a comment: `# Convention: log returns, FEC included, two-sided`. If the reference cannot be configured to match EventStudy's conventions, note the expected analytical delta rather than adjusting code.

**Warning signs:**
- t-stats agree but p-values disagree by exactly 2x: sidedness mismatch.
- CAR values agree but CAR t-stats disagree by a factor near `sqrt(1 + 1/L)`: FEC mismatch.
- All AR values agree but CAAR drifts proportionally to window length: return-type (log vs simple compounding) mismatch.
- Agreement on Market Model but divergence on Comparison-Period-Mean: df convention mismatch.
- SAR values from Patell test are 1-3% off from eventstudies: FEC scaling in denominator not applied by the reference.

**Phase to address:**
Thrust 1 (Correctness) — golden/reference-value validation phase. Convention analysis must precede test authoring.

---

### Pitfall 2: API snapshot tests that churn on every cosmetic change, destroying the signal

**What goes wrong:**
`expect_snapshot()` tests lock exact console output bytes including column widths, rounding artifacts, floating-point last digits, and message phrasing. Every print-method improvement, number formatting change, or wording tweak causes snapshot failures. The team starts running `testthat::snapshot_review()` reflexively without reading the diffs, which defeats the purpose and lets real regressions slip through.

**Why it happens:**
The existing `test-print-snapshots.R` already demonstrates the pattern: it locks exact `print.EventStudySummary`, `print.es_diagnostics`, and `print.Advice` bytes. For *public API* surface tests (function signatures, argument names, defaults, return column names/types), teams reach for `expect_snapshot()` because it is easy — but snapshots of formatted output are noise-heavy. Real API contracts are: does `run_event_study()` still accept `task=` and `parameter_set=`? Does `tidy.EventStudyTask` still return a tibble with column `abnormal_returns`? These are structural invariants, not byte-exact strings.

**How to avoid:**
Distinguish two distinct test layers:
1. **Structural signature tests** — use `formals()` to assert argument names and defaults; use `class()` / `names()` / `ncol()` / `inherits()` to assert return shapes. These never churn on formatting changes.
2. **Byte-stable print snapshots** — acceptable only for `print.*` methods and only when exact wording is part of the public contract. Scope them tightly; keep them in the already-established `test-print-snapshots.R` so the blast radius is bounded.

Never snapshot the numerical content of returned tibbles — floating-point results differ across platforms by the last digit and will cause spurious CI failures on win-devel.

**Warning signs:**
- Snapshot failures on every PR even when no public API changed.
- `testthat::snapshot_review()` is routinely accepted without reading the diff.
- Snapshot file sizes exceed 5 KB — indicates over-capturing.
- Snapshot files contain floating-point numbers.

**Phase to address:**
Thrust 2 (Stable API) — snapshot test authoring phase. Establish the structural vs. byte-stable split before writing tests.

---

### Pitfall 3: `load_all()` hides installed-package namespace failures (the `.report_table()` bug class)

**What goes wrong:**
Under `devtools::load_all()` the entire `R/` directory is sourced into the global environment with no namespace boundary. Internal functions like `.report_table()` are directly accessible. Under the *installed* package, unexported functions are hidden behind the namespace; anything in a vignette or `inst/` template that calls a bare `.report_table()` without `EventStudy:::.report_table()` gets `Error: could not find function ".report_table"` — but only in `R CMD check` or after `install.packages()`, never during development.

**Why it happens:**
This exact bug hit the package: `skeleton.Rmd` called `.report_table()` bare. The test suite ran green under `load_all()` indefinitely. `R CMD check` caught it because it installs first, then runs examples and vignettes. The current test suite still does not include an installed-namespace check layer.

**How to avoid:**
- Add a CI job that runs `R CMD INSTALL --no-multiarch --with-keep.source . && Rscript -e 'library(EventStudy); <smoke-test>'` before `R CMD check`. This forces the namespace boundary.
- Audit every `.Rmd` in `inst/rmarkdown/templates/` and `vignettes/` for bare calls to unexported functions: `grep -rn '^\s*\.[a-z_]' vignettes/ inst/`
- In the `skeleton.Rmd` template, replace any remaining bare internal calls with fully qualified `EventStudy:::` calls or export the helper.
- CI matrix must include at least one `rcmdcheck::rcmdcheck(args = "--as-cran")` run (real install, not `load_all()`).

**Warning signs:**
- All tests pass under `devtools::test()` but `R CMD check` emits `Error: could not find function`.
- Vignette build failures that do not reproduce in an interactive session.
- `system.file("rmarkdown/templates/...", package = "EventStudy")` returns `""` because the template was not installed.

**Phase to address:**
Thrust 3 (Install-tested CI) — the CI layer phase must use `R CMD check` (or `rcmdcheck`), not just `devtools::check(load_all = TRUE)`.

---

### Pitfall 4: Non-ASCII characters trigger a CRAN WARNING that blocks submission

**What goes wrong:**
CRAN's automated checker rejects any `.R` source file containing raw non-ASCII bytes with a WARNING: `Warning: found non-ASCII characters`. A WARNING (not NOTE) blocks automatic CRAN acceptance on most platforms.

**Why it happens:**
Direct byte-level inspection confirms 16 R/ source files contain non-ASCII bytes:
- `single_event_test_statistics.R` — right single quotation mark (U+2019), rightwards arrow (U+2192)
- `models.R` — curly quotes (U+2018, U+2019), em dashes (U+2014)
- `multi_event_test_statistics.R` — `o-umlaut` (U+00F6) in "Pynnonen" in roxygen strings
- `execute.R`, `contract.R`, `parameter_set.R` — em dash (U+2014) in inline comments
- `models_time_varying.R`, `advise.R`, `provider.R`, `advisor_pro.R`, `es_diagnostics.R` — various curly quotes and arrows
- `EventStudy-package.R` — U+00F6 in `KolariPynnonenTest` roxygen item

The key distinction: `\uXXXX` escape sequences in R source are fine for CRAN (confirmed: `knowledge_base.R` and `report_narrative.R` already use them correctly). Raw UTF-8 bytes in `*.R` files are not.

**How to avoid:**
- Run `tools::showNonASCIIfile()` on every R/ file; add it as a CI assertion.
- Replace raw bytes with `\uXXXX` escapes: em-dash becomes `—`, right-quote `’`, o-umlaut `ö`, rightwards arrow `→`.
- CI assertion: `Rscript -e 'bad <- Filter(function(f) length(tools::showNonASCIIfile(f)) > 0, list.files("R", "[.]R$", full.names=TRUE)); if(length(bad)) quit(status=1)'`
- Update the existing non-ASCII baseline guard so *new* files entering the flagged list cause a CI failure, not just line-number shifts.

**Warning signs:**
- `R CMD check --as-cran` emits a WARNING (not NOTE) about non-ASCII characters.
- `tools::showNonASCIIfile("R/multi_event_test_statistics.R")` returns rows.
- A new source file added in a phase causes the non-ASCII CI guard to fail with new entries.

**Phase to address:**
Thrust 4 (CRAN resubmission) — non-ASCII sweep, but must be verified clean before submitting. Each earlier phase must not introduce new raw non-ASCII bytes.

---

### Pitfall 5: `median` and `tail` undefined-globals NOTE is fixable and must not reach submission

**What goes wrong:**
`R CMD check --as-cran` currently emits a NOTE: `es_diagnostics: no visible global function definition for 'median'` and `'tail'` in `R/es_diagnostics.R`. On a resubmission of an archived package, CRAN reviewers treat actionable pre-existing NOTEs as grounds for rejection — the submission signals the author did not do basic housekeeping.

**Why it happens:**
`median()` is in `stats` and `tail()` is in `utils`. Both require either `@importFrom stats median` / `@importFrom utils tail` in the roxygen header, or `stats::median()` / `utils::tail()` at each call site. They are currently in neither. Adding them to `globalVariables()` is wrong — that suppresses NSE column-name notes, not missing-function notes. This has been deferred since Phase 5 (v0.60.0).

**How to avoid:**
Qualify all call sites in `R/es_diagnostics.R`: replace `median(...)` with `stats::median(...)` and `tail(...)` with `utils::tail(...)`. This is the safest fix (no pollution of the global import list, explicit at every call site). Alternatively add `@importFrom stats median` and `@importFrom utils tail` to `EventStudy-package.R`.

**Warning signs:**
- The NOTE appears in every `R CMD check` run and has been listed as pre-existing in `cran-comments.md` since v0.60.0.
- The NOTE survives the v0.66.0 sweep because it was again deferred.

**Phase to address:**
Thrust 4 (CRAN resubmission) — pre-submission hygiene sweep. Fix takes under 10 minutes; no excuse to defer further.

---

### Pitfall 6: Stale `EventStudy_0.62.0.tar.gz` in repo root causes NOTE and install confusion

**What goes wrong:**
`EventStudy_0.62.0.tar.gz` is present in the repo root (confirmed by `ls *.tar.gz`). `R CMD check --as-cran` emits a NOTE for non-standard top-level files, and CRAN reviewers flag tarballs in the source tree as poor housekeeping. More concretely: any CI script or contributor doing `R CMD INSTALL EventStudy_0.62.0.tar.gz` from the repo root installs an outdated build.

**How to avoid:**
Delete `EventStudy_0.62.0.tar.gz` from the repo root. Add `*.tar.gz` to `.gitignore`. Add `^EventStudy.*\.tar\.gz$` to `.Rbuildignore` as belt-and-suspenders.

**Warning signs:**
- `git ls-files | grep '\.tar\.gz'` returns results.
- `R CMD check` NOTE: "Non-standard files/directories found at top level".

**Phase to address:**
Thrust 4 (CRAN resubmission) — pre-submission cleanup.

---

### Pitfall 7: `gridExtra` in `Suggests` but a second unguarded `::` call could surface as an ERROR

**What goes wrong:**
`R/plotting.R:351` has a `requireNamespace("gridExtra")` guard before `gridExtra::grid.arrange()` — that specific call is safe. However, if any future phase adds a second `gridExtra::` call without the guard, `R CMD check --as-cran` emits an ERROR (not NOTE) when gridExtra is absent: `there is no package called 'gridExtra'`. This blocks check completion.

**Why it happens:**
Optional packages in `Suggests` must be guarded at *every* call site. It is easy to add a second grid-arrange call in a new diagnostic helper and forget the guard because the first call has it. The same risk applies to `patchwork` and `tinytable` (both in Suggests, used in v0.65.0 theming/reporting).

**How to avoid:**
- CI assertion: `grep -rn 'gridExtra::' R/` must show only lines inside `requireNamespace(...)` blocks.
- Extend the audit to `patchwork::` and `tinytable::` calls.
- Run `_R_CHECK_FORCE_SUGGESTS_=true devtools::check()` in at least one CI job.

**Warning signs:**
- `R CMD check` with `_R_CHECK_FORCE_SUGGESTS_=true` emits ERROR "there is no package called 'gridExtra'".

**Phase to address:**
Thrust 4 (CRAN resubmission) — Suggests-guard audit sweep.

---

### Pitfall 8: `\dontrun{}` overuse blocks CRAN from verifying examples

**What goes wrong:**
CRAN policy discourages `\dontrun{}` for examples that could run offline. Examples inside `\dontrun{}` are never checked by `R CMD check`, so broken examples stay broken indefinitely. CRAN reviewers flag exported functions whose only example is in `\dontrun{}` — on archived-package resubmissions this is a rejection signal.

**Why it happens:**
EventStudy currently has 12 `\dontrun{}` occurrences: `advise_offline.R` (x2), `execute.R` (`run_event_study`), `report.R` (x2), `provider.R` (x3), `advise.R` (x1), `task_intraday.R` (x1). Many are legitimately network-dependent. However `run_event_study()` in `execute.R` uses bundled data and runs fully offline — `\donttest{}` is the correct wrapper there. `\donttest{}` is checked by `R CMD check --run-donttest` but not by CRAN's automated check; it signals "skip if slow" while remaining checkable.

**How to avoid:**
- Use `\dontrun{}` only for examples that genuinely require network access, API keys, or user-filespace writes.
- Use `\donttest{}` for slow or optional-package-dependent examples that can run offline.
- For functions with no safe offline example, add a 2-line synthetic example using bundled `dieselgate` data.
- Audit: review each `\dontrun{}` location. `execute.R` and `advise_offline.R` are candidates for `\donttest{}` or real examples. `provider.R`, `advise.R`, `report.R` (LLM-dependent) stay as `\dontrun{}`.

**Warning signs:**
- CRAN reviewer email mentions "examples wrapped in `\dontrun`".
- `devtools::run_examples()` executes nothing for the main pipeline functions.
- `R CMD check` passes with zero example errors because all examples are `\dontrun{}`.

**Phase to address:**
Thrust 4 (CRAN resubmission) — example audit. Some fixes belong with whichever correctness/API phase touches the relevant function first.

---

### Pitfall 9: Archived-package resubmission rejected for missing archival acknowledgment in cover letter

**What goes wrong:**
CRAN requires the `cran-comments.md` cover letter for a resubmission of a previously archived package to explicitly acknowledge the archival, state why it was archived, confirm the issue is resolved, and describe all changes since the last CRAN version. Submitting without this acknowledgment gets an automatic rejection with a form response — adding days of delay.

The package was archived 2024-04-20. The v0.50.0 `cran-comments.md` notes section says "previously archived on 2024-04-20 ... completely rewritten" — but that note was written for the v0.50.0 milestone and the resubmission never happened. The v0.66.0 cover letter must be fresh and specific.

**How to avoid:**
The cover letter for v0.66.0 submission must include:
1. Explicit statement: "This package was archived on 2024-04-20. [Reason from archival notice]. That issue has been resolved by [specific fix]."
2. Summary of all changes since last CRAN version (0.39.2 to 0.66.0 — a short paragraph suffices, not a full changelog).
3. `R CMD check` results on all checked platforms: local Linux, win-devel (`devtools::check_win_devel()`), win-release, macOS via R-hub.
4. Confirmation that all Suggests packages are `requireNamespace()`-guarded.
5. Confirmation of no network calls in examples/tests/vignettes by default.

**Warning signs:**
- `cran-comments.md` still references v0.50.0 milestone context.
- No "resubmission" section in the notes.
- Platform check results section is missing win-devel.

**Phase to address:**
Thrust 4 (CRAN resubmission) — cover letter authoring, last step before `devtools::submit_cran()`.

---

### Pitfall 10: Examples or vignettes with runtime > 5 seconds trigger CRAN timing NOTE or rejection

**What goes wrong:**
CRAN policy requires each example to run in under 5 seconds on CRAN check machines (which are slower than typical development machines). The 18 CRAN-shipped vignettes run full event study pipelines; if any includes a GARCH model fit, a bootstrap run with `n_boot = 999`, or a panel DiD run without `skip_on_cran()`, the vignette timing can exceed limits.

**Why it happens:**
- `GARCHModel$new()$fit()` on 250 rows takes 0.5-2s per event on a fast machine; CRAN machines run slower.
- `bootstrap_test()` with default `n_boot = 999` takes several seconds.
- Panel DiD with Callaway-Sant'Anna on >50 units is slow.

**How to avoid:**
- Wrap slow examples in `\donttest{}` (not `\dontrun{}`).
- In CRAN vignettes, use small bundled datasets (dieselgate with 4 events), set `n_boot = 9` in demonstrations, and wrap GARCH/DCC-GARCH chunks with `eval = requireNamespace("rugarch", quietly = TRUE)`.
- Check timing: `devtools::run_examples()` output; flag any example over 3s.
- The `eval = FALSE` knitr option is the vignette equivalent of `\dontrun{}` — avoid it for the same reasons.

**Warning signs:**
- `R CMD check` NOTE: "Examples ... exceeded ... user time".
- A vignette includes `n_boot = 999` without a `skip_on_cran()`.
- GARCH or DCC-GARCH examples without `\donttest{}`.

**Phase to address:**
Thrust 4 (CRAN resubmission) — timing audit. Also worth checking in Thrust 1 if new golden-value tests are slow.

---

### Pitfall 11: `generate_report()` default writes to `getwd()` — a CRAN policy violation in examples or tests

**What goes wrong:**
`generate_report()` defaults `output_file = "event_study_report.html"` and resolves the directory as `getwd()` when no explicit path is given. Writing to `getwd()` in a CRAN check environment is a policy violation ("packages must not write to the user's home filespace or any location they have not explicitly requested"). All current `generate_report()` and `es_report()` examples are wrapped in `\dontrun{}`, which masks this — but any future example, vignette, or test that calls these functions without an explicit `tempdir()` path will trigger a rejection.

**How to avoid:**
- All examples demonstrating `generate_report()` / `es_report()` must pass `output_file = file.path(tempdir(), "report.html")` explicitly, even inside `\dontrun{}` (as documentation hygiene for contributors).
- Any test calling `generate_report()` must use `withr::local_tempdir()` or `file.path(tempdir(), "test-report.html")`.
- Audit: `grep -rn 'generate_report\|es_report' tests/` — verify no call writes to a non-temp path.

**Warning signs:**
- CRAN rejection: "Please ensure that your package does not write to the user's home filespace".
- After running tests, `event_study_report.html` appears in the project root.

**Phase to address:**
Thrust 4 (CRAN resubmission) — example and test audit. Cross-check with any new tests written in Thrust 1-2.

---

### Pitfall 12: `_R_CHECK_FORCE_SUGGESTS_=true` ERRORs masked by local CI configuration

**What goes wrong:**
CRAN runs checks with `_R_CHECK_FORCE_SUGGESTS_=true` by default, which means a missing Suggests package causes an ERROR, not a NOTE. The current `cran-comments.md` v0.62.0 section records ERRORs for `rugarch`, `rmgarch`, `did`, `DIDmultiplegt`, `didimputation`, and `DT`. Local CI with `_R_CHECK_FORCE_SUGGESTS_=false` (which the v0.60.0 check entry uses) masks this. On actual CRAN machines the ERRORs appear.

**Why it happens:**
Every code path reachable without an explicit `requireNamespace()` check fails when the Suggests package is absent. Vignettes that use `library(tidyquant)` at the top without `eval = requireNamespace(...)` guard are especially fragile. The `skeleton.Rmd` template may have similar exposure.

**How to avoid:**
- Test locally with `_R_CHECK_FORCE_SUGGESTS_=true`: `Sys.setenv("_R_CHECK_FORCE_SUGGESTS_" = "true"); devtools::check()`.
- Each CRAN vignette chunk using a Suggests package must begin `eval = requireNamespace("pkg", quietly = TRUE)`.
- Run `rcmdcheck::rcmdcheck(args = "--as-cran")` without the `force_suggests = FALSE` override.
- CI matrix must include at least one job with `_R_CHECK_FORCE_SUGGESTS_=true`.

**Warning signs:**
- Local check passes with `=false` but CRAN returns ERRORs.
- Vignette builds succeed locally (optional packages installed) but fail on win-devel.

**Phase to address:**
Thrust 3 (Install-tested CI) and Thrust 4 (CRAN resubmission). At least one CI matrix job must use `_R_CHECK_FORCE_SUGGESTS_=true`.

---

### Pitfall 13: Floating-point platform divergence breaks golden-value tests on win-devel / macOS ARM

**What goes wrong:**
Golden-value tests asserting exact numerical equality pass on Linux x86_64 but fail on win-devel or R-hub macOS ARM due to differences in floating-point operation ordering, BLAS implementation, or `lm()` numerical precision. GARCH model results are especially sensitive to the BLAS backend.

**Why it happens:**
- OLS `lm()` uses LAPACK/BLAS; different platforms choose different code paths for QR decomposition.
- GARCH estimation uses numerical optimization; convergence differs by platform.
- `set.seed()` with default RNG produces the same sequence only within the same R version and OS family.

**How to avoid:**
- Use `expect_equal(..., tolerance = 1e-6)` for OLS-derived results; never zero tolerance.
- For GARCH/DCC models, use `tolerance = 1e-3` or test only sign/direction of the result.
- For golden-value tests comparing to published academic examples, `tolerance = 1e-4` is appropriate (matches published-table precision).
- Mark GARCH-dependent golden tests with `skip_on_cran()` when rugarch is absent, with a comment explaining the platform tolerance rationale.

**Warning signs:**
- Test passes on Linux but fails on win-devel in CI.
- `expect_equal` failures on platform checks with differences in the 6th-10th significant digit.
- A golden test written on macOS fails on the Linux CI runner.

**Phase to address:**
Thrust 1 (Correctness) — golden-value test authoring must specify appropriate tolerances upfront.

---

## Technical Debt Patterns

| Shortcut | Immediate Benefit | Long-term Cost | When Acceptable |
|----------|-------------------|----------------|-----------------|
| Keep `\dontrun{}` on `run_event_study()` example | No example maintenance | CRAN reviewer flags it; example bit-rots silently | Never — replace with `\donttest{}` or a real 2-line offline example |
| `globalVariables()` for `median`/`tail` instead of `@importFrom` | One-line fix | Masks the issue; `globalVariables` is for NSE column names, not function imports | Never — use `@importFrom` or `::` qualification |
| Defer non-ASCII fixes to "later" | No effort now | Accumulates across phases; becomes a WARNING not a NOTE on submission | Never acceptable before submission |
| Snapshot entire tibble output | Easy test writing | Churns on every floating-point or format change | Only for print methods, never for data content |
| `_R_CHECK_FORCE_SUGGESTS_=false` in all CI jobs | No spurious Suggests ERRORs locally | Masks real CRAN failure modes | Acceptable as a secondary job; at least one job must use `=true` |
| `set.seed()` for bootstrap golden values without documenting the RNG kind | Reproducible locally | Fails on future R version if default RNG changes | Always document `RNGkind("Mersenne-Twister")` explicitly alongside the seed |

---

## Integration Gotchas

| Integration | Common Mistake | Correct Approach |
|-------------|----------------|------------------|
| estudy2 as golden-value reference | Assume identical conventions; copy values directly into `expect_equal` | Document which return type + FEC + df convention estudy2 uses; run on matching conventions or compute the expected delta analytically |
| eventstudies (CRAN) as Patell Z reference | Use eventstudies SAR values directly | eventstudies omits FEC scaling in the Patell denominator; EventStudy includes it; expect 1-3% divergence; document this |
| `devtools::check()` as the sole CI gate | Assume `load_all()`-clean = installed-clean | Add `rcmdcheck::rcmdcheck(args = "--as-cran")` as a separate CI step that does a real install first |
| R-hub for platform checks | Submit only once just before CRAN submission | Submit to R-hub after each major phase; encoding and Suggests failures surface earlier |
| `withr::local_tempdir()` in report tests | Write to `getwd()` in tests | Always use `withr::local_tempdir()` or `file.path(tempdir(), ...)` for any test that generates output files |

---

## "Looks Done But Isn't" Checklist

- [ ] **Non-ASCII sweep:** `tools::showNonASCIIfile()` returns 0 rows for all 16 previously-flagged R/ files — verified with a CI assertion, not just a visual scan.
- [ ] **`median`/`tail` NOTE resolved:** `R CMD check --as-cran` shows 0 undefined-global NOTEs for `es_diagnostics.R` — verified after adding `@importFrom` tags or `::` qualifications.
- [ ] **Stale tarball removed:** `git ls-files | grep '\.tar\.gz'` returns empty.
- [ ] **gridExtra guard audit complete:** `grep -rn 'gridExtra::' R/` returns only lines inside `requireNamespace()` blocks.
- [ ] **Cover letter current:** `cran-comments.md` has a v0.66.0 section acknowledging archival + listing all platform check results.
- [ ] **Example timing verified:** `devtools::run_examples()` completes in < 60s total; no single example exceeds 5s.
- [ ] **Report tests use tempdir:** `grep -rn 'generate_report\|es_report' tests/` shows no writes to `getwd()`.
- [ ] **`\dontrun` audit complete:** `run_event_study()` example uses `\donttest{}` or a real offline example, not `\dontrun{}`.
- [ ] **`_R_CHECK_FORCE_SUGGESTS_=true` CI job passes:** No new ERRORs from absent Suggests packages.
- [ ] **Golden-value tolerances documented:** Each golden test has a comment explaining the tolerance choice and the conventions (return type, FEC, df, sidedness).
- [ ] **win-devel check clean:** `devtools::check_win_devel()` returns 0 ERRORs, 0 WARNINGs, at most pre-existing NOTEs.
- [ ] **Installed-namespace CI job passes:** `R CMD INSTALL` then smoke test runs without "could not find function" errors.

---

## Recovery Strategies

| Pitfall | Recovery Cost | Recovery Steps |
|---------|---------------|----------------|
| Convention mismatch discovered after golden tests written | MEDIUM | Identify which tests assert wrong values; add convention comments; revert "fixes" that changed correct code to match wrong reference |
| Snapshot churn already embedded in CI | LOW | Run `testthat::snapshot_review()` once to accept formatting changes; then replace content snapshots with structural assertions going forward |
| `load_all()` / installed divergence found at submission time | HIGH | Audit all `inst/` Rmds; fix bare internal calls; re-run full `R CMD check` cycle |
| Non-ASCII causes WARNING at CRAN submission | LOW | `tools::showNonASCIIfile()` each flagged file; replace raw bytes with `\uXXXX`; resubmit within 1-2 days |
| Archival acknowledgment missing from cover letter | LOW | Rewrite cover letter; resubmit; CRAN turnaround typically 1-3 business days |
| win-devel failures discovered post-submission | MEDIUM | Submit a patch version quickly; CRAN gives a 14-day grace period for minor fixes |

---

## Pitfall-to-Phase Mapping

| Pitfall | Prevention Phase | Verification |
|---------|------------------|--------------|
| Convention mismatch in golden values | Thrust 1: Reference-value validation | Each golden test has a convention comment; no test adjusted code to match a reference using different conventions |
| Snapshot churn | Thrust 2: API snapshot tests | Snapshot tests cover structure (formals, names, types), not floating-point content |
| `load_all()` vs installed namespace divergence | Thrust 3: Install-tested CI | `rcmdcheck` (real install) in CI; `inst/` Rmd audit complete |
| Non-ASCII raw bytes | Thrust 4: CRAN submission sweep | `tools::showNonASCIIfile()` CI assertion passes on all 16 files |
| `median`/`tail` NOTE | Thrust 4: CRAN submission sweep | `R CMD check --as-cran` shows 0 undefined-global NOTEs |
| gridExtra unguarded calls | Thrust 4: Suggests-guard audit | grep assertion in CI passes |
| Stale tarball | Thrust 4: Cleanup | `git ls-files` check returns clean |
| `\dontrun{}` overuse | Thrust 4: Example audit | `devtools::run_examples()` executes at least one real example per exported function |
| Missing archival acknowledgment | Thrust 4: Cover letter | `cran-comments.md` has explicit archival statement and v0.66.0 platform results |
| Example runtime > 5s | Thrust 4: Timing audit | `devtools::run_examples()` timing output; no single example > 5s |
| `generate_report()` writes to `getwd()` | Thrust 4 + Thrust 1-2 test authoring | All report tests use tempdir; examples use `file.path(tempdir(), ...)` |
| `_R_CHECK_FORCE_SUGGESTS_=true` ERRORs | Thrust 3: Install-tested CI | CI job with `=true` passes |
| Floating-point platform divergence | Thrust 1: Golden-value test authoring | `tolerance = 1e-6` default documented; GARCH tests use `1e-3` |

---

## Sources

- Direct codebase inspection: `R/es_diagnostics.R`, `R/plotting.R`, `R/models.R`, `R/single_event_test_statistics.R`, `R/multi_event_test_statistics.R`, `R/EventStudy-package.R`, `R/report.R`, `DESCRIPTION`
- Non-ASCII byte audit: Python byte-level scan of all 16 flagged R/ files (confirmed character identities: U+2014 em dash, U+2019 right single quote, U+00F6 o-umlaut, U+2192 rightwards arrow)
- `cran-comments.md`: full R CMD check history across milestones v0.50.0-v0.63.0
- CRAN Policy: Writing R Extensions §1.1.3.1 (writing to user filespace), §5.4.2 (dontrun/donttest semantics), CRAN Repository Policy §2 (non-ASCII, submission cover letter requirements)
- MacKinlay (1997) Journal of Economic Literature §3-4: forecast error correction formula and df conventions
- Patell (1976) Journal of Accounting Research: SAR denominator (FEC scaling included in original)
- Prior milestone research: `.planning/milestones/v0.50.0-phases/02-model-and-stats-sweep/02-RESEARCH.md` (reference-implementation convention analysis)

---
*Pitfalls research for: R package hardening + archived-CRAN resubmission (EventStudy v0.66.0)*
*Researched: 2026-09-10*
