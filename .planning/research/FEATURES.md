# Feature Research

**Domain:** R package stabilization & CRAN resubmission (brownfield, correctness hardening, API locking, CI, submission gate)
**Researched:** 2026-09-10
**Confidence:** MEDIUM — all findings cross-checked against official docs, package source, and primary academic references; numeric golden values verified against estudy2 vignette output.

---

## Thrust 1: Correctness of Results (Reference-Value / Golden Validation)

### Table Stakes (Users Expect These)

| Feature | Why Expected | Complexity | Notes |
|---------|--------------|------------|-------|
| AR formula matches published spec | Any published event study must reproduce AR = R_{i,t} - alpha_i - beta_i * R_{m,t} exactly | LOW | OLS alpha/beta from estimation window; this is the MacKinlay (1997) definition |
| CAR = exact cumsum(AR) over event window | Mathematical identity; if violated every downstream result is wrong | LOW | Write as a property test: `expect_equal(car, cumsum(ar_vec), tolerance=1e-10)` |
| AAR = rowwise mean of AR across firms | Identity invariant; must hold to floating-point precision | LOW | Cross-method consistency check across CSectTTest and manual aggregation |
| CAAR = cumsum(AAR) | Follows from the two above | LOW | Add as a property assertion in test_multi_event_test_statistics.R |
| Patell Z formula includes forecast-error variance correction | Without it the test is systematically anti-conservative; every finance textbook specifies it | MEDIUM | Variance = S^2_AR * [1 + 1/M + (R_m,0 - R_bar_m)^2 / sum((R_m,t - R_bar_m)^2)] |
| BMP test denominator is event-day cross-sectional spread, not estimation-window spread | Core property that makes BMP robust to event-induced variance; if wrong, test reverts to Patell | MEDIUM | t = sqrt(N) * SBAR_0 / S(SAR_0); null is t_{N-1} not N(0,1) |
| Sign test uses empirical positive fraction from estimation window (Cowan 1992 generalised) | Naive 50/50 null is known biased; generalised sign is table-stakes for any serious package | LOW | p_hat = (1/N) * sum[(1/M_i) * sum(1[AR > 0])] |

### Differentiators (Competitive Advantage)

| Feature | Value Proposition | Complexity | Notes |
|---------|-------------------|------------|-------|
| Cross-implementation regression test against estudy2 bundled dataset | Locks correctness against an independent published R implementation; no other CRAN event study package does this explicitly | MEDIUM | estudy2 securities_returns dataset (7 firms, 2019-04-01 to 2020-04-01); golden values from vignette for event 2020-03-16 to 2020-03-20: Patell pt_stat 2.5507/-2.9496/8.4216/6.3196, BW-1980 2.4864/-3.3703/8.1881/6.2334, Boehmer 2.1666/8.6521. Tolerance: `expect_equal(..., tolerance=0.01)` (1% relative) |
| Property test suite: algebraic identities across all 13 return models | Proves the pipeline composition is self-consistent regardless of model; catches silent float divergence in GARCH/rolling paths | MEDIUM | Check CAR=cumsum(AR), AAR=mean(AR_matrix), CAAR=cumsum(AAR) for each of the 13 models via parameterised helper |
| Numerical stability guards at matrix operation boundaries | Prevents silently-wrong results when OLS design matrix is near-singular; extends v0.50.0 contract to the numerical layer | HIGH | Use `kappa(X_prime_X) > 1e12` or `rcond(X_prime_X) < 1e-10` as a guard before `lm()` / `solve()`; warn and return NA rather than proceed with unstable estimates |
| GARCH convergence guard | rugarch can return a fitted object with non-converged optimizer; using those parameters produces wrong abnormal returns | MEDIUM | Check `@fit$convergence == 0` and `!any(is.na(@fit$coef))` before computing ARs; if not converged, trigger the degenerate-input contract (NA + one warning) |
| Tolerance convention documented in CONTRIBUTING | Makes the tolerance choices transparent and reproducible; reduces reviewer friction | LOW | Absolute `1e-10` for mathematical identities within one codebase; relative `1e-3` for cross-implementation comparisons where model conventions differ slightly |

### Anti-Features (Things to Deliberately NOT Do)

| Feature | Why Requested | Why Problematic | Alternative |
|---------|---------------|-----------------|-------------|
| Bit-exact snapshot of every test statistic output | "Lock the numbers" | Creates churn on every R version upgrade (floating-point changes in lm, LAPACK backends); reviewer asks "why did 47 golden tests change?" on every CRAN update | Use tolerance-bound assertions (`expect_equal(got, expected, tolerance=1e-6)`) not `identical()` / bit-exact snapshots |
| Using eventstudies (Ajay Shah, nipfpmf) as the primary golden source | It is a well-known CRAN package | eventstudies uses fundamentally different conventions (its own return transformation, zoo-based API) making numeric cross-validation impractical without manual normalization | Use estudy2 as primary; treat eventstudies as secondary sanity check only |
| Validating against EventStudyTools.com online calculator | Convenient reference | Output is proprietary, not reproducible, not pinnable to a version; results can change without notice | Use EventStudyTools formulas page as formula specification only, not as golden numbers |
| Golden test with MacKinlay (1997) Table 1 raw numbers | "The authoritative paper" | Table 1 uses CRSP value-weighted index data (1989-1993) not publicly reproducible; requires exact CRSP data access | Use MacKinlay (1997) as formula/methodology specification; use estudy2 bundled data as the reproducible golden dataset |
| Property tests that generate random market data | Looks like property-based testing | Random market data generates non-deterministic test output, making CI flaky and CRAN check non-deterministic; CRAN penalizes intermittently failing tests | Use `set.seed()` + fixed synthetic data for all property tests; no random inputs in the test suite |

---

## Thrust 2: Stable API (Signature Snapshots, Deprecation Policy, Return-Shape Contracts)

### Table Stakes (Users Expect These)

| Feature | Why Expected | Complexity | Notes |
|---------|--------------|------------|-------|
| Signature snapshot tests for all exported functions | Any package claiming "stable API" needs a regression test that breaks if an argument is added/removed/renamed | MEDIUM | `expect_snapshot(deparse(formals(run_event_study)))` in `tests/testthat/test_api_signatures.R`; covers ~30 exported functions; stored in `_snaps/test_api_signatures.md` |
| Return-shape contract: column names, types, and row-count invariants for tibble-returning functions | Downstream code (`tidy()`, `export_results()`, vignettes) depends on column names; silent renames break users | MEDIUM | Use `expect_named(result, c("relative_index","aar","caar",...))` + `expect_s3_class(result$aar, "numeric")` per function |
| Deprecation warnings via lifecycle pattern (deprecate_warn()) | R community expectation: never break silently | LOW | Use `lifecycle::deprecate_warn("0.66.0", "old_fn()", "new_fn()")` or rlang-classed warning directly |
| NEWS.md discipline: every deprecation listed with version and replacement | Users need to know what changed and when; CRAN reviewers check NEWS | LOW | Format: `## Deprecated` section in the relevant version block; "old_fn() is deprecated; use new_fn() instead." |
| Deprecation test: `expect_warning(old_fn(), class = "lifecycle_warning_deprecated")` | Ensures the deprecation fires rather than silently no-ops | LOW | lifecycle's `expect_deprecated()` helper handles the session-once suppression by setting `lifecycle_verbosity = "warning"` |

### Differentiators (Competitive Advantage)

| Feature | Value Proposition | Complexity | Notes |
|---------|-------------------|------------|-------|
| Formal lifecycle stage annotation in roxygen (`@lifecycle` badge) | Documents stability promise at the function level; surfaced on pkgdown; sets explicit user expectation | LOW | Badges: experimental / stable / deprecated / superseded — add to roxygen `@description` |
| Signature audit report before snapshotting | Prevents locking naming inconsistencies into the snapshot permanently; one-time audit catches `estimation_window` vs `est_window` style drift | LOW | `lapply(getNamespaceExports("EventStudy"), function(f) names(formals(get(f, envir=asNamespace("EventStudy")))))` |
| Return-shape contract as a vignette section | Makes the contract a user-facing promise, not just an internal test | LOW | Add "Return value shapes" to the Methods: Introduction article; list column names/types for each key function |
| Superseded stage for re-named functions (not deprecated) | Superseded does not emit runtime warnings so it does not annoy users; still documents intent | LOW | Use `lifecycle::deprecate_soft()` or `@lifecycle superseded` roxygen tag |

### Anti-Features (Things to Deliberately NOT Do)

| Feature | Why Requested | Why Problematic | Alternative |
|---------|---------------|-----------------|-------------|
| Snapshot the full print() output of EventStudyTask objects | "Lock the user-visible representation" | Print output changes on every R/rlang/tibble version causing constant `snapshot_accept()` cycles | Test specific fields: `expect_equal(task$n_events, 3L)` not `expect_snapshot(print(task))` |
| Snapshot test with `cran = TRUE` for signature tests | "Run on CRAN too" | CRAN check environments vary by platform and locale; snapshot generated on Linux may not match Windows rendering | Use `cran = FALSE` (default) for all signature snapshots |
| One monolithic snapshot file covering all 30 exported functions | "Efficient" | A single change causes the entire snapshot to appear diff'd; reviewers cannot tell what actually changed | One `expect_snapshot()` call per function or per logical group |
| Immediate hard removal (deprecate_stop) in the same version as deprecate_warn | "Clean up fast" | Breaks users who just started getting warnings; expectation is warn for at least one minor version cycle | Warn in v0.66.0, make defunct earliest in v0.67.0; document the timeline explicitly in NEWS.md |
| Adding `lifecycle` to Imports as a hard dependency | "Cleaner code" | Increases the dependency surface CRAN evaluates; not needed at runtime if only used for deprecation signals | Keep lifecycle in Suggests; use `rlang::warn()` with class `lifecycle_warning_deprecated` directly, or guard with `requireNamespace("lifecycle")` |

---

## Thrust 3: Install-Tested CI

### Table Stakes (Users Expect These)

| Feature | Why Expected | Complexity | Notes |
|---------|--------------|------------|-------|
| CI runs `rcmdcheck::rcmdcheck(args = "--as-cran")` not just `devtools::test()` | `load_all()` bypasses NAMESPACE restrictions; installed-package test is the only way to catch "works in dev, broken when installed" bugs | LOW | Use r-lib/actions `check-r-package` workflow; it installs and runs `R CMD check` not `devtools::test()` |
| Windows CI coverage (win-devel or win-release) | CRAN checks Windows; packages failing only on Windows get rejected | LOW | `devtools::check_win_devel()` locally + GitHub Actions matrix with `{os: windows-latest, r: release}` |
| `_R_CHECK_FORCE_SUGGESTS_=false` set in CI | Suggested packages (rugarch, did, etc.) are not installed in standard check environments; matches what CRAN runs | LOW | Add `env: _R_CHECK_FORCE_SUGGESTS_: false` to the GHA step |
| Test suite must pass under R CMD check, not just under `devtools::test()` | Known cases where tests pass under `load_all` but fail under check (NAMESPACE export gaps, missing importFrom) | LOW | Validate by running `devtools::check(args="--no-manual --no-build-vignettes")` locally before any CRAN submission attempt |

### Differentiators (Competitive Advantage)

| Feature | Value Proposition | Complexity | Notes |
|---------|-------------------|------------|-------|
| macOS CI leg (r-release on macos-latest) | Some packages break on Apple Silicon (arm64 BLAS, rugarch linking) and CRAN checks mac; one mac leg catches this class of issue before submission | LOW | Add `{os: macos-latest, r: release}` to GHA matrix |
| Scheduled weekly CI run even with no commits | Catches breakage caused by upstream package updates (tidyverse releases, R minor version bumps) | LOW | Add `schedule: - cron: '0 6 * * 1'` to the GHA YAML |
| Vignette build as separate CI step | Vignettes fail offline for different reasons than code; separating them makes it easier to diagnose the failure class | LOW | Run `devtools::build_vignettes()` separately; compare vs the `--no-build-vignettes` baseline |

### Anti-Features (Things to Deliberately NOT Do)

| Feature | Why Requested | Why Problematic | Alternative |
|---------|---------------|-----------------|-------------|
| Test all 12 optional Suggests packages in CI | "Full coverage" | rugarch, did, DIDmultiplegt, rmgarch, quadprog require Fortran compilers or system libraries; build time doubles or more | Test with `_R_CHECK_FORCE_SUGGESTS_=false` (matching CRAN); add a separate optional-deps workflow that is not a required gate |
| `devtools::check(document=TRUE)` as the CI gate | Faster, developer-friendly | Does not run `--as-cran`; misses incoming feasibility NOTE, URL validation, strict NOTE/WARNING thresholds | Use `rcmdcheck::rcmdcheck(args="--as-cran")` as the gate; reserve `devtools::check` for local iteration |

---

## Thrust 4: CRAN Resubmission (Archived Package)

### Table Stakes (Users Expect These)

| Feature | Why Expected | Complexity | Notes |
|---------|--------------|------------|-------|
| All pre-existing WARNINGs resolved before submission | CRAN treats WARNINGs as blocking regardless of whether they are "pre-existing"; documenting them does not excuse them | MEDIUM | Known WARNINGs to fix: (a) non-ASCII chars in R/advise.R, R/knowledge_base.R, R/report.R — replace with `\uXXXX` escapes; (b) `median`/`tail` undefined globals in es_diagnostics.R — add `@importFrom stats median` and `@importFrom utils tail` |
| cran-comments.md resubmission section with archival acknowledgment | CRAN reviewers read this file; it is the cover letter | LOW | `## Resubmission` section at top: list each change; `## R CMD check results`: "0 errors, 0 warnings, N notes" with each NOTE explained; `## Test environments`; `## Reverse dependencies: none (package was archived)`. Optional comment field on CRAN submission form: "This is a resubmission of EventStudy, archived 2024-04-20. [List what was fixed.]" |
| Version number higher than archived version (0.39.2) | CRAN requires incrementing version at each submission | RESOLVED | Current version 0.65.x → 0.66.0, well above 0.39.2 |
| `devtools::check_win_devel()` green before submission | Windows-devel is the most demanding CRAN platform; a WARNING there causes rejection | MEDIUM | Run locally; fix any Windows-specific issues (file path separators, encoding) before submitting |
| `_R_CHECK_FORCE_SUGGESTS_=false` check passing with 0 errors | CRAN runs with missing Suggests; if the package ERRORs here it will be rejected | LOW | Verify graceful degradation via `requireNamespace()` is complete; the current cran-comments.md records an ERROR when this flag is not set — ensure it is documented that this is env-only |
| All examples either runnable or wrapped in `\dontrun{}` / `\donttest{}` | CRAN checks examples; network-calling or long-running examples cause check failures | LOW | LLM-dependent examples must be in `\dontrun{}`; any example >5 seconds must use `\donttest{}` |
| Stale 0.62.0 tarball removed from repo | Working tree artifacts cause the `.git`/`.planning` NOTE to appear in unexpected places | LOW | `git rm` the tarball; confirm with `R CMD check` that the NOTE is gone |

### Differentiators (Competitive Advantage)

| Feature | Value Proposition | Complexity | Notes |
|---------|-------------------|------------|-------|
| R-hub or win-builder multi-platform check report in cran-comments.md | Shows CRAN team you tested beyond local Linux; reduces back-and-forth review cycles | LOW | Run `devtools::check_win_devel()` + `devtools::check_win_release()` and paste summary; or `rhub::rhub_check(platforms=c("windows","macos","linux"))` |
| NEWS.md entry for 0.66.0 explicitly citing CRAN resubmission | Reviewers appreciate transparency about what changed since the archived version | LOW | Headline item: `## CRAN resubmission after archival (2024-04-20)` with bulleted list of fixes |
| Explaining the CRAN incoming feasibility NOTE in cran-comments.md | The NOTE "Package was archived on CRAN" disappears once the package is on CRAN; documenting it prevents it being misread as a current problem | LOW | Add under Notes: "This NOTE appears because the package is currently archived (2024-04-20). It will disappear on acceptance." |

### Anti-Features (Things to Deliberately NOT Do)

| Feature | Why Requested | Why Problematic | Alternative |
|---------|---------------|-----------------|-------------|
| Submit immediately after fixing NOTEs without multi-platform testing | "It passes locally" | Local Linux checks miss Windows encoding issues, path separator differences, and Windows-only R CMD check rules | Always run `check_win_devel()` + at least one R-hub Linux check before submitting |
| Use `gridExtra` without verifying it is in DESCRIPTION | It appeared in the prior check history as a concern | If `gridExtra` is called without `requireNamespace()` guard AND not in Imports/Suggests, the package gets an undeclared-dependency NOTE/ERROR | Audit every `::` call: `grep -rn "::" R/ | grep -v "^#"` and cross-check against DESCRIPTION |
| Submitting with any un-explained WARNING in cran-comments.md | "Document it as pre-existing" | CRAN policy treats WARNINGs as blocking; documenting them as pre-existing does not excuse them; only works for NOTEs | Fix every WARNING to 0 before submitting; document only NOTEs as pre-existing |
| Relying on the existing `## Pre-existing NOTEs` section from v0.50.0 cran-comments.md without update | The old section is accurate history | The old section describes findings that may no longer apply; a stale section confuses reviewers | Rewrite cran-comments.md from scratch for the 0.66.0 submission with current check results |

---

## Feature Dependencies

```
[Thrust 1: Correctness] ──requires──> [estudy2 in test/Suggests, skip_if_not_installed]

[Thrust 2: API snapshots] ──requires──> [Thrust 2: signature audit complete first]
                                          (snapshotting before audit locks in inconsistencies)

[Thrust 2: deprecation policy] ──requires──> [lifecycle in Suggests OR rlang direct call]

[Thrust 3: Install CI] ──blocks──> [Thrust 4: CRAN submission]
                                    (CI must be green before submitting)

[Thrust 1: 0 WARNINGs] ──blocks──> [Thrust 4: CRAN submission]
  (non-ASCII + undefined globals must be fixed)

[Thrust 4: cran-comments.md] ──requires──> [Thrust 3: multi-platform check results]

[v0.50.0 contract/regression net] ──enhances──> [Thrust 1: property tests]
                                                  (degenerate-input NA discipline already tested;
                                                   golden tests extend to valid-input correctness)
```

### Dependency Notes

- **estudy2 as test dependency only:** Add to `Suggests` guarded by `skip_if_not_installed("estudy2")` in tests; do not add to Imports. Avoids polluting the installed package's dependency footprint.
- **Signature audit before snapshot:** Running snapshot tests before fixing naming inconsistencies locks the wrong names into `_snaps/` files permanently. One audit pass first, then snapshot.
- **CI green gates CRAN:** The v0.62.0 `pkgdown.yaml` CI already runs `R CMD check`; the new `--as-cran` + `FORCE_SUGGESTS=false` CI is an additive layer, not a replacement.

---

## MVP Definition (v0.66.0 Scope)

### Must Ship (blocks CRAN submission)

- [ ] Fix non-ASCII in R/advise.R, R/knowledge_base.R, R/report.R — required for 0 WARNINGs
- [ ] Fix `median`/`tail` undefined globals in es_diagnostics.R — required for clean NOTE set
- [ ] Verify `gridExtra` is declared or all uses are guarded — clean import audit
- [ ] Remove stale 0.62.0 tarball from repo
- [ ] CAR = cumsum(AR) property test across all 13 models
- [ ] estudy2 cross-validation test (securities_returns dataset, tolerance 0.01)
- [ ] Signature snapshot tests for all ~30 exported functions (after audit)
- [ ] Return-shape contract tests for tibble-returning functions
- [ ] `devtools::check_win_devel()` green
- [ ] Updated cran-comments.md with resubmission section + archival acknowledgment
- [ ] GitHub Actions `--as-cran` CI leg with `_R_CHECK_FORCE_SUGGESTS_=false`
- [ ] Submit via `devtools::submit_cran()`

### Add After Validation (v0.66.x)

- [ ] Lifecycle badges on pkgdown reference pages — if deprecation warnings generate user questions
- [ ] R-hub multi-platform check artifacts — if CRAN reviewer asks for cross-platform evidence
- [ ] Kolari-Pynnonen cross-validation against EventStudyTools formula

### Future Consideration (v0.67.0+)

- [ ] Numerical stability kappa() guards on all matrix operations — requires auditing all 13 models
- [ ] GARCH convergence guard — requires rugarch expertise; too broad for this milestone
- [ ] Superseded/defunct lifecycle cycle for renamed functions — requires deciding on new names first

---

## Feature Prioritization Matrix

| Feature | User Value | Implementation Cost | Priority |
|---------|------------|---------------------|----------|
| Fix non-ASCII WARNINGs | HIGH (blocks CRAN) | LOW | P1 |
| Fix undefined globals NOTE | HIGH (blocks CRAN) | LOW | P1 |
| CAR = cumsum(AR) property tests | HIGH (correctness net) | LOW | P1 |
| estudy2 golden cross-validation | HIGH (provable correctness) | MEDIUM | P1 |
| Signature snapshot tests (all exports) | HIGH (API lock) | MEDIUM | P1 |
| Return-shape contract tests | HIGH (downstream stability) | MEDIUM | P1 |
| cran-comments.md resubmission section | HIGH (blocks CRAN) | LOW | P1 |
| GHA --as-cran CI with FORCE_SUGGESTS=false | HIGH (install-tested gate) | LOW | P1 |
| check_win_devel() green | HIGH (blocks CRAN) | MEDIUM | P1 |
| Signature audit (naming consistency) | MEDIUM (one-time cleanup) | LOW | P2 |
| Lifecycle deprecation warnings | MEDIUM (user experience) | LOW | P2 |
| NEWS.md resubmission headline | MEDIUM (transparency) | LOW | P2 |
| macOS CI leg | MEDIUM (platform coverage) | LOW | P2 |
| kappa() numerical stability guards | LOW (edge case) | HIGH | P3 |
| GARCH convergence guard | LOW (rugarch-specific) | HIGH | P3 |

---

## Golden Source References

### Primary formula specification: MacKinlay (1997)
- **Citation:** MacKinlay, A.C. (1997). "Event Studies in Economics and Finance." *Journal of Economic Literature*, 35(1), 13-39.
- **Role:** Formula specification for AR, CAR, AAR, CAAR and the market model. Table 1 illustrates a worked earnings-announcement example using 600 Dow Jones quarterly announcements 1989-1993 (good/no/bad news split).
- **Limitation:** Table 1 uses CRSP value-weighted index data not publicly reproducible. Use as formula spec, not as reproducible golden numbers.
- **Confidence:** HIGH.

### Secondary formula derivations: Campbell, Lo & MacKinlay (1997)
- **Citation:** Campbell, J.Y., Lo, A.W., MacKinlay, A.C. (1997). *The Econometrics of Financial Markets*, Chapter 4. Princeton University Press.
- **Role:** Deeper variance correction derivations for Patell, BMP.
- **Confidence:** HIGH.

### Reproducible golden dataset: estudy2 (CRAN v0.10.0)
- **Package:** `irudnyts/estudy2` on CRAN. Bundled dataset: `securities_returns` (7 firms, 2019-04-01 to 2020-04-01; S&P 500 as index).
- **Golden values — event window 2020-03-16/17/19/20:**
  - Patell (pt_stat): 2.5507, -2.9496, 8.4216, 6.3196
  - Brown-Warner 1980 (bw_1980_stat): 2.4864, -3.3703, 8.1881, 6.2334
  - Boehmer (bh_stat): 2.1666, 8.6521
- **Tolerance for cross-validation:** `expect_equal(..., tolerance = 0.01)` — 1% relative tolerance accounts for different OLS solver conventions between packages; same sign + same significance level is the meaningful validation bar.
- **Confidence:** MEDIUM (verified against live vignette output; package may have minor version differences).

### Formula specification: EventStudyTools.com
- **URL:** https://www.eventstudytools.com/significance-tests
- **Role:** Clear formula statements for Patell Z, BMP, Sign, KP with variance expressions. Use as formula cross-check, not as golden numbers.
- **Confidence:** MEDIUM.

---

## Tolerance Conventions

| Comparison Type | Recommended Tolerance | Rationale |
|----------------|----------------------|-----------|
| Mathematical identities within one codebase (CAR=cumsum(AR), AAR=mean) | `1e-10` absolute | Same code path; only floating-point accumulation error permitted |
| Cross-implementation (EventStudy vs estudy2) | `0.01` relative (1%) | Different OLS solvers, return conventions; 1% catches true formula bugs while tolerating solver differences |
| Cross-implementation significance flags | Same sign + same `***/**/*` level | Statistical conclusion must match even when exact test statistics differ by solver |
| Patell/BMP vs formula derivation | `0.001` relative | Formula differences are small; 0.1% catches implementation bugs |

In testthat 3e: `expect_equal(got, expected, tolerance = 1e-10)` uses `all.equal()` with relative tolerance by default. For absolute tolerance: `expect_equal(got, expected, tolerance = 1e-10, scale = 1)`.

---

## Sources

- MacKinlay (1997) via [Semantic Scholar](https://www.semanticscholar.org/paper/Event-Studies-in-Economics-and-Finance-Mackinlay/61d66e74e0d6973baf01ced1ddc27bc182c88bce) and [EconPapers](https://econpapers.repec.org/RePEc:aea:jeclit:v:35:y:1997:i:1:p:13-39)
- [estudy2 RDocumentation](https://www.rdocumentation.org/packages/estudy2/versions/0.10.0) and [vignette](https://irudnyts.github.io/estudy2/articles/estudy2-intro.html) — primary golden values source
- [estudy2 parametric test source](https://rdrr.io/cran/estudy2/src/R/car_parametric_tests.R)
- [EventStudyTools significance tests](https://www.eventstudytools.com/significance-tests) — formula specifications for Patell Z, BMP, Sign, KP
- [lifecycle package stages](https://lifecycle.r-lib.org/articles/stages.html) — deprecation lifecycle documentation
- [testthat snapshotting](https://testthat.r-lib.org/articles/snapshotting.html) — snapshot test workflow and pitfalls
- [R Packages (2e) Ch. 22: CRAN release](https://r-pkgs.org/release.html) — submission procedure
- [R Packages (2e) Appendix A: R CMD check](https://r-pkgs.org/R-CMD-check.html) — check findings taxonomy
- [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html) — official submission rules
- [r-package-devel archived package thread](https://stat.ethz.ch/pipermail/r-package-devel/2022q4/008604.html) — community experience with archived resubmissions
- [Marine Data Science CRAN checklist](https://www.marinedatascience.co/blog/2020/01/09/checklist-for-r-package-re-submissions-on-cran/) — multi-platform check requirements

---
*Feature research for: EventStudy v0.66.0 Stabilization & CRAN Resubmission*
*Researched: 2026-09-10*
