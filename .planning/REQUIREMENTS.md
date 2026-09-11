# Requirements: EventStudy v0.66.0 Stabilization & CRAN Resubmission

**Defined:** 2026-09-10
**Core Value:** Trustworthy numbers, trustworthy interpretation — the pipeline is never silently wrong. This milestone proves that correctness numerically, locks the public API against accidental breakage, and gets the package back onto CRAN.

## v1 Requirements

Requirements for this milestone. Each maps to exactly one roadmap phase.

### CRAN Hygiene (blocks all downstream check runs — must land first)

- [x] **HYG-01**: Every non-ASCII byte in `R/*.R` is replaced with a `\uXXXX` escape (or removed), so `R CMD check --as-cran` emits no non-ASCII WARNING; the CI non-ASCII baseline guard is refreshed to match.
- [x] **HYG-02**: `median` and `tail` in `R/es_diagnostics.R` are namespace-qualified (or added to `importFrom`/`globalVariables`), eliminating the undefined-globals NOTE.
- [x] **HYG-03**: The stale `EventStudy_0.62.0.tar.gz` is removed from the repo and a `tar.gz` ignore rule is added, so no non-standard-file NOTE appears.
- [x] **HYG-04**: All optional-package call sites are audited for `requireNamespace()` guarding + Suggests declaration (explicitly `gridExtra` in `R/plotting.R`); any unguarded/undeclared use is fixed.

### Correctness of Results

- [x] **CORR-01**: Each of the 13+ return models and 8+ test statistics is audited against its published academic formula, and its convention choices (return type, forecast-error correction, degrees of freedom, p-value sidedness, Patell denominator) are documented; any discrepancy is fixed with a regression test.
- [x] **CORR-02**: Golden-value regression tests pin the key statistics against reference values (published-table numbers and/or `estudy2`-derived constants), each test annotating the exact conventions it assumes and using an explicit tolerance (relative for cross-implementation, tight absolute for algebraic identities).
- [ ] **CORR-03**: Property-based / invariant tests assert cross-cutting identities — e.g. `CAR == cumsum(AR)`, cross-method consistency, boundary/degenerate windows — across the model and statistic matrix.
- [ ] **CORR-04**: Numerical-stability guards protect precision/overflow/conditioning in the sensitive paths (matrix ops, GARCH convergence, bootstrap, long-window CAR cumulation), with tests documenting chosen tolerances to avoid cross-platform CI flakiness.

### Stable API

- [ ] **APIS-01**: A signature-consistency audit reconciles inconsistent argument names/order/defaults across the public API; outliers are either aligned or scheduled for deprecation with a recorded rationale. (Runs before the snapshot is captured.)
- [ ] **APIS-02**: Return-shape contracts lock the column names/types/shapes of pipeline-returned tibbles in a new `R/shape_contracts.R` (sibling to `R/contract.R`), opt-in via an option and default-off, covering both valid and degenerate (`is_fitted = FALSE`) outputs.
- [ ] **APIS-03**: API snapshot tests capture the full public signature surface (exports via `getNamespaceExports()`, `formals()` per function, registered S3 methods) using structural assertions — not rendered `print()` output — and are install-gated with `skip_if_not_installed("EventStudy")` so an accidental break fails CI.
- [ ] **APIS-04**: A formal deprecation policy + lifecycle is documented and wired (warn, never silently break) using a base `.Deprecated()` / `lifecycle`-Suggests shim, with NEWS discipline, so future API changes have a backward-compatible path.

### Install-Tested CI

- [ ] **CI-01**: CI gates on `R CMD check` / `rcmdcheck` against the *installed* package (not `devtools::load_all()`), so "green in dev, broken when installed" bugs (the `.report_table()` class) fail CI; at least one job runs with `_R_CHECK_FORCE_SUGGESTS_` set to exercise Suggests-present behavior.
- [ ] **CI-02**: `inst/rmarkdown/` templates and examples/vignettes are audited for bare internal calls and default network access, closing the load_all/installed divergence surface.

### CRAN Resubmission

- [ ] **CRAN-01**: `R CMD check --as-cran` is clean (0 ERRORs, 0 WARNINGs; only explainable NOTEs) on the local environment with the full suite green.
- [ ] **CRAN-02**: Multi-platform checks pass — `devtools::check_win_devel()` + `check_win_release()` and rhub v2 (and/or macOS) — with results captured.
- [ ] **CRAN-03**: The exact 2024-04-20 archival reason is retrieved and `cran-comments.md` is rewritten for v0.66.0 with an explicit archival acknowledgment, the reason, the fixes made, and the platform check results.
- [ ] **CRAN-04**: Examples/tests/vignettes are CRAN-policy compliant — no network by default, no writing outside tempdir, no gratuitous `\dontrun{}`, example runtimes within policy — verified before submission.
- [ ] **CRAN-05**: The package is submitted to CRAN (`devtools::submit_cran()` / webform) and the maintainer email confirmation is completed.

## Future Requirements

Deferred — tracked, not in this roadmap.

- **INDEP-01..03**: Native reimplementation of did/DIDmultiplegt/rugarch — separate independence milestone.
- **SCALE-01..03**: Streaming / data.table backend / sparse FE — orthogonal performance work.
- **PRO-01..02**: RAG-corpus "Advisor Pro" — waitlist-gated commercial tier.

## Out of Scope

Explicitly excluded to prevent scope creep.

| Feature | Reason |
|---------|--------|
| Changing the statistical intent of any existing method | Correctness/robustness/API-lock only; behavior on valid inputs must not change |
| New statistical methods, models, or task types | Stabilization milestone — no new capability surface |
| New hard `Imports` dependencies | New tooling (`lifecycle`, `waldo`, `patrick`, `hedgehog`) stays dev/test-only in Suggests |
| Adding `estudy2`/`eventstudies` to DESCRIPTION | Both are themselves archived on CRAN; usable only as source-level golden-value derivation tools |
| The 1.0 release decision | Remains a separate future decision; v0.66.0 is a ship-when-good minor |
| Byte-level `print()` snapshot tests for the full API | They churn on tibble/rlang version bumps; API lock uses structural assertions instead |

## Traceability

| Requirement | Phase | Status |
|-------------|-------|--------|
| HYG-01 | Phase 25 | Complete |
| HYG-02 | Phase 25 | Complete |
| HYG-03 | Phase 25 | Complete |
| HYG-04 | Phase 25 | Complete |
| CORR-01 | Phase 26 | Complete |
| CORR-02 | Phase 26 | Complete |
| CORR-03 | Phase 27 | Pending |
| CORR-04 | Phase 27 | Pending |
| APIS-01 | Phase 28 | Pending |
| APIS-02 | Phase 28 | Pending |
| APIS-03 | Phase 28 | Pending |
| APIS-04 | Phase 28 | Pending |
| CI-01 | Phase 29 | Pending |
| CI-02 | Phase 29 | Pending |
| CRAN-01 | Phase 30 | Pending |
| CRAN-02 | Phase 30 | Pending |
| CRAN-03 | Phase 30 | Pending |
| CRAN-04 | Phase 30 | Pending |
| CRAN-05 | Phase 30 | Pending |

**Coverage:**

- v1 requirements: 19 total
- Mapped to phases: 19 (Phases 25–30)
- Unmapped: 0 ✓

---
*Requirements defined: 2026-09-10*
*Last updated: 2026-09-10 after roadmap creation (Phases 25–30 mapped)*
