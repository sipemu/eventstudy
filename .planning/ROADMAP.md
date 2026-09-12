# Roadmap: EventStudy

## Milestones

- ✅ **v0.50.0 Robustness Hardening** — degenerate-input contract + regression net (shipped 2026-09-02)
- ✅ **v0.60.0 Grounded AI Advisor** — offline diagnostics + grounded `es_advise()` + provider abstraction (shipped 2026-09-04)
- ✅ **v0.61.0 Advisor Vignette** — advisor vignette + bundled dieselgate dataset (shipped 2026-09-04)
- ✅ **v0.62.0 Docs Site** — curated pkgdown site + CI/CD deploy (shipped 2026-09-06)
- ✅ **v0.63.0 Docs Depth** — Methods articles + worked-examples gallery + per-domain datasets (shipped 2026-09-06)
- ✅ **v0.64.0 Automated AI Reporting** — Phases 17–19.1 (shipped 2026-09-07)
- ✅ **v0.65.0 Polish** — Phases 20–24 (shipped 2026-09-09)
- 🚧 **v0.66.0 Stabilization & CRAN Resubmission** — Phases 25–30 (in progress)

## Phases

<details>
<summary>✅ v0.50.0 – v0.63.0 — SHIPPED</summary>

Earlier milestones are archived under `.planning/milestones/` (per-milestone `-ROADMAP.md`, `-REQUIREMENTS.md`, and `-phases/` directories). See `.planning/MILESTONES.md` for the shipped-accomplishments summary of each.

</details>

<details>
<summary>✅ v0.64.0 Automated AI Reporting (Phases 17–19.1) — SHIPPED 2026-09-07</summary>

- [x] Phase 17: Grounding-Prose Hardening, Offline Report Fallback & CRAN Baseline (3/3 plans) — GROUND-01..03, OFFLINE-01, REPORT-03
- [x] Phase 18: Multi-Format Renderer, Fixed Template & Grounded Narrative Assembly (2/2 plans) — NARR-01..05, FORMAT-01..04, TMPL-01/02, OFFLINE-02
- [x] Phase 19: es_report() Orchestrator, run_event_study(report=) & CRAN-Clean Release Gate (3/3 plans) — REPORT-01/02/04, CRAN-01/02
- [x] Phase 19.1: Close gap GROUND-01/02/03 — wire prose grounding guard into report path (1/1 plan, INSERTED) — gap closure from milestone audit

Full detail: `.planning/milestones/v0.64.0-ROADMAP.md` · requirements: `.planning/milestones/v0.64.0-REQUIREMENTS.md` · audit: `.planning/milestones/v0.64.0-MILESTONE-AUDIT.md`

</details>

<details>
<summary>✅ v0.65.0 Polish (Phases 20–24) — SHIPPED 2026-09-09</summary>

- [x] Phase 20: Brand & Visual Identity (3/3 plans) — BRAND-01..07, CRAN-02/03/04
- [x] Phase 21: Shared Theme & Plot Aesthetics (1/1 plan) — VIZ-01/02/03, CRAN-01
- [x] Phase 22: Report Aesthetics (1/1 plan) — VIZ-04/05/06/07, CRAN-05
- [x] Phase 23: API & Message Polish (3/3 plans) — API-01..06, CRAN-06
- [x] Phase 24: Docs & Site Polish (2/2 plans) — DOCS-01..04

Milestone audit PASSED (30/30 requirements, 5/5 phases). Suite green (2359 pass / 0 fail); zero new `R CMD check` findings vs the 1-NOTE baseline. Annotated tag `v0.65.0` created locally (not pushed); CRAN submission not performed.

</details>

### 🚧 v0.66.0 Stabilization & CRAN Resubmission (In Progress)

**Milestone Goal:** Make results *provably correct* and the public API *stable/locked* — backed by a durable regression net and install-tested CI — then use that hardened base to get EventStudy back onto CRAN (archived 2024-04-20). Ship-when-good minor; not a 1.0 gate.

**Hard invariants (carried into every phase):**

- Behavior on valid inputs must not change — this is correctness/robustness/API-lock plus a submission gate, not a redesign.
- The existing ~2359-test suite stays green throughout.
- New tooling (`lifecycle`, `waldo`, `patrick`, `hedgehog`) is Suggests-only, `requireNamespace()`-guarded — no new hard `Imports`.
- Deprecations are backward-compatible (warn, never silently break).
- No new `R CMD check --as-cran` NOTEs/WARNINGs introduced.

**Execution Order:** Phases run strictly in numeric order — the build order is a hard sequential dependency chain, not parallelizable:

- **Phase 25 (CRAN Hygiene)** must land *first*: a non-ASCII WARNING and an undefined-globals NOTE corrupt every downstream `R CMD check` run, so a clean check baseline must be established before any correctness/test work.
- **Phase 26 (Formula Audit → Golden Values)** performs the formula/implementation audit and documents convention choices *before* pinning golden values — a discovered bug must be fixed before its golden value is locked, and convention traps (log-vs-simple, forecast-error correction, degrees of freedom, p-value sidedness, Patell denominator) must be documented first to avoid false test failures.
- **Phase 27 (Property & Stability Tests)** adds the cross-cutting invariant tests and numerical-stability guards on top of the audited, golden-pinned base.
- **Phase 28 (API Stabilization)** runs the signature-consistency audit (APIS-01) and locks return-shape contracts (APIS-02) *before* capturing the API snapshot (APIS-03) — the snapshot must reflect final, audited signatures; capturing it first would lock bad names.
- **Phase 29 (Install-Tested CI)** gates on the *installed* package / `R CMD check`, catching "green under `load_all`, broken when installed" bugs.
- **Phase 30 (CRAN Resubmission)** is the final gate; it depends on every prior phase being complete.

- [x] **Phase 25: CRAN Hygiene & Clean Check Baseline** - Non-ASCII sweep, undefined-globals fix, stale-tarball removal, optional-package guard audit — a clean `--as-cran` baseline before any test work (completed 2026-09-11)
- [x] **Phase 26: Formula Audit & Golden-Value Validation** - Audit each model/statistic against its published formula, document conventions, fix discrepancies, then pin golden values against reference numbers (completed 2026-09-11)
- [x] **Phase 27: Property & Numerical-Stability Tests** - Cross-cutting invariant/property tests plus numerical-stability guards on the sensitive numeric paths (completed 2026-09-11)
- [x] **Phase 28: API Stabilization & Signature Lock** - Signature-consistency audit + return-shape contracts + deprecation lifecycle, then a structural API snapshot capturing the audited surface (completed 2026-09-12)
- [x] **Phase 29: Install-Tested CI** - CI gates on the installed package / `R CMD check`, plus a load_all-vs-installed divergence audit of templates and examples (completed 2026-09-12)
- [ ] **Phase 30: CRAN Resubmission** - Clean `--as-cran`, multi-platform checks, archival-acknowledging cover letter, policy compliance, and submission

## Phase Details

### Phase 25: CRAN Hygiene & Clean Check Baseline

**Goal**: `R CMD check --as-cran` runs clean of the known hygiene findings (non-ASCII WARNING, undefined-globals NOTE, non-standard-file NOTE) so a trustworthy check baseline exists for every downstream correctness and submission phase.
**Depends on**: Nothing (first phase of milestone)
**Requirements**: HYG-01, HYG-02, HYG-03, HYG-04
**Success Criteria** (what must be TRUE):

  1. `R CMD check --as-cran` emits no non-ASCII WARNING — every non-ASCII byte in `R/*.R` is a `\uXXXX` escape (or removed) — and the CI non-ASCII baseline guard is refreshed to match.
  2. The undefined-globals NOTE is gone: `median` and `tail` in `R/es_diagnostics.R` are namespace-qualified (or added to `importFrom`/`globalVariables`).
  3. No non-standard-file NOTE appears: the stale `EventStudy_0.62.0.tar.gz` is removed from the repo and a `tar.gz` ignore rule prevents recurrence.
  4. Every optional-package call site (explicitly `gridExtra` in `R/plotting.R`) is `requireNamespace()`-guarded and declared in Suggests; any unguarded/undeclared use is fixed.
  5. The full ~2359-test suite stays green and behavior on valid inputs is unchanged.

**Plans**: 1/1 plans executed

- [x] 25-01-PLAN.md — Clear the 3 CRAN findings (non-ASCII escape sweep + baseline refresh, median/tail namespace-qualify, stale-tarball removal + ignore rules) and audit optional-package guards; verified against a clean `--as-cran` gate

### Phase 26: Formula Audit & Golden-Value Validation

**Goal**: Every return model and test statistic is verified correct against its published academic formula with its convention choices documented, and the key statistics are pinned to reference values — so a wrong number can never silently pass, and a correct number can never be falsely failed by a convention mismatch.
**Depends on**: Phase 25 (needs the clean check baseline)
**Requirements**: CORR-01, CORR-02
**Success Criteria** (what must be TRUE):

  1. Each of the 13+ return models and 8+ test statistics is audited against its published formula, and its convention choices (return type, forecast-error correction, degrees of freedom, p-value sidedness, Patell denominator) are documented in a durable reference.
  2. Any discrepancy found in the audit is fixed and locked with a regression test (the fix precedes the golden value being pinned).
  3. Golden-value regression tests pin the key statistics against reference values (published-table numbers and/or `estudy2`-derived constants), with `estudy2`/`eventstudies` used only as source-level derivation tools — not added to DESCRIPTION.
  4. Each golden-value test annotates the exact conventions it assumes and uses an explicit tolerance — relative for cross-implementation comparisons, tight absolute for algebraic identities.
  5. Behavior on valid inputs is unchanged and the full suite stays green.

**Plans**: 1/1 plans executed

- [x] 26-01-PLAN.md — Audit every return model (13+) and test statistic (8+) against its published formula, document conventions in a new CRAN-shipped `statistical-conventions.Rmd` vignette, fix genuine errors (fix precedes pin, locked by regression test), and pin key statistics to published/closed-form golden values (tracer-first: Market Model AR/CAR t, then the full matrix)

### Phase 27: Property & Numerical-Stability Tests

**Goal**: Cross-cutting invariants hold across the whole model/statistic matrix and the sensitive numeric paths are protected against precision, overflow, and conditioning failures — without introducing cross-platform CI flakiness.
**Depends on**: Phase 26 (invariants and stability tests build on the audited, golden-pinned base)
**Requirements**: CORR-03, CORR-04
**Success Criteria** (what must be TRUE):

  1. Property-based / invariant tests assert cross-cutting identities — `CAR == cumsum(AR)`, cross-method consistency, boundary/degenerate windows — across the model and statistic matrix.
  2. Numerical-stability guards protect precision/overflow/conditioning in the sensitive paths: matrix ops, GARCH convergence, bootstrap, and long-window CAR cumulation.
  3. Every stability test documents its chosen tolerance so results are reproducible across platforms and CI does not flake.
  4. Any new test tooling (`patrick`, `hedgehog`) is Suggests-only and `requireNamespace()`-guarded; no new hard `Imports`.
  5. Behavior on valid inputs is unchanged and the full suite stays green.

**Plans**: 1/1 plans executed

- [x] 27-01-PLAN.md — Cross-cutting invariant tests (CAR==cumsum(AR), boundary/degenerate windows, monotonic growth, cross-method + statistic-layer consistency) across the model/statistic matrix, plus additive contract-routed numerical-stability guards (ill-conditioned OLS, bootstrap degeneracy, long-window CAR, GARCH non-convergence)

### Phase 28: API Stabilization & Signature Lock

**Goal**: The public API is reconciled to be internally consistent, its return shapes and signature surface are locked against accidental breakage, and a documented deprecation lifecycle guarantees future changes stay backward-compatible.
**Depends on**: Phase 27 (API lock comes after correctness is proven; the signature audit APIS-01 and shape contracts APIS-02 must precede the snapshot APIS-03)
**Requirements**: APIS-01, APIS-02, APIS-03, APIS-04
**Success Criteria** (what must be TRUE):

  1. A signature-consistency audit reconciles inconsistent argument names/order/defaults across the public API; each outlier is either aligned or scheduled for deprecation with a recorded rationale — and this audit completes *before* the snapshot is captured.
  2. Return-shape contracts lock the column names/types/shapes of pipeline-returned tibbles in a new `R/shape_contracts.R` (sibling to `R/contract.R`), opt-in via an option and default-off, covering both valid and degenerate (`is_fitted = FALSE`) outputs.
  3. A formal deprecation policy + lifecycle is documented and wired (warn, never silently break) via a base `.Deprecated()` / `lifecycle`-Suggests shim, with NEWS discipline.
  4. API snapshot tests capture the full public signature surface (`getNamespaceExports()`, `formals()` per function, registered S3 methods) using structural assertions — not rendered `print()` output — and are install-gated with `skip_if_not_installed("EventStudy")` so an accidental break fails CI.
  5. Behavior on valid inputs is unchanged and the full suite stays green.

**Plans**: 3/3 plans executed
**Wave 1**

- [x] 28-01-PLAN.md — Signature audit + in-place reconciliation with shims (APIS-01) + deprecation lifecycle policy (APIS-04)

**Wave 2** *(blocked on Wave 1 completion)*

- [x] 28-02-PLAN.md — Return-shape contracts in R/shape_contracts.R, option-gated/default-off, warn-only, covering single/multi-event + degenerate shapes (APIS-02)

**Wave 3** *(blocked on Wave 2 completion)*

- [x] 28-03-PLAN.md — Structural install-gated API snapshot pinning the reconciled surface (APIS-03)

### Phase 29: Install-Tested CI

**Goal**: CI exercises the *installed* package and gates on `R CMD check`, so "green under `load_all`, broken when installed" bugs (the `.report_table()` / `skeleton.Rmd` class) fail CI instead of shipping.
**Depends on**: Phase 28 (CI gates the audited, API-locked package)
**Requirements**: CI-01, CI-02
**Success Criteria** (what must be TRUE):

  1. CI gates on `R CMD check` / `rcmdcheck` against the *installed* package (not `devtools::load_all()`), so install-only divergence bugs fail CI.
  2. At least one CI job runs with `_R_CHECK_FORCE_SUGGESTS_` set to exercise Suggests-present behavior.
  3. `inst/rmarkdown/` templates and examples/vignettes are audited for bare internal calls and default network access, closing the load_all/installed divergence surface.
  4. The install-gated CI passes on the current package with the full suite green.

**Plans**: 1/1 plans executed

- [x] 29-01-PLAN.md — export report_table + fix skeleton :::, add Suggests-present CI leg, tarball/vignette hygiene audit

### Phase 30: CRAN Resubmission

**Goal**: EventStudy is back on CRAN — a clean multi-platform `--as-cran` package, an honest archival-acknowledging cover letter, verified policy compliance, submitted and confirmed.
**Depends on**: Phase 29 (final gate — depends on all prior phases: hygiene, correctness, API lock, and install-tested CI)
**Requirements**: CRAN-01, CRAN-02, CRAN-03, CRAN-04, CRAN-05
**Success Criteria** (what must be TRUE):

  1. `R CMD check --as-cran` is clean locally (0 ERRORs, 0 WARNINGs; only explainable NOTEs) with the full suite green.
  2. Multi-platform checks pass — `check_win_devel()` + `check_win_release()` and rhub v2 (and/or macOS) — with results captured.
  3. `cran-comments.md` is rewritten for v0.66.0 with an explicit archival acknowledgment, the exact 2024-04-20 reason, the fixes made, and the platform check results.
  4. Examples/tests/vignettes are CRAN-policy compliant — no network by default, no writing outside tempdir, no gratuitous `\dontrun{}`, example runtimes within policy.
  5. The package is submitted (`devtools::submit_cran()` / webform) and the maintainer email confirmation is completed.

**Risk / Dependency**: The exact 2024-04-20 archival reason is not recorded in any planning artifact and must be retrieved (CRAN archive / maintainer records / package check history) *before* the cover letter (CRAN-03) can be written. Resolved during planning: archived 2024-04-20 at v0.39.2, stated reason "issues were not corrected despite reminders" (captured in 30-CONTEXT.md / 30-RESEARCH.md).
**Plans**: 2 plans
**Wave 1** (autonomous prep)

- [ ] 30-01-PLAN.md — Version bump to 0.66.0, CRAN-policy example compliance (`\dontrun`→`\donttest`), clean `--as-cran` on the built tarball + full suite green, win-builder dispatch, and the archival-acknowledging `cran-comments.md` rewrite (CRAN-01..04)

**Wave 2** (human-gated submission handoff, blocked on Wave 1)

- [ ] 30-02-PLAN.md — Capture win-builder results into `cran-comments.md`, write the operator submission runbook, and hand off at a blocking human-action checkpoint; the agent never fires `submit_cran()` (CRAN-02 result-capture, CRAN-05)

## Progress

**Execution Order:** Phases execute in numeric order: 25 → 26 → 27 → 28 → 29 → 30 (strict sequential chain)

| Phase | Milestone | Plans Complete | Status | Completed |
|-------|-----------|----------------|--------|-----------|
| 20. Brand & Visual Identity | v0.65.0 | 3/3 | Complete | 2026-09-09 |
| 21. Shared Theme & Plot Aesthetics | v0.65.0 | 1/1 | Complete | 2026-09-09 |
| 22. Report Aesthetics | v0.65.0 | 1/1 | Complete | 2026-09-09 |
| 23. API & Message Polish | v0.65.0 | 3/3 | Complete | 2026-09-09 |
| 24. Docs & Site Polish | v0.65.0 | 2/2 | Complete | 2026-09-09 |
| 25. CRAN Hygiene & Clean Check Baseline | v0.66.0 | 1/1 | Complete    | 2026-09-11 |
| 26. Formula Audit & Golden-Value Validation | v0.66.0 | 1/1 | Complete    | 2026-09-11 |
| 27. Property & Numerical-Stability Tests | v0.66.0 | 1/1 | Complete    | 2026-09-11 |
| 28. API Stabilization & Signature Lock | v0.66.0 | 3/3 | Complete    | 2026-09-12 |
| 29. Install-Tested CI | v0.66.0 | 1/1 | Complete    | 2026-09-12 |
| 30. CRAN Resubmission | v0.66.0 | 0/2 | Not started | - |
