# Phase 27: Property & Numerical-Stability Tests - Context

**Gathered:** 2026-09-11
**Status:** Ready for planning

<domain>
## Phase Boundary

Cross-cutting invariants hold across the whole model/statistic matrix and the
sensitive numeric paths are protected against precision, overflow, and conditioning
failures -- without introducing cross-platform CI flakiness. Requirements:
CORR-03, CORR-04.

In scope:
- Property-based / invariant tests asserting cross-cutting identities
  (`CAR == cumsum(AR)`, cross-method consistency, boundary/degenerate windows)
  across the model and statistic matrix.
- Numerical-stability guards protecting precision/overflow/conditioning in the
  sensitive paths: matrix ops, GARCH convergence, bootstrap, and long-window CAR
  cumulation.
- Documented tolerance on every stability test so results reproduce across platforms
  and CI does not flake.

Out of scope: API/signature reconciliation and shape contracts (Phase 28), CI
restructuring / install-testing (Phase 29), CRAN resubmission (Phase 30). This phase
builds on the Phase 26 audited, golden-pinned base. Behavior on valid inputs must not
change -- guards fire only on degenerate/boundary paths.

</domain>

<decisions>
## Implementation Decisions

### Test tooling (LOCKED)
- **Hand-rolled parameterized tests in base testthat 3e.** Deterministic
  loops/helpers, zero new dependencies, no generative randomness. Chosen because
  SC3 explicitly warns against CI flakiness and the suite is already testthat-idiomatic.
- No `patrick` / `hedgehog` dependency is added. (If a later need arises they remain
  Suggests-only / `requireNamespace()`-guarded per SC4, but the default is no new dep.)

### Invariant-matrix breadth (LOCKED)
- **Tiered / representative coverage.** Universal identities (`CAR == cumsum(AR)`,
  boundary/degenerate windows, monotonic window growth) are asserted on EVERY return
  model; cross-method-consistency and the heavier invariants run on a representative
  subset. Invalid model x statistic pairings are not forced.

### Numerical-guard behavior (LOCKED)
- **Follow the established hardening contract.** On degenerate / ill-conditioned /
  overflow paths, return `NA` with one clear warning (or error where the existing
  contract already errors). Guards are additive -- valid-input math is left untouched
  (SC5). Consistent with the prior robustness milestone's degenerate-input contract.
- No reformulation of valid-input computations (no log-space rewrite / pivoted-solve
  swap on the happy path) -- guards only.

### Tolerance policy (carried from Phase 26)
- Match existing codebase norms: absolute `1e-10` / `1e-8` for algebraic identities,
  relative `1e-6` for cross-method comparisons. Every stability test annotates its
  chosen tolerance and rationale inline (SC3).

### Claude's Discretion
- Exact invariant selection per tier, which models/statistics constitute the
  "representative subset" for the heavier invariants, the specific sensitive-path
  guards (matrix conditioning check, bootstrap degeneracy, long-window cumulation
  overflow, GARCH non-convergence), and test-file layout are at Claude's discretion,
  guided by the success criteria and existing conventions.

</decisions>

<code_context>
## Existing Code Insights

### Reusable Assets
- Golden-value + fixture infrastructure from Phase 26 (`helper-golden-data.R`,
  `test_golden_values.R`) and `helper-mock-data.R` are the fixture idioms to mirror.
- The degenerate-input contract from the prior hardening milestone (contract mode
  threading through models) is the behavior guards must conform to.
- Sensitive numeric paths: `R/models.R` / `R/models_time_varying.R` (OLS solves,
  GARCH convergence), `R/bootstrap.R`, and CAR cumulation in the statistics layer.
- Phase 25 clean `R CMD check` baseline + Phase 26 golden pins are the regression net
  these tests extend.

### Established Patterns
- testthat 3e; `test_*.R` naming; helper fixtures in `helper-*.R`.
- Existing tolerance idioms are `1e-8` / `1e-10`-dominant (documented in Phase 26).

### Integration Points
- New tests slot into the existing `test_check("EventStudy")` entrypoint; no
  DESCRIPTION change (no new dep).

</code_context>

<specifics>
## Specific Ideas

- Universal identities to assert per model: `CAR == cumsum(AR)`; empty/degenerate
  window handling; consistency of AR across return-calculation strategies where
  defined.
- Sensitive-path guards to add: rank-deficient / ill-conditioned design matrices,
  bootstrap with degenerate resamples, long-window CAR cumulation overflow, GARCH
  non-convergence -- each returning NA + one warning per the contract.

</specifics>

<deferred>
## Deferred Ideas

- API signature/shape contracts -> Phase 28.
- Install-tested CI that would exercise these under `R CMD check` -> Phase 29.

</deferred>
