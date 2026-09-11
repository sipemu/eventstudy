# Phase 28: API Stabilization & Signature Lock - Context

**Gathered:** 2026-09-11
**Status:** Ready for planning
**Mode:** Autonomous smart-discuss

<domain>
## Phase Boundary

This phase reconciles the public API to be internally consistent, locks its
return shapes and signature surface against accidental breakage, and documents
+ wires a deprecation lifecycle so future changes stay backward-compatible.

Delivers:
- **APIS-01** — signature-consistency audit + reconciliation (runs *before* the snapshot).
- **APIS-02** — return-shape contracts in a new `R/shape_contracts.R` (sibling to `R/contract.R`), opt-in, default-off.
- **APIS-03** — API snapshot tests over the full public signature surface, install-gated.
- **APIS-04** — formal deprecation policy + lifecycle wired via base `.Deprecated()` / `lifecycle`-Suggests shim, with NEWS discipline.

Out of scope: new features, behavior changes on valid inputs (must stay
identical), CI wiring of the install-gated tests (Phase 29), CRAN submission
(Phase 30).

</domain>

<decisions>
## Implementation Decisions

### Signature Reconciliation (APIS-01)
- **Align in place now** (user override of the conservative default). Rename inconsistent public-API argument names/order/defaults to a consistent scheme *this phase*.
- Every renamed argument MUST get a `.Deprecated()`-style shim that still accepts the old name and forwards it, so all existing call sites and the 400+ test suite stay green — behavior on valid inputs is unchanged.
- Each outlier that is reconciled gets a recorded rationale; any outlier judged too risky to rename even with a shim is instead *scheduled* for deprecation with its rationale documented.
- The audit + reconciliation decisions are recorded in a durable planning artifact (e.g. `28-API-AUDIT.md`) so the "what changed and why" is auditable.
- The audit completes and reconciliation lands **before** the API snapshot (APIS-03) is captured, so the snapshot pins the *reconciled* surface.

### Shape Contracts (APIS-02)
- New file `R/shape_contracts.R`, sibling to `R/contract.R`, following the same option-resolution pattern (`getOption("EventStudy.<...>")`, default-off).
- On a column name / type / shape mismatch: **warn** (one `warning()`), mirroring the lenient degenerate-input contract philosophy in `R/contract.R`. Do not `stop()`.
- Opt-in via a package option, **default-off** so valid-input behavior and the existing suite are untouched.
- **Coverage: core pipeline outputs + degenerate shapes** — the single-event statistics tibble, the multi-event AAR/CAAR tibble, AND both of their `is_fitted = FALSE` degenerate shapes.

### API Snapshot (APIS-03)
- Capture the full public signature surface structurally: `getNamespaceExports()`, `formals()` per exported function, and registered S3 methods — **structural assertions, not rendered `print()` output**.
- Storage mechanism: **testthat `expect_snapshot_value()`** with a structural list; testthat manages the snapshot under `_snaps/`.
- Install-gated with `skip_if_not_installed("EventStudy")` so it runs against the installed package and an accidental break fails CI (the CI wiring itself is Phase 29).

### Deprecation Lifecycle (APIS-04)
- Base `.Deprecated()` as the always-available mechanism; `lifecycle` used only if available (Suggests, guarded by `requireNamespace()`), never a hard dependency.
- Policy: **warn, never silently break.** Deprecated surface keeps working and emits a deprecation warning pointing at the replacement.
- NEWS discipline: every deprecation / rename gets a `NEWS.md` entry.

</decisions>

<code_context>
## Existing Code Insights

### Reusable Assets
- `R/contract.R` — the degenerate-input contract. Provides the exact option-resolution pattern to mirror: `.resolve_degenerate_mode()` reads `ParameterSet` field > `getOption("EventStudy.degenerate_handling")` > default. `R/shape_contracts.R` should follow this shape.
- `EventStudy-package.R` — `globalVariables()` + `@importFrom` declarations; new option/NEWS references live near here.
- Public pipeline entry points: `run_event_study()` (`R/execute.R`), `prepare_event_study()`, `fit_model()`, `calculate_statistics()` — primary signature-audit targets.
- Result tibbles: single-event stats (`R/single_event_test_statistics.R`, `R/test_statistics_set.R`) and multi-event AAR/CAAR (`R/multi_event_test_statistics.R`) — the shape-contract targets.

### Established Patterns
- Options namespaced `EventStudy.*`, resolved with a `default =` fallback; `match.arg()` for enumerated modes.
- Lenient default = warn once + propagate NA; strict = descriptive `stop()`. Shape contract adopts the *warn* half.
- `requireNamespace()` guards for all Suggests-only packages (the `lifecycle` shim must follow this).
- testthat 3e; install-gated tests use `skip_if_not_installed("EventStudy")`.

### Integration Points
- Deprecation warnings fire from the renamed public functions in `R/execute.R` and wherever outlier signatures live.
- Shape-contract checks hook into the pipeline result assembly (post `calculate_statistics()`), gated by the new option.
- Snapshot test is a new `tests/testthat/test-api-snapshot.R` (install-gated).

</code_context>

<specifics>
## Specific Ideas

- The signature audit MUST precede the snapshot capture (ordering is a hard success-criterion, not a preference).
- Keep `Suggests` vs `Imports` boundaries intact — `lifecycle` stays optional and guarded.
- No new `R CMD check` NOTEs/WARNINGs may be introduced.

</specifics>

<deferred>
## Deferred Ideas

- Actual *removal* of any deprecated argument/function (defunct stage) — deferred to a future major version; this phase only warns.
- CI wiring that gates on the install-tested snapshot — Phase 29.
- WR-01 code-review follow-up from Phase 27 (`car_t_dist` NA-ing in the CARTest overflow guard) — non-blocking, tracked in STATE; may be folded in opportunistically but is not an APIS requirement.

</deferred>
