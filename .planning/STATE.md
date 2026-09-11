---
gsd_state_version: "1.0"
milestone: v0.66.0
milestone_name: Stabilization & CRAN Resubmission
current_phase: 28
current_phase_name: API Stabilization & Signature Lock
status: executing
stopped_at: context exhaustion at 75% (2026-09-11)
last_updated: "2026-09-11T21:40:17.419Z"
last_activity: 2026-09-11
last_activity_desc: Phase 27 complete, transitioned to Phase 28
state_head: f4801fc59a5a995bd4bc2b92c7891eef126a4492
progress:
  total_phases: 6
  completed_phases: 3
  total_plans: 6
  completed_plans: 3
  percent: 50
---

# Project State

## Project Reference

See: .planning/PROJECT.md (updated 2026-09-10)

**Core value:** Trustworthy numbers, trustworthy interpretation — the pipeline is never silently wrong. This milestone proves correctness numerically, locks the public API against accidental breakage, and gets the package back onto CRAN (archived 2024-04-20).
**Current focus:** v0.66.0 roadmap created (Phases 25–30) — ready to plan Phase 25

## Current Position

Phase: 28 (API Stabilization & Signature Lock) — READY TO EXECUTE
Plan: Not started
Status: Ready to execute
Last activity: 2026-09-11 — Phase 27 complete, transitioned to Phase 28

Progress: [█████░░░░░] 50%

## Milestone Roadmap (v0.66.0 — Phases 25–30)

Strict sequential build-order chain (no parallelization): 25 → 26 → 27 → 28 → 29 → 30.

- **Phase 25: CRAN Hygiene & Clean Check Baseline** — HYG-01..04. Non-ASCII `\uXXXX` sweep of `R/*.R` + refreshed CI baseline guard; namespace-qualify `median`/`tail` in `R/es_diagnostics.R`; remove stale `EventStudy_0.62.0.tar.gz` + add `tar.gz` ignore rule; audit optional-package call sites (`gridExtra` in `R/plotting.R`) for `requireNamespace()` + Suggests. Establishes the clean `--as-cran` baseline every downstream phase depends on.
- **Phase 26: Formula Audit & Golden-Value Validation** — CORR-01, CORR-02. Audit all 13+ models / 8+ statistics against published formulas; document convention choices (return type, FEC, df, p-value sidedness, Patell denominator); fix discrepancies with regression tests; THEN pin golden values (published tables / `estudy2`-derived, source-level only) with annotated conventions + explicit tolerances. Bug-fix precedes golden-value pin.
- **Phase 27: Property & Numerical-Stability Tests** — CORR-03, CORR-04. Cross-cutting invariant/property tests (`CAR == cumsum(AR)`, cross-method consistency, boundary/degenerate windows) across the model/statistic matrix; numerical-stability guards on matrix ops / GARCH / bootstrap / long-window cumulation with documented tolerances (no CI flake). `patrick`/`hedgehog` Suggests-only if used.
- **Phase 28: API Stabilization & Signature Lock** — APIS-01..04. Signature-consistency audit (APIS-01) → return-shape contracts in new `R/shape_contracts.R`, opt-in/default-off (APIS-02) → deprecation policy + `lifecycle`-Suggests shim (APIS-04) → structural API snapshot via `getNamespaceExports()`/`formals()`/S3 methods, install-gated (APIS-03). Audit + contracts BEFORE snapshot so the snapshot locks the final surface.
- **Phase 29: Install-Tested CI** — CI-01, CI-02. CI gates on `rcmdcheck` against the *installed* package (not `load_all`), one job with `_R_CHECK_FORCE_SUGGESTS_` set; audit `inst/rmarkdown/` templates + examples/vignettes for bare internal calls / default network access. Catches the `.report_table()` load_all-vs-installed divergence class.
- **Phase 30: CRAN Resubmission** — CRAN-01..05. Clean local `--as-cran`; multi-platform (win-devel/release + rhub v2/macOS) with captured results; rewrite `cran-comments.md` for v0.66.0 with archival acknowledgment + reason + fixes; CRAN-policy compliance (no network, tempdir-only, no gratuitous `\dontrun{}`, runtime); submit + confirm. Final gate depending on all prior phases.

Coverage: 19/19 v0.66.0 requirements mapped, 0 unmapped.

## Accumulated Context

### Decisions

Decisions are logged in PROJECT.md Key Decisions table.
Recent decisions affecting current work:

- Build order is a hard sequential chain: hygiene first (corrupt check runs block everything), audit-before-golden (fix a bug before pinning its value), API audit/contracts before snapshot (don't lock bad names), install-tested CI, CRAN last.
- New tooling (`lifecycle`, `waldo`, `patrick`, `hedgehog`) is Suggests-only / `requireNamespace()`-guarded — no new hard Imports.
- `estudy2`/`eventstudies` used only as source-level golden-value derivation tools; never added to DESCRIPTION (both are themselves archived on CRAN).
- Deprecations are backward-compatible (warn, never silently break); API lock uses structural assertions, not byte-level `print()` snapshots.
- [Phase 27]: Phase 27: numerical-stability guards are additive and route through the degenerate-input contract (NA+one warning lenient / stop strict); valid-input math untouched (SC5)
- [Phase 27]: Phase 27: CAR overflow guard fires on is.infinite|is.nan ONLY, never plain NA, so it never emits a second contract warning on a legitimately-degenerate all-NA CAR (CONTRACT-04)
- [Phase 27]: Phase 27: no new dependency — hand-rolled registry-driven parameterized tests in base testthat 3e (patrick/hedgehog stay absent, grep-asserted 0)

### v0.64.0/v0.65.0 invariants that MUST NOT regress (carried into every phase)

- `.validate_grounding()` (advise.R) — drop-and-keep, single warning, never-stop; extended to report prose in Phase 19.1.
- `.handle_degenerate()` (contract.R) — exactly-one-warning per degenerate event, NA propagation, strict/lenient routing.
- `knitr::is_html_output()` switch in `skeleton.Rmd` — static/interactive selection must not move.
- `JOINT_HYPOTHESIS_CAVEAT` wording (report_narrative.R) — fixed correctness constant.
- `.sanitise_prose()` ordering (ampersand-first) — hardened.
- Behavior on valid inputs unchanged; ~2359-test suite stays green throughout.

### Pending Todos

Carried into this milestone from v0.65.0 (now formally in scope):

- **Phase 25 (from v0.65.0 tech debt):** `gridExtra::grid.arrange` used unguarded in `R/plotting.R` (~L351) + stale `@return` "patchwork-style" doc — verify gridExtra guarded/declared (HYG-04).
- **Phase 25:** Non-ASCII sweep + CI baseline refresh; the CI non-ASCII guard is baseline-aware (fails only on NEW non-ASCII) — refresh baseline after the sweep (HYG-01).
- **Phase 30 (open question):** The exact 2024-04-20 CRAN archival reason is not recorded in any planning artifact — must be retrieved (CRAN archive / maintainer records / check history) before `cran-comments.md` (CRAN-03) can be written. Within-phase dependency, not a blocker.
- **Phase 30:** Report-render tests emit transient `file*.log` into `skeleton/` (not gitignored) — carried v0.65.0 hygiene item, fold into CRAN-policy compliance (CRAN-04) if it affects tarball cleanliness.
- **Phase 27 code-review follow-up (non-blocking, 0 critical):** see `.planning/phases/27-property-numerical-stability-tests/27-REVIEW.md`. Worth applying: WR-01 — in `R/single_event_test_statistics.R` also NA the `car_t_dist` column inside the `if (any(car_overflow))` overflow block (currently only `corrected_car`/`car_t` are cleared, so `car_t_dist` retains a Student-t centred on Inf; no current consumer, but inconsistent with the guard's promise). Others (WR-02 test-coverage overstatement, IN-01..03) are informational. Apply WR-01 opportunistically, e.g. at the start of the next autonomous window before Phase 28 planning.

### Blockers/Concerns

- None blocking roadmap. Phase 30 carries the archival-reason retrieval as a within-phase dependency to resolve during planning.

## Deferred Items

| Category | Item | Status | Deferred At | Milestone |
|----------|------|--------|-------------|-----------|
| verification_gaps | 22/22-VERIFICATION.md | human_needed (felt-visual eyeball on full-TeX-Live box) | 2026-09-09 | v0.65.0 |
| deferred_items | 24/deferred-items.md: report-render tests emit transient file*.log into skeleton/ (not gitignored) | acknowledged | 2026-09-09 | v0.65.0 |
| verification_gaps | 12/12-VERIFICATION.md (archived v0.62.0) | human_needed | 2026-09-07 | v0.64.0 |
| todos | phase12-code-review-carryforward.md | acknowledged (presence-only) | 2026-09-07 | v0.64.0 |
| Independence | INDEP-01..03: native reimplementation of did/DIDmultiplegt/rugarch | Deferred | v0.50.0 init | v2 |
| Scale | SCALE-01..03: streaming/data.table/sparse FE | Deferred | v0.50.0 init | v2 |
| Advisor Pro | PRO-01..02: RAG corpus advisor + managed hosting | Deferred | v0.60.0 roadmap | future (waitlist-gated) |

## Session Continuity

Last session: 2026-09-11T21:33:19.909Z
Stopped at: context exhaustion at 75% (2026-09-11)
Resume file: None

## Operator Next Steps

- Review the roadmap, then plan the first phase with `/gsd-plan-phase 25`.

## Performance Metrics

| Plan | Duration | Tasks | Files |
|------|----------|-------|-------|
| Phase 25 P01 | ~2h | 6 tasks | 41 files |
| Phase 27 P01 | 40min | 6 tasks | 6 files |
