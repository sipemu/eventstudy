# Phase 29: Install-Tested CI - Context

**Gathered:** 2026-09-12
**Status:** Ready for planning
**Mode:** Autonomous smart-discuss (infrastructure phase — discuss skipped)

<domain>
## Phase Boundary

Wire CI so it exercises the *installed* package and gates on `R CMD check`, so the
"green under `devtools::load_all()`, broken when installed" class of bug (the
`.report_table()` / `skeleton.Rmd` divergence) fails CI instead of shipping.

Delivers:
- CI gates on `R CMD check` / `rcmdcheck` against the **installed** package (built tarball,
  not `load_all()`), so install-only divergence bugs fail CI.
- At least one CI job runs with `_R_CHECK_FORCE_SUGGESTS_` set to exercise Suggests-present
  behavior (and, where useful, a Suggests-absent job to prove graceful `requireNamespace()` degradation).
- `inst/rmarkdown/` templates and examples/vignettes audited for bare internal (`:::`-style /
  unexported) calls and default network access — closing the load_all/installed divergence surface.
- The install-gated CI passes on the current package with the full suite green (including the
  Phase-28 install-gated `test-api-snapshot.R`, which only runs against an installed package).

Out of scope: CRAN submission (Phase 30); new package features or behavior changes on valid inputs;
pkgdown site deploy (separate `pkgdown.yaml`, tracked elsewhere).
</domain>

<decisions>
## Implementation Decisions

### Claude's Discretion
All implementation choices are at Claude's discretion — this is a pure infrastructure phase.
Guiding defaults grounded in the existing repo:
- Extend the existing `.github/workflows/R-CMD-check.yaml` (already uses
  `r-lib/actions/check-r-package@v2`, which builds + installs the tarball and runs `R CMD check`
  on the installed package) rather than introducing a new CI provider.
- Keep the check gate at CRAN-equivalent strictness matching the Phase-25 baseline: the one
  pre-existing "CRAN-archived" NOTE is the accepted baseline; introduce no new NOTEs/WARNINGs/ERRORs.
- Add a job (or matrix leg) that sets `_R_CHECK_FORCE_SUGGESTS_=true` so Suggests-present code
  paths (lifecycle, openxlsx, rugarch, did, etc.) are exercised; the default CRAN legs keep
  `_R_CHECK_FORCE_SUGGESTS_=false` so Suggests-absent graceful degradation stays covered.
- The rmarkdown/vignette audit is a code-hardening task: find and fix bare internal calls and
  default network access; where a fix is out of scope, document it as a tracked follow-up.
</decisions>

<code_context>
## Existing Code Insights

### Reusable Assets
- `.github/workflows/R-CMD-check.yaml` — existing GitHub Actions workflow using
  `r-lib/actions/setup-r`, `setup-r-dependencies`, and `check-r-package@v2` (installed-tarball check).
- `.github/workflows/pkgdown.yaml` — existing docs-site workflow (out of scope here).
- `inst/rmarkdown/templates/` and `inst/rmarkdown/report.css` — the report/skeleton templates that
  are the known load_all/installed divergence surface to audit.
- `tests/testthat/test-api-snapshot.R` (Phase 28) — install-gated via `skip_if_not_installed("EventStudy")`;
  the canonical example of a test that only exercises meaning under an installed package.

### Established Patterns
- Suggests packages guarded with `requireNamespace()`; `_R_CHECK_FORCE_SUGGESTS_=false` is the
  Phase-25 baseline check invocation.
- Phase-25 CRAN baseline: exactly 1 NOTE (CRAN-archived feasibility), 0 WARNINGs, 0 ERRORs.

### Integration Points
- CI runs against the built tarball, so install-gated tests (`skip_if_not_installed`) actually run
  there while they skip locally under `load_all` — CI is where they earn their keep.
</code_context>

<specifics>
## Specific Ideas

- The install-gated `test-api-snapshot.R` from Phase 28 must actually execute (not skip) in at least
  one CI leg — that is the concrete proof the "installed package" gate works.
- Do not regress the Phase-25 CRAN check baseline (1 NOTE only).
</specifics>

<deferred>
## Deferred Ideas

- CRAN resubmission and cran-comments.md — Phase 30.
- pkgdown site deploy / GitHub Pages enablement — operator step tracked separately.
- Phase-28 code-review follow-ups WR-01/02/03 (deprecation double-warning, shape-contract first-row-only
  scan, empty warning context) — recorded in `28-REVIEW.md`; may be folded in opportunistically but not
  a Phase-29 requirement.
</deferred>
