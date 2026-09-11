# Phase 26: Formula Audit & Golden-Value Validation - Context

**Gathered:** 2026-09-11
**Status:** Ready for planning

<domain>
## Phase Boundary

Every return model (13+) and test statistic (8+) is verified correct against its
published academic formula, its convention choices are documented in a durable
reference, and the key statistics are pinned to reference values — so a wrong
number can never silently pass, and a correct number can never be falsely failed
by a convention mismatch. Requirements: CORR-01, CORR-02.

In scope:
- Formula audit of each return model and test statistic against its published source.
- A durable, shipped reference documenting each model/statistic's convention
  choices (return type, forecast-error correction, degrees of freedom, p-value
  sidedness, Patell denominator, etc.).
- Golden-value regression tests pinning key statistics to reference constants.
- Fixing any genuine formula error found (fix precedes the golden pin) and locking
  it with a regression test.

Out of scope: cross-cutting property/invariant tests and numerical-stability
hardening (Phase 27), API/signature reconciliation (Phase 28), CI restructuring
(Phase 29), CRAN resubmission (Phase 30). Behavior on valid inputs must not change
— only genuine errors are fixed; defensible convention differences are documented,
not "corrected."

</domain>

<decisions>
## Implementation Decisions

### Conventions Reference (durable documentation)
- The convention-choices reference is a **new CRAN-shipped vignette**:
  `vignettes/statistical-conventions.Rmd`. Chosen for discoverability (pkgdown
  site + `browseVignettes`), versioning, and fit with the package's ~19-vignette
  convention.
- It documents, per return model and per test statistic, the audited convention
  choices: return type, forecast-error correction, degrees of freedom, p-value
  sidedness, Patell denominator, and any other choice that affects the number.
- Each documented convention cites its published source.

### Golden-Value Derivation Source
- **Published-first, estudy2 fallback.** Use published-table numbers where they
  exist (e.g. Patell 1976, Boehmer/BMP 1991, Kolari–Pynnönen 2010, Fama–French);
  derive the remaining constants from `estudy2` (and/or `eventstudies`).
- `estudy2` / `eventstudies` are **source-level derivation tools only** — they are
  NOT added to DESCRIPTION (locked by ROADMAP). Any derivation script that uses
  them stays non-shipped (outside the built package; `.Rbuildignore`'d if kept in
  the repo).
- Golden constants are pinned as literals in the test files with a provenance
  comment naming the exact source (published table + page, or the estudy2
  derivation).

### Tolerance Policy
- Match existing codebase norms: tight **absolute** tolerance for algebraic
  identities (`1e-10`, `1e-8` as the general default), and **relative** tolerance
  (`1e-6`) for cross-implementation comparisons against `estudy2`-derived numbers.
- Each golden-value test annotates the exact conventions it assumes and its
  tolerance rationale inline.
- Rationale: the suite is already `1e-8`/`1e-10`-dominant; `1e-12` risks
  cross-platform/BLAS flakiness on cross-impl comparisons, and `1e-4` is a weaker
  pin than the audited numbers warrant.

### Discrepancy Classification (fix vs. document)
- **Fix genuine errors; document defensible conventions.** A real formula error is
  fixed and locked with a regression test, and the fix precedes the golden value
  being pinned (criterion 2).
- A difference that is a defensible, literature-backed convention choice is
  recorded in the conventions vignette rather than "fixed" — preserving valid-input
  behavior (criterion 5).

### Claude's Discretion
- Exact audit ordering, per-model vignette section structure, choice of which
  statistics are "key" enough to warrant a golden pin (guided by the 13+ models /
  8+ statistics coverage requirement), and the layout of the non-shipped derivation
  script are at Claude's discretion, guided by the success criteria and existing
  conventions.

</decisions>

<code_context>
## Existing Code Insights

### Reusable Assets
- Models in `R/models.R`, `R/models_time_varying.R`; test statistics in
  `R/single_event_test_statistics.R`, `R/multi_event_test_statistics.R`.
- Existing test suite is `1e-8` (61×) / `1e-10` (35×)-dominant, with `1e-6` (7×),
  `1e-12` (6×), `1e-4` (3×) used sparingly — the golden tolerance policy above
  matches this.
- No dedicated golden-value / reference-value test file exists yet — these are
  net-new (add under `tests/testthat/`, following `test_*.R` naming).
- `.planning/codebase/CONVENTIONS.md` and `TESTING.md` capture existing style.
- Phase 25 established a clean `R CMD check --as-cran` baseline (0 ERROR/0 WARNING,
  1 expected new-submission NOTE) to diff against.

### Established Patterns
- testthat 3e; helper mock data in `tests/testthat/helper-*.R`.
- Vignettes are `.Rmd` under `vignettes/` (knitr/rmarkdown), CRAN-shipped.

### Integration Points
- New vignette registers via its own YAML header (no DESCRIPTION VignetteBuilder
  change needed — knitr already declared).
- Golden tests slot into the existing `test_check("EventStudy")` entrypoint.

</code_context>

<specifics>
## Specific Ideas

- Convention checklist to document per statistic (from ROADMAP criterion 1): return
  type, forecast-error correction, degrees of freedom, p-value sidedness, Patell
  denominator.
- The non-shipped derivation script must be reproducible (records exact estudy2
  version + inputs) so pinned constants can be regenerated and audited later.

</specifics>

<deferred>
## Deferred Ideas

- Cross-cutting invariant/property tests across the full model×statistic matrix →
  Phase 27.
- Numerical-stability (overflow/conditioning) hardening → Phase 27.

</deferred>
