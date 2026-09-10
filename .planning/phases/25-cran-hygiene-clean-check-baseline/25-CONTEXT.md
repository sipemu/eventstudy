# Phase 25: CRAN Hygiene & Clean Check Baseline - Context

**Gathered:** 2026-09-10
**Status:** Ready for planning
**Mode:** Auto-generated (infrastructure phase — discuss skipped)

<domain>
## Phase Boundary

`R CMD check --as-cran` must run clean of the known hygiene findings (non-ASCII
WARNING, undefined-globals NOTE, non-standard-file NOTE) so a trustworthy check
baseline exists for every downstream correctness and submission phase. Scope is
limited to the four hygiene requirements HYG-01..04:

- **HYG-01:** Non-ASCII sweep of `R/*.R` — every non-ASCII byte becomes a
  `\uXXXX` escape (or is removed); refresh the CI non-ASCII baseline guard to match.
- **HYG-02:** Undefined-globals NOTE — namespace-qualify `median`/`tail` in
  `R/es_diagnostics.R` (or add to `importFrom`/`globalVariables`).
- **HYG-03:** Non-standard-file NOTE — remove stale `EventStudy_0.62.0.tar.gz`
  and add a `tar.gz` ignore rule to prevent recurrence.
- **HYG-04:** Optional-package call sites (explicitly `gridExtra` in
  `R/plotting.R`) — `requireNamespace()`-guarded and declared in Suggests.

Out of scope: any correctness/formula work (Phase 26+), API changes (Phase 28),
CI restructuring beyond the baseline guard refresh (Phase 29).

</domain>

<decisions>
## Implementation Decisions

### Claude's Discretion
All implementation choices are at Claude's discretion — pure infrastructure
phase. Use the ROADMAP success criteria, existing codebase conventions, and CRAN
policy as the guide. Non-negotiables carried from prior milestones:

- The full ~2359-test suite stays green and behavior on valid inputs is
  unchanged (correctness/robustness only, no redesign).
- New tooling stays Suggests-only / `requireNamespace()`-guarded — no new hard Imports.
- Prefer `\uXXXX` escapes over transliteration where a non-ASCII glyph is
  semantically meaningful; the CI non-ASCII guard is baseline-aware (fails only
  on NEW non-ASCII) and must be refreshed after the sweep.

</decisions>

<code_context>
## Existing Code Insights

Codebase context will be gathered during plan-phase research. Known touch points
from the roadmap and carried tech debt:

- `R/es_diagnostics.R` — `median`/`tail` unqualified (HYG-02).
- `R/plotting.R` (~L351) — `gridExtra::grid.arrange` used unguarded + stale
  `@return` "patchwork-style" doc (HYG-04, carried from v0.65.0 tech debt).
- Repo root — stale `EventStudy_0.62.0.tar.gz` (HYG-03).
- CI non-ASCII baseline guard — refreshed in commit 16ec41c to current line
  numbers; must be refreshed again after the sweep (HYG-01).

</code_context>

<specifics>
## Specific Ideas

No specific requirements — infrastructure phase. Refer to ROADMAP phase
description and success criteria.

</specifics>

<deferred>
## Deferred Ideas

None — infrastructure phase.

</deferred>
