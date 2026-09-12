# Phase 30: CRAN Resubmission - Context

**Gathered:** 2026-09-12
**Status:** Ready for planning
**Mode:** Smart discuss (autonomous) — operator decisions captured on submission boundary, platform checks, and archival reason

<domain>
## Phase Boundary

Get EventStudy back onto CRAN. This phase delivers a submission-ready package:
a clean local `R CMD check --as-cran` (0 ERRORs, 0 WARNINGs, only explainable
NOTEs) with the full suite green; the DESCRIPTION version bumped to 0.66.0; a
rewritten `cran-comments.md` that honestly acknowledges the 2024-04-20 archival
and its reason, lists the fixes, and reports platform-check results; verified
CRAN-policy compliance across examples/tests/vignettes (no default network, no
writes outside tempdir, no gratuitous `\dontrun{}`, example runtimes within
policy); and win-builder multi-platform checks initiated.

**Explicit boundary:** the phase stops at *submission-ready*. It does NOT run
`devtools::submit_cran()` and does NOT complete the maintainer email
confirmation — those are operator-only (CRAN-05 is a human checkpoint). The
executor hands the operator the exact submit steps.

Covers CRAN-01..05, with CRAN-05 (submit + confirm) delivered as a documented
operator handoff rather than an autonomous action.

</domain>

<decisions>
## Implementation Decisions

### Submission Boundary (CRAN-05)
- **Prepare-to-ready; operator submits.** Autonomous execution performs
  everything up to submission — clean `--as-cran`, version bump to 0.66.0,
  `cran-comments.md` rewrite, policy compliance, tarball build — then STOPS.
- The executor produces an explicit operator handoff: the exact
  `devtools::submit_cran()` (and/or webform) steps for the maintainer (Simon
  Mueller, sm@data-zoo.de) to run, plus a reminder to complete the CRAN email
  confirmation.
- Rationale: submission is outward-facing and hard to reverse; only the
  maintainer can receive/confirm the CRAN acknowledgment email.

### Multi-Platform Checks (CRAN-02)
- **Claude runs win-builder:** invoke `devtools::check_win_devel()` and
  `devtools::check_win_release()` (submits to win-builder; results are emailed
  to the maintainer). Capture the submission confirmation / any immediately
  available output.
- **Lean on Phase 29 CI:** the install-tested multi-OS GitHub Actions CI
  (Phase 29, `rcmdcheck` against the installed package with a forced-Suggests
  leg) stands as the Linux/multi-OS platform evidence.
- **rhub documented, not run:** rhub is not installed locally and rhub v2 needs
  GitHub Actions / tokens. The executor documents the exact `rhub::rhub_check()`
  (v2) commands for the operator to run optionally; it does not install or
  trigger rhub autonomously.

### Archival Reason (CRAN-03 gating dependency)
- **Retrieved from the CRAN archive.** EventStudy was archived **2024-04-20**,
  last version **0.39.2**. Stated reason: **"issues were not corrected despite
  reminders."**
- Underlying 0.39.2 check findings (all NOTE-level, on the *old* EventStudyTools
  web-API-client design): undocumented Rd arguments (`doHttrRequest.Rd`,
  `errorMessage.Rd`, `isError.Rd`); installed size 7.0 MB (6.0 MB in `doc/`);
  unused imports (`RColorBrewer`, `curl`, `scales`, `stringr`); undeclared R6 in
  Rd cross-references.
- **Cover-letter framing:** v0.66.0 is an unrelated ground-up rewrite (composable
  R6 pipeline), not the archived 0.39.2 web-API client. The letter acknowledges
  the archival + reason, and states the resubmission is a substantially different
  package that resolves the class of issues (clean docs, guarded optional deps,
  policy-compliant).
- The executor should confirm/tighten the exact archival wording during
  execution (re-fetch the CRAN archive index / removal notice); ask the operator
  only if the web record is inconclusive.

### Claude's Discretion
- DESCRIPTION version bump to 0.66.0 (and NEWS.md 0.66.0 stanza if not present).
- Exact structure/wording of the rewritten `cran-comments.md`.
- Details of the CRAN-policy compliance audit (network guards, tempdir-only
  writes, `\dontrun{}` audit, example runtime trimming) and any minor fixes
  needed to reach a clean `--as-cran`.

</decisions>

<code_context>
## Existing Code Insights

### Current CRAN-facing State
- **DESCRIPTION Version: 0.65.0** — must bump to 0.66.0 this phase.
- **Maintainer:** Simon Mueller <sm@data-zoo.de> (the CRAN maintainer = the operator).
- **`cran-comments.md` is stale:** it documents the old v0.5x "robustness-hardening"
  milestone (Phase 4 baseline, commit 63d67a1), not v0.66.0. Full rewrite required.
- **Local toolchain:** R 4.6.1, `devtools` + `rcmdcheck` installed; **`rhub` NOT installed**.

### Established Patterns (carried invariants — must not regress)
- Optional packages stay Suggests-only, guarded by `requireNamespace()` (the
  existing Suggests-not-available NOTEs are expected/explainable).
- Behavior on valid inputs unchanged; ~2359-test suite stays green.
- Phase 29 added `report_table` as an exported function; API snapshot accepts it.

### Integration Points
- `DESCRIPTION` (version), `NEWS.md` (0.66.0 stanza), `cran-comments.md` (rewrite),
  `inst/rmarkdown/` templates + vignettes (policy compliance), the Phase 29 CI
  workflow (platform evidence).

</code_context>

<specifics>
## Specific Ideas

- The archived package (0.39.2) and the current package (0.66.0) share only the
  name — the cover letter must make the "this is a rewrite" framing explicit so
  the CRAN reviewer isn't confused by the version jump and different function set.
- Report-render tests emit transient `file*.log` into `skeleton/` (carried
  v0.65.0 hygiene item) — fold into the CRAN-policy / tarball-cleanliness check
  (CRAN-04) if it affects the built tarball.

</specifics>

<deferred>
## Deferred Ideas

- Actual `submit_cran()` invocation and CRAN email confirmation (CRAN-05) — an
  operator checkpoint by explicit decision, not executed autonomously.
- rhub v2 execution — documented for the operator; not triggered autonomously.

</deferred>
