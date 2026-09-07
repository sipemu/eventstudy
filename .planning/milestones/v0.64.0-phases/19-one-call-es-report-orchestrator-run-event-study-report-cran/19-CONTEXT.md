# Phase 19: One-Call `es_report()` Orchestrator, `run_event_study(report=)` & CRAN-Clean Release Gate - Context

**Gathered:** 2026-09-07
**Status:** Ready for planning

<domain>
## Phase Boundary

This phase delivers the **headline one-call value** and the **milestone release
gate** on top of the Phase 17 seams (grounding-prose guard, offline
`report_writing` fallback, `narrative=` renderer seam) and the Phase 18 renderer
(multi-format template, section-by-section grounded narrative assembler). It
produces:

1. A public **`es_report()` orchestrator** — takes a fitted `EventStudyTask`
   through diagnostics (`es_diagnostics()`) → grounded narrative
   (`assemble_report_narrative()` / `es_advise()`) → multi-format render
   (`generate_report()`) in a single call, returning the output file path(s)
   and **never mutating the caller's task** (deep-cloned at entry).
2. An **additive `run_event_study(..., report = TRUE)` convenience** — defaults
   `FALSE`; the omitted/`FALSE` path is byte-identical to the prior release.
3. The **final CRAN release gate** — `R CMD check --as-cran` with no new
   NOTEs/WARNINGs vs baseline, full existing suite green, DESCRIPTION bumped to
   `0.64.0`, and a `NEWS.md` `v0.64.0` entry recording the automated reporting
   feature.

Out of scope: the renderer/template/narrative internals (Phase 18); grounding +
offline fallback + CRAN hygiene discipline (Phase 17, inherited here). Deferred:
RPTX-01/02/03, RPTC-01, and a full new vignette section for `es_report()`.

</domain>

<decisions>
## Implementation Decisions

### `es_report()` API & Return Contract
- **Thin wrapper** over `generate_report()`, mirroring its user-facing params
  (`task`, `output_file`, `format`, `sections`, `provider`, `title`, `author`,
  `confidence_level`, `interactive`) plus a `...` passthrough. `es_report()`
  owns the diagnostics → advise → render composition; `generate_report()` stays
  the pure renderer.
- **Return value:** a character vector of the output file path(s) — one entry
  per rendered format — returned **visibly** (so the path prints at the REPL).
- **Non-mutation (REPORT-04):** `task$clone(deep = TRUE)` at function entry,
  before any diagnostics/advise/render work touches the object, so the caller's
  `EventStudyTask` is provably unchanged afterward.
- **Default format:** `"html"` (matches `generate_report()`); the user opts into
  `pdf`/`word`/`md` explicitly.

### `run_event_study(report=)` Convenience
- **Signature:** add `report = FALSE` and `report_args = list()` (a named list
  passed through to `es_report()`) to
  `run_event_study(task, parameter_set = ParameterSet$new())`.
- **Return when `report = TRUE`:** still return the **task** (return type stays
  stable), with the rendered path(s) attached as `attr(task, "report_path")` and
  one `message()` reporting where the report was written.
- **`report = FALSE` / omitted path is byte-identical** to the prior release —
  no diagnostics/advise/render is invoked at all on that path (guarded early).
- **Output location:** default `event_study_report.html` in the working
  directory, overridable via `report_args` (e.g. `output_file`, `format`).

### CRAN Release Gate & Versioning
- **DESCRIPTION version:** `0.62.0` → **`0.64.0`** (0.63.0 was a docs-only
  milestone with no DESCRIPTION bump; the version jumps straight to the milestone
  number).
- **NEWS.md:** new `# EventStudy 0.64.0` section documenting `es_report()`,
  `run_event_study(report = TRUE)`, and the multi-format grounded reporting
  feature.
- **`tinytex`** stays in **Suggests** (already present), `requireNamespace()`-
  guarded — no move to Imports, no new transitive hard deps.
- **Gate scope:** run `R CMD check --as-cran` locally; the release gate requires
  **no new NOTEs/WARNINGs vs the current baseline** and the **full existing test
  suite green**. Render/network/toolchain tests remain `skip_on_cran()` /
  `skip_if_not_installed()` so the check triggers no network or LaTeX toolchain.

### Test & Docs Coverage
- **`es_report()` tests:** deep-clone non-mutation assertion (caller object
  identical before/after), return-path assertion, offline path renders with no
  provider, single- vs multi-format. All render-touching tests guarded with
  `skip_on_cran()` + `skip_if_not_installed()`.
- **`run_event_study(report=)` tests:** `FALSE` path returns an object identical
  to the prior (byte-identical contract); `TRUE` path writes a file and sets the
  `report_path` attribute.
- **Examples/docs:** `@export` with roxygen; `\dontrun{}` on render-touching
  examples.
- **README:** a short bullet surfacing the one-call `es_report()`; **no** new
  vignette section (deferred).

### Claude's Discretion
- Exact wording of the `NEWS.md` entry, README bullet, and roxygen prose.
- Internal structure of `es_report()` (helper decomposition) and how
  `report_args` is validated/forwarded.
- Precise mechanism for the `run_event_study()` byte-identical guard and how the
  `report_path` attribute/message is surfaced.

</decisions>

<code_context>
## Existing Code Insights

### Reusable Assets
- `R/report.R` — `generate_report(task, output_file, format, title, author,
  sections, cross_sectional, confidence_level, interactive, advice, narrative,
  provider, ...)`; the pure multi-format renderer `es_report()` composes.
  Already validates format vector, locates the bundled template, and enforces
  the single-narrative-assembly-before-loop budget.
- `R/report_narrative.R` — `assemble_report_narrative(diagnostics, provider,
  sections_to_narrate)`; the section-by-section grounded assembler
  `generate_report()` already calls when `narrative = NULL`. `es_report()`
  either lets `generate_report()` assemble it, or assembles once and passes it
  through the `narrative=` seam.
- `R/es_diagnostics.R` — `es_diagnostics(task, max_events = 20L)`; the
  deterministic diagnostics harvester.
- `R/advise.R` — `es_advise(diagnostics, task_type, provider, model, ...)` and
  the runtime grounding guard.
- `R/execute.R:13` — `run_event_study(task, parameter_set = ParameterSet$new())`;
  the entry point to extend with the additive `report=`/`report_args=` params.
- `DESCRIPTION` — `Version: 0.62.0` (line 4); `tinytex` already in Suggests
  (line 67).
- `NEWS.md` — top entry `# EventStudy 0.62.0`; add a `0.64.0` section above it.

### Established Patterns
- Optional deps in Suggests, `requireNamespace(..., quietly = TRUE)`-guarded.
- Drop-and-keep / single-warning contract; `stop()` only for genuinely fatal
  input (e.g. wrong task class, no valid format).
- `\dontrun{}` on render-touching examples; `skip_on_cran()` +
  `skip_if_not_installed()` on all render/PDF/Word/toolchain tests.
- Non-ASCII escaped for CRAN cleanliness.
- R6 `$clone(deep = TRUE)` is the established non-mutation mechanism.

### Integration Points
- `es_report()` calls `es_diagnostics()` → `assemble_report_narrative()` (or
  lets `generate_report()` assemble) → `generate_report()`, and returns the
  path(s) `generate_report()` produced.
- `run_event_study(report = TRUE)` calls `es_report()` on the fitted task via
  `report_args`, attaches the returned path(s), returns the task.
- `@export es_report` must be added to NAMESPACE via roxygen; new tests under
  `tests/testthat/`.

</code_context>

<specifics>
## Specific Ideas

- The **deep-clone non-mutation invariant (REPORT-04)** is a concrete testable
  assertion: capture the caller task's state before `es_report()` and assert it
  is identical afterward.
- The **byte-identical `report=FALSE` invariant (REPORT-02)** is a concrete
  testable assertion: `run_event_study(task, ps)` and
  `run_event_study(task, ps, report = FALSE)` produce identical objects, and the
  `FALSE` path invokes no report machinery.
- The **release gate** is itself the phase verification: `R CMD check --as-cran`
  clean vs baseline + full suite green + version/NEWS bumped.

</specifics>

<deferred>
## Deferred Ideas

- A dedicated `es_report()` vignette section (README bullet only for now).
- RPTX-01/02/03 (panel/intraday/synthetic report support, bootstrap-CI
  reporting, officedown rich Word) and RPTC-01 (custom templates) remain
  deferred per the v0.64.0 roadmap.
</deferred>
