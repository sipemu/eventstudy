# Phase 18: Multi-Format Renderer, Fixed Template & Grounded Narrative Assembly - Context

**Gathered:** 2026-09-07
**Status:** Ready for planning

<domain>
## Phase Boundary

This phase delivers the report **rendering and narrative-assembly layer** on top of
the Phase 17 seams (`narrative=` param, offline `report_writing` fallback,
prose-grounding guard). It produces:

1. A **section-by-section grounded narrative assembler** — each report section is
   an independent `es_advise()` request (LLM contacted once per section, not per
   output format), with references pulled from KB citation records (never
   LLM-generated), significance language from a static calibration function, and a
   fixed joint-hypothesis caveat in every report.
2. **One fixed template** (executive summary · data/methods · results · diagnostics
   · robustness/caveats · references) with section presence arg-toggled, no custom
   templating; data/methods + results content auto-filled from task metadata and
   `es_diagnostics()` keys (never the LLM).
3. **Multi-format rendering** — HTML (default), PDF, Word (.docx), Markdown,
   selectable per call; plots switch to static ggplot2 for non-HTML via
   `knitr::is_html_output()`; prose sanitised per format.
4. **Graceful toolchain degradation** and a **visible offline-vs-AI mode
   distinction**.

Out of scope: the public `es_report()` orchestrator and `run_event_study(report=)`
convenience (Phase 19); the final CRAN release gate (Phase 19).

</domain>

<decisions>
## Implementation Decisions

### Template & Multi-Format Rendering
- **Evolve the existing bundled template** `inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd`
  into the fixed 6-section template — reuse the `system.file()` install path already
  wired in `generate_report()` (`R/report.R`), do not add a second template file.
- **Format selection** via a `format = c("html","pdf","word","md")` vector; the
  renderer loops and renders each requested format to **one output file per format**
  sharing a common basename. Callers may request one or several.
- **Plot switch** happens *inside the template* on `knitr::is_html_output()` —
  interactive plotly only in HTML, static ggplot2 for PDF/Word/Markdown (FORMAT-03).
- **Section toggling** via boolean `sections=` parameters passed to the template
  through `rmarkdown::render(params = ...)`, driving conditional (`eval`/`asis`)
  chunks — no custom templating engine (TMPL-01).

### Section-by-Section Narrative Assembly
- **LLM-authored interpretive prose** is produced only for **executive summary,
  results interpretation, and robustness/caveats**. Data/methods gets a short
  auto-generated lead-in; all tables/numbers (windows, model, AR/CAR/AAR/CAAR,
  significance) are always auto-filled from task metadata + `es_diagnostics()`
  (TMPL-02), never from the LLM.
- **Significance calibration** is a static function of the p-value with four tiers:
  `p < 0.01` -> "strongly significant"; `< 0.05` -> "significant";
  `< 0.10` -> "marginally significant"; else -> "not statistically significant".
  The LLM is told the label, never asked to infer it (NARR-04).
- **References** are pulled from KB citation records, deduplicated by citation key
  and ordered **alphabetically by author** (NARR-03). Never LLM-generated.
- **Per-section failure / guard-drop** falls back to offline rule-based text for
  that section (per Phase 17 decision), so the report is always complete —
  never abort, never drop a whole section to nothing.
- The **joint-hypothesis caveat** is fixed static text present in every report
  (NARR-05).

### Format Degradation, Sanitisation & Mode Distinction
- **Toolchain-skip channel:** `message()` — exactly one informative line per skipped
  optional format (PDF/Word/Markdown), then continue (FORMAT-02). Only total
  inability to render the **HTML baseline** raises `stop()`.
- **Prose sanitiser** is a per-format map: LaTeX-special escaping for PDF, XML-entity
  escaping for Word, smart-quotes/em-dashes normalised to ASCII for all formats
  (FORMAT-04) so PDF/Word render without corruption.
- **AI-vs-offline distinction** is surfaced **both** ways (OFFLINE-02): a
  section-heading label ("AI-grounded interpretation" vs "Automated rule-based
  interpretation") **and** one console `message()` reporting the mode used.
- **Mode granularity:** report-level mode is AI if *any* section used the LLM
  provider, else offline; the per-section heading label reflects the actual source
  where sections differ (some AI, some offline-fallback).

### Claude's Discretion
- Exact wording of static significance labels, heading labels, and the
  joint-hypothesis caveat text (within the semantics above).
- Internal structure of the sanitiser map and the section-assembly helper functions.
- Whether the narrative assembler lives in a new `R/` file or extends `advise.R`.

</decisions>

<code_context>
## Existing Code Insights

### Reusable Assets
- `R/report.R` — `generate_report(task, ..., advice = NULL, narrative = NULL)`;
  already locates the bundled template via `system.file()`, sets `output_format`
  (html/pdf), validates the `narrative=` named-list seam and degrades a bad arg to
  NULL with one warning. This is the renderer to extend to the full multi-format
  loop + fixed template.
- `inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd` — the bundled
  template to evolve into the fixed 6-section layout.
- `R/advise.R` — `es_advise()`, the `Advice` S3, and the runtime grounding guard
  `.validate_grounding()` (single warning-emitting point; drop-and-keep; abs/rel
  tolerance + rounding-aware match extended to prose in Phase 17). Each narrative
  section is an independent `es_advise()` request through this path.
- `R/advise_offline.R` — the rule-based offline engine, extended in Phase 17 to
  synthesize per-section prose using the same section keys as the LLM path
  (exec summary · data/methods · results · robustness/caveats). Source of the
  per-section offline fallback.
- `R/knowledge_base.R` — pure-R assumption->test KB with academic citations; the
  source of grounded references (never LLM-generated).
- `R/es_diagnostics.R` — deterministic diagnostics harvester; its value registry is
  what auto-fills data/methods + results tables and what prose literals are checked
  against.

### Established Patterns
- Drop-and-keep contract; single warning-emitting point per guard concern; never
  `stop()` on a guard failure.
- Optional deps in Suggests, `requireNamespace(..., quietly = TRUE)`-guarded
  (rmarkdown, tinytex, openxlsx). PDF/Word/LaTeX toolchains stay optional.
- `\dontrun{}` on render-touching examples; `skip_on_cran()` +
  `skip_if_not_installed()` on all render/PDF/Word/toolchain tests (Phase 17
  CRAN-hygiene discipline).
- Non-ASCII characters escaped to keep CRAN clean.

### Integration Points
- The narrative the assembler produces is passed into `generate_report()` via the
  Phase 17 `narrative=` seam — the assembler builds a named list keyed by section;
  the renderer stays a pure renderer.
- The section keys must match across LLM path, offline path, and template params.
- `format=` vector plus `sections=` toggles are the contract Phase 19's
  `es_report()` orchestrator will call through.

</code_context>

<specifics>
## Specific Ideas

- Spike (from STATE.md pending todos): **prose sanitisation fixtures** (em-dash,
  smart quotes, Unicode, XML entities) per output format — verify PDF/Word render
  without corruption before locking the sanitiser.
- Spike (from STATE.md pending todos): **multi-format sequential render**
  (HTML+PDF+Word) figure-directory-deletion edge case — confirm figures survive
  when several formats render in one call.
- The single-LLM-call-per-section (independent of format count) property is a
  concrete, testable NARR-01 invariant: assert the provider is contacted N times
  for N sections regardless of how many formats are requested.

</specifics>

<deferred>
## Deferred Ideas

- RPTX-01/02/03, RPTC-01 (panel/intraday/synthetic report support, bootstrap-CI
  reporting, officedown rich Word, user-supplied templates) remain deferred per
  the v0.64.0 roadmap.
- `es_report()` orchestrator and `run_event_study(report=)` are Phase 19, not here.

</deferred>
