# Phase 17: Grounding-Prose Hardening, Offline Report Fallback & CRAN Baseline - Context

**Gathered:** 2026-09-07
**Status:** Ready for planning

<domain>
## Phase Boundary

This phase lands the milestone's core-value gate first and establishes the seams
every downstream reporting phase depends on. It delivers four things and nothing
more:

1. **Prose-grounding hardening (GROUND-01..03)** — extend the runtime grounding
   guard so it catches a fabricated numeric literal sitting in free-text
   narrative prose, not just in structured `evidence[]` arrays.
2. **Offline `report_writing` fallback (OFFLINE-01)** — resolve the current
   `report_writing`-is-LLM-only `stop()` so a full narrative renders with no LLM
   provider configured, via the rule-based offline advice engine.
3. **`generate_report()` `narrative=` seam (REPORT-03)** — add a
   backward-compatible `narrative = NULL` parameter; the NULL path stays
   byte-identical to the v0.63.x baseline.
4. **CRAN hygiene discipline (REPORT-03)** — establish `\dontrun{}` /
   `skip_on_cran()` / `skip_if_not_installed()` / Suggests-only conventions up
   front so no downstream phase leaks an `R CMD check` regression.

Out of scope here: the multi-format renderer, the fixed template, and the
section-by-section narrative assembler (Phase 18); the one-call `es_report()`
orchestrator and final release gate (Phase 19).

</domain>

<decisions>
## Implementation Decisions

### Prose Grounding Scanner (GROUND-01..03)
- Detect claims by extracting **every numeric literal** from each free-text
  narrative field via regex and checking each against the `es_diagnostics()`
  value registry.
- On a fabricated number (a literal absent from diagnostics beyond tolerance):
  **drop the offending section and emit exactly one warning** — mirrors the
  existing drop-and-keep contract; the unverified number is never emitted into
  rendered output.
- Tolerance model reuses the existing `EventStudy.guard_abs_tol` /
  `EventStudy.guard_rel_tol` options, **plus a rounding-aware match**: a literal
  that is a correct rounding of an actual diagnostic value at the literal's
  displayed precision counts as grounded (handles "2.35" for 2.3456).
- **Exempt structural integers** that match metadata already present in
  diagnostics (N observations, window indices/bounds, event counts); scrutinize
  only statistical decimals as claims.

### Offline `report_writing` Fallback (OFFLINE-01)
- Produce offline narrative by **extending the existing rule-based offline engine
  (`advise_offline.R`)** to synthesize per-section prose from KB rules +
  diagnostics.
- **Remove `report_writing` from `LLM_ONLY_TYPES`** and move it into the
  KB-grounded / offline-capable set so `provider = NULL` no longer `stop()`s.
- Offline narrative uses the **same section keys** as the LLM path (exec summary
  · data/methods · results · robustness/caveats) so the renderer is agnostic to
  the narrative's source.
- When a provider IS configured but a section fails or is guard-dropped,
  **fall back to offline text per-section** so the report is always complete.

### `generate_report()` `narrative=` Seam (REPORT-03)
- Parameter shape: `narrative = NULL` default, accepting a **named list / S3
  object keyed by section**; the NULL path is byte-identical to the v0.63.x
  baseline.
- `narrative` and the existing `advice` param are **independent seams**:
  `narrative` = section prose, `advice` = structured recommendations block; both
  independently NULL-able.
- Backward compatibility proven by a **golden-file diff test** locking the
  `narrative = NULL` / `advice = NULL` output byte-identical to the baseline.
- `generate_report()` **only accepts** narrative — it stays a pure renderer. The
  Phase 19 `es_report()` orchestrator produces the narrative and passes it in.

### CRAN Hygiene Discipline (REPORT-03)
- Wrap all `render()`-touching examples in **`\dontrun{}`**.
- Every render / PDF / Word / toolchain test uses **`skip_on_cran()` +
  `skip_if_not_installed()`**.
- PDF/Word/LaTeX dependencies stay **Suggests-only, `requireNamespace()`-guarded**
  (`tinytex` added to Suggests); zero new hard deps (already locked in PROJECT.md
  Key Decisions).
- **Snapshot the current `R CMD check` NOTEs/WARNINGs up front** as the diff
  baseline every downstream phase is measured against.

</decisions>

<code_context>
## Existing Code Insights

### Reusable Assets
- `R/advise.R` — `es_advise()`, the `Advice` S3, and the runtime grounding guard
  `.validate_grounding()` (the SINGLE warning-emitting point for guard drops).
  Guard already implements drop-and-keep, abs/rel tolerance via
  `EventStudy.guard_abs_tol` / `EventStudy.guard_rel_tol`, and Inf-safe numeric
  compare. `LLM_ONLY_TYPES` includes `"report_writing"` — this is the `stop()`
  source to resolve.
- `R/advise_offline.R` — the rule-based offline advice engine (KB rules +
  diagnostics → structured advice) to extend for offline narrative.
- `R/knowledge_base.R` — pure-R assumption→test KB with academic citations;
  source of grounded prose content and references.
- `R/es_diagnostics.R` — the deterministic diagnostics harvester; its value
  registry is what prose literals are checked against.
- `R/report.R` — `generate_report(task, ..., advice = NULL)`; already degrades a
  non-`Advice` `advice` arg to NULL with one warning. Add the `narrative=` seam
  here.

### Established Patterns
- Drop-and-keep contract: drop only the ungrounded item, keep the rest; never
  `stop()` on a guard failure (only on missing provider for LLM-only types).
- Single warning-emitting point per guard concern (`.validate_grounding()`).
- Optional deps in Suggests, `requireNamespace(..., quietly = TRUE)`-guarded
  (jsonlite, httr2 already follow this).
- `—` unicode escaping to keep CRAN non-ASCII clean (prior quick task fixed
  this in advise.R/knowledge_base.R/report.R).

### Integration Points
- Grounding guard extension must scan the four free-text prose fields the
  narrative introduces, using the same diagnostics registry the evidence[] path
  already uses.
- `generate_report()` `narrative=` seam is the contract Phase 18's renderer and
  Phase 19's orchestrator both build against.
- `report_writing` moving out of `LLM_ONLY_TYPES` changes the `es_advise()`
  provider-required branch — verify the ADV-06 stop() logic still holds for the
  remaining LLM-only types (`interpret`, `recommend_model`, `design_discussion`).

</code_context>

<specifics>
## Specific Ideas

- Spike (from STATE.md pending todos): validate the numeric-literal regex +
  tolerance for the prose scanner against real LLM output samples
  (rounding vs off-by-one vs fabricated) before locking the guard.
- The golden-file diff test for `generate_report()` backward compatibility is the
  concrete proof artifact for REPORT-03 success criterion 3.
- Capture the `R CMD check --as-cran` baseline as a committed artifact so
  "no new NOTEs/WARNINGs" is diffable, not from memory.

</specifics>

<deferred>
## Deferred Ideas

- Multi-format renderer, fixed template, section-by-section narrative assembler —
  Phase 18.
- `es_report()` one-call orchestrator, `run_event_study(..., report=TRUE)`, final
  CRAN release gate — Phase 19.
- officedown rich Word (RPTX-03), custom templates (RPTC-01), panel/intraday/
  synthetic report support (RPTX-01), bootstrap-CI reporting (RPTX-02) — deferred
  to future milestones per STATE.md.

</deferred>
