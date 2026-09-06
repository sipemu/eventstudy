# Architecture Research

**Domain:** One-call AI report wrapper (v0.64.0) — R package additive orchestration layer
**Researched:** 2026-09-06
**Confidence:** HIGH (based on direct codebase inspection of all relevant source files)

## Standard Architecture

### System Overview

The v0.64.0 `es_report()` wrapper sits entirely above the existing pipeline. It is a pure composition of already-built pieces. No existing function signatures break.

```
┌──────────────────────────────────────────────────────────────────────┐
│                    NEW: One-Call Entry Layer                          │
│  es_report(task, ...)     run_event_study(..., report=TRUE)           │
│  R/es_report.R            R/execute.R (additive report= param)        │
└──────────────────────────────────┬───────────────────────────────────┘
                                   │ orchestrates
          ┌────────────────────────┼────────────────────────┐
          ▼                        ▼                        ▼
┌──────────────────┐   ┌──────────────────────┐  ┌──────────────────────┐
│ EXISTING         │   │ EXISTING              │  │ EXISTING             │
│ es_diagnostics() │   │ es_advise()           │  │ generate_report()    │
│ R/es_diagnostics │   │ task_type="report_    │  │ R/report.R           │
│ .R               │   │ writing" (LLM) or     │  │ Rendered via         │
│                  │   │ flag_robustness/       │  │ rmarkdown::render()  │
│ Returns          │   │ recommend_stat         │  │                      │
│ es_diagnostics   │   │ (offline KB)          │  │ Returns file path    │
│ S3 object        │   │ Returns Advice S3     │  │ (invisibly)          │
└──────────────────┘   └──────────────────────┘  └──────────────────────┘
          │                        │                        │
          └────────────────────────┴────────────────────────┘
                                   │
          ┌────────────────────────┼───────────────────────────────────┐
          ▼                        ▼                                   ▼
┌──────────────────┐  ┌───────────────────────────────┐  ┌────────────────────┐
│ EXISTING         │  │ NEW: Narrative Assembler       │  │ MODIFIED           │
│ run_event_study()│  │ .assemble_report_advice()      │  │ skeleton.Rmd       │
│ (when task not   │  │ R/es_report.R (internal)       │  │ inst/rmarkdown/    │
│ yet fitted)      │  │                                │  │ templates/event_   │
│                  │  │ Calls es_advise() N times for  │  │ study_report/      │
│                  │  │ multi-section narrative;        │  │ skeleton/          │
│                  │  │ returns named list of Advice   │  │ skeleton.Rmd       │
└──────────────────┘  └───────────────────────────────┘  └────────────────────┘
```

### Component Responsibilities

| Component | Responsibility | File | Status |
|-----------|----------------|------|--------|
| `es_report()` | One-call entry point; accepts fitted or unfitted task, all format/section args; orchestrates the four-step sequence; returns named list of output paths | `R/es_report.R` (NEW) | New |
| `.assemble_report_advice()` | Calls `es_advise()` for each narrative section (executive_summary, data_methods, results, robustness_caveats); returns named list of `Advice` objects keyed by section; handles offline-first fallback per section independently | `R/es_report.R` (internal, `@noRd`) | New |
| `run_event_study()` | Unchanged core behavior; `es_report()` calls it when task is unfitted | `R/execute.R` | Unchanged (optional `report=` convenience added last) |
| `es_diagnostics()` | Unchanged; `es_report()` calls it immediately after fitting | `R/es_diagnostics.R` | Unchanged |
| `es_advise()` | Unchanged interface; called once per narrative section with `task_type="report_writing"` (LLM path) or KB types for offline path | `R/advise.R` | Unchanged |
| `.validate_grounding()` | Unchanged; runs inside every `es_advise()` call automatically | `R/advise.R` | Unchanged |
| `generate_report()` | Signature gains a `narrative=NULL` param (named list of Advice objects by section); backward-compatible — existing callers passing `advice=` are unaffected | `R/report.R` | Modified (additive only) |
| `skeleton.Rmd` | Gains new parameterized narrative chunks per section (executive_summary, data_methods, results, robustness_caveats); existing chunks untouched; `output:` YAML extended for word_document and md_document | `inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd` | Modified (additive only) |

## Recommended Project Structure

```
R/
├── es_report.R                    # NEW: es_report() + .assemble_report_advice()
├── execute.R                      # MODIFIED: optional report= param on run_event_study()
├── report.R                       # MODIFIED: narrative= param on generate_report()
├── advise.R                       # UNCHANGED
├── es_diagnostics.R               # UNCHANGED
└── ... (all other files unchanged)

inst/
└── rmarkdown/
    └── templates/
        └── event_study_report/
            └── skeleton/
                └── skeleton.Rmd   # MODIFIED: narrative params + new chunks
```

### Structure Rationale

- **`R/es_report.R` is a new file** rather than appending to `execute.R` or `report.R` because it owns the orchestration concern (pipeline → diagnostics → advise → render) that spans three existing layers. Keeping it separate preserves single-responsibility and avoids tangling the execute/report files.
- **`generate_report()` gets `narrative=` not a wholesale signature change** because the existing `advice=` single-block path stays intact for users calling `generate_report()` directly. The `narrative=` list-of-Advice is additive and only consumed by the new skeleton chunks.
- **`skeleton.Rmd` is modified, not replaced** because the existing section chunks (summary, data, diagnostics, single_event, multi_event, cross_sectional, appendix) are correct and tested. New narrative chunks are inserted before each corresponding statistical section.

## Architectural Patterns

### Pattern 1: Four-Step Orchestration Sequence

**What:** `es_report()` executes in order: (1) optionally run pipeline, (2) harvest diagnostics, (3) assemble narrative Advice objects, (4) render for each requested format. Each step is independent and can be short-circuited (already-fitted task skips step 1; no provider skips LLM in step 3).

**When to use:** Always. This is the complete call sequence inside `es_report()`.

**Trade-offs:** Sequential, single-threaded (matches the package's existing threading model). Each step can fail independently; steps 3 and 4 degrade gracefully (offline fallback for step 3, clear error on step 4 only if rmarkdown is not installed).

**Call sequence in `es_report()`:**
```r
# Step 1: ensure task is fitted
if (!"model" %in% names(task$data_tbl)) {
  task <- run_event_study(task, parameter_set)
}

# Step 2: harvest diagnostics (zero-dependency, always succeeds)
diagnostics <- es_diagnostics(task)

# Step 3: assemble narrative (offline-first; LLM enriches when provider supplied)
narrative <- .assemble_report_advice(diagnostics, provider = provider,
                                     sections = narrative_sections)

# Step 4: render to each requested format
paths <- lapply(formats, function(fmt) {
  generate_report(task,
                  output_file = .report_output_path(output_dir, output_stem, fmt),
                  format      = fmt,
                  narrative   = narrative,
                  ...)
})
names(paths) <- formats
```

### Pattern 2: Multi-Section Narrative via Repeated es_advise() Calls

**What:** Each narrative section (executive_summary, data_methods, results, robustness_caveats) maps to one `es_advise()` call. The grounding guard runs independently on each returned `Advice` object. A failure on one section produces an empty `Advice` for that section only; the other sections are unaffected.

**When to use:** LLM path (provider supplied). Each call is a separate guarded round-trip.

**Trade-offs:** Four LLM calls per report. This is deliberate: smaller prompts fit more reliably within provider context windows, each section's guard result is isolated, and section-level failures degrade gracefully. The cost is latency (4 sequential provider calls). Given the package's single-threaded R model, parallelism via `future` is out of scope for this milestone.

**Section-to-task_type mapping (fixed constant in `R/es_report.R`):**
```r
NARRATIVE_SECTIONS <- list(
  executive_summary  = "report_writing",   # LLM-only; offline = empty Advice
  data_methods       = "report_writing",   # LLM-only; offline = empty Advice
  results            = "report_writing",   # LLM-only; offline = empty Advice
  robustness_caveats = "flag_robustness"   # KB-backed; offline = rule-based es_advice
)
```

The `robustness_caveats` section uses `flag_robustness` so offline mode produces grounded robustness text via the KB engine. The other three sections are LLM-only and produce empty narrative offline — the statistical tables and plots still render complete.

### Pattern 3: Offline-First Fallback

**What:** When `provider = NULL`, `es_report()` produces a complete report. The `robustness_caveats` narrative section uses the KB engine (offline, deterministic). The executive_summary, data_methods, and results narrative slots are empty `Advice` objects. The report is never blocked or errored by a missing provider.

**Critical implementation note:** `.assemble_report_advice()` must wrap `es_advise()` calls for LLM-only task types in a `tryCatch` when `provider = NULL`, because `es_advise()` calls `stop()` for LLM-only types without a provider (ADV-06 contract). The assembler catches this and returns `.empty_advice()` instead of propagating the stop.

```r
.assemble_report_advice <- function(diagnostics, provider, sections) {
  lapply(sections, function(sec) {
    task_type <- NARRATIVE_SECTIONS[[sec]]
    if (is.null(provider) && task_type %in% LLM_ONLY_TYPES) {
      return(.empty_advice("offline", task_type))
    }
    tryCatch(
      es_advise(diagnostics, task_type = task_type, provider = provider),
      error   = function(e) .empty_advice("error",    task_type),
      warning = function(w) { warning(w); invokeRestart("muffleWarning") }
    )
  })
}
```

Note: `.empty_advice` is an existing internal in `R/advise.R` (marked `@noRd`). `.assemble_report_advice()` lives in `R/es_report.R` which is in the same package namespace, so it can call `.empty_advice()` directly.

### Pattern 4: Multi-Format Rendering via One Parameterized Template

**What:** The single `skeleton.Rmd` is rendered N times (once per requested format) by calling `generate_report()` N times with different `format=` values. The `narrative` list is assembled once and passed to each `generate_report()` call — the LLM is contacted only 4 times total, regardless of how many formats are requested.

**Why one template, not per-format templates:** All four formats share the same section structure, narrative injection, and statistical content. Format-specific concerns (plotly vs. ggplot2, TOC, CSS) are handled by the `output_format=` argument to `rmarkdown::render()`, not by template branching.

**Format-to-rmarkdown-output mapping (new in `generate_report()`):**
```r
output_format_obj <- switch(format,
  "html"     = rmarkdown::html_document(toc = TRUE, toc_float = TRUE,
                                         theme = "flatly", code_folding = "hide"),
  "pdf"      = rmarkdown::pdf_document(toc = TRUE),
  "word"     = rmarkdown::word_document(toc = TRUE),
  "markdown" = rmarkdown::md_document(variant = "gfm"),
  stop(sprintf("generate_report(): unknown format '%s'.", format))
)
```

Currently `generate_report()` only handles `"html"` and `"pdf"` (lines 70-77 of `R/report.R`). The `"word"` and `"markdown"` branches are new. `match.arg()` on the `format` parameter must be extended to include all four values.

**Dependency discipline:** `rmarkdown` is already Suggests, already `requireNamespace()`-guarded in `generate_report()`. PDF requires LaTeX (system-level, not a package dep). Word requires pandoc (ships with RStudio; system-level otherwise). Neither becomes a hard dep. `es_report()` must wrap format-specific render failures in `tryCatch` and emit one warning per failed format rather than stopping.

**Plotly in non-HTML formats:** `interactive = FALSE` must be forced automatically when `format != "html"`. `generate_report()` should set this before calling `rmarkdown::render()`. The template already gates plotly vs. ggplot2 on `params$interactive`.

### Pattern 5: Grounding Guard Preservation in Multi-Section Narrative

**What:** The grounding guard (`.validate_grounding()` in `R/advise.R`) runs automatically inside every `es_advise()` call — once per narrative section. There is no additional guard layer at the `es_report()` level.

**Critical invariant:** The `narrative` named list passed to `generate_report()` and then to `skeleton.Rmd` contains only post-guard `Advice` objects (or `.empty_advice()` objects with no recommendations). The template renders `advice$interpretation` and `advice$recommendations` without re-validating — it trusts that objects reaching it are already guard-validated. This is safe because `Advice` S3 objects are only constructed by `.build_advice_from_parsed()` (which always calls `.validate_grounding()`) or `.empty_advice()` (no recommendations).

**What must NOT happen:** The template must never render raw LLM text that bypassed `es_advise()`. All prose must originate from an `Advice` object returned by `es_advise()`.

**Narrative chunk guard in skeleton.Rmd:**
```r
# Each narrative chunk must guard all three conditions:
advice_sec <- params$narrative[["executive_summary"]]
if (!is.null(advice_sec) &&
    (inherits(advice_sec, "Advice") || inherits(advice_sec, "es_advice")) &&
    nzchar(advice_sec$interpretation %||% "")) {
  cat("### AI Interpretation\n\n")
  cat(advice_sec$interpretation, "\n\n")
}
```

## Data Flow

### Primary Request Path (es_report, fitted task, LLM provider)

```
es_report(task, provider=p, formats=c("html","pdf"), ...)
    |
    +-- [skip: task already fitted]
    |
    +-- es_diagnostics(task)
    |       -> es_diagnostics S3 (6-section named list)
    |
    +-- .assemble_report_advice(diagnostics, provider=p, sections=[4])
    |       +-- es_advise(diag, "report_writing", provider=p)  -> Advice (exec summary)
    |       |       -> .validate_grounding() runs inside
    |       +-- es_advise(diag, "report_writing", provider=p)  -> Advice (data/methods)
    |       +-- es_advise(diag, "report_writing", provider=p)  -> Advice (results)
    |       +-- es_advise(diag, "flag_robustness", provider=p) -> Advice (robustness)
    |       -> named list: $executive_summary, $data_methods, $results, $robustness_caveats
    |
    +-- generate_report(task, format="html", narrative=narrative_list, ...)
    |       -> rmarkdown::render(skeleton.Rmd, params=list(narrative=narrative_list, ...))
    |       -> event_study_report.html
    |
    +-- generate_report(task, format="pdf", narrative=narrative_list, ...)
            -> rmarkdown::render(skeleton.Rmd, params=list(narrative=narrative_list, ...))
            -> event_study_report.pdf

Returns: c(html="path/report.html", pdf="path/report.pdf")
```

### Offline Fallback Path (provider = NULL)

```
es_report(task, provider=NULL, formats="html")
    |
    +-- es_diagnostics(task) -> diagnostics
    |
    +-- .assemble_report_advice(diagnostics, provider=NULL, sections=[4])
    |       +-- "report_writing" sections: caught stop() -> .empty_advice() x3
    |       +-- "flag_robustness": es_advise(diag, "flag_robustness", NULL)
    |               -> offline KB path -> es_advice S3 (is_deterministic=TRUE)
    |       -> named list: empty x3, $robustness_caveats = es_advice
    |
    +-- generate_report(task, format="html", narrative=narrative_list, ...)
            -> skeleton.Rmd: narrative chunks silent for exec/data/results
               robustness chunk renders KB text
            -> event_study_report.html (complete statistical + KB robustness)
```

### narrative= Injection into generate_report() and skeleton.Rmd

```
generate_report(task, narrative = list(
  executive_summary  = <Advice S3>,
  data_methods       = <Advice S3>,
  results            = <Advice S3>,
  robustness_caveats = <Advice S3 or es_advice S3>
))
    |
    +-- rmarkdown::render(params = list(
          task      = task,
          narrative = narrative,   # <-- NEW param
          advice    = NULL,        # existing single-block param (UNCHANGED)
          ...
        ))
            |
            +-- skeleton.Rmd chunks:
                  params$narrative$executive_summary$interpretation  -> before Summary
                  params$narrative$data_methods$interpretation       -> before Data section
                  params$narrative$results$interpretation            -> before Single/Multi-event
                  params$narrative$robustness_caveats$interpretation -> inside Diagnostics
```

### Backward Compatibility Contract for generate_report()

The existing `advice=` parameter (single `Advice` block, renders the "AI Advisor Interpretation" section at the bottom of the report) continues to work exactly as before. The new `narrative=` parameter defaults to `NULL`. When both are supplied, both sections render independently. The existing advice chunk in skeleton.Rmd is not moved or modified.

## Integration Points

### Internal Boundaries

| Boundary | Communication | Critical constraint |
|----------|---------------|---------------------|
| `es_report()` -> `run_event_study()` | Direct function call; task passed by reference (R6 object mutated in place) | Only called when `!"model" %in% names(task$data_tbl)`. Users who pre-fit must not trigger re-fitting. |
| `es_report()` -> `es_diagnostics()` | Direct call; returns `es_diagnostics` S3 | Will `stop()` if task not fitted — `es_report()` must guarantee fit before this call |
| `.assemble_report_advice()` -> `es_advise()` | Direct call, wrapped in `tryCatch` | Must catch `stop()` for LLM-only types with `provider=NULL` (ADV-06 contract); individual section failures must not propagate |
| `es_report()` -> `generate_report()` | Direct call N times, one per format | `generate_report()` already `requireNamespace("rmarkdown")`-guards; `es_report()` catches render errors per format |
| `generate_report()` -> `skeleton.Rmd` | `rmarkdown::render(params=list(...))` | `narrative=` is a new `params` entry; `advice=` param unchanged; template must handle `NULL` narrative gracefully |
| `skeleton.Rmd` -> narrative Advice objects | `params$narrative[[section]]$interpretation` character access | Must guard `is.null(params$narrative)`, `is.null(params$narrative[[sec]])`, and `!nzchar(interpretation)` before `cat()`-ing |

### External Dependencies (format rendering)

| Format | Tool | Dependency type | Notes |
|--------|------|-----------------|-------|
| HTML | `rmarkdown::html_document` | Suggests (already guarded) | Works everywhere |
| PDF | LaTeX (TinyTeX or TeX Live) | System-level — never a package dep | User must install; `es_report()` catches render error and warns |
| Word (.docx) | pandoc (via rmarkdown) | System-level — ships with RStudio | `rmarkdown::word_document()` — no new R package dep |
| Markdown | `rmarkdown::md_document` | Suggests (already guarded) | GFM variant; works without LaTeX/Word toolchain |

## Build Order

The following order respects all dependency edges. Each step is independently testable before the next begins.

**Step 1: extend `generate_report()` and `skeleton.Rmd`** (lowest risk, most self-contained)
- Add `narrative = NULL` to `generate_report()` signature in `R/report.R`
- Pass `narrative` into `rmarkdown::render(params = list(..., narrative = narrative))`
- Extend `match.arg(format, ...)` to include `"word"` and `"markdown"`
- Add `word_document` and `md_document` branches to the format dispatch block (lines 70-77 of `R/report.R`)
- Auto-enforce `interactive = FALSE` for non-HTML formats
- Add `narrative: NULL` to `skeleton.Rmd` `params:` YAML block
- Add four new narrative knitr chunks in `skeleton.Rmd` (one per section), each triply-guarded
- Add `word_document` and `md_document` to the `skeleton.Rmd` `output:` YAML block
- **Test gate:** all existing `generate_report()` callers (passing `advice=`, not `narrative=`) must produce byte-identical output; new `generate_report(narrative=list(...))` call renders narrative prose in correct positions

**Step 2: implement `.assemble_report_advice()`** (internal, no user-visible surface)
- New `@noRd` function in `R/es_report.R`
- Define `NARRATIVE_SECTIONS` constant mapping section names to task types
- Implement the `tryCatch`-wrapped `es_advise()` loop
- Handle `provider=NULL` + LLM-only type by returning `.empty_advice()` (calling existing internal)
- **Test gate:** with `provider=NULL`: robustness section returns KB advice, others return empty Advice; with mock provider: all four sections return post-guard Advice objects

**Step 3: implement `es_report()`** (public API, depends on steps 1 and 2)
- New `@export`-ed function in `R/es_report.R`
- Signature: `es_report(task, parameter_set = ParameterSet$new(), provider = NULL, formats = "html", output_dir = ".", output_stem = "event_study_report", sections = <default>, narrative_sections = names(NARRATIVE_SECTIONS), title = "Event Study Report", author = NULL, confidence_level = 0.95, ...)`
- Implement the four-step sequence
- Return named character vector of output paths (names = format labels)
- **Test gate:** integration test with mock provider and mock `rmarkdown::render`; offline test produces valid HTML path; grounding invariant test (mock provider returns ungrounded rec; verify it is dropped before reaching template)

**Step 4: add `report=FALSE` convenience to `run_event_study()`** (optional, lowest priority)
- Add `report = FALSE` parameter to `run_event_study()` in `R/execute.R`
- When `report = TRUE`: call `es_report(task, ...)` after `calculate_statistics()`
- Return task invisibly (side effect: report on disk), or return list with task + paths
- **Test gate:** existing callers with no `report=` produce identical output

**Step 5: regression tests for grounding invariant** (completes the milestone)
- Mock provider test: provider returns a recommendation with a fabricated diagnostic key; verify that `Advice$recommendations` is empty (dropped by guard) before the `narrative` list reaches `skeleton.Rmd`
- Offline test: `es_report(task, provider=NULL)` produces a valid HTML file with non-empty robustness section and empty exec/data/results narrative
- Backward-compat test: `generate_report(task, advice=existing_advice)` with `narrative=NULL` matches pre-v0.64.0 output exactly

## Anti-Patterns

### Anti-Pattern 1: One es_advise() call for the entire report narrative

**What people do:** Pass `task_type = "report_writing"` once and ask the LLM for all four sections in one JSON blob.

**Why it's wrong:** The current `Advice` schema has a single `interpretation` string and a flat `recommendations` list — it is not structured for multi-section narrative. A mega-prompt also risks provider context limits, and a guard drop on one section would empty the entire report narrative.

**Do this instead:** Call `es_advise()` once per section. Section isolation means a guard drop in the results section does not affect the executive summary.

### Anti-Pattern 2: Injecting narrative text that bypassed es_advise()

**What people do:** Build a prompt directly, get raw LLM text, and `cat()` it into the template via a new `params` slot that skips the `Advice` S3 and the grounding guard.

**Why it's wrong:** This breaks the grounding invariant. Fabricated numbers can appear in the highest-visibility surface (the rendered report). This is the worst failure mode in the project's "never silently wrong" contract.

**Do this instead:** All narrative text must flow through `es_advise()`, which always runs `.validate_grounding()`. The template renders only `advice$interpretation` and `advice$recommendations` — never raw character text from an unguarded source.

### Anti-Pattern 3: Calling es_advise() once per section per format

**What people do:** Rebuild narrative inside each `generate_report()` call (once per format), because the assembled narrative is not passed in.

**Why it's wrong:** `es_advise()` calls the LLM provider. Calling it N*4 times (N formats, 4 sections) multiplies API cost by N and introduces the possibility that the HTML and PDF reports disagree on which recommendations were dropped (different guard results per call).

**Do this instead:** Call `.assemble_report_advice()` once. Pass the same `narrative` list to every `generate_report()` call. The provider is contacted exactly 4 times total.

### Anti-Pattern 4: Repurposing the existing `advice=` parameter in generate_report()

**What people do:** Change `advice=` to accept the new multi-section `narrative` list, breaking existing callers who pass a single `Advice` object.

**Why it's wrong:** Signature-breaking change; existing vignette and user code calling `generate_report(advice = my_advice)` would silently misbehave.

**Do this instead:** Add `narrative = NULL` as a distinct new parameter. Keep `advice = NULL` unchanged. Both can be supplied simultaneously and render independently.

### Anti-Pattern 5: Hard-coding a single output format in skeleton.Rmd YAML

**What people do:** Leave `output: html_document:` as the only output declaration in the template YAML front matter.

**Why it's wrong:** While `rmarkdown::render(output_format=)` overrides the YAML setting, a template that only declares HTML confuses users who open it directly in RStudio and try to knit to Word or PDF.

**Do this instead:** The `skeleton.Rmd` `output:` YAML block should list `html_document`, `pdf_document`, `word_document`, and `md_document` as alternatives. The `generate_report()` `output_format=` argument selects which one is used at render time.

## Sources

- Direct codebase inspection: `R/report.R`, `R/advise.R`, `R/es_diagnostics.R`, `R/execute.R`, `R/es_diagnostics.R`, `inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd` (2026-09-06)
- `.planning/PROJECT.md` — v0.64.0 milestone requirements and key decisions
- `.planning/codebase/ARCHITECTURE.md` — existing layer and component map

---
*Architecture research for: EventStudy v0.64.0 one-call AI report wrapper integration*
*Researched: 2026-09-06*
