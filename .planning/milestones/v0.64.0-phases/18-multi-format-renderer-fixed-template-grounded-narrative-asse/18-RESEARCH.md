# Phase 18: Multi-Format Renderer, Fixed Template & Grounded Narrative Assembly — Research

**Researched:** 2026-09-07
**Domain:** R rmarkdown multi-format rendering, narrative assembly, per-format prose sanitisation
**Confidence:** HIGH (all findings sourced from direct file reads of existing codebase)

---

<user_constraints>
## User Constraints (from CONTEXT.md)

### Locked Decisions

**Template & Multi-Format Rendering**
- Evolve the existing bundled template `inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd` into the fixed 6-section template — reuse the `system.file()` install path already wired in `generate_report()`, do not add a second template file.
- Format selection via a `format = c("html","pdf","word","md")` vector; renderer loops and renders each requested format to one output file per format sharing a common basename.
- Plot switch happens inside the template on `knitr::is_html_output()` — interactive plotly only in HTML, static ggplot2 for PDF/Word/Markdown.
- Section toggling via boolean `sections=` parameters passed to the template through `rmarkdown::render(params = ...)`, driving conditional (`eval`/`asis`) chunks — no custom templating engine.

**Section-by-Section Narrative Assembly**
- LLM-authored interpretive prose only for executive summary, results interpretation, and robustness/caveats. Data/methods gets a short auto-generated lead-in; all tables/numbers always auto-filled from task metadata + `es_diagnostics()`, never from the LLM.
- Significance calibration is a static function of the p-value with four tiers: `p < 0.01` → "strongly significant"; `< 0.05` → "significant"; `< 0.10` → "marginally significant"; else → "not statistically significant". LLM is told the label, never asked to infer it.
- References pulled from KB citation records, deduplicated by citation key and ordered alphabetically by author. Never LLM-generated.
- Per-section failure / guard-drop falls back to offline rule-based text for that section; report is always complete — never abort, never drop a whole section.
- Joint-hypothesis caveat is fixed static text in every report.

**Format Degradation, Sanitisation & Mode Distinction**
- Toolchain-skip channel: `message()` — exactly one informative line per skipped optional format (PDF/Word/Markdown), then continue. Only total inability to render the HTML baseline raises `stop()`.
- Prose sanitiser is per-format: LaTeX-special escaping for PDF, XML-entity escaping for Word, smart-quotes/em-dashes normalised to ASCII for all formats.
- AI-vs-offline distinction surfaced both ways: section-heading label ("AI-grounded interpretation" vs "Automated rule-based interpretation") AND one console `message()` reporting mode used.
- Report-level mode is AI if any section used the LLM provider; per-section heading label reflects actual source.

### Claude's Discretion
- Exact wording of static significance labels, heading labels, and the joint-hypothesis caveat text (within the semantics above).
- Internal structure of the sanitiser map and the section-assembly helper functions.
- Whether the narrative assembler lives in a new `R/` file or extends `advise.R`.

### Deferred Ideas (OUT OF SCOPE)
- RPTX-01/02/03, RPTC-01 (panel/intraday/synthetic report support, bootstrap-CI reporting, officedown rich Word, user-supplied templates).
- `es_report()` orchestrator and `run_event_study(report=)` are Phase 19.
</user_constraints>

<phase_requirements>
## Phase Requirements

| ID | Description | Research Support |
|----|-------------|------------------|
| NARR-01 | Report narrative assembled section-by-section, each section an independent grounded `es_advise()` request; LLM contacted once per section, independent of output-format count | Section §5: assembler design; the format loop must consume pre-built narrative, not re-call advise per format |
| NARR-02 | Narrative covers executive summary, data/methods, results interpretation, and robustness/caveats | Section §5: four-key OfflineNarrative contract already established; extend to named-list with same keys |
| NARR-03 | References from KB citation records, never LLM-generated | Section §6: KB citation structure; deduplication + alpha-sort idiom |
| NARR-04 | Static significance calibration function from p-values; LLM told the label | Section §7: static function design; exempt constants 0.001/0.01/0.05/0.10 already in prose guard |
| NARR-05 | Joint-hypothesis caveat as fixed static text in every report | Section §7: no existing text found — Claude's discretion on wording |
| FORMAT-01 | Renders to HTML/PDF/Word/Markdown, selectable per call | Section §2: multi-format loop pattern |
| FORMAT-02 | Missing toolchains skip with one message; only HTML failure stops | Section §3: toolchain detection idioms |
| FORMAT-03 | Plots switch to static ggplot2 for non-HTML via `knitr::is_html_output()` | Section §4: exact idiom |
| FORMAT-04 | Prose sanitised per output format | Section §8: sanitiser map design |
| TMPL-01 | One fixed template, arg-toggled sections | Section §1: template extension plan |
| TMPL-02 | Data/methods + results content auto-filled from task metadata + `es_diagnostics()` keys, not LLM | Section §5: deterministic auto-fill path |
| OFFLINE-02 | Visible AI-vs-offline mode distinction via heading labels + console message | Section §9: mode-distinction pattern |
</phase_requirements>

---

## Summary

Phase 18 builds directly on top of code that already exists and was read in full this session. `generate_report()` (`R/report.R:38-165`) accepts a `narrative=` named-list seam (added in Phase 17) and already calls `rmarkdown::render()` for one format at a time. The task is to: (1) change `format=` from `match.arg()` to a vector loop, rendering each requested format independently; (2) evolve the skeleton.Rmd into the fixed 6-section template with param-toggled sections and `knitr::is_html_output()` plot switching; (3) write a section-by-section narrative assembler that calls `es_advise()` once per section and is format-agnostic; (4) add a per-format prose sanitiser and toolchain detection; (5) surface the AI-vs-offline mode distinction.

The four-section OfflineNarrative S3 object (`exec_summary`, `data_methods`, `results`, `robustness`) produced by `.build_offline_narrative()` in `advise_offline.R` is the locked section-key contract that the LLM path, offline path, and template all share. Phase 18 must not rename these keys.

The most material risk is the multi-format sequential render figure-directory edge case: when `rmarkdown::render()` is called N times on the same template, the default figure directory is derived from the template basename and shared across calls, causing earlier format's figures to be deleted by later calls. This is solvable by passing an explicit, unique `intermediates_dir` and `knit_root_dir` per render call, or by using a per-format `fig.path` knitr option via `output_options`.

**Primary recommendation:** Loop `rmarkdown::render()` once per format with unique `output_file` and per-call `output_options = list("fig.path" = ...)` to isolate figure directories. Assemble narrative before the format loop; pass the same named list into every render call.

---

## Architectural Responsibility Map

| Capability | Primary Tier | Secondary Tier | Rationale |
|------------|-------------|----------------|-----------|
| Narrative assembly | R function layer (`R/` new file or `advise.R`) | `es_advise()` / `.build_offline_narrative()` | Must complete before format loop; format-agnostic |
| Multi-format render loop | `generate_report()` in `R/report.R` | rmarkdown / pandoc | Existing renderer is the extension point |
| Plot switching | `.Rmd` template (knitr chunk) | `knitr::is_html_output()` | Must be inside template, not in R layer |
| Prose sanitisation | `generate_report()` before `render()` | Per-format map | Sanitise narrative before it enters params |
| Toolchain detection | `generate_report()` before each format render | `requireNamespace()` / `tinytex::is_tinytex()` | Detect at call time, skip with `message()` |
| KB reference extraction | Narrative assembler | `es_kb()` | Pull from KB list, never LLM |
| Section toggling | `.Rmd` template (chunk `eval=` guards) | `params$sections` from `generate_report()` | Existing `eval= "x" %in% sections` pattern |
| AI-vs-offline mode distinction | Narrative assembler (heading label injection) + `generate_report()` (console message) | — | Two-channel requirement per OFFLINE-02 |

---

## Standard Stack

### Core (already in Suggests / Imports — no new dependencies)

| Library | Version in DESCRIPTION | Purpose | Status |
|---------|------------------------|---------|--------|
| rmarkdown | (Suggests) | Multi-format rendering engine | Already present; VERIFIED in DESCRIPTION |
| knitr | (Suggests) | Template execution; `is_html_output()` | Already present |
| tinytex | (Suggests) | LaTeX toolchain detection (`tinytex::is_tinytex()`) | Already present |
| ggplot2 | (Imports) | Static plots for PDF/Word/Markdown | Already present |
| plotly | (Suggests) | Interactive plots for HTML only | Already present |

[VERIFIED: /home/simonm/projects/datascience/eventstudy/DESCRIPTION:45-67]

No new packages required. Word output uses `rmarkdown::word_document()` via pandoc (zero new dep, officedown deferred). Markdown uses `rmarkdown::md_document()`.

### Output Format Constructors

```r
# HTML (baseline — stop() if unavailable)
rmarkdown::html_document(toc = TRUE, toc_float = TRUE, theme = "flatly", code_folding = "hide")

# PDF (optional — message() + skip if tinytex/LaTeX unavailable)
rmarkdown::pdf_document(toc = TRUE)

# Word (optional — message() + skip if pandoc unavailable)
rmarkdown::word_document(toc = TRUE)   # plain, no officedown dep

# Markdown (optional — message() + skip, though rarely unavailable)
rmarkdown::md_document(variant = "gfm")  # GitHub-flavoured markdown
```

[ASSUMED] — these constructors are well-established but not read from official docs in this session.

### Extension Lookup Map (output_file extensions)

| Format | Extension | Output format constructor |
|--------|-----------|--------------------------|
| `"html"` | `.html` | `html_document()` |
| `"pdf"` | `.pdf` | `pdf_document()` |
| `"word"` | `.docx` | `word_document()` |
| `"md"` | `.md` | `md_document(variant = "gfm")` |

---

## Section 1: Template Extension Plan

The existing `skeleton.Rmd` [VERIFIED: inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd:1-251] must be evolved to the fixed 6-section layout. Current state:

- Has 9 conditional chunks keyed by `"summary" %in% sections`, `"data" %in% sections`, etc.
- Already has `params$narrative` chunk (lines 220-243) that renders `exec_summary`, `data_methods`, `results`, `robustness` when `!is.null(params$narrative)`.
- Has `params$advice` chunk for the structured Advice S3.
- The YAML `params:` block already declares `narrative: NULL` [VERIFIED: skeleton.Rmd:20].

**What the template becomes:**

The 6 fixed sections in the new template:

1. **Executive summary** — narrative$exec_summary (auto-filled lead-in if offline)
2. **Data & Methods** — deterministic auto-fill from `params$task` metadata + `params$diag` table; narrative$data_methods as prose lead-in (never LLM tables)
3. **Results** — deterministic AR/CAR/AAR/CAAR tables from task; narrative$results as interpretive prose
4. **Diagnostics** — deterministic from `es_diagnostics()` (no narrative); `eval = "diagnostics" %in% params$sections`
5. **Robustness & Caveats** — narrative$robustness + static joint-hypothesis caveat always appended
6. **References** — deterministic from KB citations extracted by assembler; `params$references` list

New params needed in the YAML block:

```yaml
params:
  task: NULL
  diag: NULL            # NEW: pre-computed es_diagnostics object
  narrative: NULL       # existing Phase 17 seam
  references: NULL      # NEW: list of KB citation records
  sections: !r c("exec_summary", "data_methods", "results", "diagnostics", "robustness", "references")
  title: "Event Study Report"
  author: ""
  confidence_level: 0.95
  advice: NULL          # kept for backward compat
```

**Section key rename:** The existing `sections=` values (`"summary"`, `"data"`, `"diagnostics"`, `"single_event"`, `"multi_event"`, `"cross_sectional"`, `"appendix"`) are replaced with the 6 new fixed sections. The `generate_report()` default must also change.

**Backward-compatibility note:** The existing `advice=` param and advice-section chunk must be kept to avoid breaking callers that pass `advice=` today. The new narrative= sections are additive.

---

## Section 2: Multi-Format Render Loop

**Current code:** `generate_report()` calls `match.arg(format)` (line 66) accepting only one value, then calls `rmarkdown::render()` once. [VERIFIED: R/report.R:66-164]

**Required change:** Accept `format` as a character vector (one or several of `"html"`, `"pdf"`, `"word"`, `"md"`). Loop render once per format. Return a named character vector of output paths (keyed by format name).

**Loop pattern:**

```r
# format is now a character vector — validated against allowed values
valid_formats <- c("html", "pdf", "word", "md")
formats <- intersect(format, valid_formats)
if (length(formats) == 0L) stop("No valid format specified.")

ext_map <- c(html = ".html", pdf = ".pdf", word = ".docx", md = ".md")
basename_no_ext <- tools::file_path_sans_ext(basename(output_file))
output_dir      <- dirname(output_file)
if (output_dir == ".") output_dir <- getwd()

output_paths <- character(0L)

for (fmt in formats) {
  ext        <- ext_map[[fmt]]
  out_name   <- paste0(basename_no_ext, ext)
  out_format <- .build_output_format(fmt)   # helper returning rmarkdown::*_document()

  if (is.null(out_format)) {
    # toolchain missing — message() and skip (FORMAT-02)
    message("generate_report(): skipping '", fmt, "' — toolchain not available.")
    next
  }

  # Per-format fig.path to avoid figure-directory collisions (see §Spike 2)
  fig_dir  <- file.path(output_dir, paste0(basename_no_ext, "_files_", fmt), "figure-")

  rmarkdown::render(
    input          = template_path,
    output_format  = out_format,
    output_file    = out_name,
    output_dir     = output_dir,
    params         = render_params,           # same for all formats
    output_options = list("fig.path" = fig_dir),
    envir          = new.env(parent = globalenv()),
    quiet          = TRUE
  )

  output_paths[[fmt]] <- file.path(output_dir, out_name)
}
```

**Return value change:** Returns a named character vector of paths rather than a single invisible path. `invisible(output_paths)` for backward compat.

---

## Section 3: Toolchain Detection (FORMAT-02)

### PDF/LaTeX detection

```r
.pdf_toolchain_available <- function() {
  # tinytex is the preferred CRAN-clean LaTeX provider
  if (requireNamespace("tinytex", quietly = TRUE)) {
    return(tinytex::is_tinytex())  # returns TRUE if TinyTeX is installed
  }
  # Fall back: check for system LaTeX (pdflatex)
  nzchar(Sys.which("pdflatex"))
}
```

[ASSUMED] — `tinytex::is_tinytex()` is the documented function for this check but not verified from official docs in this session.

### Word/pandoc detection

```r
.word_toolchain_available <- function() {
  # pandoc is bundled with RStudio; rmarkdown ships pandoc detection
  if (requireNamespace("rmarkdown", quietly = TRUE)) {
    return(rmarkdown::pandoc_available())
  }
  nzchar(Sys.which("pandoc"))
}
```

[ASSUMED] — `rmarkdown::pandoc_available()` is standard but not verified from official docs this session.

### Markdown availability

Markdown rendering through `rmarkdown::md_document()` requires only pandoc (same as Word). Use `.word_toolchain_available()` as the guard. Markdown is almost always available when rmarkdown is installed; the skip message is a safety net.

### Decision table

| Format | Guard function | Skip channel | Fail channel |
|--------|---------------|-------------|--------------|
| html | `requireNamespace("rmarkdown")` | — | `stop()` |
| pdf | `.pdf_toolchain_available()` | `message()` + next | — |
| word | `.word_toolchain_available()` | `message()` + next | — |
| md | `.word_toolchain_available()` | `message()` + next | — |

---

## Section 4: Plot Switching Inside Template (FORMAT-03)

The exact idiom in `.Rmd` chunks:

```r
# Inside a knitr chunk — no special chunk option needed
if (knitr::is_html_output()) {
  # interactive plot (plotly)
  plotly::ggplotly(p)
} else {
  # static plot (ggplot2) — works for PDF, Word, Markdown
  print(p)
}
```

`knitr::is_html_output()` returns `TRUE` for `html_document`, `html_vignette`, and `html_notebook` output formats; returns `FALSE` for `pdf_document`, `word_document`, and `md_document`. [ASSUMED] — this is the standard idiom; not verified from official docs this session but is the established pattern used across CRAN.

**Passing plot objects into the template:** The plot objects are NOT passed via `params=` (R objects in params must be serializable for caching and can be large). Instead, the template re-derives plots from `params$task` (already in params) using `EventStudy::plot_event_study(params$task)` inside the chunk, then branches on `knitr::is_html_output()`.

---

## Section 5: Section-by-Section Narrative Assembly (NARR-01/02/TMPL-02)

### Locked section-key contract

The OfflineNarrative S3 returned by `.build_offline_narrative()` has these four keys (locked by Phase 17):
[VERIFIED: R/advise_offline.R:268-278]

```r
structure(
  list(
    source           = "offline_kb",
    is_deterministic = TRUE,
    exec_summary     = exec_summary,    # character scalar
    data_methods     = data_methods,    # character scalar
    results          = results_sec,     # character scalar
    robustness       = robustness       # character scalar
  ),
  class = "OfflineNarrative"
)
```

The Phase 18 narrative assembler must produce a list with these same four keys regardless of whether the LLM or offline path was used. The template receives `params$narrative` as this list.

### Assembler design

A new function `assemble_report_narrative(diagnostics, provider = NULL, sections_to_narrate = c("exec_summary", "data_methods", "results", "robustness"), significance_fn = .calibrate_significance)`:

1. For each section key, call `es_advise(diagnostics, task_type = "report_writing", provider = provider)` — but the LLM path must be invoked once per section with a section-scoped prompt, not once for the whole narrative. This requires either:
   - A `section=` argument added to the `"report_writing"` path in `es_advise()`, OR
   - A helper that wraps `es_advise()` with a section-specific prompt injection.
2. If LLM call fails or grounding guard drops the section, fall back to `.build_offline_narrative()` prose for that section only.
3. Track which sections used LLM vs offline (for mode distinction, OFFLINE-02).
4. Return a list with the four section keys plus a `$mode` metadata field.

**NARR-01 testable invariant:** The assembler is called ONCE to build the narrative list, then the same list is passed into all format renders. The format loop never calls `es_advise()`. This is enforced by architecture: `generate_report()` calls the assembler before the format loop.

### Deterministic auto-fill path (TMPL-02)

Data/methods and results tables are rendered inside the template from `params$task` and `params$diag` (a pre-computed `es_diagnostics` object). These chunks run unconditionally — `eval = TRUE`. The narrative prose is a separate chunk (`eval = !is.null(params$narrative$data_methods)`). The LLM never produces numbers for these tables.

---

## Section 6: KB References Assembly (NARR-03)

The KB citation structure per rule record:
[VERIFIED: R/knowledge_base.R:105-121 (citation field validator) and R/knowledge_base.R:169-176 (example citation)]

```r
citation = list(
  author = "Patell, J.M.",      # character, required
  year   = 1976L,               # numeric/integer, required
  key    = "Patell1976",        # character, required
  venue  = "Journal of ..."     # character, optional
)
```

**Reference extraction pattern:**

```r
.extract_kb_references <- function(rules_fired) {
  # rules_fired: list of KB rule records (from .build_offline_advice$rules_matched)
  cites <- lapply(rules_fired, function(r) r$citation)
  # Deduplicate by key
  keys <- vapply(cites, function(c) c$key, character(1L))
  cites <- cites[!duplicated(keys)]
  # Sort alphabetically by author
  authors <- vapply(cites, function(c) c$author, character(1L))
  cites[order(authors)]
}
```

The assembler collects fired KB rules from both `stat_choice` and `robustness` categories, deduplicates by `key`, sorts by `author`, and passes the resulting list as `params$references` to the template. The references section renders this list as a formatted bibliography — never asks the LLM for citations.

---

## Section 7: Static Significance Calibration + Joint-Hypothesis Caveat (NARR-04/05)

### Significance calibration function

The four tiers are locked by CONTEXT.md decision. The function is pure, has no side effects, and is called by the assembler to compute a label that is then injected into the LLM prompt as an already-determined value:

```r
.calibrate_significance <- function(p_value) {
  if (is.na(p_value) || !is.numeric(p_value)) return("not evaluable")
  if (p_value < 0.01) return("strongly significant")
  if (p_value < 0.05) return("significant")
  if (p_value < 0.10) return("marginally significant")
  "not statistically significant"
}
```

Note: the significance constants `0.001`, `0.01`, `0.05`, `0.10` are already exempt from the prose grounding guard in `.is_grounded_literal()` [VERIFIED: R/advise.R:441-443]. This means the assembled significance label text ("strongly significant at p < 0.01") passes the guard without needing to appear in the diagnostics registry.

### Joint-hypothesis caveat text (NARR-05)

No existing text found in the codebase for this caveat [confirmed by grep returning no matches for "joint.hypothesis"]. This is Claude's discretion. Recommended static text:

```
"Note: statistical significance of abnormal returns is a joint test of the event effect and the correctness of the return model. Rejection of the null may reflect model misspecification rather than a true event effect (MacKinlay 1997)."
```

This caveat is appended to the robustness section of every report, independent of the narrative source (offline or LLM). It is fixed, not LLM-generated, and any numeric it contains must be either a significance constant (exempt) or absent (no numbers in the recommended text above — clean).

---

## Section 8: Per-Format Prose Sanitiser (FORMAT-04)

### What pandoc handles automatically vs. what R must handle

**Pandoc handles:** When rendering Markdown → PDF or Markdown → DOCX, pandoc performs its own escaping of content that passes through pandoc's AST (e.g., regular text in paragraph blocks). However, content emitted via `cat()` in `results='asis'` knitr chunks bypasses pandoc's escaping — it is injected as raw markup. Prose from the narrative that is `cat()`-ed raw into the document is NOT automatically escaped. [ASSUMED] — this is the documented knitr `results='asis'` behaviour.

### Character sets to sanitise per format

**PDF (LaTeX backend):** The 10 LaTeX special characters that break compilation if raw:
`\ { } $ % # _ & ~ ^`

R escaping pattern:
```r
.sanitise_for_pdf <- function(text) {
  # Order matters: backslash first (avoid double-escaping)
  text <- gsub("\\\\", "\\\\textbackslash{}", text, fixed = FALSE)
  for (ch in c("{", "}", "$", "%", "#", "_", "&")) {
    text <- gsub(ch, paste0("\\", ch), text, fixed = TRUE)
  }
  text <- gsub("~", "\\textasciitilde{}", text, fixed = TRUE)
  text <- gsub("^", "\\textasciicircum{}", text, fixed = TRUE)
  text
}
```

**Word/DOCX (XML backend):** The 5 XML/HTML entities:
`& < > " '`

R escaping pattern:
```r
.sanitise_for_word <- function(text) {
  text <- gsub("&",  "&amp;",  text, fixed = TRUE)
  text <- gsub("<",  "&lt;",   text, fixed = TRUE)
  text <- gsub(">",  "&gt;",   text, fixed = TRUE)
  text <- gsub("\"", "&quot;", text, fixed = TRUE)
  text <- gsub("'",  "&#39;",  text, fixed = TRUE)
  text
}
```

**Universal (all formats):** Smart quotes and typographic dashes that can fail in PDF/Word:
- Left double quote `“` → `"`
- Right double quote `”` → `"`
- Left single quote `‘` → `'`
- Right single quote `’` → `'`
- Em dash `—` → `--` (or `---` for Markdown)
- En dash `–` → `-`

```r
.sanitise_universal <- function(text) {
  text <- gsub("“", "\"", text, fixed = TRUE)
  text <- gsub("”", "\"", text, fixed = TRUE)
  text <- gsub("‘", "'",  text, fixed = TRUE)
  text <- gsub("’", "'",  text, fixed = TRUE)
  text <- gsub("—", "--", text, fixed = TRUE)
  text <- gsub("–", "-",  text, fixed = TRUE)
  text
}
```

**Sanitiser map dispatch:**

```r
.sanitise_prose <- function(text, format) {
  text <- .sanitise_universal(text)
  if (format == "pdf")  text <- .sanitise_for_pdf(text)
  if (format == "word") text <- .sanitise_for_word(text)
  text
}
```

Apply to every character scalar in the narrative list before each format's render call. [ASSUMED] — the specific character sets and their R implementations are derived from well-established knowledge; exact behaviour not verified against a LaTeX/pandoc run in this session.

---

## Section 9: AI-vs-Offline Mode Distinction (OFFLINE-02)

### Two-channel requirement

CONTEXT.md locks both channels:
1. **Section heading label** inside the template: "AI-grounded interpretation" vs "Automated rule-based interpretation"
2. **Console `message()`** from `generate_report()` after narrative assembly

### Implementation pattern

The narrative list returned by the assembler carries metadata:

```r
list(
  exec_summary = "...",
  data_methods = "...",
  results      = "...",
  robustness   = "...",
  section_sources = list(         # NEW metadata field
    exec_summary = "ai",          # or "offline"
    data_methods = "offline",     # deterministic auto-lead, always "offline"
    results      = "ai",
    robustness   = "offline"
  ),
  report_mode = "ai"              # "ai" if any section used LLM; "offline" otherwise
)
```

Inside the template, each narrative section chunk reads `params$narrative$section_sources[[section_key]]` and prepends the appropriate heading label via `cat()`:

```r
mode_label <- if (identical(params$narrative$section_sources$exec_summary, "ai")) {
  "*AI-grounded interpretation*"
} else {
  "*Automated rule-based interpretation*"
}
cat(mode_label, "\n\n")
cat(params$narrative$exec_summary, "\n\n")
```

The console `message()` in `generate_report()`:

```r
mode <- narrative$report_mode %||% "offline"
message("Report mode: ", if (mode == "ai") "AI-grounded narrative" else "Offline rule-based narrative")
```

---

## Spike Resolutions

### Spike 1: Multi-format sequential render — figure directory collision

**The problem:** `rmarkdown::render()` derives the figure directory from the input file basename. When called N times on the same `skeleton.Rmd`, all calls use the same figure path (e.g., `skeleton_files/figure-html/`). The second call's `clean = TRUE` (default) deletes the first call's figures.

**Resolution:** Pass `output_options` to each render call with a unique `fig.path` per format. The `output_options` argument is passed as a named list that overrides options in the output format object:

```r
rmarkdown::render(
  ...,
  output_options = list("fig.path" = file.path(output_dir,
    paste0(basename_no_ext, "_", fmt, "_files/figure-")))
)
```

Alternatively, set `clean = FALSE` on intermediate formats and `clean = TRUE` only on the last. But the `fig.path` approach is cleaner because it isolates each format's figure directory.

[ASSUMED] — the `output_options` list key `"fig.path"` overriding knitr's figure path is the standard approach for this problem; not verified against a live rmarkdown render this session.

### Spike 2: Prose sanitisation fixtures — verify before locking

**Recommended fixtures for tests:**

```r
test_that(".sanitise_for_pdf escapes all LaTeX specials", {
  input    <- "Results: 50% return & $100 profit #1 with {high} var_iance ~control^2"
  sanitised <- .sanitise_for_pdf(input)
  expect_false(grepl("(?<!\\\\)[%$&#_{}~^]", sanitised, perl = TRUE))
})

test_that(".sanitise_universal converts em-dash and smart quotes", {
  input     <- "“Hello” — it’s fine"
  sanitised <- .sanitise_universal(input)
  expect_equal(sanitised, "\"Hello\" -- it's fine")
})
```

### Spike 3: NARR-01 testable invariant — one LLM call per section

```r
test_that("narrative assembled once per section regardless of format count", {
  mock_provider <- local({
    call_count <- 0L
    list(
      complete = function(prompt, schema) {
        call_count <<- call_count + 1L
        list(text = "{\"interpretation\":\"\",\"recommendations\":[],\"caveats\":[]}")
      },
      get_calls = function() call_count
    )
  })

  # Render to 3 formats
  generate_report(task, format = c("html", "pdf", "word"), provider = mock_provider, ...)

  # N sections (e.g. 3 LLM sections), not N*3 formats
  expect_equal(mock_provider$get_calls(), 3L)  # exec_summary, results, robustness
})
```

This invariant is enforced by architecture: the assembler runs before the format loop. The test verifies it.

---

## Don't Hand-Roll

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Format detection / output object | Custom format dispatcher | `rmarkdown::html_document()`, `pdf_document()`, `word_document()`, `md_document()` | Standard constructors; pandoc-backed |
| LaTeX availability check | Shell `which pdflatex` | `tinytex::is_tinytex()` / `rmarkdown::pandoc_available()` | Cross-platform; already in Suggests |
| HTML/non-HTML branch in template | Custom output format param | `knitr::is_html_output()` | knitr-native; respects all HTML output variants |
| Bibliography formatting | Custom citation renderer | Extract KB `citation` records and format inline | KB already has the structured data |
| Numeric significance judgment | Letting the LLM decide | `.calibrate_significance(p)` static function | Deterministic; grounding-guard compatible |

---

## Common Pitfalls

### Pitfall 1: format= `match.arg()` drops to first element on vector input
**What goes wrong:** The current `format <- match.arg(format)` on line 66 of `report.R` silently takes the first element of a vector. If a caller passes `format = c("html","pdf")`, only html renders.
**How to avoid:** Change `match.arg(format)` to explicit vector validation: `formats <- intersect(format, c("html","pdf","word","md"))`.
**Warning signs:** No error, but only one output file produced.

### Pitfall 2: Figure directory collision on sequential renders
**What goes wrong:** All N render calls use the same `skeleton_files/` directory. The second call's cleanup deletes the first call's figures.
**How to avoid:** Pass `output_options = list("fig.path" = ...)` with a unique per-format path to each render call.
**Warning signs:** HTML report has figures; PDF report has empty figure placeholders.

### Pitfall 3: `results='asis'` bypasses pandoc escaping
**What goes wrong:** Narrative prose `cat()`-ed into a `results='asis'` chunk is injected as raw LaTeX/XML. An em-dash in LLM output corrupts a PDF.
**How to avoid:** Sanitise the entire narrative list before it enters `params=`, not inside the template.
**Warning signs:** PDF fails with LaTeX error referencing a special character; Word produces garbled text.

### Pitfall 4: Narrative section keys renamed from Phase 17 contract
**What goes wrong:** Phase 17 locked `exec_summary`, `data_methods`, `results`, `robustness` as the four section keys. If Phase 18 uses different names, the offline path (`.build_offline_narrative()`) returns an `OfflineNarrative` that doesn't match the template.
**How to avoid:** Always use the four locked keys. The new `section_sources` and `report_mode` metadata fields are additive — they don't replace the prose keys.
**Warning signs:** Template renders blank sections despite narrative being non-NULL.

### Pitfall 5: Significance constants dropping through grounding guard
**What goes wrong:** If the static significance calibration function produces text like "p < 0.05 — significant", the `0.05` literal is extracted by `.extract_numeric_literals()` and tested against the diagnostics registry. If the mean CAR p-value isn't exactly 0.05, the section is dropped as ungrounded.
**How to avoid:** The constants `0.001`, `0.01`, `0.05`, `0.10` are already exempt in `.is_grounded_literal()` [VERIFIED: R/advise.R:441-443]. The significance label text must cite only these exempt constants, not inline p-values from diagnostics. If the prose says "the mean p-value of 0.043 is significant", the `0.043` is tested — it must appear in the registry (which it will as `mean(car_p_vals)`). The significance label itself (`< 0.05`) uses only exempt constants.

### Pitfall 6: LLM path for `report_writing` task type ignores section scoping
**What goes wrong:** The current `"report_writing"` prompt in `.build_prompt()` [VERIFIED: R/advise.R:795-800] generates a single narrative, not four section-scoped ones. Calling `es_advise()` once returns one aggregate block; the assembler needs to call it once per section with a section-scoped prompt.
**How to avoid:** The assembler should pass a `section=` context into the prompt, either by adding a `section` argument to the `"report_writing"` task instruction in `.build_prompt()`, or by using a thin wrapper that overrides the task instruction per section.

---

## Architecture Patterns

### System Architecture Diagram

```
[generate_report(task, format=c(...), provider=)]
         |
         v
[.assemble_narrative(diag, provider)]
    |         |         |         |
    v         v         v         v
[es_advise]  [es_advise] [es_advise] [offline fallback]
exec_summary  results   robustness  data_methods (always offline)
    |         |         |         |
    +-----> narrative named-list (4 keys + section_sources + report_mode)
                         |
                   .sanitise_prose(narrative, fmt) -- per format
                         |
         +---------------+------------------+------------------+
         v               v                  v                  v
    render(html)    render(pdf)        render(word)       render(md)
         |               |                  |                  |
    output_paths[["html"]] ...   ...   ...
```

### Recommended Project Structure (new/changed files only)

```
R/
├── report.R             # generate_report() — extend format loop, add assembler call
├── report_narrative.R   # NEW: assemble_report_narrative(), .calibrate_significance(),
│                        #      .sanitise_prose(), .extract_kb_references()
│                        #      (or extend advise.R — Claude's discretion)
inst/rmarkdown/templates/event_study_report/skeleton/
├── skeleton.Rmd         # evolve to 6-section fixed template
tests/testthat/
├── test_report_multiformat.R   # NEW: format loop, toolchain skip, figure isolation
├── test_report_narrative_asm.R # NEW: assembler, significance fn, caveat, NARR-01 invariant
├── test_prose_sanitiser.R      # NEW: per-format sanitisation fixtures
```

---

## Validation Architecture

### Phase Requirements → Test Map

| Req ID | Behavior | Test Type | File | Automated Command |
|--------|----------|-----------|------|-------------------|
| NARR-01 | LLM called once per section, not per format | unit (mock provider) | test_report_narrative_asm.R | `testthat::test_file(...)` |
| NARR-02 | Four section keys present in assembled narrative | unit | test_report_narrative_asm.R | fast |
| NARR-03 | References from KB only; alphabetical by author | unit | test_report_narrative_asm.R | fast |
| NARR-04 | `.calibrate_significance()` returns correct tier | unit | test_report_narrative_asm.R | fast |
| NARR-05 | Caveat text present in every rendered report | unit (HTML render) | test_report_multiformat.R | skip_on_cran |
| FORMAT-01 | Vector format= renders N files | integration (HTML only via skip_on_cran) | test_report_multiformat.R | skip_on_cran |
| FORMAT-02 | Missing toolchain emits message(), no stop() | unit (mock toolchain check) | test_report_multiformat.R | fast |
| FORMAT-03 | `knitr::is_html_output()` branch — untestable in unit; covered by HTML render | integration | test_report_multiformat.R | skip_on_cran |
| FORMAT-04 | `.sanitise_prose()` fixtures per format | unit | test_prose_sanitiser.R | fast |
| TMPL-01 | 6 sections present in skeleton.Rmd params block | unit (readLines check) | test_report_multiformat.R | fast |
| TMPL-02 | Data/methods table sourced from task, not narrative | unit (render + grep output) | test_report_multiformat.R | skip_on_cran |
| OFFLINE-02 | Mode label and console message emitted | unit (mock + capture.output) | test_report_narrative_asm.R | fast |

### Wave 0 Gaps (files that must be created before implementation)

- [ ] `tests/testthat/test_report_multiformat.R` — covers FORMAT-01..04, TMPL-01/02, NARR-05
- [ ] `tests/testthat/test_report_narrative_asm.R` — covers NARR-01..04, OFFLINE-02
- [ ] `tests/testthat/test_prose_sanitiser.R` — covers FORMAT-04 sanitisation fixtures

---

## Environment Availability

| Dependency | Required By | Available | Fallback |
|------------|------------|-----------|----------|
| rmarkdown (Suggests) | All render | Already in DESCRIPTION Suggests | stop() with install message |
| knitr (Suggests) | Template execution | Already in DESCRIPTION Suggests | stop() with install message |
| tinytex (Suggests) | PDF render | Already in DESCRIPTION Suggests | skip PDF with message() |
| pandoc (system) | Word/MD render | Bundled with RStudio on dev machine | skip Word/MD with message() |
| ggplot2 (Imports) | Static plots | Hard dependency | — |
| plotly (Suggests) | HTML interactive plots | Already in DESCRIPTION Suggests | skip plotly, use ggplot2 |

---

## Security Domain

No external network access in the renderer. The only security-relevant surface is LLM provider calls in the assembler — these are inherited from the existing `es_advise()` provider path and are already guarded by the grounding guard. No new ASVS categories apply beyond what Phase 17 already covers.

---

## Assumptions Log

| # | Claim | Section | Risk if Wrong |
|---|-------|---------|---------------|
| A1 | `tinytex::is_tinytex()` is the correct function for PDF toolchain detection | §3 | Wrong function name → toolchain always reported unavailable; PDF format always skipped |
| A2 | `rmarkdown::pandoc_available()` is the correct function for Word/MD detection | §3 | Wrong function name → same consequence |
| A3 | `output_options = list("fig.path" = ...)` passed to `rmarkdown::render()` isolates figure directories across sequential render calls | §2/Spike 1 | Figure collision remains → earlier format's figures deleted |
| A4 | `knitr::is_html_output()` returns FALSE for `word_document` and `md_document` (not just `pdf_document`) | §4 | Static ggplot2 not used for Word/MD → plotly fails in non-HTML formats |
| A5 | `results='asis'` knitr chunks bypass pandoc escaping for raw narrative prose | §8 | If pandoc does escape, double-escaping corrupts output (e.g., `&amp;amp;` in Word) |
| A6 | `rmarkdown::md_document(variant = "gfm")` is the correct constructor for GitHub-flavoured Markdown | §Standard Stack | Wrong variant → non-standard Markdown output |

---

## Open Questions

1. **Section-scoped LLM prompt for `report_writing`**
   - What we know: `.build_prompt()` has a single `"report_writing"` case that generates aggregate prose. Each section needs its own prompt context.
   - What's unclear: Should the assembler add a `section=` argument to `es_advise()` routing, or build a thin internal wrapper that injects section context into the prompt?
   - Recommendation: Add an internal-only `section_hint=` argument to `.build_prompt()` that appends a section-scoping instruction when non-NULL. This avoids changing the public `es_advise()` API.

2. **Backward compatibility of `sections=` default**
   - What we know: The current default is `c("summary", "data", "diagnostics", "single_event", "multi_event", "cross_sectional", "appendix")`. The new template uses six different section keys.
   - What's unclear: Whether any callers outside the package depend on the old section names.
   - Recommendation: Change the default; add a backward-compat check that translates old section names to new ones with a deprecation warning.

3. **`generate_report()` return type change**
   - What we know: Currently returns `invisible(output_path)` (single character). Multi-format returns a named vector.
   - What's unclear: Whether Phase 19's `es_report()` depends on a specific return type.
   - Recommendation: Return `invisible(output_paths)` as a named character vector. Single-format call returns a length-1 named vector — `output_paths[["html"]]` still works; `output_paths[[1L]]` also works. This is backward-compatible in practice.

---

## Sources

### Primary (VERIFIED — read from source this session)

- `R/report.R:1-165` — `generate_report()` complete implementation; format loop extension point
- `R/advise.R:1-1076` — `es_advise()`, grounding guard, prose scanner, significance-constant exemption list
- `R/advise_offline.R:234-471` — `.build_offline_narrative()`, `OfflineNarrative` S3, four-key section contract
- `inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd:1-251` — current template; narrative-section chunk; params block
- `R/knowledge_base.R:100-394` — KB rule structure, citation field schema, all 8 KB rules
- `R/es_diagnostics.R:1-98` — `es_diagnostics()` return structure; six-section schema
- `DESCRIPTION:31-70` — Imports/Suggests; confirmed rmarkdown, knitr, tinytex, ggplot2, plotly presence
- `tests/testthat/test_report_narrative.R:1-80` — existing Phase 17 narrative tests; golden-file pattern

### Secondary (ASSUMED — from training knowledge, not verified against official docs this session)

- rmarkdown multi-format render loop pattern; `output_options` behaviour for `fig.path`
- `tinytex::is_tinytex()` and `rmarkdown::pandoc_available()` function names
- `knitr::is_html_output()` returning FALSE for word/md formats
- LaTeX special character set and XML entity set for sanitisation
- `rmarkdown::md_document(variant = "gfm")` constructor syntax
