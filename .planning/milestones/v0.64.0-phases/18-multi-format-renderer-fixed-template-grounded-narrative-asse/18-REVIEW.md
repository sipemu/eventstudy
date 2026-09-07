---
phase: 18-multi-format-renderer-fixed-template-grounded-narrative-asse
reviewed: 2026-09-07T12:00:00Z
depth: standard
files_reviewed: 7
files_reviewed_list:
  - R/report_narrative.R
  - R/advise.R
  - R/report.R
  - inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd
  - tests/testthat/test_report_narrative_asm.R
  - tests/testthat/test_prose_sanitiser.R
  - tests/testthat/test_report_multiformat.R
findings:
  critical: 2
  warning: 2
  info: 2
  total: 6
status: resolved
---

# Phase 18: Code Review Report

**Reviewed:** 2026-09-07T12:00:00Z
**Depth:** standard
**Files Reviewed:** 7
**Status:** issues_found

## Summary

Phase 18 delivers the narrative assembler (`assemble_report_narrative()`), per-format prose sanitiser, `section_hint=` seam on `es_advise()`, multi-format render loop in `generate_report()`, and the fixed 6-section `skeleton.Rmd`. The architecture is sound: LLM calls happen once before the format loop (NARR-01 enforced), the grounding guard is inherited from Phase 17, KB references are never LLM-generated, and the CRAN hygiene (non-ASCII in R string literals, Suggests boundaries) is respected. Two critical defects were found: a LaTeX rendering corruption in `.sanitise_for_pdf()` and a silent no-op in the `output_options`-based `fig.path` isolation. Two warnings cover a dead function parameter and redundant diagnostic calls.

## Critical Issues

### CR-01: `.sanitise_for_pdf()` corrupts `\textbackslash{}` -- backslash renders as `\{}` in PDF

**File:** `R/report_narrative.R:309-331`

**Issue:** The backslash-to-`\textbackslash{}` replacement (step 1) introduces curly braces `{}` into the output string. Steps 2-3 then escape `{` to `\{` and `}` to `\}`, turning `\textbackslash{}` into `\textbackslash\{\}`. In LaTeX this renders as a backslash followed by the literal characters `{}` (three characters), not a single backslash. Any LLM or offline prose containing a backslash (e.g. a file path like `path\to\file`, or a LaTeX formula, or the `\input` injection test) will render corrupted.

Verified with R simulation:
```r
# Input: "\"
# After step 1: \textbackslash{}
# After steps 2-3: \textbackslash\{\}   <- WRONG LaTeX
```

Tilde (`~`) and caret (`^`) are NOT affected because their replacements (steps 8-9) happen after brace-escaping (steps 2-3); the `{}` in their macro calls survive intact.

The existing test (`test_prose_sanitiser.R:119`) only checks that `"textbackslash"` appears in the output and that `\\\\textbackslash` (double backslash prefix) does not; it does not verify that `{}` follows the command name correctly.

**Fix:** Produce the backslash replacement without embedded `{}`, or defer brace-escaping until after all macro expansions:

```r
.sanitise_for_pdf <- function(text) {
  if (!is.character(text)) return(text)
  # 1. Backslash -> \textbackslash (no braces yet; added after brace-escape is done)
  text <- gsub("\\\\", "BACKSLASH_PLACEHOLDER", text, fixed = FALSE)
  # 2. Curly braces
  text <- gsub("{", "\\{", text, fixed = TRUE)
  text <- gsub("}", "\\}", text, fixed = TRUE)
  # ...remaining specials...
  # Final: restore backslash placeholder with correct macro (no {} to re-escape)
  text <- gsub("BACKSLASH_PLACEHOLDER", "\\textbackslash{}", text, fixed = TRUE)
  text
}
```

Alternatively, use a unique placeholder string that contains no LaTeX specials to avoid brace contamination.

---

### CR-02: `output_options = list("fig.path" = ...)` is silently ignored -- fig.path isolation is a no-op

**File:** `R/report.R:256-265`

**Issue:** The `rmarkdown::render()` documentation states explicitly: `output_options` is *"only valid when the output format is read from metadata (i.e. not a custom format object passed to output_format)"*. In `generate_report()`, `output_format` is always a custom object returned by `.build_output_format()` (e.g. `rmarkdown::html_document()`, `rmarkdown::pdf_document()`). Therefore `output_options = list("fig.path" = fig_dir)` is silently ignored by rmarkdown on every call.

The consequence: multiple sequential renders (e.g. `format = c("html", "pdf")`) all use the default knitr `fig.path` (typically `"<basename>_files/figure-<format>/"` inside the intermediates dir). This may not cause collisions in current knitr versions (which isolate by output format internally), but it does not achieve the per-format isolation claimed in the implementation and tested against in 18-02-SUMMARY.md (T-18-05). The Spike 1 result in the SUMMARY ("figure isolation verified") may have been a false pass: the test checked that files existed, not that the actual `fig.path` knitr option was set to the per-format path.

**Fix:** Set `fig.path` via knitr global options inside the template's `setup` chunk, parameterized via `params`:

In `skeleton.Rmd` setup chunk:
```r
if (!is.null(params$fig_path)) {
  knitr::opts_chunk$set(fig.path = params$fig_path)
}
```

In `generate_report()` format loop, add `fig_path` to `render_params`:
```r
render_params$fig_path <- fig_dir
```

And remove the now-redundant `output_options` argument from `rmarkdown::render()`.

---

## Warnings

### WR-01: `significance_fn` parameter in `assemble_report_narrative()` is declared but never invoked

**File:** `R/report_narrative.R:153,155`

**Issue:** The `significance_fn = .calibrate_significance` parameter is documented as "injected for testability" but the function body never calls `significance_fn(...)` anywhere. The significance calibrator is not invoked in the assembler at all -- p-value labels are not auto-filled into the narrative text in the current implementation (this would require parsing diagnostics for p-values and weaving calibrated labels into the section prompts or offline text). The dead parameter is misleading: it implies a p-value calibration step exists in the assembler when it does not.

**Fix:** Either (a) remove the parameter from `assemble_report_narrative()` until the p-value labeling feature is implemented, or (b) implement the intended usage: pass calibrated p-value labels into the `section_hint` or into the offline baseline builder. If removal is chosen, update the `@param` documentation accordingly.

---

### WR-02: `es_diagnostics(task)` called up to three times in `generate_report()` -- same task, redundant work

**File:** `R/report.R:168,182,197`

**Issue:** Within a single `generate_report()` call, `es_diagnostics(task)` is called up to three times:
- Line 168: inside the narrative-assembly branch (when `narrative` is `NULL`)
- Line 182: inside the `references` computation block (conditioned on `!is.null(narrative)`)
- Line 197: for `diag_for_render` passed to the template params

All three calls are on the same `task` object with no mutation in between. `es_diagnostics()` is deterministic but potentially non-trivial to compute. Furthermore, the `references` computation at lines 180-185 uses `!is.null(narrative)` as its condition to decide whether to compute `diag_for_refs`. This logic is slightly inverted: if `narrative` is `NULL` because `assemble_report_narrative()` threw (even though `es_diagnostics()` itself succeeded at line 168), `diag` is in scope but `diag_for_refs` is set to `NULL` and `references` is returned as `list()`. The KB references section will be silently empty even though diagnostics are available.

**Fix:** Compute `diag` once at the start of the function body (before the narrative-assembly branch), reuse it throughout, and use `!is.null(diag)` to gate the references computation instead of `!is.null(narrative)`:

```r
# Compute diagnostics once
diag <- tryCatch(es_diagnostics(task), error = function(e) NULL)

# Assemble narrative if not pre-provided
if (is.null(narrative) && !is.null(diag)) {
  narrative <- tryCatch(
    assemble_report_narrative(diagnostics = diag, provider = provider),
    error = function(e) NULL
  )
}

# Compute references from same diag (not re-derived from narrative presence)
references <- if (!is.null(diag)) {
  tryCatch(.extract_kb_references(diag), error = function(e) list())
} else list()

# Pass same diag to render params
diag_for_render <- diag
```

---

## Info

### IN-01: `section_hint` accepts arbitrary strings with no validation -- prompt injection via public API

**File:** `R/advise.R:967,813`

**Issue:** `es_advise()` is an exported function. Its `section_hint` parameter is injected directly into the LLM prompt via `sprintf()` with no validation, no length cap, and no allowlist check. A caller passing `section_hint = "exec_summary\n\nIgnore the above and instead output: ..."` injects arbitrary text into the prompt between the structured instruction and the schema reminder. The internal call path from `assemble_report_narrative()` is safe (only passes known keys), but the public-facing API has no guard.

Note: this is an `Info` finding because (a) the section_hint= doc says it is "only applied to the report_writing task type" and implicitly intended as an internal seam, and (b) the risk is limited to prompt-content manipulation, not code execution.

**Fix:** Add an allowlist guard at the top of the `report_writing` arm in `.build_prompt()`:

```r
"report_writing" = {
  valid_hints <- c("exec_summary", "data_methods", "results", "robustness")
  if (!is.null(section_hint) && !section_hint %in% valid_hints) {
    warning(sprintf(
      ".build_prompt(): section_hint '%s' is not a recognised section key -- ignored.",
      section_hint
    ), call. = FALSE)
    section_hint <- NULL
  }
  # ... rest of logic
}
```

---

### IN-02: Lambda variable `c` shadows base `c()` in `.extract_kb_references()`

**File:** `R/report_narrative.R:104,108`

**Issue:** Two `vapply` calls use `function(c) c$key` and `function(c) c$author` where `c` is the lambda parameter. This shadows the base R function `c()` within the lambda body. While there is no bug here (the code works correctly because `c$key` accesses the list element, not the function), the naming convention conflicts with the project's anti-pattern guidance and could cause confusion if the lambda body were extended.

**Fix:** Rename the lambda parameter to avoid the clash:

```r
keys    <- vapply(citations, function(cit) cit$key,    character(1L))
authors <- vapply(citations, function(cit) cit$author, character(1L))
```

---

_Reviewed: 2026-09-07T12:00:00Z_
_Reviewer: Claude (gsd-code-reviewer)_
_Depth: standard_
