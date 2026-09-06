# Stack Research

**Domain:** R package — multi-format automated AI report rendering (v0.64.0 addition to EventStudy)
**Researched:** 2026-09-06
**Confidence:** MEDIUM (rmarkdown/knitr API verified via official docs and CRAN package pages; version numbers verified on CRAN 2026-09-06; tinytex detection API verified via CRAN refman; officedown/officer versions and dep tree verified on CRAN)

---

## Context: What Already Exists

The following are **already in EventStudy's `Suggests`** and must not be re-added or moved to `Imports`:

| Package | Current Suggests entry | Role in existing code |
|---------|------------------------|----------------------|
| `rmarkdown` | yes (no version pin) | `generate_report()` renderer — `requireNamespace()`-guarded |
| `knitr` | yes (no version pin) | Vignette builder + report code chunks |

The existing `generate_report()` in `R/report.R` already uses `rmarkdown::render()` with `html_document()` and `pdf_document()`. The new `es_report()` wrapper **must call `generate_report()`** as its lower-level renderer, not bypass it. This is a hard architectural constraint from `PROJECT.md`.

---

## Recommended Stack — New Additions for v0.64.0

### Core Report Rendering (already in Suggests — no changes needed)

| Technology | Current CRAN Version | Output Function | Why Sufficient |
|------------|---------------------|-----------------|----------------|
| `rmarkdown` | 2.32 (2026-09-01) | `html_document()`, `pdf_document()`, `word_document()`, `md_document()` | All four required output formats are built-in; no new packages needed for HTML/Word/Markdown |
| `knitr` | 1.52 (2026-09-06) | Code chunk execution engine for `.Rmd` template | Already required for all report rendering; no change |

### New Optional Detection Dependency (add to Suggests)

| Package | Current CRAN Version | Purpose | Why Suggests Not Imports |
|---------|---------------------|---------|--------------------------|
| `tinytex` | 0.60 (2026-06-16) | `tinytex::is_tinytex()` — runtime check whether TinyTeX is the active LaTeX distribution | Only needed for the PDF pre-flight guard; `Sys.which("pdflatex")` covers non-TinyTeX LaTeX installs without this dep. Add to Suggests with `requireNamespace()` guard only. |

### System-Level Requirements (not R packages — document in `SystemRequirements`)

| Requirement | Role | How to Detect in R | Optional? |
|-------------|------|---------------------|-----------|
| pandoc >= 2.8 | All rmarkdown output formats (HTML, PDF, Word, Markdown) | `rmarkdown::pandoc_available("2.8")` | No — required for any rmarkdown render (already a de facto requirement of existing `generate_report()`) |
| pdflatex / xelatex / lualatex | PDF output only | `nchar(Sys.which("pdflatex")) > 0` or `tinytex::is_tinytex()` | Yes — PDF silently skipped if absent, with one `message()` |

---

## Output Format Functions — Which Renders What

| Target Format | rmarkdown Function | File Extension | External Toolchain Beyond pandoc | Confidence |
|---------------|-------------------|----------------|----------------------------------|-----------|
| HTML | `rmarkdown::html_document(toc=TRUE, toc_float=TRUE, theme="flatly", code_folding="hide", self_contained=TRUE)` | `.html` | None — pandoc alone sufficient | MEDIUM (verified via official docs) |
| PDF | `rmarkdown::pdf_document(toc=TRUE)` | `.pdf` | LaTeX engine (pdflatex/xelatex/lualatex) — must guard | MEDIUM (verified via official docs) |
| Word (.docx) | `rmarkdown::word_document(toc=TRUE, reference_docx="default")` | `.docx` | None beyond pandoc — officer/officedown NOT required | MEDIUM (verified via official docs) |
| Markdown | `rmarkdown::md_document(variant="gfm", toc=FALSE, ext=".md")` | `.md` | None — same pandoc requirement as HTML | MEDIUM (verified via official docs) |

The existing `generate_report()` already uses `html_document()` and `pdf_document()`. Extending it to add `word_document()` and `md_document()` requires only adding these two format paths to the existing `format` argument dispatch — no new packages.

---

## Multi-Format Rendering Pattern

Do NOT pass a vector of format names to a single `render()` call relying on YAML frontmatter. That couples format selection to the `.Rmd` template header and makes programmatic control fragile. Use a per-format loop instead:

```r
# Called once per enabled format from es_report()
.render_one_format <- function(template_path, fmt_obj, output_path, params,
                               intermediates_dir) {
  rmarkdown::render(
    input             = template_path,
    output_format     = fmt_obj,
    output_file       = basename(output_path),
    output_dir        = dirname(output_path),
    params            = params,
    intermediates_dir = intermediates_dir,   # prevents temp-file collisions
    envir             = new.env(parent = globalenv()),
    quiet             = TRUE
  )
}
```

The `intermediates_dir` argument is critical when the same `.Rmd` is rendered to multiple formats: without it, each format's `.md` intermediate file overwrites the previous one, causing the second+ renders to silently use stale intermediate state.

---

## Optional Toolchain Guarding Strategy

The offline-first requirement means **no format may hard-stop the entire `es_report()` call**. Each format is independently guarded and silently skipped (with one `message()`) if its toolchain is absent. Only the rmarkdown/knitr/pandoc baseline — which all four formats share — justifies a `stop()`.

```r
# Pre-flight helpers — call before attempting any render
.can_render_any <- function() {
  requireNamespace("rmarkdown", quietly = TRUE) &&
    requireNamespace("knitr",     quietly = TRUE) &&
    rmarkdown::pandoc_available("2.8")
}

.can_render_pdf <- function() {
  # Prefer explicit Sys.which: covers system TeX Live, MacTeX, MiKTeX
  latex_ok <- nchar(Sys.which("pdflatex")) > 0 ||
               nchar(Sys.which("xelatex"))  > 0 ||
               nchar(Sys.which("lualatex")) > 0
  # Augment with tinytex if available (catches PATH-invisible TinyTeX installs)
  if (!latex_ok && requireNamespace("tinytex", quietly = TRUE)) {
    latex_ok <- tinytex::is_tinytex()
  }
  latex_ok
}
```

Format-specific guard matrix for `es_report(formats = c("html", "pdf", "word", "md"))`:

| Format | Pre-flight check | Missing action |
|--------|-----------------|----------------|
| `html` | `.can_render_any()` | `stop()` — HTML is the baseline; if absent the call is meaningless. Matches existing `generate_report()` behavior. |
| `pdf` | `.can_render_any()` + `.can_render_pdf()` | `message("PDF skipped: no LaTeX engine found. Install TinyTeX: tinytex::install_tinytex()")` then continue |
| `word` | `.can_render_any()` (pandoc handles .docx natively) | `message("Word skipped: ...")` — realistically same gate as HTML |
| `md` | `.can_render_any()` (pandoc handles GFM natively) | `message("Markdown skipped: ...")` — realistically same gate as HTML |

---

## Supporting Libraries

| Library | Version | Purpose | When to Use |
|---------|---------|---------|-------------|
| `tinytex` | 0.60 | `is_tinytex()` for TinyTeX detection | Only in PDF pre-flight guard; skip cleanly if absent |
| `DT` | already in Suggests | Interactive result tables in HTML output | Already present; use in HTML report if available |

---

## Development Tools

| Tool | Purpose | Notes |
|------|---------|-------|
| pandoc >= 2.8 | Document conversion engine for all four formats | System requirement, not an R package. Check with `rmarkdown::pandoc_available()`. |
| TinyTeX (optional) | LaTeX distribution for PDF | Install with `tinytex::install_tinytex()` if system LaTeX absent |

---

## Installation

```r
# Only new addition needed in DESCRIPTION Suggests:
install.packages("tinytex")   # for PDF toolchain detection

# All other required packages already present in EventStudy Suggests:
# rmarkdown (2.32), knitr (1.52)

# For PDF output, users need LaTeX — recommend TinyTeX:
tinytex::install_tinytex()    # one-time user setup, not a package dep
```

---

## Alternatives Considered

| Recommended | Alternative | When to Use Alternative |
|-------------|-------------|-------------------------|
| `rmarkdown::word_document()` | `officedown::rdocx_document()` | Only if rich cross-references (bookdown), branded corporate templates with custom table/list styles, or officer-specific formatting blocks are required. None of those apply to this report. |
| `rmarkdown::md_document(variant="gfm")` | `variant="markdown_strict"` | Strict is better for max portability, but GFM is more readable on GitHub and in standard Markdown viewers — the natural distribution channel for a research report Markdown. |
| Per-format `render()` loop | Single `render("all")` | "all" requires YAML frontmatter to list formats, making format selection a template concern rather than a caller concern. The loop is fully programmatic and caller-controlled. |
| `Sys.which("pdflatex")` + optional `tinytex::is_tinytex()` | Hard-require `tinytex` in Suggests for all PDF detection | Many users have system LaTeX (TeX Live, MacTeX, MiKTeX) without TinyTeX. `Sys.which` catches them all; tinytex is an enhancement only for TinyTeX-specific PATH-visibility edge cases. |
| Keep `generate_report()` as the low-level renderer | Replace or rewrite `generate_report()` | Backward-compat is non-negotiable per PROJECT.md; existing callers must not break. `es_report()` is additive, composing `generate_report()` under the hood. |

---

## What NOT to Add

| Avoid | Why | Use Instead |
|-------|-----|-------------|
| `officedown` in Suggests | Pulls in officer (>=0.6.7), rvg, xml2, uuid, memoise — ~6 transitive deps for features not needed in a standard statistical report | `rmarkdown::word_document(reference_docx="default")` |
| `officer` in Suggests | Only useful as an officedown dependency; no direct use case in the report | Not needed |
| `quarto` CLI dependency | Separate binary runtime, not a CRAN package; adds opaque system requirement; rmarkdown already covers all four formats natively | `rmarkdown` |
| `pagedown` | HTML-to-PDF via headless Chrome — heavyweight, fragile in CI, adds `chromote`/`processx` deps | `rmarkdown::pdf_document()` + LaTeX |
| `reporttools`, `reporter`, `r2rtf` | Narrow-scope table/RTF generators; no standard Rmd integration; parallel non-Rmd rendering path required | `rmarkdown` + pandoc |
| Moving `rmarkdown` or `knitr` to `Imports` | Makes report rendering a hard dep for all users who never call `es_report()` — violates CRAN Suggests discipline | Keep in `Suggests` with `requireNamespace()` guard |
| `bookdown` output formats (`bookdown::html_document2`, `bookdown::pdf_document2`) | Pulls `bookdown` dep; cross-references are not needed in a single-file statistical report; adds complexity | `rmarkdown::html_document()` / `rmarkdown::pdf_document()` |

---

## DESCRIPTION Changes Required

```
Suggests:
    ...existing entries...,
    tinytex          # NEW: optional PDF toolchain detection (tinytex::is_tinytex())
```

No version pin needed for tinytex — `is_tinytex()` has been stable since early versions; any modern CRAN release works.

`rmarkdown` and `knitr` Suggests entries are already present — do not add version pins; unpinned Suggests entries avoid unnecessary `R CMD check` warnings on older R environments.

Add or extend `SystemRequirements: pandoc (>= 2.8)` — this was already implied by the existing `generate_report()` but should be made explicit for v0.64.0.

---

## Version Compatibility

| Package | Minimum Compatible | Rationale |
|---------|-------------------|-----------|
| `rmarkdown` | >= 2.14 | `html_document(code_folding)` and `md_document(variant="gfm")` stable; `params` passthrough via `render()` stable |
| `knitr` | >= 1.37 | `params` passing in `render()` stable across this range |
| `tinytex` | >= 0.50 | `is_tinytex()` has been stable since well before this version; no strict pin needed |
| pandoc | >= 2.8 | rmarkdown 2.32 `SystemRequirements` specifies this |
| R | >= 4.1.0 | Existing package constraint — unchanged |

---

## Stack Patterns by Variant

**If the user requests PDF and LaTeX is absent:**
- Emit one `message()` with install instructions (`tinytex::install_tinytex()`)
- Skip PDF silently
- Continue rendering remaining formats

**If the user requests Word on a system without pandoc:**
- Emit one `message()` (same pre-flight as HTML)
- Word format requires the same pandoc gate as HTML; in practice, if HTML renders, Word renders

**If no LLM provider is configured:**
- `es_advise()` falls back to rule-based offline engine
- Report renders with rule-based narrative — identical structure, no format changes needed

**If `es_report()` is called without any format argument:**
- Default to `formats = "html"` — the safest, zero-extra-toolchain format
- Users opt in to PDF/Word/Markdown explicitly

---

## Sources

- CRAN rmarkdown package page — version 2.32, 2026-09-01, pandoc >= 2.8 system requirement (MEDIUM): https://cran.r-project.org/web/packages/rmarkdown/index.html
- rmarkdown `html_document()` reference — full signature verified (MEDIUM): https://rmarkdown.rstudio.com/docs/reference/html_document.html
- rmarkdown `word_document()` reference — signature + `reference_docx` behavior verified (MEDIUM): https://rmarkdown.rstudio.com/docs/reference/word_document.html
- rmarkdown `md_document()` reference — variant options and `ext` parameter verified (MEDIUM): https://rmarkdown.rstudio.com/docs/reference/md_document.html
- rmarkdown `render()` reference — multi-format vector behavior + `intermediates_dir` (MEDIUM): https://pkgs.rstudio.com/rmarkdown/reference/render.html
- rmarkdown `pandoc_available()` reference — signature and usage verified (MEDIUM): https://search.r-project.org/CRAN/refmans/rmarkdown/html/pandoc_available.html
- CRAN knitr package page — version 1.52, 2026-09-06 (MEDIUM): https://cran.r-project.org/web/packages/knitr/index.html
- CRAN tinytex package page — version 0.60, 2026-06-16 (MEDIUM): https://cran.r-project.org/web/packages/tinytex/index.html
- tinytex `is_tinytex()` reference — detection logic and return value verified (MEDIUM): https://search.r-project.org/CRAN/refmans/tinytex/html/is_tinytex.html
- CRAN officedown — version 0.4.1, dep tree (officer, rvg, xml2, uuid, memoise) confirmed (MEDIUM): https://cran.r-project.org/web/packages/officedown/index.html
- CRAN officer — version 0.7.6, 2026-07-16 (MEDIUM): https://cran.r-project.org/web/packages/officer/index.html
- officedown vs word_document comparison — feature differences confirmed (MEDIUM): https://rdrr.io/cran/officedown/man/rdocx_document.html
- R Packages (2e) — Suggests guard pattern (`requireNamespace()` + `stop()` vs graceful degradation) (MEDIUM): https://r-pkgs.org/dependencies-in-practice.html

---
*Stack research for: R package multi-format automated AI report rendering (v0.64.0)*
*Researched: 2026-09-06*
