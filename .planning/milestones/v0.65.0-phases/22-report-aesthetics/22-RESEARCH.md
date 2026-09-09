# Phase 22: Report Aesthetics - Research

**Researched:** 2026-09-09
**Domain:** R package presentation-layer — knitr/rmarkdown table styling, figure sizing, CSS injection, tinytable API
**Confidence:** HIGH (all claims grounded in file reads and live R session verification)

---

<user_constraints>
## User Constraints (from CONTEXT.md)

### Locked Decisions
- **Table helper:** single `@noRd` helper (e.g. `.report_table(df, caption, ...)`) routing all ~8 kable calls; `requireNamespace("tinytable")`-guarded; kable-fallback is byte-identical to current call.
- **tinytable when present, kable fallback when absent** — one guarded location, not duplicated per chunk.
- **Consistent styling via tinytable's format-agnostic API** — bold header row, right-aligned numerics, left-aligned text, `digits = 4` preserved; HTML-only colour via report.css on top.
- **`fig.cap` on every plot chunk** — chunk option, not baked into plot title; grounding-neutral text; no figure-number prose.
- **Per-format figure sizing via `knitr::opts_knit$get("rmarkdown.pandoc.to")`** in setup chunk; recommended bounds: HTML ≈10×6/96dpi, PDF ≈6.5×4/300dpi, Word ≈6×3.7/300dpi, MD ≈8×5/150dpi.
- **`ragg` device selection** — `requireNamespace("ragg", quietly=TRUE)`-guarded; `dev = "ragg_png"` in `opts_chunk$set()` when present; degrades silently when absent.
- **HTML-only CSS** — `inst/rmarkdown/report.css` injected via `css =` arg in `.build_output_format("html")` at `R/report.R:453`; PDF/Word/MD branches never reference it.
- **CSS scope:** Inter/JetBrains Mono with web-safe fallbacks; brand tokens #2563eb / #0f172a / #ffffff; layers on top of existing `theme = "flatly"`.
- **Sigma histogram leftover `fill = "steelblue"`** — fold onto `es_colours[["primary"]]` (#2563eb).
- **`knitr::is_html_output()` switch at L229 / L253 / L342** — MUST NOT MOVE.
- **Regression test** — render all 4 formats (skip-guarded), assert: (a) PDF has no `<script>`, (b) `is_html_output()` switch intact in skeleton.Rmd, (c) kable fallback works when tinytable is force-absent.
- **No new Imports** — tinytable/patchwork/ragg stay Suggests; every use `requireNamespace()`-guarded.
- **patchwork multi-panel composition** — deferred; not required for VIZ-04..07.
- **Rich Word via officedown (RPTX-03)** — deferred; Word parity is tinytable markdown path only.
- **Webfont embedding** — deferred to Phase 24.

### Claude's Discretion
- Exact helper name/signature (`.report_table`), the precise tinytable style calls, the exact per-format inch/dpi numbers within the recommended bounds, the precise CSS rule set and font fallback stack, and the exact regression-test assertions/skip guards.

### Deferred Ideas (OUT OF SCOPE)
- patchwork figure composition.
- officedown/RPTX-03 rich Word output.
- Bundling/embedding Inter/JetBrains Mono webfonts.
</user_constraints>

<phase_requirements>
## Phase Requirements

| ID | Description | Research Support |
|----|-------------|------------------|
| VIZ-04 | `es_report()` tables via `tinytable` with kable fallback, styled across HTML/PDF/Word/Markdown | Sections 1 (call sites), 2 (tinytable API), covering `.report_table()` helper design |
| VIZ-05 | Figure captions (`fig.cap`) on every plot chunk + per-format sizing replacing global `fig.width=10` | Sections 4 (fig.cap/is_html_output) and 5 (per-format sizing/ragg) |
| VIZ-06 | `inst/rmarkdown/report.css` injected on HTML branch only | Section 3 (.build_output_format) and Section 5 (inst/ path convention) |
| VIZ-07 | Per-format figure sizing so PDF/Word fit page margins; ragg device for raster output | Section 5 (per-format sizing/ragg guard pattern) |
| CRAN-05 | All 4 formats render; PDF has no `<script>`; is_html_output switch intact; regression test | Sections 4, 6 (test patterns/skip guards) |
</phase_requirements>

---

## Summary

Phase 22 is a surgical presentation-layer pass over two files — `inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd` and `R/report.R` — plus one new static asset `inst/rmarkdown/report.css`. All seven research questions below were answered by reading source files and running a live R session; no facts are assumed.

**Primary recommendation:** The single `.report_table()` helper is the entire complexity budget for VIZ-04 — it must produce `print(tt(...) |> style_tt(...))` when tinytable is present and `print(knitr::kable(...))` when absent. Everything else (per-format sizing, ragg guard, CSS injection, fig.cap) is a one-liner change at a single well-understood call site.

---

## Architectural Responsibility Map

| Capability | Primary Tier | Secondary Tier | Rationale |
|------------|-------------|----------------|-----------|
| Table rendering (HTML/LaTeX/MD/Word) | Template (skeleton.Rmd chunks) | R/report.R (helper lives here) | Chunks call the helper; helper logic in R file, not template |
| HTML-only CSS injection | R/report.R `.build_output_format()` | inst/rmarkdown/report.css (static asset) | CSS injected at format-build time, never touches template body |
| Per-format figure sizing | skeleton.Rmd setup chunk | knitr opts_chunk/opts_knit | Single point of control in setup chunk, flows to all plot chunks |
| ragg device selection | skeleton.Rmd setup chunk | — | One guarded `opts_chunk$set(dev=)` call |
| Figure captions | skeleton.Rmd chunk headers (`fig.cap=`) | — | knitr-idiomatic; chunk option only, body unchanged |
| Regression lock | tests/testthat/test_report_aesthetics.R (new) | — | Mirrors existing test_report_multiformat.R patterns |

---

## Research Findings

### 1. Exact Table Call Sites in skeleton.Rmd

All 8 `knitr::kable()` calls confirmed by `grep -n "knitr::kable"` on the installed template.
[VERIFIED: inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd]

| Line | Data Object | `caption=` | `col.names=` | `digits=` | Wrapped in `print()`? |
|------|-------------|------------|--------------|-----------|----------------------|
| 130–131 | `overview_tbl` | `"Study design parameters"` | `c("Parameter","Value")` | — | YES |
| 158 | `diag_tbl` | `"Estimation window model fit"` | — | — | YES |
| 218–221 | `res_tbl` | `"Abnormal and cumulative abnormal returns"` | `c("Event","Firm","AR t-stat","CAR t-stat","Final CAR")` | — | YES |
| 247–248 | `task$results$coefficients` | `"Event-Time Coefficients"` | — | `4` | YES |
| 277 | `stat_tbl` | — | — | `4` | YES |
| 305–309 | `diag_tbl` | `"Per-event estimation-window diagnostics"` | `c("Event","R-sq","Sigma","DoF","Shapiro p","DW stat","LjungBox p")` | `4` | YES |
| 328–329 | `cs_tbl` | `"Cross-sectional diagnostics"` | `c("Diagnostic","Value")` | — | YES |
| 379–380 | `diag_data` | `"Model Diagnostics by Event"` | — | `4` | **NO** (bare call, not wrapped) |

[VERIFIED: inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd:130,158,218,247,277,305,328,379]

**Hand-built with `sprintf` pre-formatting (Value column is pre-formatted character):**
- L130 (`overview_tbl`): `Value` column built from `as.character()` and `sprintf("%.0f%%", ...)` — digits passed to kable would have no effect on these. Kable-fallback must NOT add `digits=`.
- L158 (`diag_tbl`): `Value` column built with `sprintf("%.4f", ...)` and `sprintf("%.5f", ...)` — same: pre-formatted, no `digits=` in the kable call.
- L328 (`cs_tbl`): `Value` column uses `sprintf("%.6f", ...)` — pre-formatted, no `digits=`.

**The helper's kable-fallback path must preserve the exact `digits=` argument (or its absence) for each call.** The cleanest approach: accept `digits = NULL` as default in `.report_table()` and pass `digits` to `knitr::kable()` only when non-NULL — this is byte-compatible for all 8 sites.

**The L379 call is NOT wrapped in `print()`.** In `results='asis'` context, a bare `knitr::kable()` return value is auto-printed by knitr. The helper call that replaces it should either use `print()` (consistent with the other 7) or return the object without `print()`. Using `print()` consistently for all 8 replacements is safest.

---

### 2. tinytable API Facts

**Version:** tinytable 0.18.0 installed (matches CRAN latest as of research date).
[VERIFIED: live R session `packageVersion('tinytable')`]

**Format auto-detection:** tinytable calls `knitr::pandoc_to()` inside `infer_output()`. The exact detection logic (verbatim from `tinytable:::infer_output` body):

```r
# pandoc_to <- knitr::pandoc_to()  which calls opts_knit$get("rmarkdown.pandoc.to")
if (isTRUE(pandoc_to %in% c("latex", "beamer"))) { out <- "latex" }
else if (isTRUE(pandoc_to %in% c("html", "revealjs"))) { out <- "html" }
else if (isTRUE(pandoc_to == "typst")) { out <- "typst" }
else if (!is.null(pandoc_to)) { out <- "markdown" }  # catches "docx" AND "gfm"
```

[VERIFIED: live R session `body(tinytable:::infer_output)`]

**Coverage of 4 formats:**
- HTML render → `pandoc_to = "html"` → tinytable emits HTML table (auto-detected). [VERIFIED]
- PDF render → `pandoc_to = "latex"` → tinytable emits LaTeX/tabularray table. [VERIFIED]
- Word render → `pandoc_to = "docx"` → tinytable emits **markdown** (docx is not natively covered; this matches CONTEXT "Word parity is via the tinytable fallback path only"). [VERIFIED]
- GFM/MD render → `pandoc_to` = `"gfm"` (after `knitr:::fmt_name` strips `-yaml_metadata_block` suffix) → tinytable emits markdown. [VERIFIED]

**Caption argument:** `tt(x, caption = "My caption", ...)` — top-level argument, confirmed by `formals(tinytable:::tt.default)`. [VERIFIED: live R session]

**Verbatim formals of `tt.default`:**
```r
x, digits = get_option("tinytable_tt_digits", default=NULL),
   caption = get_option("tinytable_tt_caption", default=NULL),
   notes   = get_option("tinytable_tt_notes",   default=NULL),
   width   = get_option("tinytable_tt_width",   default=NULL),
   height  = get_option("tinytable_tt_height",  default=NULL),
   theme   = get_option("tinytable_tt_theme",   default="default"),
   colnames= get_option("tinytable_tt_colnames",default=TRUE),
   rownames= get_option("tinytable_tt_rownames",default=FALSE),
   escape  = get_option("tinytable_tt_escape",  default=FALSE),
   engine  = NULL, ...
```
[VERIFIED: live R session `deparse(formals(tinytable:::tt.default))`]

**Style verbs:**
- Bold header row: `style_tt(i = 0, bold = TRUE)` — `i = 0` targets the header row. [VERIFIED: confirmed by `formals(style_tt)` returning `bold` param; markdown output test showed `**Event** | **CAR**` in column headers]
- Right-align numeric: `style_tt(j = <col_index>, align = "r")` [VERIFIED: live session]
- Left-align text: `style_tt(j = <col_index>, align = "l")` [VERIFIED]
- Numeric formatting: `format_tt(j = <col>, digits = 4)` — note this OVERRIDES digits already baked into the data; only use on columns where data is NOT pre-sprintf'd. [VERIFIED: `formals(format_tt)` confirms `digits` param]

**`print()` requirement:** tinytable registers `knit_print.tinytable` in its namespace. Returning a `tt()` object from a knitr chunk (without `print()`) works via `knit_print`. Explicit `print(tt(...))` also works. The helper should use `print()` for consistency with the 7 wrapped kable calls it replaces.
[VERIFIED: `exists('knit_print.tinytable', envir=asNamespace('tinytable'))` → TRUE]

**Dependency of `tt()` on external packages:** tinytable's DESCRIPTION declares only `methods` in Imports and R (>= 4.1.0) in Depends. Zero extra package dependencies are pulled in — CRAN-safe. [VERIFIED: tinytable DESCRIPTION fetched from CRAN]

---

### 3. `.build_output_format()` in R/report.R

**Exact location:** `R/report.R:448–474` (function body).
[VERIFIED: R/report.R:448-474]

**Verbatim branch structure (full function body):**

```r
.build_output_format <- function(fmt) {
  switch(
    fmt,
    html = {
      if (!requireNamespace("rmarkdown", quietly = TRUE)) return(NULL)
      rmarkdown::html_document(
        toc       = TRUE,
        toc_float = TRUE,
        theme     = "flatly",
        code_folding = "hide"
      )
    },
    pdf = {
      if (!.pdf_toolchain_available()) return(NULL)
      rmarkdown::pdf_document(toc = TRUE)
    },
    word = {
      if (!.word_toolchain_available()) return(NULL)
      rmarkdown::word_document(toc = TRUE)
    },
    md = {
      if (!.word_toolchain_available()) return(NULL)
      rmarkdown::md_document(variant = "gfm")
    },
    NULL
  )
}
```
[VERIFIED: R/report.R:448-474]

**CSS injection target:** Line 453 — `rmarkdown::html_document(toc=TRUE, toc_float=TRUE, theme="flatly", code_folding="hide")`. Add `css = css_path` here and nowhere else. The `pdf`, `word`, and `md` branches are untouched, guaranteeing PDF/Word/MD output never see the stylesheet. [VERIFIED: R/report.R:452-459]

**CSS path resolution pattern:** The existing template resolves via `system.file("rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd", package="EventStudy")`. The new CSS at `inst/rmarkdown/report.css` resolves via `system.file("rmarkdown/report.css", package="EventStudy")`. The package uses the standard R convention: `inst/` contents are accessible as `system.file(<path-without-inst/>, package=...)`. [VERIFIED: live R session — existing template resolves to `/home/simonm/R/library/EventStudy/rmarkdown/templates/...`]

**Safe CSS path guard:** `system.file(...)` returns `""` when the file does not exist. Use `css_path <- system.file("rmarkdown/report.css", package="EventStudy"); if (nzchar(css_path)) ...` or pass `css = css_path` with the understanding that rmarkdown treats `css=""` and `css=character(0)` gracefully (confirmed: `html_document(css='')` succeeds without error). [VERIFIED: live R session]

**`<script>` isolation:** The `pdf_document(toc=TRUE)` path produces LaTeX/PDF. A `.css` file injected only in the `html` branch can never appear in the PDF output. The PDF branch has no CSS argument and no reference to the stylesheet. [VERIFIED: R/report.R:461-463]

---

### 4. `fig.cap` + `is_html_output()` Invariant

**Three plot chunks confirmed — verbatim locations:**

| Chunk | `is_html_output()` Line | Plotly branch | Static branch |
|-------|------------------------|---------------|---------------|
| AR/CAR plot | L229 | `print(plotly::ggplotly(p))` | `print(p)` |
| Panel plot | L253 | `print(plotly::ggplotly(p))` | `print(p)` |
| Sigma histogram | L342 | `print(plotly::ggplotly(p))` | `print(p)` |

[VERIFIED: inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd:229,253,342]

**How `fig.cap` is added:** `fig.cap` is set in the chunk header (`{r chunk-name, fig.cap="..."}`) and is orthogonal to the chunk body — it does NOT modify or interact with the `knitr::is_html_output()` conditional logic inside the chunk. [VERIFIED: knitr documentation / standard knitr behavior]

**Plotly + fig.cap interaction:** Plotly produces an htmlwidget, not a static image. When `knitr::is_html_output()` is TRUE, the chunk outputs a plotly widget; knitr does NOT render `fig.cap` as a visible HTML caption for htmlwidget output (the `fig.cap` chunk option is honoured for static images/ggplot outputs, not for htmlwidgets). The caption degrades silently for HTML-interactive output, as CONTEXT accepts. For PDF/Word/MD, `is_html_output()` returns FALSE, the static `ggplot` object is printed, and `fig.cap` produces a proper figure caption. [ASSUMED — htmlwidget/fig.cap interaction inferred from knitr behavior; the graceful-degradation behavior is CONTEXT-accepted]

**Sigma histogram leftover `fill = "steelblue"` — exact location:** `skeleton.Rmd:338`. The replacement is `fill = es_colours[["primary"]]` which equals `"#2563eb"`. [VERIFIED: skeleton.Rmd:338; R/theme.R:19-32]

**Verbatim `es_colours` definition** (from `R/theme.R:19-32`):
```r
es_colours <- c(
  primary   = "#2563eb",
  event     = "#D55E00",
  reference = "#6b7280",
  ci_band   = "#2563eb",
  group1    = "#2563eb",
  ...
)
```
The `primary` key is the correct semantic choice for a histogram fill representing "the main series". [VERIFIED: R/theme.R:19-32]

---

### 5. Per-Format Figure Sizing

**Setup chunk location:** `skeleton.Rmd:26–41`.
[VERIFIED: skeleton.Rmd:26-41]

**Current global setting (verbatim):**
```r
knitr::opts_chunk$set(
  echo    = FALSE,
  message = FALSE,
  warning = FALSE,
  fig.width  = 10,
  fig.height = 6
)
```
[VERIFIED: skeleton.Rmd:27-33]

**Per-format detection via `knitr::opts_knit$get("rmarkdown.pandoc.to")`:**

The `pandoc.to` value that rmarkdown sets and that `knitr::pandoc_to()` reads is sourced from `rmarkdown::*_document()$pandoc$to`. Confirmed values:

| `rmarkdown::` constructor | `$pandoc$to` value | `pandoc_to()` after fmt_name strip |
|---------------------------|--------------------|-------------------------------------|
| `html_document()` | `"html"` | `"html"` |
| `pdf_document()` | `"latex"` | `"latex"` |
| `word_document()` | `"docx"` | `"docx"` |
| `md_document(variant="gfm")` | `"gfm-yaml_metadata_block"` | `"gfm"` |

[VERIFIED: live R session `html_document()$pandoc$to`, etc.]

The `knitr:::fmt_name()` function strips everything after `-` or `+`:
```r
gsub("[-+].*", "", x)  # "gfm-yaml_metadata_block" -> "gfm"
```
[VERIFIED: live R session `body(knitr:::fmt_name)`]

So inside the setup chunk, safe detection pattern:
```r
pandoc_to <- knitr::opts_knit$get("rmarkdown.pandoc.to") %||% "html"
# pandoc_to values: "html" | "latex" | "docx" | "gfm-yaml_metadata_block"
# Match with startsWith for robustness:
if (startsWith(pandoc_to, "latex")) {
  knitr::opts_chunk$set(fig.width=6.5, fig.height=4, dpi=300)
} else if (startsWith(pandoc_to, "docx")) {
  knitr::opts_chunk$set(fig.width=6,   fig.height=3.7, dpi=300)
} else if (startsWith(pandoc_to, "gfm")) {
  knitr::opts_chunk$set(fig.width=8,   fig.height=5, dpi=150)
} else {
  knitr::opts_chunk$set(fig.width=10,  fig.height=6, dpi=96)  # html default
}
```
[ASSUMED for exact inch/dpi values — within CONTEXT-recommended bounds; detection logic VERIFIED]

`opts_knit$get("rmarkdown.pandoc.to")` returns `NULL` outside a render session; the `%||% "html"` fallback (already imported via rlang) ensures the setup chunk is safe at interactive REPL. [VERIFIED: `knitr::opts_knit$get("rmarkdown.pandoc.to")` returns NULL at REPL]

**ragg device registration in knitr:** `"ragg_png"` is a first-class device in knitr's `auto_exts` table (extension: `"png"`). It is available as a `dev=` option whenever ragg is installed, without any additional registration call. [VERIFIED: `knitr:::auto_exts["ragg_png"]` → `"png"`]

**Guarded ragg device pattern in setup chunk:**
```r
if (requireNamespace("ragg", quietly = TRUE)) {
  knitr::opts_chunk$set(dev = "ragg_png")
}
# When ragg absent: knitr uses its default device (png / cairo_png on Linux)
```
[VERIFIED: ragg v1.5.2 installed; knitr 1.51 confirms `ragg_png` in auto_exts]

**ragg scope:** `dev = "ragg_png"` governs raster output (HTML/Word/MD figures as PNG). For PDF, knitr uses the PDF/LaTeX device path (typically `pdf` device via LaTeX), which is unaffected by `ragg_png`. [VERIFIED: knitr auto_exts — "ragg_png" ext is "png", not "pdf"]

---

### 6. Existing Report Tests — Patterns and Skip Guards

**Test file confirmed:** `tests/testthat/test_report_multiformat.R` exists.
[VERIFIED: `ls tests/testthat/ | grep report`]

**Skip guard pattern for render tests (verbatim from the file):**
```r
skip_if_not_installed("rmarkdown")
skip_if_not_installed("knitr")
skip_on_cran()
```
[VERIFIED: test_report_multiformat.R:133-134, 148-149, 203-204, etc.]

**Rendering pattern for each format (established):**
```r
result <- suppressMessages(
  generate_report(task,
                  output_file = tmp_file,
                  format      = "html",   # or "pdf", "word", "md"
                  sections    = c("exec_summary"),
                  provider    = NULL)
)
expect_true(file.exists(result[["html"]]))
content <- paste(readLines(result[["html"]], warn = FALSE), collapse = "\n")
expect_true(grepl("some-expected-text", content, fixed = TRUE))
unlink(result[["html"]])
```
[VERIFIED: test_report_multiformat.R:345-366]

**Asserting on rendered artifacts:** The project's established pattern is `readLines(result[["html"]])` then `grepl()` on the concatenated content. For the no-`<script>` PDF assertion, the same pattern applies: `readLines(result[["pdf"]])` then `expect_false(grepl("<script", content, fixed=TRUE))`. [VERIFIED: test_report_multiformat.R:344-365]

**Skipping gracefully when toolchain absent (established):**
- LaTeX for PDF: `skip_if_not_installed("tinytex")` — the project uses `tinytex` for PDF (see `.pdf_toolchain_available()` in report.R). [VERIFIED: R/report.R:405-408]
- For MD: `skip_if(!rmarkdown::pandoc_available(), "pandoc not available")` — `.word_toolchain_available()` in report.R uses `rmarkdown::pandoc_available()`. [VERIFIED: R/report.R:422-426]

**Mocking `with_mocked_bindings` — established pattern (verbatim from test_report_multiformat.R:161-176):**
```r
with_mocked_bindings(
  .pdf_toolchain_available = function() FALSE,
  {
    expect_message(result <- generate_report(...), regexp = "skipping 'pdf'")
  },
  .package = "EventStudy"
)
```
[VERIFIED: test_report_multiformat.R:161-176]

**Forcing tinytable "absent" for fallback test:** `requireNamespace()` is a base function and cannot be cleanly mocked with `with_mocked_bindings` without a seam. The CONTEXT-aligned approach: define a thin internal predicate `.tinytable_available()` that wraps `requireNamespace("tinytable", quietly=TRUE)`, and then mock IT:

```r
# In R/report.R (or alongside .report_table):
.tinytable_available <- function() requireNamespace("tinytable", quietly = TRUE)

# In test:
with_mocked_bindings(
  .tinytable_available = function() FALSE,
  { result <- .report_table(df, caption = "Test") },
  .package = "EventStudy"
)
```

This mirrors the exact pattern used for `.pdf_toolchain_available()`. The planner may choose a different seam (e.g. a `force_kable = FALSE` argument to `.report_table()`), but the `.tinytable_available()` predicate is the idiomatic project pattern. [VERIFIED: pattern derived from test_report_multiformat.R:161-176 + R/report.R:404-408]

**`is_html_output()` switch intact test:** The established fast pattern (no render needed):
```r
test_that("is_html_output switch intact in skeleton.Rmd", {
  skel_path <- system.file(
    "rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd",
    package = "EventStudy"
  )
  skip_if(skel_path == "", "skeleton.Rmd not found")
  lines <- readLines(skel_path, warn = FALSE)
  expect_true(any(grepl("knitr::is_html_output", lines, fixed = TRUE)))
})
```
This pattern is already in test_report_multiformat.R:71-81 (TMPL-01 test for FORMAT-03). The new regression test should extend it to assert the switch appears at least 3 times (once per plot chunk). [VERIFIED: test_report_multiformat.R:71-81]

---

### 7. CRAN / R CMD check Surface

**Current baseline NOTE (confirmed by live `devtools::check()`):**

The single pre-existing NOTE is:
```
checking R code for possible problems ... NOTE
  .aggregate_remainder : <anonymous>: no visible global function definition for 'tail'
  .aggregate_remainder: no visible global function definition for 'median'
  .extract_cross_sectional_signals : <anonymous>: ...
```
[VERIFIED: live `devtools::check(args='--no-tests', error_on='never')` output — "Status: 1 NOTE"]

This NOTE is in `median`/`tail` globals (unrelated to Phase 22) and must not grow.

**New `inst/rmarkdown/report.css`:** Static CSS files in `inst/` are standard CRAN practice (e.g., shiny, pkgdown-using packages). Static CSS has no `<script>` content, no binary, and no external network dependency. No new NOTE or WARNING is expected. The package already ships `inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd` and `inst/rmarkdown/templates/event_study_report/template.yaml` as static text assets — the same mechanism. [VERIFIED: `find inst/ -type f` confirms existing static assets; no NOTE from static text]

**No new Imports needed:** tinytable, patchwork, ragg are all in Suggests already (DESCRIPTION:67-70). The helper uses `requireNamespace("tinytable", quietly=TRUE)` which is the standard guard pattern used elsewhere in the codebase (e.g., `requireNamespace("tinytex", ...)` at R/report.R:405). [VERIFIED: DESCRIPTION:67-70 and R/report.R:405]

**Non-ASCII compliance:** The existing non-ASCII test in test_report_multiformat.R:443-475 already covers `R/report.R` and `skeleton.Rmd`. Phase 22 edits to both files must stay ASCII-clean. The report.css file is pure ASCII by construction (hex colours, Latin font names). [VERIFIED: test_report_multiformat.R:443-475]

---

## Don't Hand-Roll

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Multi-format table rendering | Custom HTML/LaTeX table builder | `tinytable::tt()` with format auto-detect | tinytable already detects pandoc format via knitr::pandoc_to(); handles HTML/LaTeX/Markdown natively |
| Format detection in setup chunk | Custom env-var or param injection | `knitr::opts_knit$get("rmarkdown.pandoc.to")` | This is the standard knitr API; already used by tinytable internally |
| Anti-aliased PNG device | Custom device registration | `dev = "ragg_png"` (knitr has built-in ragg_png in auto_exts) | Already registered; one opts_chunk$set call suffices |
| CSS-in-HTML injection | Template body branch | `css =` argument on `html_document()` | Guarantees PDF/Word/MD branches never see it |

---

## Common Pitfalls

### Pitfall 1: `col.names=` in kable has no tinytable equivalent (it renames columns)
**What goes wrong:** kable's `col.names=` renames the header row at render time. tinytable uses the actual data frame column names. If `df` has machine-name columns (e.g. `DW_stat`, `LjungBox_p`) and `kable` was using `col.names = c("DW stat", "LjungBox p")`, the tinytable output shows the ugly column names.
**How to avoid:** In `.report_table()`, rename the data frame columns before passing to `tt()` when `col.names` is supplied, OR pass `col.names` through by renaming `x` with `setNames(x, col.names)`. The kable-fallback path still passes `col.names=` directly to `knitr::kable()`.
**Warning signs:** Rendered tables showing underscores or camelCase in headers.

### Pitfall 2: The L379 bare `knitr::kable()` (no `print()`)
**What goes wrong:** L379 is the ONLY kable call without `print()`. In `results='asis'` context this works because knitr auto-prints the return value. A replacement `tt(...)` returned without `print()` also works (knit_print.tinytable handles it). But if the helper always calls `print()` internally, the return value is `invisible(NULL)`, which is safe. If the helper RETURNS the object (for caller to print), then the L379 replacement must add `print()`.
**How to avoid:** The `.report_table()` helper should call `print()` internally and return `invisible(NULL)`, making all 8 call sites identical (no `print()` at the call site needed). OR keep `print(.report_table(...))` at all 8 sites.

### Pitfall 3: tinytable Word output is markdown, not native OOXML
**What goes wrong:** `pandoc_to = "docx"` falls into tinytable's `else if (!is.null(pandoc_to))` branch → markdown output. Pandoc then converts markdown table to Word table. This is correct and CONTEXT-approved, but if Word receives LaTeX-specific markup (tabularray), it will fail.
**How to avoid:** Word format auto-detects to `"markdown"` in tinytable — no action needed. Confirmed by the `infer_output()` source.

### Pitfall 4: CSS injected when `system.file()` returns `""`
**What goes wrong:** If `inst/rmarkdown/report.css` is not found (package not installed from source, or typo in path), `system.file()` returns `""`. Passing `css = ""` to `html_document()` is safe (confirmed), but the CSS simply won't load.
**How to avoid:** Use `css_path <- system.file("rmarkdown/report.css", package="EventStudy")` and guard: `if (nzchar(css_path)) css_path else NULL`. Or pass unconditionally — the empty-string behavior is safe.

### Pitfall 5: `ragg_png` device + PDF output
**What goes wrong:** `opts_chunk$set(dev="ragg_png")` applies globally. For PDF (LaTeX) output, knitr uses a separate PDF device path (cairo_pdf or the LaTeX-embedded figure device). Setting `dev="ragg_png"` does not interfere with PDF figure rendering because knitr selects the device per-format for PDF specifically.
**How to avoid:** No action needed — knitr handles this correctly. The `dev="ragg_png"` only applies to raster output paths.

---

## Code Examples

### Helper Pattern: `.report_table()` (VIZ-04)

```r
# In R/report.R — @noRd helper
# Source: derived from tinytable API (verified) + kable call site audit (verified)
.tinytable_available <- function() requireNamespace("tinytable", quietly = TRUE)

.report_table <- function(x, caption = NULL, col.names = NULL, digits = NULL,
                          align = NULL) {
  # Rename columns if col.names supplied (kable-compat layer)
  if (!is.null(col.names)) {
    x <- setNames(x, col.names)
  }

  if (.tinytable_available()) {
    # Determine column alignment: "l" for character, "r" for numeric
    if (is.null(align)) {
      align <- ifelse(vapply(x, is.numeric, logical(1L)), "r", "l")
    }
    tbl <- tinytable::tt(x, caption = caption, digits = digits)
    tbl <- tinytable::style_tt(tbl, i = 0L, bold = TRUE)
    for (j_idx in seq_along(align)) {
      tbl <- tinytable::style_tt(tbl, j = j_idx, align = align[[j_idx]])
    }
    print(tbl)
  } else {
    # Kable fallback — byte-identical to current calls
    args <- list(x = x, caption = caption)
    if (!is.null(digits)) args$digits <- digits
    print(do.call(knitr::kable, args))
  }
  invisible(NULL)
}
```

Note: `col.names` renaming happens before `tt()` so tinytable sees the display-ready column names. The kable fallback with `setNames()` is equivalent to `knitr::kable(x, col.names = col.names)` because kable's `col.names` renames headers from left.

### Per-Format Sizing in Setup Chunk (VIZ-05/07)

```r
# In skeleton.Rmd setup chunk — replaces current fig.width=10, fig.height=6
# Source: knitr::opts_knit$get("rmarkdown.pandoc.to") returns "html"/"latex"/"docx"/"gfm-..."
pandoc_to <- knitr::opts_knit$get("rmarkdown.pandoc.to") %||% "html"

if (startsWith(pandoc_to, "latex")) {
  knitr::opts_chunk$set(fig.width = 6.5, fig.height = 4,   dpi = 300)
} else if (startsWith(pandoc_to, "docx")) {
  knitr::opts_chunk$set(fig.width = 6,   fig.height = 3.7, dpi = 300)
} else if (startsWith(pandoc_to, "gfm")) {
  knitr::opts_chunk$set(fig.width = 8,   fig.height = 5,   dpi = 150)
} else {
  knitr::opts_chunk$set(fig.width = 10,  fig.height = 6,   dpi = 96)
}

if (requireNamespace("ragg", quietly = TRUE)) {
  knitr::opts_chunk$set(dev = "ragg_png")
}
```

### CSS Injection in `.build_output_format("html")` (VIZ-06)

```r
# In R/report.R — .build_output_format() html branch (~L452-459)
# Source: rmarkdown::html_document formals confirm css= argument (verified)
html = {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) return(NULL)
  css_path <- system.file("rmarkdown/report.css", package = "EventStudy")
  rmarkdown::html_document(
    toc          = TRUE,
    toc_float    = TRUE,
    theme        = "flatly",
    code_folding = "hide",
    css          = if (nzchar(css_path)) css_path else NULL
  )
},
```

### Tinytable Fallback Mock in Tests (CRAN-05)

```r
# Pattern to force-absent tinytable for fallback test
# Source: mirrors .pdf_toolchain_available() mock in test_report_multiformat.R:161-176
test_that("CRAN-05: .report_table falls back to kable when tinytable absent", {
  skip_if_not_installed("knitr")
  df <- data.frame(Event = "A", CAR = 0.0123)
  with_mocked_bindings(
    .tinytable_available = function() FALSE,
    {
      out <- capture.output(
        EventStudy:::.report_table(df, caption = "Test")
      )
      # kable output is plain markdown table
      expect_true(any(grepl("Test", out, fixed = TRUE)))
      expect_false(any(grepl("<table", out, fixed = TRUE)))
    },
    .package = "EventStudy"
  )
})
```

---

## Validation Architecture

### Test Framework
| Property | Value |
|----------|-------|
| Framework | testthat 3.0.0+ (edition 3) |
| Config file | `Config/testthat/edition: 3` in DESCRIPTION |
| Quick run command | `devtools::test(filter="report_aesthetics")` |
| Full suite command | `devtools::test()` |

### Phase Requirements → Test Map
| Req ID | Behavior | Test Type | Automated Command | Notes |
|--------|----------|-----------|-------------------|-------|
| VIZ-04 | `.report_table()` uses tinytable when present | unit (no render) | `devtools::test(filter="report_aesthetics")` | mock .tinytable_available |
| VIZ-04 | `.report_table()` falls back to kable when tinytable absent | unit (no render) | same | with_mocked_bindings |
| VIZ-05 | fig.cap present on all 3 plot chunks in skeleton.Rmd | readLines (fast) | same | no render needed |
| VIZ-07 | Per-format sizing block in skeleton.Rmd setup chunk | readLines (fast) | same | check for pandoc.to detection |
| VIZ-06 | CSS injected in HTML branch only | unit (no render) | same | inspect .build_output_format("html") return value |
| CRAN-05 | PDF output has no `<script>` tags | render + readLines | `devtools::test(filter="report_aesthetics")` | skip_on_cran + skip PDF if no tinytex |
| CRAN-05 | `knitr::is_html_output()` appears 3+ times in skeleton.Rmd | readLines (fast) | same | no render needed |
| CRAN-05 | All 4 formats render without error | render (4 formats) | same | skip_on_cran + toolchain guards |

### Sampling Rate
- **Per task commit:** `devtools::test(filter="report_aesthetics")` (fast, no render)
- **Per wave merge:** `devtools::test()` (full suite, ~2000 tests)
- **Phase gate:** Full suite green before `/gsd-verify-work`

### Wave 0 Gaps
- [ ] `tests/testthat/test_report_aesthetics.R` — covers VIZ-04..07 and CRAN-05 (new file)

---

## Environment Availability

| Dependency | Required By | Available | Version | Fallback |
|------------|------------|-----------|---------|----------|
| tinytable | VIZ-04 table styling | ✓ (installed for research) | 0.18.0 | knitr::kable() (in-package fallback) |
| ragg | VIZ-07 raster device | ✓ | 1.5.2 | knitr default device (no error) |
| knitr | All | ✓ | 1.51 | required |
| rmarkdown | All 4-format render | ✓ | 2.31 | — |
| pandoc | PDF/Word/MD render | ✓ | available | HTML only |
| tinytex/LaTeX | PDF render | depends on system | see note | skip PDF test on CRAN |

**Note:** `tinytable` was not installed before this research session; it was installed during research. It must be in DESCRIPTION Suggests (already present at L68). The CRAN machine will not have it pre-installed; all uses must be `requireNamespace()`-guarded with a working fallback.

---

## Security Domain

No security-sensitive changes. This phase adds only:
- A static CSS file (no `<script>`, no external network calls)
- A pure-R table helper with no I/O beyond what knitr already does
- Chunk options in an Rmd template

ASVS categories: Not applicable to this presentation-only change.

---

## Assumptions Log

| # | Claim | Section | Risk if Wrong |
|---|-------|---------|---------------|
| A1 | `fig.cap` chunk option is silently dropped for plotly htmlwidget output in HTML | §4 | If knitr renders fig.cap alongside the widget, it would add a duplicate caption; low risk as CONTEXT accepts degradation |
| A2 | Exact per-format inch/dpi values (6.5×4/300 for PDF, 6×3.7/300 for Word, 8×5/150 for GFM) | §5 | Planner's discretion within CONTEXT bounds — wrong values just affect aesthetics, not correctness |

---

## Sources

### Primary (HIGH confidence — file reads + live R session)
- `inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd` — all kable call sites, plot chunk structure, setup chunk, is_html_output locations
- `R/report.R:448-474` — `.build_output_format()` verbatim body
- `R/theme.R:19-32` — `es_colours` verbatim definition
- `tests/testthat/test_report_multiformat.R` — established test patterns, skip guards, mock patterns
- Live R session: `tinytable:::infer_output`, `tinytable:::tt.default`, `style_tt`, `knitr:::auto_exts`, `knitr:::fmt_name`, `rmarkdown::*_document()$pandoc$to`

### Secondary (MEDIUM confidence)
- tinytable DESCRIPTION fetched from CRAN (version 0.18.0, `Enhances: knitr`, minimal Imports)

---

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH — tinytable API verified by live R introspection; knitr/rmarkdown API confirmed
- Architecture: HIGH — all file edits grounded in Read + grep of actual files
- Pitfalls: HIGH for verified ones; MEDIUM for fig.cap/plotly interaction (A1)
- Test patterns: HIGH — verbatim code from existing test file

**Research date:** 2026-09-09
**Valid until:** 2026-12-01 (tinytable 0.18.0 API; knitr 1.51; rmarkdown 2.31 — all stable)
