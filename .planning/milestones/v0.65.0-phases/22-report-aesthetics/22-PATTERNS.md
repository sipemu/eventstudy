# Phase 22: Report Aesthetics - Pattern Map

**Mapped:** 2026-09-09
**Files analyzed:** 4 (2 modified, 1 created, 1 created)
**Analogs found:** 4 / 4

## File Classification

| New/Modified File | Role | Data Flow | Closest Analog | Match Quality |
|---|---|---|---|---|
| `R/report.R` (add `.tinytable_available`, `.report_table`) | utility/helper | request-response | `R/report.R` `.pdf_toolchain_available` + `.word_toolchain_available` (L404-427) | exact — same file, same `@noRd` predicate + guarded-helper pattern |
| `R/report.R` (modify `.build_output_format` html branch, L452-459) | config/factory | request-response | `R/report.R` `.build_output_format` (L448-474) | exact — same function, surgical arg addition |
| `inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd` (setup chunk + 8 kable sites + 3 fig.cap + steelblue fix) | template | transform | `inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd` (L26-61, L130, L158, L218, L247, L277, L305, L328, L379) | exact — same file, surgical replacement |
| `inst/rmarkdown/report.css` (new) | config/static asset | — | `inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd` (inst/ convention) + `R/theme.R` (palette tokens) | partial — same inst/ shipping convention; palette from theme.R |
| `tests/testthat/test_report_aesthetics.R` (new) | test | request-response | `tests/testthat/test_report_multiformat.R` (L60-81, L147-188, L344-366) | exact — same skip-guard, mock, readLines assertion pattern |

---

## Pattern Assignments

### `R/report.R` — new `.tinytable_available()` predicate

**Analog:** `R/report.R:404-409` — `.pdf_toolchain_available()`

**Pattern (lines 395-409):**
```r
# ---------------------------------------------------------------------------
# .pdf_toolchain_available() -- check if a PDF/LaTeX toolchain is available
# @return Logical scalar.
# @noRd
# ---------------------------------------------------------------------------

.pdf_toolchain_available <- function() {
  if (requireNamespace("tinytex", quietly = TRUE)) {
    return(isTRUE(tinytex::is_tinytex()))
  }
  nzchar(Sys.which("pdflatex"))
}
```

**Copy this pattern for `.tinytable_available()`:**
- Same `# ---` comment block with `# @return Logical scalar.` and `# @noRd`
- Body is a one-liner: `requireNamespace("tinytable", quietly = TRUE)`
- No fallback needed (unlike pdf_toolchain which has a Sys.which fallback) — tinytable is either installed or not
- Place immediately before `.report_table()` in `R/report.R`, after `.build_output_format()` (i.e., after L474)

**Insertion point in R/report.R:** After line 474 (end of `.build_output_format`).

---

### `R/report.R` — new `.report_table()` helper

**Analog:** `R/export.R:158-163` — `.export_xlsx()` (requireNamespace-guarded helper, `@noRd`)

**requireNamespace guard pattern (export.R:158-163):**
```r
#' @noRd
.export_xlsx <- function(tables, file, ...) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required for Excel export. ",
         "Install it with: install.packages('openxlsx')")
  }
  ...
}
```

**Adapted for `.report_table()` — uses predicate instead of inline guard:**
```r
# ---------------------------------------------------------------------------
# .report_table() -- render a data frame as a styled table in all 4 formats
#
# When tinytable is available (see .tinytable_available()), renders via
# tinytable::tt() with bold header, auto-aligned columns, and optional digits.
# Falls back to knitr::kable() when tinytable is absent — byte-identical to
# the current scattered knitr::kable() calls.
#
# @param x         data.frame to render.
# @param caption   Character scalar caption, or NULL.
# @param col.names Character vector of display column names, or NULL. When
#                  supplied, x is renamed before tt()/kable() so both paths
#                  show the display names.
# @param digits    Integer, or NULL. Passed to tt() and knitr::kable().
#                  Pass NULL for pre-sprintf()-formatted tables (L130, L158,
#                  L328 call sites).
# @return invisible(NULL). Side-effect: prints the table via knit_print.
# @noRd
# ---------------------------------------------------------------------------

.report_table <- function(x, caption = NULL, col.names = NULL, digits = NULL) {
  if (!is.null(col.names)) {
    x <- setNames(x, col.names)
  }
  if (.tinytable_available()) {
    align <- ifelse(vapply(x, is.numeric, logical(1L)), "r", "l")
    tbl <- tinytable::tt(x, caption = caption, digits = digits)
    tbl <- tinytable::style_tt(tbl, i = 0L, bold = TRUE)
    for (j_idx in seq_along(align)) {
      tbl <- tinytable::style_tt(tbl, j = j_idx, align = align[[j_idx]])
    }
    print(tbl)
  } else {
    args <- list(x = x, caption = caption)
    if (!is.null(digits)) args$digits <- digits
    print(do.call(knitr::kable, args))
  }
  invisible(NULL)
}
```

**Key decisions baked in:**
- `col.names` rename happens before both branches (avoids kable-vs-tinytable discrepancy)
- `digits = NULL` default + conditional pass-through preserves the exact per-site behavior (L130/L158/L328 pass no digits; L247/L277/L305/L379 pass `digits = 4`)
- `print()` called internally; all 8 call sites become bare `.report_table(...)` with no wrapping `print()`
- `invisible(NULL)` return — consistent with `R/report.R` helper conventions

**Insertion point:** After `.tinytable_available()`, still after L474.

---

### `R/report.R` — modify `.build_output_format()` html branch (L452-459)

**Analog:** `R/report.R:448-474` — current `.build_output_format()` (read in full above)

**Current html branch (L451-459):**
```r
html = {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) return(NULL)
  rmarkdown::html_document(
    toc       = TRUE,
    toc_float = TRUE,
    theme     = "flatly",
    code_folding = "hide"
  )
},
```

**Modified html branch — add `css =` (surgical addition at L457):**
```r
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

**Anchor:** Edit only lines 452-459. The `pdf`, `word`, `md` branches (L460-473) are untouched. `system.file()` returning `""` is safe per RESEARCH §3 verification.

---

### `inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd` — setup chunk (L26-41)

**Analog:** `skeleton.Rmd:26-41` — current `opts_chunk$set()` block + `params$fig_path` pattern at L34-41

**Current setup (L27-33):**
```r
knitr::opts_chunk$set(
  echo    = FALSE,
  message = FALSE,
  warning = FALSE,
  fig.width  = 10,
  fig.height = 6
)
```

**Replacement — per-format sizing + ragg guard, inserted after the current `opts_chunk$set()` block:**
```r
knitr::opts_chunk$set(
  echo    = FALSE,
  message = FALSE,
  warning = FALSE
)
# Per-format figure sizing (VIZ-05/07)
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
# ragg anti-aliased raster device when available (VIZ-07, CRAN-05)
if (requireNamespace("ragg", quietly = TRUE)) {
  knitr::opts_chunk$set(dev = "ragg_png")
}
```

**Pattern derivation:** `%||%` already imported via `library(EventStudy)` at L42. The `startsWith()` approach is robust to the `"gfm-yaml_metadata_block"` suffix confirmed by RESEARCH §5.

---

### `inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd` — 8 kable call sites

**Analog:** current kable calls at L130, L158, L218, L247, L277, L305, L328, L379

**Substitution table** (the `digits=` column drives the `.report_table()` call):

| Line | Current call | `.report_table()` replacement | digits? |
|------|-------------|-------------------------------|---------|
| L130-131 | `print(knitr::kable(overview_tbl, col.names=c("Parameter","Value"), caption="Study design parameters"))` | `.report_table(overview_tbl, caption="Study design parameters", col.names=c("Parameter","Value"))` | NULL (pre-formatted Value col) |
| L158 | `print(knitr::kable(diag_tbl, caption="Estimation window model fit"))` | `.report_table(diag_tbl, caption="Estimation window model fit")` | NULL (pre-formatted) |
| L218-221 | `print(knitr::kable(res_tbl, col.names=c(...), caption="Abnormal and cumulative abnormal returns"))` | `.report_table(res_tbl, caption="Abnormal and cumulative abnormal returns", col.names=c(...))` | NULL |
| L247-248 | `print(knitr::kable(task$results$coefficients, caption="Event-Time Coefficients", digits=4))` | `.report_table(task$results$coefficients, caption="Event-Time Coefficients", digits=4L)` | 4 |
| L277 | `print(knitr::kable(stat_tbl, digits=4))` | `.report_table(stat_tbl, digits=4L)` | 4 |
| L305-309 | `print(knitr::kable(diag_tbl, caption="Per-event estimation-window diagnostics", col.names=c(...), digits=4))` | `.report_table(diag_tbl, caption="Per-event estimation-window diagnostics", col.names=c(...), digits=4L)` | 4 |
| L328-329 | `print(knitr::kable(cs_tbl, caption="Cross-sectional diagnostics", col.names=c("Diagnostic","Value")))` | `.report_table(cs_tbl, caption="Cross-sectional diagnostics", col.names=c("Diagnostic","Value"))` | NULL (pre-formatted) |
| L379-380 | `knitr::kable(diag_data, digits=4)` (no `print()`) | `.report_table(diag_data, digits=4L)` | 4 |

**Note on L379:** The bare call (no `print()`) is safe to replace with `.report_table()` because the helper calls `print()` internally and returns `invisible(NULL)`, which knitr handles correctly in `results='asis'` context.

---

### `inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd` — 3 plot chunks + steelblue fix

**Analog:** `skeleton.Rmd:225-240` (AR/CAR chunk), `L250-264` (panel chunk), `L332-354` (sigma histogram chunk)

**fig.cap addition pattern** — chunk header only, body untouched:

| Chunk (approx line) | Current header | New header |
|---|---|---|
| AR/CAR plot (~L225) | `` ```{r results-plot, results='asis', ...} `` | Add `, fig.cap="Abnormal and cumulative abnormal returns around the event date."` |
| Panel plot (~L250) | `` ```{r panel-plot, results='asis', ...} `` | Add `, fig.cap="Event-time coefficients with confidence bands."` |
| Sigma histogram (~L332) | `` ```{r sigma-hist, results='asis', ...} `` | Add `, fig.cap="Distribution of estimation-window residual sigma across events."` |

**steelblue fix — exact location:** `skeleton.Rmd:338`

Current:
```r
fill = "steelblue"
```
Replacement:
```r
fill = es_colours[["primary"]]
```

`es_colours` is available because `library(EventStudy)` is in the setup chunk (L42). `es_colours[["primary"]]` resolves to `"#2563eb"` per `R/theme.R:19-20`.

---

### `inst/rmarkdown/report.css` (new static asset)

**Analog (inst/ shipping convention):** `inst/rmarkdown/templates/event_study_report/template.yaml` and `skeleton.Rmd` — both plain text files in `inst/` with no special registration required; `system.file("rmarkdown/report.css", package="EventStudy")` resolves after `R CMD INSTALL`.

**Palette tokens from `R/theme.R:19-32`:**
```r
es_colours <- c(
  primary   = "#2563eb",   # brand primary — links, accent rules, header emphasis
  reference = "#6b7280",   # muted grey — secondary text, borders
  ...
)
```
**Phase-20 locked tokens:** `#2563eb` (primary), `#0f172a` (foreground), `#ffffff` (background)

**CSS scope (brand-aligned, layers on flatly bslib theme):**
- Body typography: Inter → system-ui → sans-serif fallback stack; JetBrains Mono → Consolas → monospace for `code`/`pre`
- Heading colour: `#0f172a`; link/accent colour: `#2563eb`
- Table: `thead` background subtle `#f1f5f9`, bold header, `#2563eb` bottom border on `thead tr`, right-align `.tinytable-col-numeric` if tinytable adds that class
- No `@import url(...)` for external fonts (offline-safe); no `<script>` content
- File is pure ASCII

**CRAN safety:** `inst/` static text file — no new NOTE. Pattern confirmed by existing `template.yaml` and `skeleton.Rmd` in the same directory.

---

### `tests/testthat/test_report_aesthetics.R` (new test file)

**Analog:** `tests/testthat/test_report_multiformat.R` — complete pattern source

**Skip guard pattern (test_report_multiformat.R:133-134, 203-204):**
```r
skip_if_not_installed("rmarkdown")
skip_if_not_installed("knitr")
skip_on_cran()
```

**with_mocked_bindings pattern (test_report_multiformat.R:161-176):**
```r
with_mocked_bindings(
  .pdf_toolchain_available = function() FALSE,
  {
    expect_message(result <- generate_report(...), regexp = "skipping 'pdf'")
    expect_true("html" %in% names(result))
    expect_false("pdf" %in% names(result))
  },
  .package = "EventStudy"
)
```

**readLines + grepl assertion pattern (test_report_multiformat.R:344-365):**
```r
result  <- suppressMessages(generate_report(task, output_file=tmp_file, format="html", sections=c("exec_summary"), provider=NULL))
content <- paste(readLines(result[["html"]], warn=FALSE), collapse="\n")
expect_true(grepl("some-expected-text", content, fixed=TRUE))
unlink(result[["html"]])
```

**skeleton.Rmd readLines pattern (test_report_multiformat.R:60-81):**
```r
skeleton_path <- system.file(
  "rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd",
  package = "EventStudy"
)
skip_if(skeleton_path == "", "skeleton.Rmd not found")
lines <- readLines(skeleton_path, warn = FALSE)
expect_true(any(grepl("knitr::is_html_output", lines, fixed = TRUE)))
```

**`.build_output_format` direct unit test pattern (test_report_multiformat.R:180-188):**
```r
result <- with_mocked_bindings(
  .pdf_toolchain_available = function() FALSE,
  EventStudy:::.build_output_format("pdf"),
  .package = "EventStudy"
)
expect_null(result)
```

**Tests to implement in `test_report_aesthetics.R`:**

| Test ID | Behavior | Pattern | Skip guards |
|---------|----------|---------|-------------|
| VIZ-04a | `.report_table()` uses tinytable when present | unit — call `.report_table()` on small df; capture.output; check for tinytable HTML/markdown markers | `skip_if_not_installed("tinytable")`; `skip_if_not_installed("knitr")` |
| VIZ-04b | `.report_table()` falls back to kable when tinytable absent | `with_mocked_bindings(.tinytable_available=function()FALSE, ...)` | `skip_if_not_installed("knitr")` |
| VIZ-04c | `.report_table()` col.names rename reaches output | unit — check column names in output | `skip_if_not_installed("knitr")` |
| VIZ-05 | `fig.cap` present on all 3 plot chunks in skeleton.Rmd | readLines + `sum(grepl("fig.cap", lines))` >= 3 | `skip_if(skel==""...)` |
| VIZ-06 | `.build_output_format("html")` return has CSS slot set | `fmt <- EventStudy:::.build_output_format("html"); expect_true(!is.null(fmt$knitr$opts_knit))` OR inspect `fmt` for css attribute | `skip_if_not_installed("rmarkdown")` |
| VIZ-07 | Per-format sizing block in skeleton.Rmd (pandoc_to detection) | readLines + `any(grepl("rmarkdown.pandoc.to", lines))` | `skip_if(skel==""...)` |
| CRAN-05a | `knitr::is_html_output()` appears 3+ times in skeleton.Rmd | readLines + `sum(grepl("knitr::is_html_output", lines))` >= 3 | `skip_if(skel==""...)` |
| CRAN-05b | PDF output has no `<script>` tags (render test) | render PDF; readLines; `expect_false(grepl("<script", content))` | `skip_on_cran()` + `skip_if_not_installed("tinytex")` |
| CRAN-05c | All 4 formats render without error | 4 render calls with `suppressMessages` | `skip_on_cran()` + toolchain guards per format |

---

## Shared Patterns

### requireNamespace guard + `@noRd` predicate helper
**Source:** `R/report.R:395-409` (`.pdf_toolchain_available`) and `R/report.R:412-427` (`.word_toolchain_available`)
**Apply to:** `.tinytable_available()` (new), any further Suggests-guarded helpers
```r
# @return Logical scalar.
# @noRd
.pdf_toolchain_available <- function() {
  if (requireNamespace("tinytex", quietly = TRUE)) {
    return(isTRUE(tinytex::is_tinytex()))
  }
  nzchar(Sys.which("pdflatex"))
}
```

### `with_mocked_bindings` for internal predicate helpers
**Source:** `tests/testthat/test_report_multiformat.R:161-176`
**Apply to:** All tests that need to force-absent an optional package (tinytable, pdf toolchain)
```r
with_mocked_bindings(
  .pdf_toolchain_available = function() FALSE,
  { ... assertions ... },
  .package = "EventStudy"
)
```

### `system.file()` for inst/ assets in format builders
**Source:** `R/report.R:448-474` (`.build_output_format`) + RESEARCH §3
**Apply to:** `report.css` path resolution in html branch
```r
css_path <- system.file("rmarkdown/report.css", package = "EventStudy")
# Guard: system.file() returns "" when not found; nzchar() is safe check
css = if (nzchar(css_path)) css_path else NULL
```

### `%||%` fallback for NULL knitr opts at REPL
**Source:** `R/EventStudy-package.R` (`%||%` imported from rlang); used throughout package
**Apply to:** Per-format sizing block in setup chunk
```r
pandoc_to <- knitr::opts_knit$get("rmarkdown.pandoc.to") %||% "html"
```

---

## No Analog Found

None. All 4 files/changes have close analogs in the existing codebase.

---

## Metadata

**Analog search scope:** `R/report.R`, `R/export.R`, `R/theme.R`, `inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd`, `tests/testthat/test_report_multiformat.R`
**Files scanned:** 5 source files + CONTEXT.md + RESEARCH.md
**Pattern extraction date:** 2026-09-09
