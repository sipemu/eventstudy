# Architecture Research

**Domain:** R package polish — brand/visual identity, report/plot aesthetics, API/message polish, docs/site polish on a mature CRAN package (EventStudy v0.65.0)
**Researched:** 2026-09-08
**Confidence:** HIGH — derived directly from live codebase inspection of all relevant files, supplemented by stable R ecosystem conventions

---

## System Overview

The existing architecture is a mature 6-layer R package. The four polish surfaces are **purely additive overlays**; none require restructuring the pipeline or changing statistical behavior.

```
+--------------------------------------------------------------------------+
|                  SURFACE A: Brand / Visual Identity                       |
|  man/figures/ (logo.svg, logo.png, hex-sticker.png) -- CRAN-shipped      |
|  pkgdown/ (extra.css, favicon.ico) -- .Rbuildignore'd, site only         |
|  _pkgdown.yml template.logo / template.assets -- site only               |
+-------------------------------+------------------------------------------+
                                |
+-------------------------------v------------------------------------------+
|                  SURFACE B: Report & Plot Aesthetics                      |
|  NEW: R/theme.R -- theme_eventstudy() + es_colours                       |
|  MODIFY: R/plotting.R -- apply theme_eventstudy() as default             |
|  MODIFY: inst/rmarkdown/skeleton.Rmd -- typography, table CSS            |
|  MODIFY: R/report.R -- html_document() options (css=)                    |
+-------------------------------+------------------------------------------+
                                |
+-------------------------------v------------------------------------------+
|                  SURFACE C: API & Message Polish                          |
|  NEW: R/conditions.R -- classed rlang conditions factory                  |
|  MODIFY: R/task.R, R/parameter_set.R, R/models.R etc. -- print methods   |
|  MODIFY: stop()/warning() -> rlang::abort()/rlang::warn() with class     |
|  MODIFY: DESCRIPTION -- lifecycle in Suggests (only if deprecation used) |
+-------------------------------+------------------------------------------+
                                |
+-------------------------------v------------------------------------------+
|                  SURFACE D: Docs & Site Polish                            |
|  MODIFY: _pkgdown.yml -- template.bslib palette, template.assets         |
|  MODIFY: pkgdown/extra.css -- eventstudy.de brand colors, typography     |
|  MODIFY: vignettes/*.Rmd and vignettes/articles/*.Rmd -- cross-links     |
|  MODIFY: README.md -- logo badge, docs badge refresh                     |
+-------------------------------+------------------------------------------+
                                |
+-------------------------------v------------------------------------------+
|              Existing Architecture (unchanged behavior)                   |
|  Pipeline: prepare -> fit -> calculate | R6 Models | Test Statistics      |
|  Advisor: es_diagnostics -> es_advise -> generate_report / es_report      |
|  Contract: .handle_degenerate() in contract.R -- untouched                |
|  Grounding guard: .validate_grounding() in advise.R -- untouched         |
+--------------------------------------------------------------------------+
```

---

## Surface A: Brand & Visual Identity

### Asset placement — the rule

**man/figures/ is the CRAN-safe, README-visible location for the logo.**
It is committed to the repo, included in the CRAN tarball, and referenced from README.md with a relative path that GitHub and pkgdown both resolve. The `pkgdown/` directory is already `.Rbuildignore`'d (confirmed in `.Rbuildignore`), which makes it the right home for site-only assets (favicon, any supplemental icon variants). The `docs/` directory is also `.Rbuildignore`'d and used for rendered output — do not put source assets there.

Concrete asset placement:

| Asset | Path | Committed | In tarball | Visible where |
|-------|------|-----------|------------|---------------|
| `logo.svg` (primary vector source) | `man/figures/logo.svg` | Yes | Yes | pkgdown navbar, README |
| `logo.png` (raster, ~240px) | `man/figures/logo.png` | Yes | Yes | pkgdown navbar fallback, README img tag |
| `hex-sticker.png` (~240px) | `man/figures/hex-sticker.png` | Yes | Yes | README badge, pkgdown home |
| `favicon.ico` (16/32px) | `pkgdown/favicon.ico` | Yes | No (.Rbuildignore'd) | pkgdown `<head>` only |
| Gallery card SVGs (existing) | `pkgdown/*.svg` | Yes | No | pkgdown gallery only |

Do NOT add `man/figures/favicon.ico` — favicon in the tarball would be a CRAN NOTE trigger for unexpected files in `man/`.

### Wiring logo into `_pkgdown.yml`

The existing `_pkgdown.yml` uses `template: bootstrap: 5` with no `logo:` or `template.assets` key. Add:

```yaml
template:
  bootstrap: 5
  math-rendering: katex
  assets: pkgdown/    # makes favicon.ico in pkgdown/ available at site root

navbar:
  logo:
    src: man/figures/logo.svg   # relative; pkgdown resolves from package root
    href: https://sipemu.github.io/eventstudy/
    alt: EventStudy logo
```

The `template.assets` key copies everything from `pkgdown/` into the built site's root, which is how `favicon.ico` reaches `<head>` automatically (pkgdown 2.x picks up `favicon.ico` from the assets directory).

### Wiring logo into README.md

Standard R package pattern — place at the top of README, before the title:

```markdown
<img src="man/figures/logo.png" align="right" height="139" alt="EventStudy logo" />
```

The `align="right"` float is the CRAN/tidyverse convention; GitHub and pkgdown homepage both honor it. `man/figures/` resolves correctly relative to the repo root in both contexts.

### CRAN tarball boundary check

The `.Rbuildignore` already excludes `^pkgdown$`, `^docs$`, `^_pkgdown\.yml$`. Logo and hex in `man/figures/` are included in the tarball — this is correct and expected (they are used by `?EventStudy` help page display and by README on CRAN). The favicon in `pkgdown/` stays out of the tarball. No new `.Rbuildignore` entries are needed beyond potentially adding the hex-sticker source file if it is generated from a separate `.R` script that should not be shipped.

---

## Surface B: Report & Plot Aesthetics

### Integration point 1 — Shared ggplot2 theme

**New file: `R/theme.R`**

The cleanest pattern for a shared publication theme is a `theme_eventstudy()` function that wraps `ggplot2::theme_minimal()` with package-specific overrides, plus a named colour palette vector. This is additive — no existing function is broken, and callers that do not use it are unaffected.

```r
# R/theme.R  (new file)

#' EventStudy ggplot2 Theme
#'
#' @param base_size Base font size. Default 12.
#' @param base_family Base font family. Default "".
#' @return A ggplot2 theme object.
#' @export
theme_eventstudy <- function(base_size = 12, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(
      plot.title    = ggplot2::element_text(size = base_size * 1.1, hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = base_size * 0.9, hjust = 0.5, color = "#6c757d"),
      axis.title    = ggplot2::element_text(size = base_size * 0.9),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      strip.text    = ggplot2::element_text(face = "bold")
    )
}

#' EventStudy Colour Palette
#'
#' Named vector of brand colours used in event study plots.
#' @export
es_colours <- c(
  primary   = "#0d6efd",   # matches existing es-tag-core in extra.css
  secondary = "#6c757d",
  success   = "#198754",
  warning   = "#fd7e14",
  danger    = "#dc3545",
  zero_line = "#495057"
)
```

**Apply in `R/plotting.R` (modify):** Replace every `ggplot2::theme_minimal()` call in `.plot_single_event()`, `.plot_multi_event()`, and `plot_diagnostics()` with `theme_eventstudy()`. The colour literals (`"steelblue"`, `"grey40"`, `"red"`) should also be replaced with `es_colours[["primary"]]` etc. This is a pure aesthetic change — existing test assertions on plot structure (not colour) remain green. The `test_plotting.R` tests do not assert on colour values, so they are safe.

**Critical: the `knitr::is_html_output()` switch must not be disturbed.** The existing plotting functions all return ggplot2 objects (not plotly). Only `plot_stocks()` returns a plotly object. The plotly-vs-ggplot2 decision happens inside `skeleton.Rmd`, not inside `plotting.R`. The `params$interactive` flag controls whether the skeleton renders plotly (HTML) or ggplot2 static (non-HTML). `theme_eventstudy()` applies only to ggplot2 objects — it has no effect on plotly traces and does not touch the HTML/non-HTML switch.

### Integration point 2 — Report HTML styling

**The report's HTML output document** is built via `rmarkdown::html_document(theme = "flatly", ...)` in `R/report.R::.build_output_format()`. To apply custom CSS to the report (typography, table styling, figure captions) without breaking PDF/Word/MD paths:

Inject a `css` argument conditionally in `html_document()`:

```r
# Inside .build_output_format(), html branch (R/report.R ~line 453)
rmarkdown::html_document(
  toc          = TRUE,
  toc_float    = TRUE,
  theme        = "flatly",
  code_folding = "hide",
  css          = system.file("rmarkdown/report.css", package = "EventStudy")
)
```

**New file: `inst/rmarkdown/report.css`** — contains typography, table, and figure-caption overrides. This file is in `inst/`, so it is CRAN-shipped (part of the package). It does not affect PDF or Word renders (the `css=` arg applies only to `html_document`).

**Modify `skeleton.Rmd`** for table styling: the skeleton's data/methods section generates deterministic tables from task metadata. These tables can be wrapped in `kableExtra::kable()` with `bootstrap_options = c("striped", "hover")` — but `kableExtra` must stay in Suggests and be guarded with `if (requireNamespace("kableExtra", quietly = TRUE))` with a plain `knitr::kable()` fallback. The fallback path preserves the byte-identical render guarantee for non-HTML.

**Interaction with per-format prose sanitiser (`.sanitise_prose()` in `report_narrative.R`):** This sanitiser operates on narrative character scalars, not on table HTML or CSS. Adding CSS does not touch the sanitiser at all.

**Interaction with grounding guard:** The grounding guard (`.validate_grounding()` in `advise.R`) operates on LLM-generated prose text, not on CSS or kable table output. Aesthetic additions to the skeleton template do not touch the guard.

### Integration point 3 — Figure captions in skeleton.Rmd

Add `fig.cap = "..."` to knitr chunk options in `skeleton.Rmd` for each plot chunk. This is skeleton-only and does not change any R function signature or behavior.

---

## Surface C: API & Message Polish

### Current state

The package uses `stop()` / `warning()` base R conditions throughout (confirmed by inspection of `task.R`, `contract.R`, `models.R`, `advise.R`). The only structured condition type is the `Advice` S3 class in `advise.R`. `rlang` is already in Imports (for `%||%` and `.data`). There are no `cli::` or `lifecycle::` calls anywhere in the current `R/` source.

### Recommended approach: classed rlang conditions, no cli dependency

**Use `rlang::abort()` and `rlang::warn()` with a class vector, not `cli`.** Rationale: `rlang` is already in Imports — zero new dependency. `cli` would be a new hard Imports entry (it cannot be Suggests-only if used inside all core functions). For a polish pass, adding `cli` as Imports is a larger decision than needed; `rlang` classed conditions give structured catchability with no new dep.

**New file: `R/conditions.R`** — condition factory functions:

```r
# R/conditions.R  (new file)

#' @noRd
.abort_bad_input <- function(msg, class = NULL, call = rlang::caller_env(), ...) {
  rlang::abort(
    message = msg,
    class   = c(class, "eventstudy_bad_input", "eventstudy_error"),
    call    = call,
    ...
  )
}

#' @noRd
.warn_degenerate <- function(msg, class = NULL, call = rlang::caller_env(), ...) {
  rlang::warn(
    message = msg,
    class   = c(class, "eventstudy_degenerate", "eventstudy_warning"),
    call    = call,
    ...
  )
}
```

**Migration strategy — additive, not big-bang.** Do not convert every `stop()` in one phase; that risks breaking tests. Instead:

1. Add `R/conditions.R` with the factory functions.
2. Convert the highest-visibility call sites: `task.R` validation errors, `contract.R` `.handle_degenerate()` warning emission, and `advise.R` grounding-guard warning. These are the user-visible messages that matter most for polish.
3. Leave internal model computation errors (`models.R` lm-failure guards) as plain `stop()`/`warning()` — they are low-visibility and the conversion risk is not worth it for polish.

**Hard constraint: `.handle_degenerate()` in `contract.R`** emits exactly one `warning()` per degenerate event. This invariant is tested. The migration from `warning()` to `rlang::warn()` preserves this because `rlang::warn()` calls `base::warning()` internally. The class vector is additive metadata — callers using `tryCatch(..., warning = ...)` still work. However, the regression tests in `test_edge_cases.R` that use `expect_warning()` may need the class added to their matchers if they check message text strictly. This is the one real integration risk in Surface C.

**Hard constraint: the grounding guard** in `advise.R:.validate_grounding()` uses `warning(msg, call. = FALSE)`. This is the drop-and-keep contract: exactly one warning, never `stop()`. Migrating to `rlang::warn()` is safe as long as `call = NULL` (equivalent to `call. = FALSE`) is used.

### Print method polish

The existing `print` methods use raw `cat()` with no alignment or separators. The `rlang` package does not help with print formatting. The recommendation is to apply a consistent header/separator pattern using only base R `cat()` calls — no new dependency. Each print method should follow:

```r
# pattern: package-level separator constant
.ES_SEP <- strrep("-", 40)

print.EventStudyXxx <- function(x, ...) {
  cat("EventStudy: <ClassName>\n")
  cat(.ES_SEP, "\n")
  # ... fields
  invisible(x)
}
```

This unifies spacing across `EventStudyTask$print()`, `ParameterSet$print()`, `print.es_diagnostics()`, `print.Advice`, `print.es_advice`, `print.es_cross_sectional`, `print.es_simulation`, and `print.EventStudySummary` (8 print surfaces across 6 files).

### Lifecycle deprecation

**`lifecycle` is not currently in DESCRIPTION.** Only add it if at least one function needs formal `deprecate_warn()` or `deprecate_soft()` signaling. For a polish pass, the approach is:

- Functions with changed signatures (if any): add `lifecycle` to Suggests (not Imports), guard with `if (requireNamespace("lifecycle", quietly = TRUE)) lifecycle::deprecate_warn(...)` else `warning(...)`.
- Functions removed entirely: use a stub that calls `lifecycle::deprecate_stop()` or plain `stop()`.
- If no signatures are actually changing in v0.65.0, skip `lifecycle` entirely — it adds a Suggests entry for no user benefit.

---

## Surface D: Docs & Site Polish

### Integration points in `_pkgdown.yml`

The existing `_pkgdown.yml` uses `template: bootstrap: 5` with no colour overrides. To align to the eventstudy.de brand:

```yaml
template:
  bootstrap: 5
  math-rendering: katex
  bslib:
    primary: "#0d6efd"
    link-color: "#0d6efd"
    font-size-base: "0.95rem"
    # Keep secondary, success, warning, danger aligned to
    # existing es-tag-* classes in pkgdown/extra.css
  assets: pkgdown/

navbar:
  logo:
    src: man/figures/logo.svg
    href: https://sipemu.github.io/eventstudy/
    alt: EventStudy logo
  bg: light
```

The `template.bslib` keys are CSS custom property overrides passed to Bootstrap 5's Sass compilation inside pkgdown. They are site-only and do not touch the CRAN tarball. These keys do not conflict with existing `extra.css` rules — the bslib overrides apply at the Sass level (affecting all generated Bootstrap utilities), while `extra.css` applies additional custom rules on top.

### Integration points in `pkgdown/extra.css`

The existing `pkgdown/extra.css` (confirmed present) already contains gallery, section-heading, and tag styles. Additions for v0.65.0:

- Typography polish: `body { font-size: 0.95rem; }` and heading-level fine-tuning.
- Navbar logo sizing: `.navbar-brand img { height: 32px; }`.
- Home page hero section: a `.es-hero` block class for the README's top section when rendered as pkgdown home.
- Numeric badge style: `.es-badge` — pill-shaped badge for key counts ("13+ models", "8+ test statistics") aligned to the eventstudy.de card layout.

None of these additions conflict with the existing gallery or tag CSS.

### Cross-link and vignette polish

The `vignettes/articles/` directory is already `.Rbuildignore`'d. The existing `_pkgdown.yml` `articles:` section already routes all 18 CRAN vignettes and pkgdown-only articles correctly. Polish work is limited to:

- Adding cross-reference links between related vignettes (`\code{\link{es_report}}` in `automated-reports.Rmd`; `\code{\link{es_advise}}` cross-links in the AI Advisor article).
- README.md refresh: add logo badge, update badges section, confirm all links resolve.
- No changes to the `articles:` or `reference:` blocks in `_pkgdown.yml` unless new articles are added.

---

## Component Responsibilities (Polish Surfaces)

| Component | File(s) | New vs Modified | Notes |
|-----------|---------|-----------------|-------|
| ggplot2 shared theme | `R/theme.R` | NEW | Exports `theme_eventstudy()`, `es_colours` |
| Colour application in plots | `R/plotting.R` | MODIFY | Replace hardcoded colour strings in 3 private helpers |
| Report HTML CSS | `inst/rmarkdown/report.css` | NEW | Injected via `html_document(css=)` only |
| Report format builder | `R/report.R:.build_output_format()` | MODIFY | Add `css=` arg to html branch only |
| Report skeleton template | `inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd` | MODIFY | Typography, kable table styling, figure captions |
| Classed conditions factory | `R/conditions.R` | NEW | `.abort_bad_input()`, `.warn_degenerate()` |
| Task print method | `R/task.R` | MODIFY | Consistent separator/header formatting |
| ParameterSet print | `R/parameter_set.R` | MODIFY | Same pattern |
| Other print methods | `R/es_diagnostics.R`, `R/advise.R`, `R/advise_offline.R`, `R/simulation.R`, `R/cross_sectional.R`, `R/task.R` (EventStudySummary) | MODIFY | 6 files, formatting-only changes |
| High-visibility stop/warning | `R/task.R`, `R/contract.R`, `R/advise.R` | MODIFY | Selected conversions to `rlang::abort()`/`rlang::warn()` with class |
| pkgdown config | `_pkgdown.yml` | MODIFY | Add `template.bslib`, `template.assets`, `navbar.logo` |
| pkgdown CSS | `pkgdown/extra.css` | MODIFY | Add hero, badge, typography, logo-size rules |
| Logo/hex assets | `man/figures/logo.svg`, `man/figures/logo.png`, `man/figures/hex-sticker.png` | NEW | CRAN-shipped via man/figures/ |
| Favicon | `pkgdown/favicon.ico` | NEW | Site-only via pkgdown/; excluded from tarball |
| README | `README.md` | MODIFY | Logo img tag, badge refresh |
| Vignette cross-links | `vignettes/*.Rmd`, `vignettes/articles/*.Rmd` | MODIFY | Cross-reference links only |

---

## Data Flow Changes

The four surfaces are **all post-computation**: no data-flow changes in the statistical pipeline.

### Plotting data flow (Surface B)

```
plot_event_study(task) or .plot_single_event() / .plot_multi_event()
    |
    v  (unchanged: extract abnormal_returns, compute CI bounds)
    |
    v  CHANGE: ggplot2::theme_minimal() -> theme_eventstudy()
    |          colour literals -> es_colours[[...]]
    v
ggplot2 object returned  (no behavior change)
```

### Report render data flow (Surface B)

```
generate_report(task, format="html", ...)
    |
    +-- .build_output_format("html")
    |       CHANGE: adds css = system.file("rmarkdown/report.css", ...)
    |
    +-- assemble_report_narrative()    <- grounding guard: UNTOUCHED
    |
    +-- rmarkdown::render(skeleton.Rmd, params = ...)
            CHANGE: skeleton adds kable styling, figure captions
            knitr::is_html_output() switch: UNTOUCHED
```

### Condition data flow (Surface C)

```
Before:  stop("task must be ...")                               -> base condition
After:   rlang::abort("task must be ...", class = "eventstudy_bad_input")

Before:  warning(msg, call. = FALSE)  <- in .handle_degenerate()
After:   rlang::warn(msg, class = "eventstudy_degenerate", call = NULL)
```

The `tryCatch()` wrappers in `report.R` and `es_report.R` that catch `error` do not need modification — `rlang::abort()` conditions are still caught by `tryCatch(..., error = function(e) ...)`.

---

## Dependency-Ordered Build Sequence

This order respects all data-flow dependencies and CRAN boundaries:

**Phase 1 — Brand/Asset Foundation (no code deps)**
- Create `man/figures/logo.svg`, `man/figures/logo.png`, `man/figures/hex-sticker.png`
- Create `pkgdown/favicon.ico`
- Modify `README.md`: add logo img tag
- Modify `_pkgdown.yml`: add `navbar.logo`, `template.assets: pkgdown/`
- Modify `pkgdown/extra.css`: add logo-size, hero, badge rules
- Verify: `pkgdown::build_site()` locally; confirm favicon appears, logo in navbar; confirm `.Rbuildignore` excludes favicon; `R CMD check --as-cran` clean

**Phase 2 — Shared Theme (must precede plot application)**
- Create `R/theme.R`: `theme_eventstudy()` + `es_colours`
- Add `@export` + roxygen docs; run `roxygen2::roxygenise()`
- Add to `_pkgdown.yml` reference section under "Plotting"
- Run `devtools::test()`: no test touches colour values, all green
- Verify: `theme_eventstudy()` available; colour palette consistent with `extra.css` es-tag-* colours

**Phase 3 — Plot Aesthetics (depends on Phase 2)**
- Modify `R/plotting.R`: apply `theme_eventstudy()` and `es_colours` in `.plot_single_event()`, `.plot_multi_event()`, `plot_diagnostics()`
- Do NOT modify `plot_stocks()` — it returns a plotly object; `theme_eventstudy()` does not apply
- Run `devtools::test()`: `test_plotting.R` tests check plot object class/structure, not colours — all green
- Verify: sample plots render with publication aesthetics

**Phase 4 — Report Aesthetics (depends on Phase 2 for colour consistency)**
- Create `inst/rmarkdown/report.css`
- Modify `R/report.R:.build_output_format()`: add `css=` to html branch
- Modify `inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd`: kable table styling (with `kableExtra` Suggests guard), figure captions
- Run `devtools::test()`: `test_report.R` tests render reports; confirm HTML output contains CSS; confirm PDF/Word paths unaffected
- Verify: `knitr::is_html_output()` behavior unchanged; per-format prose sanitiser path unchanged; grounding guard path unchanged

**Phase 5 — API/Message Polish (independent of Phases 2-4)**
- Create `R/conditions.R`
- Modify `R/task.R`, `R/contract.R`, `R/advise.R`: migrate selected `stop()`/`warning()` to classed rlang conditions
- Modify all 8 print method files: apply consistent header/separator pattern
- Run `devtools::test()`: check that `expect_warning()` tests still match (adjust class matchers if needed in `test_edge_cases.R`)
- Verify: `inherits(tryCatch(bad_call, error = identity), "eventstudy_bad_input")` is TRUE; no behavioral change on valid inputs

**Phase 6 — Docs & Site Polish (depends on Phase 1 for logo; can overlap with 5)**
- Modify `_pkgdown.yml`: add `template.bslib` colour overrides
- Modify `pkgdown/extra.css`: typography, badge, hero additions
- Modify vignettes for cross-links
- Modify README.md: badge refresh, docs link update
- Run `pkgdown::build_site()` locally; confirm all articles render, no broken links
- CI: push to main triggers `pkgdown.yaml` workflow; confirm gh-pages deploy

---

## Architecture Anti-Patterns to Avoid

### Anti-Pattern 1: Applying theme_eventstudy() inside plot_stocks()

**What people do:** Call `theme_eventstudy()` on a plotly object or pass it to plotly layout.
**Why it's wrong:** plotly objects are not ggplot2 objects; the `+` operator will throw an error at runtime. The existing tests for `plot_stocks()` would fail.
**Do this instead:** Apply `theme_eventstudy()` only in the ggplot2-returning functions (`.plot_single_event`, `.plot_multi_event`, `plot_diagnostics`). Leave `plot_stocks()` as plotly-only and style it separately via plotly's `layout()` if desired.

### Anti-Pattern 2: Putting report.css inside the skeleton/ directory with a relative path

**What people do:** Save `report.css` next to `skeleton.Rmd` and reference it with a relative path.
**Why it's wrong:** `rmarkdown::render()` is called with `input = template_path` but `output_dir` varies. Relative CSS references resolve against `input` directory only during render's intermediate step; the final HTML may not find the CSS if moved to a different output dir.
**Do this instead:** Use `system.file("rmarkdown/report.css", package = "EventStudy")` to obtain the absolute path at render time and pass it as the `css` argument to `html_document()`.

### Anti-Pattern 3: Adding cli to Imports for print method polish

**What people do:** Take a hard dependency on `cli` for pretty-printing in `print.*` methods.
**Why it's wrong:** `cli` is a substantial transitive dependency (rlang, fansi, etc.). Adding it to Imports makes every user pull it in at install time. The existing print methods are adequate; the gap is formatting consistency, not rich semantics.
**Do this instead:** Use `rlang` (already Imports) for classed conditions. Use plain `cat()` with a package-internal separator constant for formatting. Reserve `cli` consideration for a dedicated messaging overhaul milestone, not a polish pass.

### Anti-Pattern 4: Modifying the grounding guard or report_narrative.R for aesthetics

**What people do:** Edit `.validate_grounding()` or `JOINT_HYPOTHESIS_CAVEAT` to inject HTML styling or change wording.
**Why it's wrong:** The grounding guard is a correctness invariant locked by regression tests (Phase 19 hardening). Any edit risks breaking the "never render ungrounded literal" guarantee or the single-warning discipline.
**Do this instead:** All HTML styling goes into `report.css` and the skeleton template. The narrative assembler and grounding guard remain completely untouched.

### Anti-Pattern 5: Placing logo in pkgdown/ only (not man/figures/)

**What people do:** Save `logo.svg` only in `pkgdown/` and reference it from `_pkgdown.yml`.
**Why it's wrong:** `pkgdown/` is `.Rbuildignore`'d, so the logo is absent from the CRAN tarball. The `README.md` on CRAN's web interface (which does not run pkgdown) would show a broken image.
**Do this instead:** Logo in `man/figures/` (CRAN-shipped, README-accessible). Favicon and site-only overlays in `pkgdown/`.

---

## Integration Boundaries: Grounding Guard & Degenerate-Input Contract

These two invariants must not be touched by any polish surface:

| Invariant | Location | What must not change |
|-----------|----------|----------------------|
| Grounding guard | `R/advise.R:.validate_grounding()` | Drop-and-keep logic, single warning emission, never-stop contract |
| Degenerate-input contract | `R/contract.R:.handle_degenerate()` | Exactly-one-warning discipline, NA propagation, lenient/strict routing |
| JOINT_HYPOTHESIS_CAVEAT | `R/report_narrative.R:30-35` | Fixed text constant -- wording is a correctness invariant, not aesthetics |
| Narrative LLM-call budget | `R/report.R:280-285` | NARR-01: `assemble_report_narrative()` called once before format loop |
| `knitr::is_html_output()` | `skeleton.Rmd` | Controls static/interactive switch -- CSS or caption additions must not move this flag |

The condition-class migration in Surface C modifies the *form* of warnings and errors from `contract.R` and `advise.R`, but the *count* (exactly one per event), *receiver* (same `tryCatch` callers), and *behavior* (NA propagation vs stop) are entirely unchanged.

---

## Sources

- Live codebase inspection: `R/plotting.R`, `R/report.R`, `R/report_narrative.R`, `R/advise.R`, `R/contract.R`, `R/task.R`, `R/parameter_set.R`, `inst/rmarkdown/templates/event_study_report/skeleton/skeleton.Rmd`, `_pkgdown.yml`, `pkgdown/extra.css`, `.Rbuildignore`, `DESCRIPTION` (HIGH confidence -- direct file read)
- pkgdown 2.x logo/favicon conventions: `template.assets`, `navbar.logo`, `man/figures/` pattern -- widely used by tidyverse packages; usethis::use_logo places assets in man/figures/ (MEDIUM confidence -- stable ecosystem convention)
- ggplot2 `%+replace%` theme extension pattern -- standard since ggplot2 2.x, documented in `vignette("extending-ggplot2")` (MEDIUM confidence -- stable)
- rlang classed conditions pattern -- `rlang::abort()` / `rlang::warn()` with class vector, used by tidyverse packages; rlang already in Imports (MEDIUM confidence -- stable)

---

*Architecture research for: EventStudy v0.65.0 Polish milestone*
*Researched: 2026-09-08*
